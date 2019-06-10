#![allow(dead_code)]
use std::borrow::Borrow;
use std::env;
use std::rc::Rc;
use std::time::{Duration, Instant};

mod lf;
mod protos;

use crate::lf::*;

mod i64_aux {
    pub fn checked_exp(base: i64, exponent: i64) -> i64 {
        let mut exponent = exponent;
        if exponent < 0 {
            panic!("checked_exp: negavtive exponent");
        } else {
            let mut base_opt = Some(base);
            let mut res = 1;
            while exponent > 0 {
                let base = base_opt.expect("checked_exp: overflow");
                if exponent & 1 == 1 {
                    res = i64::checked_mul(res, base).expect("checked_exp: overflow");
                }
                base_opt = i64::checked_mul(base, base);
                exponent >>= 1;
            }
            res
        }
    }
}

// NOTE(MH): Cloning this must remain cheap.
#[derive(Debug, Clone)]
enum Prim<'a> {
    Builtin(Builtin),
    RecCon(&'a TypeCon, &'a Vec<String>),
    RecProj(&'a TypeCon, &'a String),
    VariantCon(&'a TypeCon, &'a String),
    Lam(&'a Expr, Env<'a>),
}

#[derive(Debug)]
enum Value<'a> {
    Unit,
    Bool(bool),
    Int64(i64),
    Text(String),
    RecCon(&'a TypeCon, &'a Vec<String>, Vec<Rc<Value<'a>>>),
    VariantCon(&'a TypeCon, &'a String, Rc<Value<'a>>),
    Nil,
    Cons(Rc<Value<'a>>, Rc<Value<'a>>),
    None,
    Some(Rc<Value<'a>>),
    PAP(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
}

fn arity(builtin: Builtin) -> usize {
    use self::Builtin::*;
    match builtin {
        EqualBool => 2,

        AddInt64 => 2,
        SubInt64 => 2,
        MulInt64 => 2,
        DivInt64 => 2,
        ModInt64 => 2,
        ExpInt64 => 2,

        EqualInt64 => 2,
        LeqInt64 => 2,
        GeqInt64 => 2,
        LessInt64 => 2,
        GreaterInt64 => 2,

        AppendText => 2,
        ImplodeText => 1,
        ExplodeText => 1,

        EqualText => 2,
        LeqText => 2,
        GeqText => 2,
        LessText => 2,
        GreaterText => 2,

        Int64ToText => 1,
        TextToText => 1,

        Int64FromText => 1,

        Cons => 2,
        Foldr => 3,
        Foldl => 3,
        EqualList => 3,

        Some => 1,
        Error => 1,

        Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
    }
}

fn interpret<'a>(builtin: Builtin, args: &Vec<Rc<Value<'a>>>) -> Value<'a> {
    use self::Builtin::*;
    assert!(
        args.len() == arity(builtin),
        "Bad arity for builtin {:?}: {}",
        builtin,
        args.len()
    );
    match builtin {
        EqualBool => Value::Bool(args[0].as_bool() == args[1].as_bool()),

        AddInt64 => Value::Int64(i64::checked_add(args[0].as_i64(), args[1].as_i64()).unwrap()),
        SubInt64 => Value::Int64(i64::checked_sub(args[0].as_i64(), args[1].as_i64()).unwrap()),
        MulInt64 => Value::Int64(i64::checked_mul(args[0].as_i64(), args[1].as_i64()).unwrap()),
        DivInt64 => Value::Int64(i64::checked_div(args[0].as_i64(), args[1].as_i64()).unwrap()),
        ModInt64 => Value::Int64(i64::checked_rem(args[0].as_i64(), args[1].as_i64()).unwrap()),
        ExpInt64 => Value::Int64(i64_aux::checked_exp(args[0].as_i64(), args[1].as_i64())),

        EqualInt64 => Value::Bool(args[0].as_i64() == args[1].as_i64()),
        LeqInt64 => Value::Bool(args[0].as_i64() <= args[1].as_i64()),
        GeqInt64 => Value::Bool(args[0].as_i64() >= args[1].as_i64()),
        LessInt64 => Value::Bool(args[0].as_i64() < args[1].as_i64()),
        GreaterInt64 => Value::Bool(args[0].as_i64() > args[1].as_i64()),

        AppendText => {
            let mut res = args[0].as_string().clone();
            res.push_str(args[1].as_string());
            Value::Text(res)
        }
        ImplodeText => {
            let mut res = String::new();
            for val in Value::as_list_iter(&args[0]) {
                res.push_str(val.as_string());
            }
            Value::Text(res)
        }
        ExplodeText => {
            let arg: &Value = args[0].borrow();
            let mut res = Value::Nil;
            for c in arg.as_string().chars().rev() {
                res = Value::Cons(Rc::new(Value::Text(c.to_string())), Rc::new(res));
            }
            res
        }

        EqualText => Value::Bool(args[0].as_string() == args[1].as_string()),
        LeqText => Value::Bool(args[0].as_string() <= args[1].as_string()),
        GeqText => Value::Bool(args[0].as_string() >= args[1].as_string()),
        LessText => Value::Bool(args[0].as_string() < args[1].as_string()),
        GreaterText => Value::Bool(args[0].as_string() > args[1].as_string()),

        Int64ToText => Value::Text(args[0].as_i64().to_string()),
        // NOTE(MH): We handle `TextToText` special to avoid cloning.
        TextToText => panic!("Builtin::TextToText is handled in step"),

        Int64FromText => match args[0].as_string().parse() {
            Err(_) => Value::None,
            Ok(n) => Value::Some(Rc::new(Value::Int64(n))),
        },

        Cons => {
            let head = Rc::clone(args[0].borrow());
            let tail = Rc::clone(args[1].borrow());
            Value::Cons(head, tail)
        }
        Foldr => panic!("Builtin::Foldr is handled in step"),
        Foldl => panic!("Builtin::Foldl is handled in step"),
        EqualList => panic!("Builtin::EqualLit is handled in step"),

        Some => {
            let body = Rc::clone(args[0].borrow());
            Value::Some(body)
        }
        Error => {
            let msg = args[0].as_string();
            panic!("User error: {}", msg)
        }

        Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
    }
}

#[derive(Debug)]
enum Ctrl<'a> {
    Evaluating,
    Expr(&'a Expr),
    Value(Rc<Value<'a>>),
}

#[derive(Clone, Debug)]
struct Env<'a> {
    stack: Vec<Rc<Value<'a>>>,
}

impl<'a> Env<'a> {
    fn new() -> Self {
        Env { stack: Vec::new() }
    }

    fn get(&self, idx: usize) -> &Rc<Value<'a>> {
        self.stack
            .get(self.stack.len() - idx)
            .expect("Bad de Bruijn index")
    }

    fn push(&mut self, value: Rc<Value<'a>>) {
        self.stack.push(value);
    }

    fn push_many(&mut self, args: &Vec<Rc<Value<'a>>>) {
        self.stack.extend_from_slice(args);
    }

    fn pop(&mut self, count: usize) {
        let new_len = self.stack.len() - count;
        self.stack.truncate(new_len);
    }
}

#[derive(Debug)]
enum Kont<'a> {
    Dump(Env<'a>),
    Pop(usize),
    Arg(&'a Expr),
    ArgVal(Rc<Value<'a>>),
    Fun(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
    Match(&'a Vec<Alt>),
    Let(&'a Var, &'a Expr),
    EqualList(Rc<Value<'a>>, ValueListIter<'a>, ValueListIter<'a>),
}

#[derive(Debug)]
struct State<'a> {
    ctrl: Ctrl<'a>,
    env: Env<'a>,
    kont: Vec<Kont<'a>>,
}

#[derive(Debug)]
struct ValueListIter<'a> {
    value: Rc<Value<'a>>,
}

impl<'a> Iterator for ValueListIter<'a> {
    type Item = Rc<Value<'a>>;

    fn next(&mut self) -> Option<Rc<Value<'a>>> {
        match self.value.borrow() {
            Value::Nil => None,
            Value::Cons(head, tail) => {
                let head = Rc::clone(&head);
                let tail = Rc::clone(&tail);
                self.value = tail;
                Some(head)
            }
            v => panic!("Expexted list, found {:?}", v),
        }
    }
}

impl<'a> Value<'a> {
    fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Expected Bool, found {:?}", self),
        }
    }

    fn as_i64(&self) -> i64 {
        match self {
            Value::Int64(i) => *i,
            _ => panic!("Expected Int64, found {:?}", self),
        }
    }

    fn as_string(&self) -> &String {
        match self {
            Value::Text(s) => &s,
            _ => panic!("Expected Text, found {:?}", self),
        }
    }

    fn as_list_iter(this: &Rc<Self>) -> ValueListIter<'a> {
        ValueListIter {
            value: Rc::clone(this),
        }
    }
}

impl<'a> Ctrl<'a> {
    fn from_value(v: Value<'a>) -> Self {
        Ctrl::Value(Rc::new(v))
    }
    fn from_prim(prim: Prim<'a>, arity: usize) -> Self {
        Ctrl::Value(Rc::new(Value::PAP(prim, Vec::new(), arity)))
    }
}

impl<'a> State<'a> {
    fn from_expr(expr: &'a Expr) -> Self {
        State {
            ctrl: Ctrl::Expr(expr),
            env: Env::new(),
            kont: Vec::new(),
        }
    }

    fn step(&mut self, world: &'a World) {
        let old_ctrl = std::mem::replace(&mut self.ctrl, Ctrl::Evaluating);

        let new_ctrl = match old_ctrl {
            Ctrl::Evaluating => panic!("Control was not update after last step"),

            Ctrl::Expr(expr) => match expr {
                Expr::Var { name: _, index } => {
                    let v = self.env.get(*index);
                    Ctrl::Value(Rc::clone(&v))
                }

                Expr::Val { module_ref, name } => {
                    let new_env = Env::new();
                    let old_env = std::mem::replace(&mut self.env, new_env);
                    self.kont.push(Kont::Dump(old_env));
                    let def = world.get_value(module_ref, name);
                    Ctrl::Expr(&def.expr)
                }

                Expr::Builtin(opcode) => Ctrl::from_prim(Prim::Builtin(*opcode), arity(*opcode)),

                Expr::PrimLit(lit) => {
                    let val = match lit {
                        PrimLit::Unit => Value::Unit,
                        PrimLit::Bool(b) => Value::Bool(*b),
                        PrimLit::Nil => Value::Nil,
                        PrimLit::None => Value::None,
                        PrimLit::Int64(i) => Value::Int64(*i),
                        PrimLit::Text(s) => Value::Text(s.clone()),
                        PrimLit::Unsupported(msg) => panic!("PrimLit::Unsupported({})", msg),
                    };
                    Ctrl::from_value(val)
                }

                Expr::RecCon {
                    tycon,
                    fields,
                    exprs,
                } => {
                    // TODO(MH): Find a less imperative way to do this.
                    self.kont.reserve(exprs.len());
                    for expr in exprs.iter().rev() {
                        self.kont.push(Kont::Arg(expr));
                    }
                    Ctrl::from_prim(Prim::RecCon(tycon, fields), exprs.len())
                }

                Expr::RecProj {
                    tycon,
                    field,
                    record,
                } => {
                    self.kont.push(Kont::Arg(record.borrow()));
                    Ctrl::from_prim(Prim::RecProj(tycon, field), 1)
                }

                Expr::VariantCon { tycon, con, arg } => {
                    self.kont.push(Kont::Arg(arg.borrow()));
                    Ctrl::from_prim(Prim::VariantCon(tycon, con), 1)
                }

                Expr::App { fun, args } => {
                    // TODO(MH): Find a less imperative way to do this.
                    self.kont.reserve(args.len());
                    for arg in args.iter().rev() {
                        self.kont.push(Kont::Arg(arg));
                    }
                    Ctrl::Expr(fun)
                }

                Expr::Lam { params, body } => {
                    Ctrl::from_prim(Prim::Lam(body, self.env.clone()), params.len())
                }

                Expr::Case { scrut, alts } => {
                    self.kont.push(Kont::Match(alts));
                    Ctrl::Expr(scrut)
                }

                Expr::Let {
                    binder,
                    bound,
                    body,
                } => {
                    self.kont.push(Kont::Let(binder, body));
                    Ctrl::Expr(bound)
                }

                Expr::Unsupported(msg) => panic!("Unsupported: {}", msg),
            },

            Ctrl::Value(v) => match v.borrow() {
                Value::PAP(prim, args, 0) => match prim {
                    Prim::Builtin(Builtin::TextToText) => Ctrl::Value(Rc::clone(&args[0])),
                    // TODO(MH): There's plenty of room for optimizations in foldr
                    // and foldl, but let's get something simple and correct first.
                    Prim::Builtin(Builtin::Foldr) => {
                        let f = args[0].borrow();
                        let z = args[1].borrow();
                        match args[2].borrow() {
                            // foldr f z [] = z
                            Value::Nil => Ctrl::Value(Rc::clone(z)),
                            // foldr f z (x::xs) = f x (foldr f z xs)
                            Value::Cons(x, xs) => {
                                let args2 = vec![Rc::clone(f), Rc::clone(z), Rc::clone(xs)];
                                self.kont.push(Kont::ArgVal(Rc::new(Value::PAP(
                                    Prim::Builtin(Builtin::Foldr),
                                    args2,
                                    0,
                                ))));
                                self.kont.push(Kont::ArgVal(Rc::clone(x)));
                                Ctrl::Value(Rc::clone(f))
                            }
                            v => panic!("Foldr not on list: {:?}", v),
                        }
                    }
                    Prim::Builtin(Builtin::Foldl) => {
                        let f = args[0].borrow();
                        let z = args[1].borrow();
                        match args[2].borrow() {
                            // foldl f z [] = z
                            Value::Nil => Ctrl::Value(Rc::clone(z)),
                            // foldl f z (x::xs) = foldl f (f z x) xs
                            Value::Cons(x, xs) => {
                                self.kont.push(Kont::ArgVal(Rc::clone(xs)));
                                let args2 = vec![Rc::clone(f)];
                                self.kont
                                    .push(Kont::Fun(Prim::Builtin(Builtin::Foldl), args2, 2));
                                self.kont.push(Kont::ArgVal(Rc::clone(x)));
                                self.kont.push(Kont::ArgVal(Rc::clone(z)));
                                Ctrl::Value(Rc::clone(f))
                            }
                            v => panic!("Foldl not on list: {:?}", v),
                        }
                    }
                    Prim::Builtin(Builtin::EqualList) => {
                        self.kont.push(Kont::EqualList(
                            Rc::clone(args[0].borrow()),
                            Value::as_list_iter(args[1].borrow()),
                            Value::as_list_iter(args[2].borrow()),
                        ));
                        Ctrl::from_value(Value::Bool(true))
                    }
                    Prim::Builtin(opcode) => Ctrl::from_value(interpret(*opcode, args)),
                    Prim::RecCon(tycon, fields) => {
                        Ctrl::from_value(Value::RecCon(tycon, fields, args.clone()))
                    }
                    Prim::RecProj(_tycon, field) => {
                        if let Value::RecCon(_tycon, fields, vals) = args[0].borrow() {
                            let idx = fields.iter().position(|x| x == *field).unwrap();
                            Ctrl::Value(Rc::clone(vals[idx].borrow()))
                        } else {
                            panic!("RecProj not on RecCon")
                        }
                    }
                    Prim::VariantCon(tycon, con) => {
                        Ctrl::from_value(Value::VariantCon(tycon, con, Rc::clone(args[0].borrow())))
                    }
                    Prim::Lam(body, env) => {
                        let mut new_env = env.clone();
                        new_env.push_many(args);
                        let old_env = std::mem::replace(&mut self.env, new_env);
                        self.kont.push(Kont::Dump(old_env));
                        Ctrl::Expr(body)
                    }
                },

                _ => match self.kont.pop().expect("Step on final state") {
                    Kont::Dump(env) => {
                        self.env = env;
                        Ctrl::Value(Rc::clone(&v))
                    }
                    Kont::Pop(count) => {
                        self.env.pop(count);
                        Ctrl::Value(Rc::clone(&v))
                    }
                    Kont::Arg(arg) => match v.borrow() {
                        Value::PAP(prim, args, missing) => {
                            let mut args = args.clone();
                            args.reserve(*missing);
                            self.kont.push(Kont::Fun(prim.clone(), args, *missing));
                            Ctrl::Expr(arg)
                        }
                        _ => panic!("Applying value"),
                    },
                    // TODO(MH): Avoid duplication with above.
                    Kont::ArgVal(arg) => match v.borrow() {
                        Value::PAP(prim, args, missing) => {
                            self.kont
                                .push(Kont::Fun(prim.clone(), args.clone(), *missing));
                            Ctrl::Value(arg)
                        }
                        _ => panic!("Applying value"),
                    },
                    Kont::Fun(prim2, mut args2, missing2) => {
                        // NOTE(MH): We're short circuitig if the next `kont` frame is an `Arg`
                        // and we're still missing arguments. The unoptimized version would be:
                        // args2.push(Rc::clone(&v));
                        // Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
                        args2.push(Rc::clone(&v));
                        if missing2 > 1 {
                            let kont_opt = self.kont.pop();
                            match kont_opt {
                                Some(Kont::Arg(arg)) => {
                                    self.kont.push(Kont::Fun(prim2, args2, missing2 - 1));
                                    Ctrl::Expr(&arg)
                                }
                                Some(kont) => {
                                    self.kont.push(kont);
                                    Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
                                }
                                None => {
                                    Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
                                }
                            }
                        } else {
                            Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
                        }
                    }
                    Kont::Match(alts) => match v.borrow() {
                        Value::Bool(b) => Ctrl::Expr(&alts[if *b { 1 } else { 0 }].body),
                        Value::VariantCon(_tycon, con1, arg) => {
                            let alt_opt = alts.iter().find(|alt| match &alt.pattern {
                                Pat::Variant(con2, _var) if *con1 == con2 => {
                                    // TODO(MH): Doing side effecting stuff in the predicate is
                                    // pretty bad style. Improve this when `Iterable::find_map` lands.
                                    self.kont.push(Kont::Pop(1));
                                    self.env.push(Rc::clone(arg.borrow()));
                                    true
                                }
                                Pat::Default => true,
                                _ => false,
                            });
                            if let Some(alt) = alt_opt {
                                Ctrl::Expr(&alt.body)
                            } else {
                                panic!("No match for {:?} in {:?}", v, alts)
                            }
                        }
                        Value::Nil => Ctrl::Expr(&alts[0].body),
                        Value::Cons(head, tail) => {
                            let alt = &alts[1];
                            if let Pat::Cons(..) = alt.pattern {
                                self.kont.push(Kont::Pop(2));
                                self.env.push(Rc::clone(head.borrow()));
                                self.env.push(Rc::clone(tail.borrow()));
                            };
                            Ctrl::Expr(&alt.body)
                        }

                        _ => panic!("Pattern match on non-data value"),
                    },
                    Kont::Let(_name, body) => {
                        self.kont.push(Kont::Pop(1));
                        self.env.push(Rc::clone(&v));
                        Ctrl::Expr(body)
                    }
                    Kont::EqualList(eq, mut lhs, mut rhs) => {
                        if v.as_bool() {
                            match (lhs.next(), rhs.next()) {
                                (None, None) => Ctrl::from_value(Value::Bool(true)),
                                (None, Some(_)) | (Some(_), None) => {
                                    Ctrl::from_value(Value::Bool(false))
                                }
                                (Some(x), Some(y)) => {
                                    self.kont.push(Kont::EqualList(Rc::clone(&eq), lhs, rhs));
                                    self.kont.push(Kont::ArgVal(y));
                                    self.kont.push(Kont::ArgVal(x));
                                    Ctrl::Value(eq)
                                }
                            }
                        } else {
                            Ctrl::from_value(Value::Bool(false))
                        }
                    }
                },
            },
        };

        self.ctrl = new_ctrl
    }

    fn is_final(&self) -> bool {
        match self.ctrl.borrow() {
            Ctrl::Value(v) => match v.borrow() {
                Value::PAP(..) => false,
                _ => self.kont.is_empty(),
            },
            _ => false,
        }
    }
}

const DEBUG: bool = false;

struct RunResult<'a> {
    count: i64,
    duration: Duration,
    value: Rc<Value<'a>>,
}

fn make_entry_point(world: &World) -> Expr {
    Expr::Val {
        module_ref: ModuleRef {
            package_id: world.main.clone(),
            module_name: DottedName {
                segments: vec![String::from("Main")],
            },
        },
        name: String::from("main"),
    }
}

fn run<'a>(world: &'a lf::World, entry_point: &'a Expr) -> RunResult<'a> {
    let start = Instant::now();
    let mut state = State::from_expr(&entry_point);
    let mut count = 0;
    // eprintln!("State 0: {:?}", state);
    while !state.is_final() {
        state.step(&world);
        count += 1;
        // eprintln!("State {}: {:?}", count, state);
    }
    let duration = start.elapsed();
    let value = match state.ctrl {
        Ctrl::Value(v) => v,
        _ => panic!("IMPOSSIBLE: final control is always a value"),
    };

    RunResult {
        count,
        duration,
        value,
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let world = lf::World::load(filename)?;

    let entry_point = make_entry_point(&world);
    let run_result = run(&world, &entry_point);

    println!(
        "Input:  {}\nSteps:  {}\nTime:   {:?}\nResult: {:?}",
        filename, run_result.count, run_result.duration, run_result.value,
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn queens() {
        let world = lf::World::load("test/Queens.dar").unwrap();

        let entry_point = make_entry_point(&world);
        let run_result = run(&world, &entry_point);
        assert_eq!(run_result.count, 5116403);
        match *run_result.value {
            Value::Int64(n) => assert_eq!(n, 92),
            _ => assert!(false),
        }
    }

    #[test]
    fn sort() {
        let world = lf::World::load("test/Sort.dar").unwrap();

        let entry_point = make_entry_point(&world);
        let run_result = run(&world, &entry_point);
        assert_eq!(run_result.count, 255086);
        match *run_result.value {
            Value::Int64(n) => assert_eq!(n, -487896960),
            _ => assert!(false),
        }
    }

    #[test]
    fn equal_list() {
        let world = lf::World::load("test/EqualList.dar").unwrap();

        let entry_point = make_entry_point(&world);
        let run_result = run(&world, &entry_point);
        assert_eq!(run_result.count, 885);
        match *run_result.value {
            Value::Bool(b) => assert!(b),
            _ => assert!(false),
        }
    }
}
