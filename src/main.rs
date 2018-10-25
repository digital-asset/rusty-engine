#![allow(dead_code)]
extern crate protobuf;

use std::borrow::Borrow;
use std::env;
use std::rc::Rc;
use std::time::Instant;

mod daml_lf;
mod daml_lf_1;
mod lf;
use lf::*;

// NOTE(MH): Cloning this must remain cheap.
#[derive(Debug, Clone)]
enum Prim<'a> {
  Builtin(Builtin),
  RecCon(&'a TypeCon, &'a Vec<String>),
  RecProj(&'a TypeCon, &'a String),
  VariantCon(&'a TypeCon, &'a String),
  Lam(&'a Expr, Rc<Env<'a>>),
  Cons,
}

#[derive(Debug)]
enum Value<'a> {
  PrimLit(PrimLit),
  RecCon(&'a TypeCon, &'a Vec<String>, Vec<Rc<Value<'a>>>),
  VariantCon(&'a TypeCon, &'a String, Rc<Value<'a>>),
  Nil,
  Cons(Rc<Value<'a>>, Rc<Value<'a>>),
  PAP(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
}

fn arity(builtin: Builtin) -> usize {
  use Builtin::*;
  match builtin {
    AddInt64 => 2,
    SubInt64 => 2,
    MulInt64 => 2,
    ModInt64 => 2,
    EqualInt64 => 2,
    LeqInt64 => 2,
    GeqInt64 => 2,
    LessInt64 => 2,
    GreaterInt64 => 2,
    Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
  }
}

fn interpret<'a>(builtin: Builtin, args: &Vec<Rc<Value<'a>>>) -> Value<'a> {
  use Builtin::*;
  use Value::*;
  use PrimLit::*;
  match builtin {
    AddInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => PrimLit(Int64(i64::checked_add(*x, *y).expect("ADD Int64 failed"))),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    SubInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => PrimLit(Int64(i64::checked_sub(*x, *y).expect("SUB Int64 failed"))),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    MulInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => PrimLit(Int64(i64::checked_mul(*x, *y).expect("MUL Int64 failed"))),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    ModInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => PrimLit(Int64(i64::checked_rem(*x, *y).expect("MOD Int64 failed"))),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    EqualInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => Value::mk_bool(x == y),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    LeqInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => Value::mk_bool(x <= y),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    GeqInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => Value::mk_bool(x >= y),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    LessInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => Value::mk_bool(x < y),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    GreaterInt64 =>
      match args.as_slice() {
        [x, y] => match (x.borrow(), y.borrow()) {
          (PrimLit(Int64(x)), PrimLit(Int64(y))) => Value::mk_bool(x > y),
          _ => panic!("Expected two Int64 arguments"),
        }
        _ => panic!("Expected two Int64 arguments"),
      },
    Builtin::Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
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
    self
      .stack
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
  Fun(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
  Match(&'a Vec<Alt>),
  Let(&'a Var, &'a Expr),
}

#[derive(Debug)]
struct State<'a> {
  ctrl: Ctrl<'a>,
  env: Env<'a>,
  kont: Vec<Kont<'a>>,
}

impl<'a> Value<'a> {
  fn mk_unit() -> Self {
    Value::PrimLit(PrimLit::Unit)
  }

  fn mk_bool(b: bool) -> Self {
    Value::PrimLit(PrimLit::Bool(b))
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

  fn step(&mut self, package: &'a Package) {
    let old_ctrl = std::mem::replace(&mut self.ctrl, Ctrl::Evaluating);

    let new_ctrl = match old_ctrl {
      Ctrl::Evaluating => panic!("Control was not update after last step"),

      Ctrl::Expr(expr) => match expr {
        Expr::Var { name: _, index } => {
          let v = self.env.get(*index);
          Ctrl::Value(Rc::clone(&v))
        }

        Expr::Val { module_ref, name } => {
          let mut new_env = Env::new();
          let old_env = std::mem::replace(&mut self.env, new_env);
          self.kont.push(Kont::Dump(old_env));
          let def = package.get_value(module_ref, name);
          Ctrl::Expr(&def.expr)
        }

        Expr::Builtin(opcode) =>
          Ctrl::from_prim(Prim::Builtin(*opcode), arity(*opcode)),

        // TODO(MH): Figure out if we actually need to clone?
        Expr::PrimLit(lit) =>
          Ctrl::from_value(Value::PrimLit(lit.clone())),

        Expr::RecCon { tycon, fields, exprs } => {
          // TODO(MH): Find a less imperative way to do this.
          self.kont.reserve(exprs.len());
          for expr in exprs.iter().rev() {
            self.kont.push(Kont::Arg(expr));
          }
          Ctrl::from_prim(Prim::RecCon(tycon, fields), exprs.len())
        }

        Expr::RecProj { tycon, field, record } => {
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
          Ctrl::from_prim(Prim::Lam(body, Rc::new(self.env.clone())), params.len())
        }

        Expr::Case { scrut, alts } => {
          self.kont.push(Kont::Match(alts));
          Ctrl::Expr(scrut)
        }

        Expr::Let { binder, bound, body } => {
          self.kont.push(Kont::Let(binder, body));
          Ctrl::Expr(bound)
        }

        Expr::Nil =>
          Ctrl::from_value(Value::Nil),

        Expr::Cons { head, tail } => {
          self.kont.push(Kont::Arg(tail));
          self.kont.push(Kont::Arg(head));
          Ctrl::from_prim(Prim::Cons, 2)
        }

        Expr::Unsupported(msg) =>
          panic!("Unsupported: {}", msg)
      }

      Ctrl::Value(v) => match v.borrow() {
        Value::PAP(prim, args, 0) => match prim {
          Prim::Builtin(opcode) =>
            Ctrl::from_value(interpret(*opcode, args)),
          Prim::RecCon(tycon, fields) =>
            Ctrl::from_value(Value::RecCon(tycon, fields, args.clone())),
          Prim::RecProj(_tycon, field) => {
            if let Value::RecCon(_tycon, fields, vals) = args[0].borrow() {
              let idx = fields.iter().position(|x| x == *field).unwrap();
              Ctrl::Value(Rc::clone(vals[idx].borrow()))
            }
            else {
              panic!("RecProj not on RecCon")
            }
          }
          Prim::VariantCon(tycon, con) =>
            Ctrl::from_value(Value::VariantCon(tycon, con, Rc::clone(args[0].borrow()))),
          Prim::Lam(body, env) => {
            let env: &Env = env.borrow();
            let mut new_env = env.clone();
            new_env.push_many(args);
            let old_env = std::mem::replace(&mut self.env, new_env);
            self.kont.push(Kont::Dump(old_env));
            Ctrl::Expr(body)
          }
          Prim::Cons => {
            let head = Rc::clone(args[0].borrow());
            let tail = Rc::clone(args[1].borrow());
            Ctrl::from_value(Value::Cons(head, tail))
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
              self.kont.push(Kont::Fun(prim.clone(), args.clone(), *missing));
              Ctrl::Expr(arg)
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
                None =>
                  Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
              }
            }
            else {
              Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
            }
          }
          Kont::Match(alts) => match v.borrow() {
            Value::PrimLit(PrimLit::Bool(b1)) => {
              let alt_opt = alts.iter().find(|alt| match &alt.pattern {
                Pat::Bool(b2) => b1 == b2,
                Pat::Default => true,
                _ => false,
              });
              if let Some(alt) = alt_opt {
                Ctrl::Expr(&alt.body)
              }
              else {
                panic!("No match for {:?} in {:?}", v, alts)
              }
            }
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
              }
              else {
                panic!("No match for {:?} in {:?}", v, alts)
              }
            }
            Value::Nil => {
              let alt_opt = alts.iter().find(|alt| match &alt.pattern {
                Pat::Nil | Pat::Default => true,
                _ => false,
              });
              if let Some(alt) = alt_opt {
                Ctrl::Expr(&alt.body)
              }
              else {
                panic!("No match for {:?} in {:?}", v, alts)
              }
            }
            Value::Cons(head, tail) => {
              let alt_opt = alts.iter().find(|alt| match &alt.pattern {
                Pat::Cons(_head_var, _tail_var) => {
                  // TODO(MH): Doing side effecting stuff in the predicate is
                  // pretty bad style. Improve this when `Iterable::find_map` lands.
                  self.kont.push(Kont::Pop(2));
                  self.env.push(Rc::clone(head.borrow()));
                  self.env.push(Rc::clone(tail.borrow()));
                  true
                }
                Pat::Default => true,
                _ => false,
              });
              if let Some(alt) = alt_opt {
                Ctrl::Expr(&alt.body)
              }
              else {
                panic!("No match for {:?} in {:?}", v, alts)
              }
            }

            _ => panic!("Pattern match on non-data value"),
          },
          Kont::Let(_name, body) => {
            self.kont.push(Kont::Pop(1));
            self.env.push(Rc::clone(&v));
            Ctrl::Expr(body)
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

fn main() -> std::io::Result<()> {
  let args: Vec<String> = env::args().collect();
  let filename = &args[1];
  let package = lf::Package::load(filename)?;

  let entry_point = Expr::Val {
    module_ref: ModuleRef {
      module_name: DottedName { segments: vec![String::from("Main")] }
    },
    name: String::from("main"),
  };

  let start = Instant::now();
  let mut state = State::from_expr(&entry_point);
  let mut count = 0;
  // eprintln!("State 0: {:?}", state);
  while !state.is_final() {
    state.step(&package);
    count += 1;
    // eprintln!("State {}: {:?}", count, state);
  }
  let duration = start.elapsed();

  eprintln!("==========\nSteps: {}\nTime: {:?}\nResult: {:?}", count, duration, state.ctrl);

  Ok(())
}
