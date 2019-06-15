// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::FnvHashSet;
use std::rc::Rc;

use crate::ast::*;
use crate::builtin::*;
use crate::store::Store;
use crate::value::*;

#[derive(Debug)]
enum Ctrl<'a> {
    Evaluating,
    Expr(&'a Expr),
    Value(Rc<Value<'a>>),
    Error(String),
}

#[derive(Debug)]
enum Kont<'a> {
    Dump(Env<'a>),
    DumpAuth(FnvHashSet<Party>),
    Pop(usize),
    Arg(&'a Expr),
    ArgVal(Rc<Value<'a>>),
    Fun(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
    Match(&'a Vec<Alt>),
    Let(&'a Binder, &'a Expr),
    EqualList(Rc<Value<'a>>, ValueListIter<'a>, ValueListIter<'a>),
}

#[derive(Debug)]
pub struct State<'a> {
    ctrl: Ctrl<'a>,
    env: Env<'a>,
    kont: Vec<Kont<'a>>,
    auth: FnvHashSet<Party>,
    time: Time,
}

impl<'a> Ctrl<'a> {
    fn from_value(v: Value<'a>) -> Self {
        Ctrl::Value(Rc::new(v))
    }

    fn from_prim(prim: Prim<'a>, arity: usize) -> Self {
        Ctrl::Value(Rc::new(Value::PAP(prim, Vec::new(), arity)))
    }

    fn catch<F>(f: F) -> Self
    where
        F: FnOnce() -> Result<Self, String>,
    {
        match f() {
            Err(msg) => Ctrl::Error(msg),
            Ok(ctrl) => ctrl,
        }
    }
}

impl<'a> State<'a> {
    pub fn init(expr: &'a Expr) -> Self {
        State {
            ctrl: Ctrl::Expr(expr),
            env: Env::new(),
            kont: vec![Kont::ArgVal(Rc::new(Value::Token))],
            auth: FnvHashSet::default(),
            time: Time::EPOCH,
        }
    }

    /// Step when the control contains an expression.
    fn step_expr(&mut self, world: &'a World, ctrl_expr: &'a Expr) -> Ctrl<'a> {
        match ctrl_expr {
            Expr::Var { index, .. } => {
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
                self.kont.extend(exprs.iter().rev().map(Kont::Arg));
                Ctrl::from_prim(Prim::RecCon(tycon, fields), exprs.len())
            }

            Expr::RecProj {
                tycon,
                field,
                record,
            } => {
                self.kont.push(Kont::Arg(&record));
                Ctrl::from_prim(Prim::RecProj(tycon, field), 1)
            }

            Expr::RecUpd {
                tycon,
                field,
                record,
                value,
            } => {
                self.kont.push(Kont::Arg(&value));
                self.kont.push(Kont::Arg(&record));
                Ctrl::from_prim(Prim::RecUpd(tycon, field), 2)
            }

            Expr::VariantCon { tycon, con, arg } => {
                self.kont.push(Kont::Arg(&arg));
                Ctrl::from_prim(Prim::VariantCon(tycon, con), 1)
            }

            Expr::App { fun, args } => {
                self.kont.extend(args.iter().rev().map(Kont::Arg));
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

            Expr::Create {
                template_ref,
                payload,
            } => {
                self.kont.push(Kont::Arg(payload));
                Ctrl::from_prim(Prim::CreateCall(template_ref), 1)
            }
            Expr::Fetch {
                template_ref,
                contract_id,
            } => {
                self.kont.push(Kont::Arg(contract_id));
                Ctrl::from_prim(Prim::Fetch(template_ref), 1)
            }
            Expr::Exercise {
                template_ref,
                choice,
                contract_id,
                arg,
            } => {
                // TODO(MH): Lookup template and choice here instead of
                // multiple times further down.
                self.kont.push(Kont::Arg(arg));
                self.kont.push(Kont::Arg(contract_id));
                Ctrl::from_prim(Prim::ExerciseCall(template_ref, choice), 2)
            }
            Expr::Submit {
                should_succeed,
                submitter,
                update,
            } => {
                self.kont.push(Kont::Arg(update));
                self.kont.push(Kont::Arg(submitter));
                let prim = Prim::Submit {
                    should_succeed: *should_succeed,
                };
                Ctrl::from_prim(prim, 2)
            }

            Expr::Unsupported(msg) => panic!("Unsupported: {}", msg),
        }
    }

    /// Step when control contains a fully applied primitive.
    fn step_saturated_prim(
        &mut self,
        world: &'a World,
        store: &mut Store<'a>,
        prim: &Prim<'a>,
        args: &[Rc<Value<'a>>],
    ) -> Ctrl<'a> {
        match prim {
            Prim::Builtin(Builtin::TextToText) => Ctrl::Value(Rc::clone(&args[0])),
            Prim::Builtin(Builtin::GetTime) => Ctrl::from_value(Value::Time(self.time)),
            Prim::Builtin(Builtin::AdvanceTime) => {
                let delta = args[0].as_i64();
                let time = Time::from_micros_since_epoch(
                    i64::checked_add(self.time.to_micros_since_epoch(), delta).unwrap(),
                );
                self.time = time;
                Ctrl::from_value(Value::Time(time))
            }
            // TODO(MH): There's plenty of room for optimizations in foldr
            // and foldl, but let's get something simple and correct first.
            Prim::Builtin(Builtin::Foldr) => {
                let f = &args[0];
                let z = &args[1];
                match &*args[2] {
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
                let f = &args[0];
                let z = &args[1];
                match &*args[2] {
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
                    Rc::clone(&args[0]),
                    args[1].as_list(),
                    args[2].as_list(),
                ));
                Ctrl::from_value(Value::Bool(true))
            }
            Prim::Builtin(opcode) => Ctrl::catch(|| {
                let value = interpret(*opcode, args)?;
                Ok(Ctrl::from_value(value))
            }),
            Prim::RecCon(tycon, fields) => {
                Ctrl::from_value(Value::RecCon(tycon, fields, args.to_vec()))
            }
            Prim::RecProj(_tycon, field) => {
                if let Value::RecCon(_tycon, fields, vals) = &*args[0] {
                    let idx = fields.iter().position(|x| x == *field).unwrap();
                    Ctrl::Value(Rc::clone(&vals[idx]))
                } else {
                    panic!("RecProj not on RecCon")
                }
            }
            Prim::RecUpd(_tycon, field) => {
                if let Value::RecCon(tycon, fields, vals) = &*args[0] {
                    let idx = fields.iter().position(|x| x == *field).unwrap();
                    let mut vals = vals.clone();
                    vals[idx] = Rc::clone(&args[1]);
                    Ctrl::from_value(Value::RecCon(tycon, fields, vals))
                } else {
                    panic!("RecUpd not on RecCon")
                }
            }
            Prim::VariantCon(tycon, con) => {
                Ctrl::from_value(Value::VariantCon(tycon, con, Rc::clone(&args[0])))
            }
            Prim::Lam(body, env) => {
                let mut new_env = env.clone();
                new_env.push_many(args);
                let old_env = std::mem::replace(&mut self.env, new_env);
                self.kont.push(Kont::Dump(old_env));
                Ctrl::Expr(body)
            }

            Prim::CreateCall(template_ref) => {
                let payload = Rc::clone(&args[0]);
                let template = world.get_template(template_ref);

                let mut new_env = Env::new();
                new_env.push(payload);
                let old_env = std::mem::replace(&mut self.env, new_env);
                self.kont.push(Kont::Dump(old_env));
                self.kont.push(Kont::Arg(&template.signatories));
                self.kont.push(Kont::Arg(&template.precondtion));
                Ctrl::from_prim(Prim::CreateExec(template_ref), 2)
            }
            Prim::CreateExec(template_ref) => {
                let payload = self.env.pop();

                let precondtion: bool = args[0].as_bool();
                let signatories: FnvHashSet<Party> = args[1]
                    .as_list()
                    .map(|value| value.as_party().clone())
                    .collect();
                if !precondtion {
                    Ctrl::Error(format!(
                        "Template pre-condition violated for {}: {:?}",
                        template_ref, payload
                    ))
                } else if !signatories.is_subset(&self.auth) {
                    Ctrl::Error(format!(
                        "authorization missing for {}: {:?}",
                        template_ref, payload
                    ))
                } else {
                    let contract_id = store.create(template_ref, payload, signatories);
                    Ctrl::from_value(Value::ContractId(contract_id))
                }
            }
            Prim::Fetch(template_ref) => Ctrl::catch(|| {
                let contract_id = args[0].as_contract_id();
                let contract = store.fetch(template_ref, contract_id)?;
                Ok(Ctrl::Value(Rc::clone(&contract.payload)))
            }),
            Prim::ExerciseCall(template_ref, choice_name) => Ctrl::catch(|| {
                let template = world.get_template(template_ref);
                let choice = template.choices.get::<String>(choice_name).unwrap();

                let contract_id = &args[0];
                let contract = store.fetch(template_ref, contract_id.as_contract_id())?;
                let payload = Rc::clone(&contract.payload);
                let arg = Rc::clone(&args[1]);

                let mut new_env = Env::new();
                new_env.push(payload);
                new_env.push(arg);
                let old_env = std::mem::replace(&mut self.env, new_env);
                self.kont.push(Kont::Dump(old_env));
                self.kont.push(Kont::ArgVal(Rc::clone(contract_id)));
                self.kont.push(Kont::Arg(&choice.controllers));
                Ok(Ctrl::from_prim(
                    Prim::ExerciseExec(template_ref, choice_name),
                    2,
                ))
            }),
            Prim::ExerciseExec(template_ref, choice_name) => Ctrl::catch(|| {
                let template = world.get_template(template_ref);
                let choice = template.choices.get::<String>(choice_name).unwrap();

                let controllers: FnvHashSet<Party> = args[0]
                    .as_list()
                    .map(|value| value.as_party().clone())
                    .collect();
                let contract_id = &args[1];
                let arg = self.env.pop();

                if !controllers.is_subset(&self.auth) {
                    Err(format!(
                        "authorization missing for {}@{}: {:?} {:?}",
                        template_ref,
                        choice_name,
                        contract_id.as_contract_id(),
                        arg,
                    ))
                } else {
                    // TODO(MH): Avoid fetching contract a second time.
                    let contract = store.fetch(template_ref, contract_id.as_contract_id())?;
                    let mut new_auth: FnvHashSet<Party> = controllers;
                    new_auth.extend(contract.signatories.iter().cloned());

                    if choice.consuming {
                        store.archive(template_ref, contract_id.as_contract_id())?;
                    }

                    self.env.push(Rc::clone(contract_id));
                    self.env.push(arg);

                    let old_auth = std::mem::replace(&mut self.auth, new_auth);
                    self.kont.push(Kont::DumpAuth(old_auth));
                    self.kont.push(Kont::ArgVal(Rc::new(Value::Token)));
                    Ok(Ctrl::Expr(&choice.consequence))
                }
            }),
            Prim::Submit { should_succeed } => {
                let submitter = args[0].as_party().clone();
                let mut auth = FnvHashSet::default();
                auth.insert(submitter);
                let update = Rc::clone(&args[1]);
                let state = State {
                    ctrl: Ctrl::Value(update),
                    env: Env::new(),
                    kont: vec![Kont::ArgVal(Rc::new(Value::Token))],
                    auth,
                    time: self.time,
                };
                let result = state.run(world, store);
                match result {
                    Ok(value) => {
                        if *should_succeed {
                            store.commit();
                            Ctrl::Value(value)
                        } else {
                            Ctrl::Error(String::from("unexpected success"))
                        }
                    }
                    Err(msg) => {
                        if *should_succeed {
                            Ctrl::Error(msg)
                        } else {
                            store.rollback();
                            Ctrl::from_value(Value::Unit)
                        }
                    }
                }
            }
        }
    }

    /// Step when the control contains a (proper) value.
    fn step_value(&mut self, value: Rc<Value<'a>>) -> Ctrl<'a> {
        let kont = self.kont.pop().expect("Step on final state");
        match kont {
            Kont::Dump(env) => {
                self.env = env;
                Ctrl::Value(Rc::clone(&value))
            }
            Kont::DumpAuth(auth) => {
                self.auth = auth;
                Ctrl::Value(Rc::clone(&value))
            }
            Kont::Pop(count) => {
                self.env.pop_many(count);
                Ctrl::Value(Rc::clone(&value))
            }
            Kont::Arg(arg) => {
                if let Value::PAP(prim, args, missing) = &*value {
                    self.kont
                        .push(Kont::Fun(prim.clone(), args.clone(), *missing));
                    Ctrl::Expr(arg)
                } else {
                    panic!("Applying value: {:?}", value)
                }
            }
            // TODO(MH): This seems inefficient. We should apply all args
            // in one go.
            Kont::ArgVal(arg) => {
                if let Value::PAP(prim, args, missing) = &*value {
                    self.kont
                        .push(Kont::Fun(prim.clone(), args.clone(), *missing));
                    Ctrl::Value(arg)
                } else {
                    panic!("Applying value")
                }
            }
            Kont::Fun(prim2, mut args2, missing2) => {
                assert!(missing2 > 0);
                // NOTE(MH): We're short circuitig if the next `kont` frame is an `Arg`
                // and we're still missing arguments. The unoptimized version would be:
                // args2.push(Rc::clone(&value));
                // Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
                args2.push(Rc::clone(&value));
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
                        None => Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1))),
                    }
                } else {
                    Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
                }
            }
            Kont::Match(alts) => match &*value {
                Value::Bool(b) => Ctrl::Expr(&alts[*b as usize].body),
                Value::VariantCon(_tycon, con1, arg) => {
                    let alt_opt = alts.iter().find_map(|alt| match &alt.pattern {
                        Pat::Variant(con2, _var) if *con1 == con2 => Some((alt, true)),
                        Pat::Default => Some((alt, false)),
                        _ => None,
                    });
                    let (alt, bind_arg) =
                        alt_opt.unwrap_or_else(|| panic!("No match for {:?} in {:?}", value, alts));
                    if bind_arg {
                        self.kont.push(Kont::Pop(1));
                        self.env.push(Rc::clone(&arg));
                    }
                    Ctrl::Expr(&alt.body)
                }
                Value::Nil => Ctrl::Expr(&alts[0].body),
                Value::Cons(head, tail) => {
                    let alt = &alts[1];
                    if let Pat::Cons(..) = alt.pattern {
                        self.kont.push(Kont::Pop(2));
                        self.env.push(Rc::clone(&head));
                        self.env.push(Rc::clone(&tail));
                    };
                    Ctrl::Expr(&alt.body)
                }
                Value::None => Ctrl::Expr(&alts[0].body),
                Value::Some(body) => {
                    let alt = &alts[1];
                    if let Pat::Some(_) = alt.pattern {
                        self.kont.push(Kont::Pop(1));
                        self.env.push(Rc::clone(&body));
                    };
                    Ctrl::Expr(&alt.body)
                }

                _ => panic!("Pattern match on non-data value"),
            },
            Kont::Let(_name, body) => {
                self.kont.push(Kont::Pop(1));
                self.env.push(Rc::clone(&value));
                Ctrl::Expr(body)
            }
            Kont::EqualList(eq, mut lhs, mut rhs) => {
                if value.as_bool() {
                    match (lhs.next(), rhs.next()) {
                        (None, None) => Ctrl::from_value(Value::Bool(true)),
                        (None, Some(_)) | (Some(_), None) => Ctrl::from_value(Value::Bool(false)),
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
        }
    }

    pub fn step(&mut self, world: &'a World, store: &mut Store<'a>) {
        let old_ctrl = std::mem::replace(&mut self.ctrl, Ctrl::Evaluating);

        let new_ctrl = match old_ctrl {
            Ctrl::Evaluating => panic!("Control was not updated after last step"),
            Ctrl::Error(msg) => panic!("Interpretation continues after error: {}", msg),

            Ctrl::Expr(expr) => self.step_expr(world, expr),

            Ctrl::Value(value) => {
                if let Value::PAP(ref prim, ref args, 0) = *value {
                    self.step_saturated_prim(world, store, prim, args)
                } else {
                    self.step_value(value)
                }
            }
        };

        self.ctrl = new_ctrl
    }

    pub fn is_final(&self) -> bool {
        match &self.ctrl {
            Ctrl::Value(v) => match **v {
                Value::PAP(_, _, 0) => false,
                _ => self.kont.is_empty(),
            },
            Ctrl::Error(_) => true,
            _ => false,
        }
    }

    pub fn get_result(self) -> Result<Rc<Value<'a>>, String> {
        match self.ctrl {
            Ctrl::Value(v) => Ok(v),
            Ctrl::Error(msg) => Err(msg),
            _ => panic!("IMPOSSIBLE: final control is always a value"),
        }
    }

    pub fn run(mut self, world: &'a World, store: &mut Store<'a>) -> Result<Rc<Value<'a>>, String> {
        while !self.is_final() {
            self.step(&world, store);
        }
        self.get_result()
    }

    #[allow(dead_code)]
    pub fn print_debug(&self) {
        println!("ctrl: {:?}", self.ctrl);
        println!("env:");
        for val in self.env.stack.iter().rev() {
            println!("# {:?}", val);
        }
        println!("kont:");
        for kont in self.kont.iter().rev() {
            println!("$ {:?}", kont);
        }
    }
}
