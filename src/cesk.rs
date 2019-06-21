// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::{FnvHashMap, FnvHashSet};
use std::rc::Rc;

use crate::ast::*;
use crate::store::*;
use crate::value::*;

#[derive(Debug)]
pub enum Prim<'a> {
    Builtin(Builtin),
    Lam(&'a Expr, Env<'a>),
    RecCon(&'a TypeConRef, &'a Vec<String>),
    RecProj(&'a TypeConRef, &'a String),
    RecUpd(&'a TypeConRef, &'a String),
    VariantCon(&'a TypeConRef, &'a String),
    TupleCon(&'a Vec<String>),
    TupleProj(&'a String),
    CreateCall(&'a DefTemplate),
    CreateCheckPrecondition(&'a DefTemplate),
    CreateExec(&'a DefTemplate),
    Fetch(&'a TypeConRef),
    ExerciseCall(&'a Choice),
    ExerciseExec(&'a Choice),
    Submit { should_succeed: bool },
    AdvanceTime,
}

#[derive(Debug)]
struct PAP<'a> {
    prim: Prim<'a>,
    args: Vec<Rc<Value<'a>>>,
    missing: usize,
}

#[derive(Debug)]
enum Ctrl<'a> {
    Evaluating,
    Expr(&'a Expr),
    PAP(PAP<'a>),
    Value(Rc<Value<'a>>),
    Error(String),
}

#[derive(Debug)]
enum Kont<'a> {
    Dump(Env<'a>),
    DumpParties {
        authorizers: FnvHashSet<Party>,
        witnesses: FnvHashSet<Party>,
    },
    Pop(usize),
    Arg(&'a Expr),
    ArgVal(Rc<Value<'a>>),
    Fun(PAP<'a>),
    Match(&'a Vec<Alt>),
    Let(&'a Binder, &'a Expr),
    EqualList(Rc<Value<'a>>, ValueListIter<'a>, ValueListIter<'a>),
}

#[derive(Debug)]
struct UpdateMode {
    submitter: Party,
    // NOTE(MH): The `authorizers` must be a subset of the `witnesses`.
    authorizers: FnvHashSet<Party>,
    witnesses: FnvHashSet<Party>,
}

#[derive(Debug)]
enum Mode {
    Update(UpdateMode),
    Scenario,
}

#[derive(Debug)]
pub struct State<'a> {
    ctrl: Ctrl<'a>,
    env: Env<'a>,
    kont: Vec<Kont<'a>>,
    time: Time,
    mode: Mode,
}

impl<'a> Ctrl<'a> {
    fn from_value(v: Value<'a>) -> Self {
        Ctrl::Value(Rc::new(v))
    }

    fn into_value(self) -> Rc<Value<'a>> {
        match self {
            Ctrl::Value(value) => value,
            Ctrl::PAP(PAP {
                prim,
                args,
                missing,
            }) => match prim {
                Prim::Builtin(builtin) => Rc::new(Value::PAP(builtin, args, missing)),
                Prim::Lam(body, env) => Rc::new(Value::Lam(body, env, args, missing)),
                _ => panic!("Putting bad prim in heap: {:?}", prim),
            },
            _ => panic!("expected value, found {:?}", self),
        }
    }

    fn into_pap(self) -> PAP<'a> {
        match self {
            Ctrl::Value(value) => match &*value {
                Value::PAP(builtin, args, missing) => PAP {
                    prim: Prim::Builtin(*builtin),
                    args: args.clone(),
                    missing: *missing,
                },
                Value::Lam(body, env, args, missing) => PAP {
                    prim: Prim::Lam(body, env.clone()),
                    args: args.clone(),
                    missing: *missing,
                },
                _ => panic!("expected PAP, found {:?}", value),
            },
            Ctrl::PAP(pap) => pap,
            _ => panic!("expected PAP, found {:?}", self),
        }
    }

    fn from_prim(prim: Prim<'a>, arity: usize) -> Self {
        Ctrl::PAP(PAP {
            prim,
            args: Vec::new(),
            missing: arity,
        })
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

impl Mode {
    fn as_update_mode(&self) -> &UpdateMode {
        match self {
            Mode::Update(update_mode) => update_mode,
            Mode::Scenario => panic!("expected UpdateMode, found {:?}", self),
        }
    }

    fn as_mut_update_mode(&mut self) -> &mut UpdateMode {
        match self {
            Mode::Update(update_mode) => update_mode,
            Mode::Scenario => panic!("expected UpdateMode, found {:?}", self),
        }
    }
}

impl<'a> State<'a> {
    pub fn init(expr: &'a Expr) -> Self {
        State {
            ctrl: Ctrl::Expr(expr),
            env: Env::new(),
            kont: vec![Kont::ArgVal(Rc::new(Value::Token))],
            mode: Mode::Scenario,
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

            Expr::Builtin(opcode) => Ctrl::from_prim(Prim::Builtin(*opcode), opcode.arity()),

            Expr::PrimLit(lit) => {
                let val = match lit {
                    PrimLit::Unit => Value::Unit,
                    PrimLit::Bool(b) => Value::Bool(*b),
                    PrimLit::Nil => Value::Nil,
                    PrimLit::None => Value::None,
                    PrimLit::Int64(i) => Value::Int64(*i),
                    PrimLit::Text(s) => Value::Text(s.clone()),
                    PrimLit::MapEmpty => Value::Map(FnvHashMap::default()),
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

            Expr::TupleCon { fields, exprs } => {
                self.kont.extend(exprs.iter().rev().map(Kont::Arg));
                Ctrl::from_prim(Prim::TupleCon(fields), exprs.len())
            }

            Expr::TupleProj { field, tuple } => {
                self.kont.push(Kont::Arg(&tuple));
                Ctrl::from_prim(Prim::TupleProj(field), 1)
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
                let template = world.get_template(template_ref);
                self.kont.push(Kont::Arg(payload));
                Ctrl::from_prim(Prim::CreateCall(template), 1)
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
                let template = world.get_template(template_ref);
                let choice = template.choices.get::<String>(choice).unwrap();
                self.kont.push(Kont::Arg(arg));
                self.kont.push(Kont::Arg(contract_id));
                Ctrl::from_prim(Prim::ExerciseCall(&choice), 2)
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
            Expr::GetTime => Ctrl::from_value(Value::Time(self.time)),
            Expr::AdvanceTime { delta } => {
                self.kont.push(Kont::Arg(delta));
                Ctrl::from_prim(Prim::AdvanceTime, 1)
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
                        // NOTE(MH): Putting a fully applied `Prim` into
                        // `ArgVal` is a bit sketchy since it prevents us from
                        // assuming that `ArgVal` is a proper value.
                        self.kont
                            .push(Kont::ArgVal(Rc::new(Value::PAP(Builtin::Foldr, args2, 0))));
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
                        let pap = PAP {
                            prim: Prim::Builtin(Builtin::Foldl),
                            args: vec![Rc::clone(f)],
                            missing: 2,
                        };
                        self.kont.push(Kont::Fun(pap));
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
                let value = opcode.interpret(args, world)?;
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
            Prim::TupleCon(fields) => Ctrl::from_value(Value::TupleCon(fields, args.to_vec())),
            Prim::TupleProj(field) => {
                if let Value::TupleCon(fields, vals) = &*args[0] {
                    let idx = fields.iter().position(|x| x == *field).unwrap();
                    Ctrl::Value(Rc::clone(&vals[idx]))
                } else {
                    panic!("TupleProj not on TupleCon")
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

            Prim::CreateCall(template) => {
                let payload = Rc::clone(&args[0]);

                let mut new_env = Env::new();
                new_env.push(payload);
                let old_env = std::mem::replace(&mut self.env, new_env);
                self.kont.push(Kont::Dump(old_env));
                self.kont.push(Kont::Arg(&template.precondtion));
                Ctrl::from_prim(Prim::CreateCheckPrecondition(template), 1)
            }
            Prim::CreateCheckPrecondition(template) => {
                let payload = self.env.top();
                let precondtion: bool = args[0].as_bool();
                if !precondtion {
                    Ctrl::Error(format!(
                        "Template pre-condition violated for {}: {:?}",
                        template.self_ref, payload
                    ))
                } else {
                    self.kont.push(Kont::Arg(&template.observers));
                    self.kont.push(Kont::Arg(&template.signatories));
                    Ctrl::from_prim(Prim::CreateExec(template), 2)
                }
            }
            Prim::CreateExec(template) => {
                let payload = self.env.pop();
                let update_mode = self.mode.as_update_mode();

                let signatories = args[0].as_party_set();
                let observers = args[1].as_party_set();
                if !signatories.is_subset(&update_mode.authorizers) {
                    Ctrl::Error(format!(
                        "authorization missing for create {}: {:?}",
                        template.self_ref, payload
                    ))
                } else {
                    let mut witnesses = update_mode.witnesses.clone();
                    witnesses.extend(observers.iter().cloned());

                    let contract_id = store.create(Contract {
                        template_ref: &template.self_ref,
                        payload,
                        signatories,
                        observers,
                        witnesses,
                    });
                    Ctrl::from_value(Value::ContractId(contract_id))
                }
            }
            Prim::Fetch(template_ref) => Ctrl::catch(|| {
                let update_mode = self.mode.as_update_mode();
                let contract_id = args[0].as_contract_id();
                let contract = store.fetch(
                    &update_mode.submitter,
                    &update_mode.witnesses,
                    template_ref,
                    contract_id,
                )?;
                if update_mode.authorizers.is_disjoint(&contract.signatories)
                    && update_mode.authorizers.is_disjoint(&contract.observers)
                {
                    Err(format!(
                        "authorization missing for fetch {}: {:?}",
                        template_ref, contract_id
                    ))
                } else {
                    Ok(Ctrl::Value(Rc::clone(&contract.payload)))
                }
            }),
            Prim::ExerciseCall(choice) => Ctrl::catch(|| {
                let update_mode = self.mode.as_update_mode();
                let contract_id = &args[0];
                let contract = store.fetch(
                    &update_mode.submitter,
                    &update_mode.witnesses,
                    &choice.template_ref,
                    contract_id.as_contract_id(),
                )?;
                let payload = Rc::clone(&contract.payload);
                let arg = Rc::clone(&args[1]);

                let mut new_env = Env::new();
                new_env.push(payload);
                new_env.push(arg);
                let old_env = std::mem::replace(&mut self.env, new_env);
                self.kont.push(Kont::Dump(old_env));
                self.kont.push(Kont::ArgVal(Rc::clone(contract_id)));
                self.kont.push(Kont::Arg(&choice.controllers));
                Ok(Ctrl::from_prim(Prim::ExerciseExec(choice), 2))
            }),
            Prim::ExerciseExec(choice) => Ctrl::catch(|| {
                let update_mode = self.mode.as_mut_update_mode();
                let controllers = args[0].as_party_set();
                let contract_id = &args[1];
                let arg = self.env.pop();

                if !controllers.is_subset(&update_mode.authorizers) {
                    Err(format!(
                        "authorization missing for exercise {}@{}: {:?} {:?}",
                        choice.template_ref,
                        choice.name,
                        contract_id.as_contract_id(),
                        arg,
                    ))
                } else {
                    let contract = store.fetch(
                        &update_mode.submitter,
                        &update_mode.witnesses,
                        &choice.template_ref,
                        contract_id.as_contract_id(),
                    )?;
                    let mut new_authorizers: FnvHashSet<Party> = controllers;
                    new_authorizers.extend(contract.signatories.iter().cloned());
                    let mut new_witnesses = update_mode.witnesses.clone();
                    new_witnesses.extend(contract.signatories.iter().cloned());

                    if choice.consuming {
                        new_witnesses.extend(contract.observers.iter().cloned());
                        store.archive(
                            &update_mode.submitter,
                            &update_mode.witnesses,
                            &choice.template_ref,
                            contract_id.as_contract_id(),
                        )?;
                    }

                    self.env.push(Rc::clone(contract_id));
                    self.env.push(arg);

                    let old_authorizers =
                        std::mem::replace(&mut update_mode.authorizers, new_authorizers);
                    let old_witnesses =
                        std::mem::replace(&mut update_mode.witnesses, new_witnesses);
                    self.kont.push(Kont::DumpParties {
                        authorizers: old_authorizers,
                        witnesses: old_witnesses,
                    });
                    self.kont.push(Kont::ArgVal(Rc::new(Value::Token)));
                    Ok(Ctrl::Expr(&choice.consequence))
                }
            }),
            Prim::Submit { should_succeed } => {
                let submitter = args[0].as_party().clone();
                let mut authorizers = FnvHashSet::default();
                authorizers.insert(submitter.clone());
                let witnesses = authorizers.clone();
                let update = Rc::clone(&args[1]);
                let mut state = State {
                    ctrl: Ctrl::Value(update),
                    env: Env::new(),
                    kont: vec![Kont::ArgVal(Rc::new(Value::Token))],
                    mode: Mode::Update(UpdateMode {
                        submitter,
                        authorizers,
                        witnesses,
                    }),
                    time: self.time,
                };
                while !state.is_final() {
                    state.step(&world, store);
                }
                let result = state.get_result();
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
            Prim::AdvanceTime => {
                let delta = args[0].as_i64();
                let time = Time::from_micros_since_epoch(
                    i64::checked_add(self.time.to_micros_since_epoch(), delta).unwrap(),
                );
                self.time = time;
                Ctrl::from_value(Value::Time(time))
            }
        }
    }

    /// Step when the control contains a (proper) value.
    fn step_value(&mut self, ctrl: Ctrl<'a>) -> Ctrl<'a> {
        let kont = self.kont.pop().expect("Step on final state");
        match kont {
            Kont::Dump(env) => {
                self.env = env;
                ctrl
            }
            Kont::DumpParties {
                authorizers,
                witnesses,
            } => {
                let update_mode = self.mode.as_mut_update_mode();
                update_mode.authorizers = authorizers;
                update_mode.witnesses = witnesses;
                ctrl
            }
            Kont::Pop(count) => {
                self.env.pop_many(count);
                ctrl
            }
            Kont::Arg(arg) => {
                self.kont.push(Kont::Fun(ctrl.into_pap()));
                Ctrl::Expr(arg)
            }
            Kont::ArgVal(arg) => {
                self.kont.push(Kont::Fun(ctrl.into_pap()));
                Ctrl::Value(arg)
            }
            Kont::Fun(mut pap) => {
                assert!(pap.missing > 0);
                pap.args.push(ctrl.into_value());
                pap.missing -= 1;
                Ctrl::PAP(pap)
            }
            Kont::Match(alts) => {
                let value = ctrl.into_value();
                match &*value {
                    Value::Bool(b) => Ctrl::Expr(&alts[*b as usize].body),
                    Value::VariantCon(_tycon, con1, arg) => {
                        let alt_opt = alts.iter().find_map(|alt| match &alt.pattern {
                            Pat::Variant(con2, _var) if *con1 == con2 => Some((alt, true)),
                            Pat::Default => Some((alt, false)),
                            _ => None,
                        });
                        let (alt, bind_arg) = alt_opt
                            .unwrap_or_else(|| panic!("No match for {:?} in {:?}", value, alts));
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
                }
            }
            Kont::Let(_name, body) => {
                self.kont.push(Kont::Pop(1));
                self.env.push(ctrl.into_value());
                Ctrl::Expr(body)
            }
            Kont::EqualList(eq, mut lhs, mut rhs) => {
                if ctrl.into_value().as_bool() {
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

            Ctrl::Value(ref value) => {
                if let Value::PAP(prim, ref args, 0) = **value {
                    self.step_saturated_prim(world, store, &Prim::Builtin(prim), args)
                } else {
                    self.step_value(old_ctrl)
                }
            }
            Ctrl::PAP(PAP {
                ref prim,
                ref args,
                missing,
            }) => {
                if missing == 0 {
                    self.step_saturated_prim(world, store, prim, args)
                } else {
                    self.step_value(old_ctrl)
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
            Ctrl::PAP(pap) => pap.missing > 0 && self.kont.is_empty(),
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
