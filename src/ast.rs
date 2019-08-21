// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::FnvHashMap;
use protobuf::SingularPtrField;
use std::fmt;
use std::io::*;

use crate::protos::da::daml_lf;
use crate::protos::da::daml_lf_1;

mod debruijn {
    use super::PackageId;
    use std::borrow::Borrow;
    use std::collections::HashMap;

    pub struct Env {
        pub self_package_id: PackageId,
        rev_indices: HashMap<super::Binder, Vec<usize>>,
        depth: usize,
    }

    impl Env {
        pub fn new(self_package_id: PackageId) -> Self {
            Env {
                self_package_id,
                rev_indices: HashMap::new(),
                depth: 0,
            }
        }

        pub fn get(&self, var: &str) -> usize {
            self.depth - self.rev_indices.get(var).and_then(|v| v.last()).unwrap()
        }

        pub fn push(&mut self, var: &str) {
            self.rev_indices
                .entry(var.to_owned())
                .or_insert_with(Vec::new)
                .push(self.depth);
            self.depth += 1;
        }

        pub fn push_many<'a, T, I>(&mut self, vars: I)
        where
            T: Borrow<String> + 'a,
            I: IntoIterator<Item = &'a T>,
        {
            for var in vars {
                self.push(var.borrow());
            }
        }

        pub fn pop(&mut self, var: &str) {
            self.rev_indices.get_mut(var).and_then(|v| v.pop()).unwrap();
            self.depth -= 1;
        }

        pub fn pop_many<'a, T, I>(&mut self, vars: I)
        where
            T: Borrow<String> + 'a,
            I: IntoIterator<Item = &'a T>,
        {
            for var in vars {
                self.pop(var.borrow());
            }
        }
    }
}

use self::debruijn::Env;

pub type Binder = String;

pub type PackageId = String;

pub fn dotted_name_from_proto(proto: SingularPtrField<daml_lf_1::DottedName>) -> String {
    proto.unwrap().segments.into_vec().join(".")
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ModuleRef {
    pub package_id: PackageId,
    pub module_name: String,
}

impl fmt::Display for ModuleRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.package_id, self.module_name)
    }
}

impl ModuleRef {
    fn from_proto(proto: SingularPtrField<daml_lf_1::ModuleRef>, env: &Env) -> Self {
        Self::from_proto_unboxed(proto.unwrap(), env)
    }

    fn from_proto_unboxed(proto: daml_lf_1::ModuleRef, env: &Env) -> Self {
        use daml_lf_1::PackageRef_oneof_Sum;
        let package_ref = proto.package_ref.unwrap();
        let package_id = match package_ref.Sum.unwrap() {
            PackageRef_oneof_Sum::field_self(_) => env.self_package_id.clone(),
            PackageRef_oneof_Sum::package_id(id) => id,
        };
        let module_name = dotted_name_from_proto(proto.module_name);
        ModuleRef {
            package_id,
            module_name,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Location {
    pub module: Option<ModuleRef>,
    pub start_line: i32,
    pub start_col: i32,
    pub end_line: i32,
    pub end_col: i32,
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let range = format!(
            "{}:{}-{}:{}",
            self.start_line, self.start_col, self.end_line, self.end_col
        );
        match &self.module {
            None => write!(f, "{}", range),
            Some(module) => write!(f, "{}:{}", module, range),
        }
    }
}

impl Location {
    fn from_proto(proto: SingularPtrField<daml_lf_1::Location>, env: &Env) -> Option<Self> {
        proto.into_option().map(|proto| {
            let module = proto
                .module
                .into_option()
                .map(|module| ModuleRef::from_proto_unboxed(module, env));
            let range = proto.range.unwrap();
            Location {
                module,
                start_line: range.start_line,
                start_col: range.start_col,
                end_line: range.end_line,
                end_col: range.end_col,
            }
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeConRef {
    module_ref: ModuleRef,
    name: String,
}

impl TypeConRef {
    fn from_proto(proto: SingularPtrField<daml_lf_1::TypeConName>, env: &Env) -> TypeConRef {
        let proto = proto.unwrap();
        let module_ref = ModuleRef::from_proto(proto.module, env);
        let name = dotted_name_from_proto(proto.name);
        TypeConRef { module_ref, name }
    }
}

impl fmt::Display for TypeConRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.module_ref, self.name)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    // Boolean comparison
    EqualBool,

    // Integer arithmetic
    AddInt64,
    SubInt64,
    MulInt64,
    DivInt64,
    ModInt64,
    ExpInt64,

    // Integer comparison
    EqualInt64,
    LeqInt64,
    GeqInt64,
    LessInt64,
    GreaterInt64,

    // Text operations
    AppendText,
    ImplodeText,
    ExplodeText,
    Sha256Text,
    TextToCodePoints,
    TextFromCodePoints,

    // Text comparison
    EqualText,
    LeqText,
    GeqText,
    LessText,
    GreaterText,

    // Party comparison
    EqualParty,
    LeqParty,
    GeqParty,
    LessParty,
    GreaterParty,

    // ContractId operations
    EqualContractId,
    CoerceContractId,

    // Time comparison
    EqualTime,
    LeqTime,
    GeqTime,
    LessTime,
    GreaterTime,

    // Time operations
    TimeToMicrosSinceEpoch,
    TimeFromMicrosSinceEpoch,

    // Date comparison
    EqualDate,
    LeqDate,
    GeqDate,
    LessDate,
    GreaterDate,

    // Date operations
    DateToDaysSinceEpoch,
    DateFromDaysSinceEpoch,

    // Conversion to text
    Int64ToText,
    TextToText,
    PartyToText,
    PartyToQuotedText,
    TimeToText,
    DateToText,

    // Conversion from text
    Int64FromText,
    PartyFromText,
    GetParty,

    // List operations
    Cons,
    Foldr,
    Foldl,
    EqualList,

    // Misc
    Some,
    Error,

    // Map operations
    MapInsert,
    MapLookup,
    MapDelete,
    MapToList,
    MapSize,

    Unsupported(daml_lf_1::BuiltinFunction),
}

impl Builtin {
    fn from_proto(proto: daml_lf_1::BuiltinFunction) -> Builtin {
        use self::Builtin::*;
        use daml_lf_1::BuiltinFunction::*;
        match proto {
            EQUAL_BOOL => EqualBool,

            ADD_INT64 => AddInt64,
            SUB_INT64 => SubInt64,
            MUL_INT64 => MulInt64,
            DIV_INT64 => DivInt64,
            MOD_INT64 => ModInt64,
            EXP_INT64 => ExpInt64,

            EQUAL_INT64 => EqualInt64,
            LEQ_INT64 => LeqInt64,
            GEQ_INT64 => GeqInt64,
            LESS_INT64 => LessInt64,
            GREATER_INT64 => GreaterInt64,

            APPEND_TEXT => AppendText,
            IMPLODE_TEXT => ImplodeText,
            EXPLODE_TEXT => ExplodeText,
            SHA256_TEXT => Sha256Text,
            TO_TEXT_CODE_POINTS => TextFromCodePoints,
            FROM_TEXT_CODE_POINTS => TextToCodePoints,

            EQUAL_TEXT => EqualText,
            LEQ_TEXT => LeqText,
            GEQ_TEXT => GeqText,
            LESS_TEXT => LessText,
            GREATER_TEXT => GreaterText,

            EQUAL_PARTY => EqualParty,
            LEQ_PARTY => LeqParty,
            GEQ_PARTY => GeqParty,
            LESS_PARTY => LessParty,
            GREATER_PARTY => GreaterParty,

            EQUAL_CONTRACT_ID => EqualContractId,
            COERCE_CONTRACT_ID => CoerceContractId,

            EQUAL_TIMESTAMP => EqualTime,
            LEQ_TIMESTAMP => LeqTime,
            LESS_TIMESTAMP => LessTime,
            GEQ_TIMESTAMP => GeqTime,
            GREATER_TIMESTAMP => GreaterTime,

            TIMESTAMP_TO_UNIX_MICROSECONDS => TimeToMicrosSinceEpoch,
            UNIX_MICROSECONDS_TO_TIMESTAMP => TimeFromMicrosSinceEpoch,

            EQUAL_DATE => EqualDate,
            LEQ_DATE => LeqDate,
            LESS_DATE => LessDate,
            GEQ_DATE => GeqDate,
            GREATER_DATE => GreaterDate,

            DATE_TO_UNIX_DAYS => DateToDaysSinceEpoch,
            UNIX_DAYS_TO_DATE => DateFromDaysSinceEpoch,

            TO_TEXT_INT64 => Int64ToText,
            TO_TEXT_TEXT => TextToText,
            TO_TEXT_PARTY => PartyToText,
            TO_QUOTED_TEXT_PARTY => PartyToQuotedText,
            TO_TEXT_TIMESTAMP => TimeToText,
            TO_TEXT_DATE => DateToText,

            FROM_TEXT_INT64 => Int64FromText,
            FROM_TEXT_PARTY => PartyFromText,

            FOLDR => Foldr,
            FOLDL => Foldl,
            EQUAL_LIST => EqualList,

            ERROR => Error,

            MAP_EMPTY => panic!("This is handled in Expr::from_proto"),
            MAP_INSERT => MapInsert,
            MAP_LOOKUP => MapLookup,
            MAP_DELETE => MapDelete,
            MAP_TO_LIST => MapToList,
            MAP_SIZE => MapSize,

            // Decimal unsupported
            ADD_DECIMAL | SUB_DECIMAL | MUL_DECIMAL | DIV_DECIMAL | ROUND_DECIMAL
            | EQUAL_DECIMAL | LEQ_DECIMAL | LESS_DECIMAL | GEQ_DECIMAL | GREATER_DECIMAL
            | TO_TEXT_DECIMAL | FROM_TEXT_DECIMAL | INT64_TO_DECIMAL | DECIMAL_TO_INT64 => {
                Unsupported(proto)
            }

            // Misc
            TRACE => Unsupported(proto),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PrimLit {
    Unit,
    Bool(bool),
    Nil,
    None,
    Int64(i64),
    Text(String),
    MapEmpty,
    Unsupported(&'static str),
}

impl PrimLit {
    fn from_proto(proto: daml_lf_1::PrimLit) -> PrimLit {
        use daml_lf_1::PrimLit_oneof_Sum::*;
        match proto.Sum.unwrap() {
            int64(x) => PrimLit::Int64(x),
            decimal(_) => PrimLit::Unsupported("PrimLit::Decimal"),
            text(x) => PrimLit::Text(x),
            timestamp(_) => PrimLit::Unsupported("PrimLit::Timestamp"),
            party(_) => PrimLit::Unsupported("PrimLit::Party"),
            date(_) => PrimLit::Unsupported("PrimLit::Date"),
        }
    }
}

#[derive(Debug)]
pub enum Pat {
    Default,
    Variant(String, Binder),
    Unit,
    Bool(bool),
    Nil,
    Cons(Binder, Binder),
    None,
    Some(Binder),
}

impl Pat {
    fn from_proto(proto: daml_lf_1::CaseAlt_oneof_Sum) -> Self {
        use daml_lf_1::CaseAlt_oneof_Sum::*;
        use daml_lf_1::PrimCon::*;
        match proto {
            default(_) => Pat::Default,
            variant(x) => Pat::Variant(x.variant, x.binder),
            prim_con(x) => match x {
                CON_UNIT => Pat::Unit,
                CON_FALSE => Pat::Bool(false),
                CON_TRUE => Pat::Bool(true),
            },
            nil(_) => Pat::Nil,
            cons(x) => Pat::Cons(x.var_head, x.var_tail),
            none(_) => Pat::None,
            some(x) => Pat::Some(x.var_body),
            field_enum(_) => panic!("UNSUPPORTED: enum types"),
        }
    }

    fn binders(&self) -> Vec<&Binder> {
        match self {
            Pat::Default => vec![],
            Pat::Variant(_, x) => vec![x],
            Pat::Unit => vec![],
            Pat::Bool(_) => vec![],
            Pat::Nil => vec![],
            Pat::Cons(x, y) => vec![x, y],
            Pat::None => vec![],
            Pat::Some(x) => vec![x],
        }
    }
}

#[derive(Debug)]
pub struct Alt {
    pub pattern: Pat,
    pub body: Expr,
}

impl Alt {
    fn from_proto(env: &mut Env, proto: daml_lf_1::CaseAlt) -> Self {
        let pattern = Pat::from_proto(proto.Sum.unwrap());
        let body = {
            let binders = pattern.binders();
            env.push_many(&binders);
            let body = Expr::from_proto(proto.body, env);
            env.pop_many(&binders);
            body
        };
        Alt { pattern, body }
    }
}

#[derive(Debug)]
pub enum Expr {
    Var {
        name: Binder,
        index: usize,
    },
    Val {
        module_ref: ModuleRef,
        name: String,
    },
    Builtin(Builtin),
    PrimLit(PrimLit),
    RecCon {
        tycon: TypeConRef,
        fields: Vec<String>,
        exprs: Vec<Expr>,
    },
    RecProj {
        tycon: TypeConRef,
        field: String,
        record: Box<Expr>,
    },
    RecUpd {
        tycon: TypeConRef,
        field: String,
        record: Box<Expr>,
        value: Box<Expr>,
    },
    VariantCon {
        tycon: TypeConRef,
        con: String,
        arg: Box<Expr>,
    },
    TupleCon {
        fields: Vec<String>,
        exprs: Vec<Expr>,
    },
    TupleProj {
        field: String,
        tuple: Box<Expr>,
    },
    App {
        fun: Box<Expr>,
        args: Vec<Expr>,
    },
    Lam {
        params: Vec<Binder>,
        body: Box<Expr>,
    },
    Case {
        scrut: Box<Expr>,
        alts: Vec<Alt>,
    },
    Let {
        binder: Binder,
        bound: Box<Expr>,
        body: Box<Expr>,
    },
    Create {
        template_ref: TypeConRef,
        payload: Box<Expr>,
    },
    Fetch {
        template_ref: TypeConRef,
        contract_id: Box<Expr>,
    },
    Exercise {
        template_ref: TypeConRef,
        choice: String,
        contract_id: Box<Expr>,
        arg: Box<Expr>,
    },
    Submit {
        should_succeed: bool,
        submitter: Box<Expr>,
        update: Box<Expr>,
    },
    GetTime,
    AdvanceTime {
        delta: Box<Expr>,
    },
    Located {
        location: Location,
        expr: Box<Expr>,
    },

    Unsupported(&'static str),
}

impl Expr {
    fn from_proto(proto: SingularPtrField<daml_lf_1::Expr>, env: &mut Env) -> Expr {
        Self::from_proto_unboxed(proto.unwrap(), env)
    }

    fn boxed_from_proto(proto: SingularPtrField<daml_lf_1::Expr>, env: &mut Env) -> Box<Expr> {
        Box::new(Self::from_proto(proto, env))
    }

    fn from_proto_unboxed(mut proto: daml_lf_1::Expr, env: &mut Env) -> Expr {
        use daml_lf_1::Expr_oneof_Sum::*;
        use daml_lf_1::PrimCon::*;

        let expr = match proto.Sum.unwrap() {
            var(x) => {
                let index = env.get(&x);
                let name = x;
                Expr::Var { name, index }
            }
            val(x) => {
                let module_ref = ModuleRef::from_proto(x.module, env);
                let name = x.name.join(".");
                Expr::Val { module_ref, name }
            }
            builtin(x) if x == daml_lf_1::BuiltinFunction::MAP_EMPTY => {
                Expr::PrimLit(PrimLit::MapEmpty)
            }
            builtin(x) => Expr::Builtin(Builtin::from_proto(x)),
            prim_con(x) => Expr::PrimLit(match x {
                CON_UNIT => PrimLit::Unit,
                CON_FALSE => PrimLit::Bool(false),
                CON_TRUE => PrimLit::Bool(true),
            }),
            prim_lit(x) => Expr::PrimLit(PrimLit::from_proto(x)),
            rec_con(x) => {
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let mut fields = Vec::new();
                fields.reserve(x.fields.len());
                let mut exprs = Vec::new();
                exprs.reserve(x.fields.len());
                for fx in x.fields.into_vec() {
                    fields.push(fx.field);
                    exprs.push(Self::from_proto(fx.expr, env));
                }
                Expr::RecCon {
                    tycon,
                    fields,
                    exprs,
                }
            }
            rec_proj(x) => {
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let field = x.field;
                let record = Self::boxed_from_proto(x.record, env);
                Expr::RecProj {
                    tycon,
                    field,
                    record,
                }
            }
            rec_upd(x) => {
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let field = x.field;
                let record = Self::boxed_from_proto(x.record, env);
                let value = Self::boxed_from_proto(x.update, env);
                Expr::RecUpd {
                    tycon,
                    field,
                    record,
                    value,
                }
            }
            variant_con(x) => {
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let con = x.variant_con;
                let arg = Self::boxed_from_proto(x.variant_arg, env);
                Expr::VariantCon { tycon, con, arg }
            }
            tuple_con(x) => {
                let mut fields = Vec::new();
                fields.reserve(x.fields.len());
                let mut exprs = Vec::new();
                exprs.reserve(x.fields.len());
                for fx in x.fields.into_vec() {
                    fields.push(fx.field);
                    exprs.push(Self::from_proto(fx.expr, env));
                }
                Expr::TupleCon { fields, exprs }
            }
            tuple_proj(x) => {
                let field = x.field;
                let tuple = Self::boxed_from_proto(x.tuple, env);
                Expr::TupleProj { field, tuple }
            }
            app(x) => {
                let fun = Self::boxed_from_proto(x.fun, env);
                let args = x
                    .args
                    .into_iter()
                    .map(|y| Self::from_proto_unboxed(y, env))
                    .collect();
                Expr::App { fun, args }
            }
            ty_app(x) => Self::from_proto(x.expr, env),
            abs(x) => {
                let params: Vec<Binder> = x.param.into_iter().map(|x| x.var).collect();
                let body = {
                    env.push_many(&params);
                    let body = Self::boxed_from_proto(x.body, env);
                    env.pop_many(&params);
                    body
                };
                Expr::Lam { params, body }
            }
            ty_abs(x) => Self::from_proto(x.body, env),
            case(x) => {
                let scrut = Self::boxed_from_proto(x.scrut, env);
                let alts = x
                    .alts
                    .into_iter()
                    .map(|y| Alt::from_proto(env, y))
                    .collect();
                Expr::make_case(scrut, alts)
            }
            field_let(x) => {
                let mut bindings = Vec::new();
                bindings.reserve(x.bindings.len());
                for binding in x.bindings.into_vec() {
                    let binder = binding.binder.unwrap().var;
                    let bound = Self::boxed_from_proto(binding.bound, env);
                    env.push(&binder);
                    bindings.push((binder, bound));
                }
                let body = Self::from_proto(x.body, env);
                for (binder, _) in bindings.iter() {
                    env.pop(binder);
                }
                bindings
                    .into_iter()
                    .rev()
                    .fold(body, |body, (binder, bound)| Expr::Let {
                        binder,
                        bound,
                        body: Box::new(body),
                    })
            }
            nil(_) => Expr::PrimLit(PrimLit::Nil),
            cons(x) => {
                let tail = Self::from_proto(x.tail, env);
                x.front.into_iter().rev().fold(tail, |tail, elem| {
                    let head = Self::from_proto_unboxed(elem, env);
                    Expr::App {
                        fun: Box::new(Expr::Builtin(Builtin::Cons)),
                        args: vec![head, tail],
                    }
                })
            }
            none(_) => Expr::PrimLit(PrimLit::None),
            some(x) => {
                let body = Self::from_proto(x.body, env);
                Expr::App {
                    fun: Box::new(Expr::Builtin(Builtin::Some)),
                    args: vec![body],
                }
            }
            enum_con(_) => Expr::Unsupported("Expr::EnumCon"),
            update(update_proto) => {
                let param = String::from("$token");
                env.push(&param);
                let token_index = env.get(&param);
                let apply_token = |fun: Expr| {
                    let fun = Box::new(fun);
                    let args = vec![Expr::Var {
                        name: param.clone(),
                        index: token_index,
                    }];
                    Expr::App { fun, args }
                };

                let body = Self::from_update_proto(update_proto, apply_token, env);

                // NOTE(MH): We need to put the location annotation inside
                // the lambda for the token to produce useful stack traces.
                let proto_location =
                    std::mem::replace(&mut proto.location, SingularPtrField::none());
                let body = if let Some(location) = Location::from_proto(proto_location, env) {
                    let expr = Box::new(body);
                    Expr::Located { location, expr }
                } else {
                    body
                };

                env.pop(&param);
                Expr::Lam {
                    params: vec![param],
                    body: Box::new(body),
                }
            }
            scenario(scenario_proto) => {
                let param = String::from("$token");
                env.push(&param);
                let token_index = env.get(&param);
                let apply_token = |fun: Expr| {
                    let fun = Box::new(fun);
                    let args = vec![Expr::Var {
                        name: param.clone(),
                        index: token_index,
                    }];
                    Expr::App { fun, args }
                };

                let body = Self::from_scenario_proto(*scenario_proto, apply_token, env);

                // NOTE(MH): We need to put the location annotation inside
                // the lambda for the token to produce useful stack traces.
                let proto_location =
                    std::mem::replace(&mut proto.location, SingularPtrField::none());
                let body = if let Some(location) = Location::from_proto(proto_location, env) {
                    let expr = Box::new(body);
                    Expr::Located { location, expr }
                } else {
                    body
                };

                env.pop(&param);
                Expr::Lam {
                    params: vec![param],
                    body: Box::new(body),
                }
            }
            tuple_upd(_) => Expr::Unsupported("Expr::TupleUpd"),
        };

        if let Some(location) = Location::from_proto(proto.location, env) {
            let expr = Box::new(expr);
            Expr::Located { location, expr }
        } else {
            expr
        }
    }

    fn from_update_proto<F>(proto: daml_lf_1::Update, apply_token: F, env: &mut Env) -> Expr
    where
        F: Fn(Expr) -> Expr,
    {
        use daml_lf_1::Update_oneof_Sum::*;
        match proto.Sum.unwrap() {
            field_pure(x) => Self::from_proto(x.expr, env),
            block(x) => {
                let mut bindings = Vec::new();
                bindings.reserve(x.bindings.len());
                for binding in x.bindings.into_vec() {
                    let binder = binding.binder.unwrap().var;
                    let bound = apply_token(Self::from_proto(binding.bound, env));
                    env.push(&binder);
                    bindings.push((binder, bound));
                }
                let body = apply_token(Self::from_proto(x.body, env));
                for (binder, _) in bindings.iter() {
                    env.pop(binder);
                }
                bindings
                    .into_iter()
                    .rev()
                    .fold(body, |body, (binder, bound)| Expr::Let {
                        binder,
                        bound: Box::new(bound),
                        body: Box::new(body),
                    })
            }
            embed_expr(x) => {
                let expr = Self::from_proto(x.body, env);
                apply_token(expr)
            }
            create(create_proto) => {
                let template_ref = TypeConRef::from_proto(create_proto.template, env);
                let payload = Self::boxed_from_proto(create_proto.expr, env);
                Expr::Create {
                    template_ref,
                    payload,
                }
            }
            fetch(fetch_proto) => {
                let template_ref = TypeConRef::from_proto(fetch_proto.template, env);
                let contract_id = Self::boxed_from_proto(fetch_proto.cid, env);
                Expr::Fetch {
                    template_ref,
                    contract_id,
                }
            }
            exercise(exercise_proto) => {
                let template_ref = TypeConRef::from_proto(exercise_proto.template, env);
                let choice = exercise_proto.choice;
                let contract_id = Self::boxed_from_proto(exercise_proto.cid, env);
                let arg = Self::boxed_from_proto(exercise_proto.arg, env);
                Expr::Exercise {
                    template_ref,
                    choice,
                    contract_id,
                    arg,
                }
            }
            get_time(_) => Expr::GetTime,
            lookup_by_key(_) => Expr::Unsupported("Expr::LookupByKey"),
            fetch_by_key(_) => Expr::Unsupported("Expr::FetchByKey"),
        }
    }

    fn from_scenario_proto<F>(proto: daml_lf_1::Scenario, apply_token: F, env: &mut Env) -> Expr
    where
        F: Fn(Expr) -> Expr,
    {
        use daml_lf_1::Scenario_oneof_Sum::*;
        match proto.Sum.unwrap() {
            field_pure(x) => Self::from_proto(x.expr, env),
            block(x) => {
                let mut bindings = Vec::new();
                bindings.reserve(x.bindings.len());
                for binding in x.bindings.into_vec() {
                    let binder = binding.binder.unwrap().var;
                    let bound = apply_token(Self::from_proto(binding.bound, env));
                    env.push(&binder);
                    bindings.push((binder, bound));
                }
                let body = apply_token(Self::from_proto(x.body, env));
                for (binder, _) in bindings.iter() {
                    env.pop(binder);
                }
                bindings
                    .into_iter()
                    .rev()
                    .fold(body, |body, (binder, bound)| Expr::Let {
                        binder,
                        bound: Box::new(bound),
                        body: Box::new(body),
                    })
            }
            embed_expr(x) => {
                let expr = Self::from_proto(x.body, env);
                apply_token(expr)
            }
            commit(x) => Expr::Submit {
                should_succeed: true,
                submitter: Self::boxed_from_proto(x.party, env),
                update: Self::boxed_from_proto(x.expr, env),
            },
            mustFailAt(x) => Expr::Submit {
                should_succeed: false,
                submitter: Self::boxed_from_proto(x.party, env),
                update: Self::boxed_from_proto(x.expr, env),
            },
            get_time(_) => Expr::GetTime,
            pass(x) => Expr::AdvanceTime {
                delta: Box::new(Self::from_proto_unboxed(*x, env)),
            },
            get_party(x) => Expr::App {
                fun: Box::new(Expr::Builtin(Builtin::GetParty)),
                args: vec![Self::from_proto_unboxed(*x, env)],
            },
        }
    }

    fn make_case(scrut: Box<Expr>, mut alts: Vec<Alt>) -> Self {
        assert!(!alts.is_empty(), "Empty case expression");
        match alts[0].pattern {
            Pat::Default => panic!("Case starting with default pattern"),
            Pat::Bool(b0) => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::Bool(b) => b0 != *b,
                        Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete boolean pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
                if b0 {
                    alts.swap(0, 1);
                }
            }
            Pat::Nil => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::Cons(..) | Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete list pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
            }
            Pat::Cons(..) => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::Nil | Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete list pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
                alts.swap(0, 1);
            }
            Pat::None => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::Some(_) | Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete optional pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
            }
            Pat::Some(_) => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::None | Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete optional pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
                alts.swap(0, 1);
            }
            _ => (),
        };
        Expr::Case { scrut, alts }
    }
}

#[derive(Debug)]
pub struct DefValue {
    pub name: String,
    pub location: Option<Location>,
    pub expr: Expr,
    pub is_test: bool,
}

impl DefValue {
    fn from_proto(proto: daml_lf_1::DefValue, env: &mut Env) -> Self {
        let name = proto.name_with_type.unwrap().name.join(".");
        let location = Location::from_proto(proto.location, env);
        let expr = Expr::from_proto(proto.expr, env);
        let is_test = proto.is_test;
        DefValue {
            name,
            location,
            expr,
            is_test,
        }
    }
}

#[derive(Debug)]
pub struct Choice {
    pub name: String,
    pub location: Option<Location>,
    pub template_ref: TypeConRef,
    pub consuming: bool,
    pub self_binder: Binder,
    pub arg_binder: Binder,
    pub controllers: Expr,
    pub consequence: Expr,
}

impl Choice {
    fn from_proto(
        proto: daml_lf_1::TemplateChoice,
        template_ref: &TypeConRef,
        env: &mut Env,
    ) -> Self {
        let name = proto.name;
        let location = Location::from_proto(proto.location, env);
        let template_ref = template_ref.clone();
        let consuming = proto.consuming;
        let self_binder = proto.self_binder;
        let arg_binder = proto.arg_binder.unwrap().var;
        env.push(&arg_binder);
        let controllers = Expr::from_proto(proto.controllers, env);
        env.pop(&arg_binder);
        env.push_many(vec![&self_binder, &arg_binder]);
        let consequence = Expr::from_proto(proto.update, env);
        env.pop_many(&[&self_binder, &arg_binder]);
        Choice {
            name,
            location,
            template_ref,
            consuming,
            self_binder,
            arg_binder,
            controllers,
            consequence,
        }
    }
}

#[derive(Debug)]
pub struct DefTemplate {
    pub name: String,
    pub location: Option<Location>,
    pub self_ref: TypeConRef,
    pub this_binder: Binder,
    pub precondtion: Expr,
    pub signatories: Expr,
    pub observers: Expr,
    pub choices: FnvHashMap<String, Choice>,
}

impl DefTemplate {
    fn from_proto(proto: daml_lf_1::DefTemplate, module_ref: &ModuleRef, env: &mut Env) -> Self {
        let name = dotted_name_from_proto(proto.tycon);
        let location = Location::from_proto(proto.location, env);
        let self_ref = TypeConRef {
            module_ref: module_ref.clone(),
            name: name.clone(),
        };
        let this_binder = proto.param;
        env.push(&this_binder);
        let precondtion = Expr::from_proto(proto.precond, env);
        let signatories = Expr::from_proto(proto.signatories, env);
        let observers = Expr::from_proto(proto.observers, env);
        let choices = proto
            .choices
            .into_iter()
            .map(|choice_proto| {
                let choice = Choice::from_proto(choice_proto, &self_ref, env);
                (choice.name.clone(), choice)
            })
            .collect();
        env.pop(&this_binder);
        DefTemplate {
            name,
            location,
            self_ref,
            this_binder,
            precondtion,
            signatories,
            observers,
            choices,
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub self_ref: ModuleRef,
    pub values: FnvHashMap<String, DefValue>,
    templates: FnvHashMap<String, DefTemplate>,
}

impl Module {
    fn from_proto(proto: daml_lf_1::Module, package_id: &str, env: &mut Env) -> Self {
        let name = dotted_name_from_proto(proto.name);
        let self_ref = ModuleRef {
            package_id: package_id.to_string(),
            module_name: name.clone(),
        };
        let values = proto
            .values
            .into_iter()
            .map(|x| {
                let y = DefValue::from_proto(x, env);
                (y.name.clone(), y)
            })
            .collect();
        let templates = proto
            .templates
            .into_iter()
            .map(|template_proto| {
                let template = DefTemplate::from_proto(template_proto, &self_ref, env);
                (template.name.clone(), template)
            })
            .collect();
        Module {
            name,
            self_ref,
            values,
            templates,
        }
    }
}

#[derive(Debug)]
pub struct Package {
    pub id: PackageId,
    pub modules: FnvHashMap<String, Module>,
}

impl Package {
    fn from_proto(proto: daml_lf::Archive) -> Self {
        let payload: daml_lf::ArchivePayload = protobuf::parse_from_bytes(&proto.payload).unwrap();
        let id = proto.hash;
        let modules = match payload.Sum.unwrap() {
            daml_lf::ArchivePayload_oneof_Sum::daml_lf_0(_) => panic!("DAML-LF 0.x not supported"),
            daml_lf::ArchivePayload_oneof_Sum::daml_lf_1(payload_proto) => {
                let mut env = Env::new(id.clone());
                payload_proto
                    .modules
                    .into_iter()
                    .map(|module_proto| {
                        let module = Module::from_proto(module_proto, &id, &mut env);
                        (module.name.clone(), module)
                    })
                    .collect()
            }
        };
        Package { id, modules }
    }

    fn load<R: Read>(reader: &mut R) -> Result<Self> {
        let proto = protobuf::parse_from_reader(reader)?;
        let package = Package::from_proto(proto);
        Ok(package)
    }
}

pub struct World {
    pub main: PackageId,
    packages: FnvHashMap<PackageId, Package>,
    pub map_entry_fields: Vec<String>,
}

impl World {
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> Result<Self> {
        use std::fs::File;
        let zip_file: File = File::open(path)?;
        let mut zip = zip::ZipArchive::new(zip_file)?;
        let manifest = zip.by_name("META-INF/MANIFEST.MF")?;
        let manifest_buffered = BufReader::new(manifest);
        let mut main_name: String = String::new();
        let mut package_names: Vec<String> = Vec::new();
        for line in manifest_buffered.lines() {
            let line = line?;
            if line.starts_with("Main-Dalf:") {
                main_name = String::from(line[10..].trim());
            } else if line.starts_with("Dalfs:") {
                package_names = line[6..]
                    .split(',')
                    .map(|x| String::from(x.trim()))
                    .collect();
            }
        }
        let main_index = package_names.iter().position(|x| x == &main_name).unwrap();
        package_names.swap(0, main_index);
        let mut packages = Vec::new();
        for name in package_names {
            let mut file = zip.by_name(&name)?;
            let package = Package::load(&mut file)?;
            packages.push(package);
        }

        let main = packages[0].id.clone();
        let packages = packages
            .into_iter()
            .map(|package| (package.id.clone(), package))
            .collect();
        let map_entry_fields = vec!["key".to_string(), "value".to_string()];
        let world = World {
            main,
            packages,
            map_entry_fields,
        };
        Ok(world)
    }

    pub fn main_package(&self) -> &Package {
        self.packages.get(&self.main).unwrap()
    }

    pub fn get_module(&self, module_ref: &ModuleRef) -> &Module {
        self.packages
            .get(&module_ref.package_id)
            .unwrap_or_else(|| panic!("unknown package id: {}", &module_ref.package_id))
            .modules
            .get(&module_ref.module_name)
            .unwrap_or_else(|| panic!("unknown module ref: {}", &module_ref))
    }

    pub fn get_value(&self, module_ref: &ModuleRef, name: &str) -> &DefValue {
        self.get_module(module_ref).values.get(name).unwrap()
    }

    pub fn get_template(&self, template_ref: &TypeConRef) -> &DefTemplate {
        self.get_module(&template_ref.module_ref)
            .templates
            .get(&template_ref.name)
            .unwrap()
    }
}
