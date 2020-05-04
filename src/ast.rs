// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use bigdecimal::BigDecimal;
use fnv::FnvHashMap;
use protobuf::SingularPtrField;
use regex::Regex;
use std::collections::HashMap;
use std::fmt;
use std::io::*;

use crate::protos::da::daml_lf;
use crate::protos::da::daml_lf_1;

mod debruijn {
    use super::{Binder, PackageId};
    use std::borrow::Borrow;
    use std::collections::HashMap;

    pub struct Env {
        pub self_package_id: PackageId,
        interned_strings: Vec<String>,
        interned_dotted_names: Vec<String>,
        rev_indices: HashMap<Binder, Vec<usize>>,
        ty_vars: HashMap<Binder, Vec<Option<usize>>>,
        depth: usize,
    }

    impl Env {
        pub fn new(
            self_package_id: PackageId,
            interned_strings: Vec<String>,
            interned_dotted_names: Vec<String>,
        ) -> Self {
            Env {
                self_package_id,
                interned_strings,
                interned_dotted_names,
                rev_indices: HashMap::new(),
                ty_vars: HashMap::new(),
                depth: 0,
            }
        }

        pub fn get_interned_string(&self, id: i32) -> String {
            self.interned_strings[id as usize].clone()
        }

        pub fn get_interned_dotted_name(&self, id: i32) -> String {
            self.interned_dotted_names[id as usize].clone()
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

        pub fn push_ty_var(&mut self, var: &str, is_nat: bool) {
            self.ty_vars
                .entry(var.to_owned())
                .or_insert_with(Vec::new)
                .push(if is_nat { Some(self.depth) } else { None });
            if is_nat {
                self.depth += 1;
            }
        }

        pub fn pop_ty_var(&mut self, var: &str) {
            let index_opt: Option<usize> = self.ty_vars.get_mut(var).and_then(|v| v.pop()).unwrap();
            if index_opt.is_some() {
                self.depth -= 1;
            }
        }

        pub fn get_ty_var(&self, var: &str) -> Option<usize> {
            self.ty_vars
                .get(var)
                .and_then(|v| v.last())
                .unwrap()
                .map(|index| self.depth - index)
        }
    }
}

use debruijn::Env;

pub type Binder = String;

pub type PackageId = String;

fn dotted_name_from_proto(proto: daml_lf_1::DottedName) -> String {
    proto.segments.into_vec().join(".")
}

fn var_with_type_from_proto(proto: Option<daml_lf_1::VarWithType_oneof_var>, env: &Env) -> String {
    use daml_lf_1::VarWithType_oneof_var::*;
    match proto.unwrap() {
        var_str(name) => name,
        var_interned_str(id) => env.get_interned_string(id),
    }
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
        use daml_lf_1::ModuleRef_oneof_module_name::*;
        use daml_lf_1::PackageRef_oneof_Sum::*;
        let package_id = match proto.package_ref.unwrap().Sum.unwrap() {
            field_self(_) => env.self_package_id.clone(),
            package_id_str(pkg_id) => pkg_id,
            package_id_interned_str(id) => env.get_interned_string(id),
        };
        let module_name = match proto.module_name.unwrap() {
            module_name_dname(dotted_name) => dotted_name_from_proto(dotted_name),
            module_name_interned_dname(id) => env.get_interned_dotted_name(id),
        };
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
        use daml_lf_1::TypeConName_oneof_name::*;
        let proto = proto.unwrap();
        let module_ref = ModuleRef::from_proto(proto.module, env);
        let name = match proto.name.unwrap() {
            name_dname(dotted_name) => dotted_name_from_proto(dotted_name),
            name_interned_dname(id) => env.get_interned_dotted_name(id),
        };
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

    // Numeric arithmetic
    AddNumeric,
    SubNumeric,
    MulNumeric,
    DivNumeric,
    CastNumeric,
    ShiftNumeric,

    // Numeric comparison
    EqualNumeric,
    LeqNumeric,
    GeqNumeric,
    LessNumeric,
    GreaterNumeric,

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
    NumericToText,
    TextToText,
    PartyToText,
    PartyToQuotedText,
    TimeToText,
    DateToText,

    // Conversion from text
    Int64FromText,
    NumericFromText,
    PartyFromText,
    GetParty,

    // Conversions
    Int64ToNumeric,
    NumericToInt64,

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

            ADD_NUMERIC => AddNumeric,
            SUB_NUMERIC => SubNumeric,
            MUL_NUMERIC => MulNumeric,
            DIV_NUMERIC => DivNumeric,
            CAST_NUMERIC => CastNumeric,
            SHIFT_NUMERIC => ShiftNumeric,

            EQUAL_NUMERIC => EqualNumeric,
            LEQ_NUMERIC => LeqNumeric,
            GEQ_NUMERIC => GeqNumeric,
            LESS_NUMERIC => LessNumeric,
            GREATER_NUMERIC => GreaterNumeric,

            APPEND_TEXT => AppendText,
            IMPLODE_TEXT => ImplodeText,
            EXPLODE_TEXT => ExplodeText,
            SHA256_TEXT => Sha256Text,
            TEXT_FROM_CODE_POINTS => TextFromCodePoints,
            TEXT_TO_CODE_POINTS => TextToCodePoints,

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
            TO_TEXT_NUMERIC => NumericToText,
            TO_TEXT_TEXT => TextToText,
            TO_TEXT_PARTY => PartyToText,
            TO_QUOTED_TEXT_PARTY => PartyToQuotedText,
            TO_TEXT_TIMESTAMP => TimeToText,
            TO_TEXT_DATE => DateToText,

            FROM_TEXT_INT64 => Int64FromText,
            FROM_TEXT_NUMERIC => NumericFromText,
            FROM_TEXT_PARTY => PartyFromText,

            INT64_TO_NUMERIC => Int64ToNumeric,
            NUMERIC_TO_INT64 => NumericToInt64,

            FOLDR => Foldr,
            FOLDL => Foldl,
            EQUAL_LIST => EqualList,

            ERROR => Error,

            TEXTMAP_EMPTY => panic!("This is handled in Expr::from_proto"),
            TEXTMAP_INSERT => MapInsert,
            TEXTMAP_LOOKUP => MapLookup,
            TEXTMAP_DELETE => MapDelete,
            TEXTMAP_TO_LIST => MapToList,
            TEXTMAP_SIZE => MapSize,

            // Decimal unsupported
            ADD_DECIMAL | SUB_DECIMAL | MUL_DECIMAL | DIV_DECIMAL | ROUND_DECIMAL
            | EQUAL_DECIMAL | LEQ_DECIMAL | LESS_DECIMAL | GEQ_DECIMAL | GREATER_DECIMAL
            | TO_TEXT_DECIMAL | FROM_TEXT_DECIMAL | INT64_TO_DECIMAL | DECIMAL_TO_INT64
            | ROUND_NUMERIC | GENMAP_EMPTY | GENMAP_INSERT | GENMAP_LOOKUP | GENMAP_DELETE
            | GENMAP_KEYS | GENMAP_VALUES | GENMAP_SIZE | EQUAL_TYPE_REP | EQUAL | LESS_EQ
            | LESS | GREATER_EQ | GREATER | TEXT_TO_UPPER | TEXT_TO_LOWER | TEXT_SLICE
            | TEXT_SLICE_INDEX | TEXT_CONTAINS_ONLY | TEXT_REPLICATE | TEXT_SPLIT_ON
            | TEXT_INTERCALATE => Unsupported(proto),

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
    Numeric(BigDecimal),
    Text(String),
    MapEmpty,
    Unsupported(&'static str),
}

impl PrimLit {
    fn from_proto(proto: daml_lf_1::PrimLit, env: &Env) -> PrimLit {
        use daml_lf_1::PrimLit_oneof_Sum::*;
        match proto.Sum.unwrap() {
            int64(x) => PrimLit::Int64(x),
            decimal_str(_) => PrimLit::Unsupported("PrimLit::Decimal"),
            numeric_interned_str(id) => PrimLit::Numeric(
                env.get_interned_string(id)
                    .parse()
                    .expect("Badly formatted Numeric"),
            ),
            text_str(x) => PrimLit::Text(x),
            text_interned_str(id) => PrimLit::Text(env.get_interned_string(id)),
            timestamp(_) => PrimLit::Unsupported("PrimLit::Timestamp"),
            party_str(_) => PrimLit::Unsupported("PrimLit::Party"),
            party_interned_str(_) => PrimLit::Unsupported("PrimLit::Party"),
            date(_) => PrimLit::Unsupported("PrimLit::Date"),
        }
    }
}

#[derive(Debug)]
pub enum Pat {
    Default,
    Variant(String, Binder),
    Enum(String),
    Unit,
    Bool(bool),
    Nil,
    Cons(Binder, Binder),
    None,
    Some(Binder),
}

impl Pat {
    fn from_proto(proto: daml_lf_1::CaseAlt_oneof_Sum, env: &Env) -> Self {
        use daml_lf_1::CaseAlt_oneof_Sum::*;
        match proto {
            default(_) => Pat::Default,
            variant(x) => {
                use daml_lf_1::CaseAlt_Variant_oneof_binder::*;
                use daml_lf_1::CaseAlt_Variant_oneof_variant::*;
                let v = match x.variant.unwrap() {
                    variant_str(name) => name,
                    variant_interned_str(id) => env.get_interned_string(id),
                };
                let b = match x.binder.unwrap() {
                    binder_str(name) => name,
                    binder_interned_str(id) => env.get_interned_string(id),
                };
                Pat::Variant(v, b)
            }
            field_enum(x) => {
                use daml_lf_1::CaseAlt_Enum_oneof_constructor::*;
                let con = match x.constructor.unwrap() {
                    constructor_str(name) => name,
                    constructor_interned_str(id) => env.get_interned_string(id),
                };
                Pat::Enum(con)
            }
            prim_con(x) => {
                use daml_lf_1::PrimCon::*;
                match x {
                    CON_UNIT => Pat::Unit,
                    CON_FALSE => Pat::Bool(false),
                    CON_TRUE => Pat::Bool(true),
                }
            }
            nil(_) => Pat::Nil,
            cons(x) => {
                use daml_lf_1::CaseAlt_Cons_oneof_var_head::*;
                use daml_lf_1::CaseAlt_Cons_oneof_var_tail::*;
                let h = match x.var_head.unwrap() {
                    var_head_str(name) => name,
                    var_head_interned_str(id) => env.get_interned_string(id),
                };
                let t = match x.var_tail.unwrap() {
                    var_tail_str(name) => name,
                    var_tail_interned_str(id) => env.get_interned_string(id),
                };
                Pat::Cons(h, t)
            }
            optional_none(_) => Pat::None,
            optional_some(x) => {
                use daml_lf_1::CaseAlt_OptionalSome_oneof_var_body::*;
                let b = match x.var_body.unwrap() {
                    var_body_str(name) => name,
                    var_body_interned_str(id) => env.get_interned_string(id),
                };
                Pat::Some(b)
            }
        }
    }

    fn binders(&self) -> Vec<&Binder> {
        match self {
            Pat::Default => vec![],
            Pat::Variant(_, x) => vec![x],
            Pat::Enum(_) => vec![],
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
        let pattern = Pat::from_proto(proto.Sum.unwrap(), env);
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
    EnumCon {
        tycon: TypeConRef,
        con: String,
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

    fn with_fields_from_proto(
        protos: protobuf::RepeatedField<daml_lf_1::FieldWithExpr>,
        env: &mut Env,
    ) -> (Vec<String>, Vec<Expr>) {
        use daml_lf_1::FieldWithExpr_oneof_field::*;
        protos
            .into_iter()
            .map(|proto| {
                let field = match proto.field.unwrap() {
                    field_str(name) => name,
                    field_interned_str(id) => env.get_interned_string(id),
                };
                (field, Expr::from_proto(proto.expr, env))
            })
            .unzip()
    }

    fn from_proto_unboxed(proto: daml_lf_1::Expr, env: &mut Env) -> Expr {
        use daml_lf_1::Expr_oneof_Sum::*;

        let mut opt_location: Option<Location> = Location::from_proto(proto.location, env);
        let expr = match proto.Sum.unwrap() {
            var_str(name) => {
                let index = env.get(&name);
                Expr::Var { name, index }
            }
            var_interned_str(id) => {
                let name = env.get_interned_string(id);
                let index = env.get(&name);
                Expr::Var { name, index }
            }
            val(x) => {
                let module_ref = ModuleRef::from_proto(x.module, env);
                let name = if x.name_dname.len() == 0 {
                    env.get_interned_dotted_name(x.name_interned_dname)
                } else if x.name_interned_dname == 0 {
                    x.name_dname.as_slice().join(".")
                } else {
                    panic!("items and id both set for internable string list")
                };
                Expr::Val { module_ref, name }
            }
            builtin(daml_lf_1::BuiltinFunction::TEXTMAP_EMPTY) => Expr::PrimLit(PrimLit::MapEmpty),
            builtin(x) => Expr::Builtin(Builtin::from_proto(x)),
            prim_con(x) => {
                use daml_lf_1::PrimCon::*;
                Expr::PrimLit(match x {
                    CON_UNIT => PrimLit::Unit,
                    CON_FALSE => PrimLit::Bool(false),
                    CON_TRUE => PrimLit::Bool(true),
                })
            }
            prim_lit(x) => Expr::PrimLit(PrimLit::from_proto(x, env)),
            rec_con(x) => {
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let (fields, exprs) = Self::with_fields_from_proto(x.fields, env);
                Expr::RecCon {
                    tycon,
                    fields,
                    exprs,
                }
            }
            rec_proj(x) => {
                use daml_lf_1::Expr_RecProj_oneof_field::*;
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let field = match x.field.unwrap() {
                    field_str(name) => name,
                    field_interned_str(id) => env.get_interned_string(id),
                };
                let record = Self::boxed_from_proto(x.record, env);
                Expr::RecProj {
                    tycon,
                    field,
                    record,
                }
            }
            rec_upd(x) => {
                use daml_lf_1::Expr_RecUpd_oneof_field::*;
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let field = match x.field.unwrap() {
                    field_str(name) => name,
                    field_interned_str(id) => env.get_interned_string(id),
                };
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
                use daml_lf_1::Expr_VariantCon_oneof_variant_con::*;
                let tycon = TypeConRef::from_proto(x.tycon.unwrap().tycon, env);
                let con = match x.variant_con.unwrap() {
                    variant_con_str(name) => name,
                    variant_con_interned_str(id) => env.get_interned_string(id),
                };
                let arg = Self::boxed_from_proto(x.variant_arg, env);
                Expr::VariantCon { tycon, con, arg }
            }
            struct_con(x) => {
                let (fields, exprs) = Self::with_fields_from_proto(x.fields, env);
                Expr::TupleCon { fields, exprs }
            }
            struct_proj(x) => {
                use daml_lf_1::Expr_StructProj_oneof_field::*;
                let field = match x.field.unwrap() {
                    field_str(name) => name,
                    field_interned_str(id) => env.get_interned_string(id),
                };
                let tuple = Self::boxed_from_proto(x.field_struct, env);
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
            ty_app(x) => {
                // NOTE(MH): Ignoring type synonyms, which we only use for
                // dictionary types right now, the only ways to construct a type
                // of kind `Nat` are type level literals and type variables of
                // kind `Nat`. The latter are tracked in the environment.
                let fun = Self::from_proto(x.expr, env);
                let args: Vec<Expr> = x
                    .types
                    .into_iter()
                    .filter_map(|typ| {
                        use daml_lf_1::Type_oneof_Sum::*;
                        match typ.Sum.unwrap() {
                            nat(n) => Some(Expr::PrimLit(PrimLit::Int64(n))),
                            var(ty_var) if ty_var.args.len() == 0 => {
                                use daml_lf_1::Type_Var_oneof_var::*;
                                let name = match ty_var.var.unwrap() {
                                    var_str(name) => name,
                                    var_interned_str(id) => env.get_interned_string(id),
                                };
                                env.get_ty_var(&name).map(|index| Expr::Var { name, index })
                            }
                            _ => None,
                        }
                    })
                    .collect();
                // TODO(MH): Once we've implemented a pass to merge adjacent
                // `Expr::App`s, we can always return the else case here.
                if args.is_empty() {
                    fun
                } else {
                    Expr::App {
                        fun: Box::new(fun),
                        args,
                    }
                }
            }
            abs(x) => {
                let params: Vec<Binder> = x
                    .param
                    .into_iter()
                    .map(|x| var_with_type_from_proto(x.var, env))
                    .collect();
                let body = {
                    env.push_many(&params);
                    let body = Self::boxed_from_proto(x.body, env);
                    env.pop_many(&params);
                    body
                };
                Expr::Lam { params, body }
            }
            ty_abs(x) => {
                use daml_lf_1::TypeVarWithKind_oneof_var::*;
                let all_params: Vec<(Binder, bool)> = x
                    .param
                    .into_iter()
                    .map(|param| {
                        let binder = match param.var.unwrap() {
                            var_str(name) => name,
                            var_interned_str(id) => env.get_interned_string(id),
                        };
                        let is_nat = matches!(
                            param.kind.unwrap().Sum.unwrap(),
                            daml_lf_1::Kind_oneof_Sum::nat(_)
                        );
                        (binder, is_nat)
                    })
                    .collect();
                let body = {
                    for (binder, is_nat) in &all_params {
                        env.push_ty_var(binder, *is_nat);
                    }
                    let body = Self::from_proto(x.body, env);
                    for (binder, _) in all_params.iter().rev() {
                        env.pop_ty_var(binder);
                    }
                    body
                };
                let params: Vec<Binder> = all_params
                    .into_iter()
                    .filter_map(|(binder, is_nat)| if is_nat { Some(binder) } else { None })
                    .collect();
                // TODO(MH): Once we've implemented a pass to merge adjacent
                // `Expr::Lam`s, we can always return the else case here.
                if params.is_empty() {
                    body
                } else {
                    Expr::Lam {
                        params,
                        body: Box::new(body),
                    }
                }
            }
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
                let mut bindings = Vec::with_capacity(x.bindings.len());
                for binding in x.bindings.into_iter() {
                    let binder = var_with_type_from_proto(binding.binder.unwrap().var, env);
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
            optional_none(_) => Expr::PrimLit(PrimLit::None),
            optional_some(x) => {
                let body = Self::from_proto(x.body, env);
                Expr::App {
                    fun: Box::new(Expr::Builtin(Builtin::Some)),
                    args: vec![body],
                }
            }
            enum_con(x) => {
                use daml_lf_1::Expr_EnumCon_oneof_enum_con::*;
                let tycon = TypeConRef::from_proto(x.tycon, env);
                let con = match x.enum_con.unwrap() {
                    enum_con_str(name) => name,
                    enum_con_interned_str(id) => env.get_interned_string(id),
                };
                Expr::EnumCon { tycon, con }
            }
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
                let body = if let Some(location) = opt_location.take() {
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
                let body = if let Some(location) = opt_location.take() {
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
            struct_upd(_) => Expr::Unsupported("Expr::TupleUpd"),
            to_any(_) => Expr::Unsupported("Expr::ToAny"),
            from_any(_) => Expr::Unsupported("Expr::FromAny"),
            type_rep(_) => Expr::Unsupported("Expr::TypeRep"),
        };

        if let Some(location) = opt_location {
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
                let mut bindings = Vec::with_capacity(x.bindings.len());
                for binding in x.bindings.into_iter() {
                    let binder = var_with_type_from_proto(binding.binder.unwrap().var, env);
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
                use daml_lf_1::Update_Exercise_oneof_choice::*;
                let template_ref = TypeConRef::from_proto(exercise_proto.template, env);
                let choice = match exercise_proto.choice.unwrap() {
                    choice_str(name) => name,
                    choice_interned_str(id) => env.get_interned_string(id),
                };
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
                let mut bindings = Vec::with_capacity(x.bindings.len());
                for binding in x.bindings.into_iter() {
                    let binder = var_with_type_from_proto(binding.binder.unwrap().var, env);
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
        let name_with_type = proto.name_with_type.unwrap();
        let name = if name_with_type.name_dname.len() == 0 {
            env.get_interned_dotted_name(name_with_type.name_interned_dname)
        } else if name_with_type.name_interned_dname == 0 {
            name_with_type.name_dname.as_slice().join(".")
        } else {
            panic!("items and id both set for internable string list")
        };
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
pub enum DataCons {
    Record { fields: Vec<String> },
    Variant { constructors: Vec<String> },
    Enum { constructors: Vec<String> },
}

impl DataCons {
    fn from_proto(proto: daml_lf_1::DefDataType_oneof_DataCons, env: &Env) -> Self {
        use daml_lf_1::DefDataType_oneof_DataCons::*;
        match proto {
            record(x) => {
                use daml_lf_1::FieldWithType_oneof_field::*;
                let fields = x
                    .fields
                    .into_iter()
                    .map(|y| match y.field.unwrap() {
                        field_str(name) => name,
                        field_interned_str(id) => env.get_interned_string(id),
                    })
                    .collect();
                DataCons::Record { fields }
            }
            variant(x) => {
                use daml_lf_1::FieldWithType_oneof_field::*;
                let constructors = x
                    .fields
                    .into_iter()
                    .map(|y| match y.field.unwrap() {
                        field_str(name) => name,
                        field_interned_str(id) => env.get_interned_string(id),
                    })
                    .collect();
                DataCons::Variant { constructors }
            }
            field_enum(x) => {
                let constructors = match (
                    x.constructors_str.is_empty(),
                    x.constructors_interned_str.is_empty(),
                ) {
                    (true, true) => Vec::new(),
                    (true, false) => x
                        .constructors_interned_str
                        .into_iter()
                        .map(|id| env.get_interned_string(id))
                        .collect(),
                    (false, true) => x.constructors_str.into_vec(),
                    (false, false) => panic!("enum with interned and non-interned constructors"),
                };
                DataCons::Enum { constructors }
            }
        }
    }
}

#[derive(Debug)]
pub struct DefDataType {
    pub name: String,
    pub location: Option<Location>,
    pub params: Vec<Binder>,
    pub cons: DataCons,
    pub is_serializable: bool,
}

impl DefDataType {
    fn from_proto(proto: daml_lf_1::DefDataType, env: &Env) -> Self {
        use daml_lf_1::DefDataType_oneof_name::*;
        use daml_lf_1::TypeVarWithKind_oneof_var::*;
        let name = match proto.name.unwrap() {
            name_dname(dotted_name) => dotted_name_from_proto(dotted_name),
            name_interned_dname(id) => env.get_interned_dotted_name(id),
        };
        let location = Location::from_proto(proto.location, env);
        let params = proto
            .params
            .into_iter()
            .map(|param| match param.var.unwrap() {
                var_str(name) => name,
                var_interned_str(id) => env.get_interned_string(id),
            })
            .collect();
        let cons = DataCons::from_proto(proto.DataCons.unwrap(), env);
        let is_serializable = proto.serializable;
        Self {
            name,
            location,
            params,
            cons,
            is_serializable,
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
        use daml_lf_1::TemplateChoice_oneof_name::*;
        use daml_lf_1::TemplateChoice_oneof_self_binder::*;
        let name = match proto.name.unwrap() {
            name_str(name) => name,
            name_interned_str(id) => env.get_interned_string(id),
        };
        let location = Location::from_proto(proto.location, env);
        let template_ref = template_ref.clone();
        let consuming = proto.consuming;
        let self_binder = match proto.self_binder.unwrap() {
            self_binder_str(name) => name,
            self_binder_interned_str(id) => env.get_interned_string(id),
        };
        let arg_binder = var_with_type_from_proto(proto.arg_binder.unwrap().var, env);
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
        use daml_lf_1::DefTemplate_oneof_param::*;
        use daml_lf_1::DefTemplate_oneof_tycon::*;
        let name = match proto.tycon.unwrap() {
            tycon_dname(dotted_name) => dotted_name_from_proto(dotted_name),
            tycon_interned_dname(id) => env.get_interned_dotted_name(id),
        };
        let location = Location::from_proto(proto.location, env);
        let self_ref = TypeConRef {
            module_ref: module_ref.clone(),
            name: name.clone(),
        };
        let this_binder = match proto.param.unwrap() {
            param_str(name) => name,
            param_interned_str(id) => env.get_interned_string(id),
        };
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
    data_types: FnvHashMap<String, DefDataType>,
}

impl Module {
    fn from_proto(proto: daml_lf_1::Module, package_id: &str, env: &mut Env) -> Self {
        use daml_lf_1::Module_oneof_name::*;
        let name = match proto.name.unwrap() {
            name_dname(dotted_name) => dotted_name_from_proto(dotted_name),
            name_interned_dname(id) => env.get_interned_dotted_name(id),
        };
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
        let data_types = proto
            .data_types
            .into_iter()
            .map(|x| {
                let data_type = DefDataType::from_proto(x, env);
                (data_type.name.clone(), data_type)
            })
            .collect();
        Module {
            name,
            self_ref,
            values,
            templates,
            data_types,
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
        use protobuf::Message;
        let mut is = protobuf::CodedInputStream::from_bytes(&proto.payload);
        is.set_recursion_limit(1000);
        let mut payload = daml_lf::ArchivePayload::new(); // protobuf::parse_from_bytes(&proto.payload).unwrap();
        payload.merge_from(&mut is).unwrap();
        payload.check_initialized().unwrap();
        let id = proto.hash;
        let modules = match payload.Sum.unwrap() {
            daml_lf::ArchivePayload_oneof_Sum::daml_lf_0(_) => panic!("DAML-LF 0.x not supported"),
            daml_lf::ArchivePayload_oneof_Sum::daml_lf_1(payload_proto) => {
                let interned_strings = payload_proto.interned_strings.into_vec();
                let interned_dotted_names = payload_proto
                    .interned_dotted_names
                    .into_iter()
                    .map(|name_proto| {
                        name_proto
                            .segments_interned_str
                            .into_iter()
                            .map(|segment_id| interned_strings[segment_id as usize].clone())
                            .collect::<Vec<_>>()
                            .join(".")
                    })
                    .collect::<Vec<String>>();
                let mut env = Env::new(id.clone(), interned_strings, interned_dotted_names);
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
    pub numeric_regex: Regex,
}

impl World {
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> Result<Self> {
        use std::fs::File;
        let zip_file: File = File::open(path)?;
        let mut zip = zip::ZipArchive::new(zip_file)?;
        let manifest = read_manifest(zip.by_name("META-INF/MANIFEST.MF")?)?;
        let main_name = manifest
            .get("Main-Dalf")
            .expect("Missing Main-Dalf in manifest")
            .to_owned();
        let mut package_names = manifest
            .get("Dalfs")
            .expect("Missing Dalfs in manifest")
            .split(',')
            .map(|x| x.trim().to_owned())
            .collect::<Vec<_>>();
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
        let numeric_regex = Regex::new(r"^[\+-]?\d+(\.\d+)?$").unwrap();
        let world = World {
            main,
            packages,
            map_entry_fields,
            numeric_regex,
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

fn read_manifest<R: Read>(reader: R) -> Result<HashMap<String, String>> {
    let buffered = BufReader::new(reader);
    let mut result = HashMap::new();
    let mut current_section_opt = None;
    let mut current_content = String::new();
    for line in buffered.lines() {
        let line = line?;
        if line.starts_with(' ') {
            if current_section_opt.is_some() {
                current_content += line.trim_start();
            } else {
                return Err(Error::from(ErrorKind::InvalidData));
            }
        } else if let Some(index) = line.find(':') {
            if let Some(current_section) = current_section_opt.take() {
                result.insert(
                    current_section,
                    std::mem::replace(&mut current_content, String::new()),
                );
            }
            let (section, content) = line.split_at(index);
            current_section_opt = Some(section.to_owned());
            current_content = content[1..].trim_start().to_owned();
        } else {
            return Err(Error::from(ErrorKind::InvalidData));
        }
    }
    if let Some(current_section) = current_section_opt {
        result.insert(current_section, current_content);
    }
    Ok(result)
}
