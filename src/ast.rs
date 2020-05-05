// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use bigdecimal::BigDecimal;
use fnv::FnvHashMap;
use regex::Regex;
use std::fmt;

use crate::protos::da::daml_lf;
use crate::protos::da::daml_lf_1;

mod closure_conversion;
mod debruijn;
mod from_proto;
mod iter;
mod load;

pub type Binder = String;

pub type PackageId = String;

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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeConRef {
    module_ref: ModuleRef,
    name: String,
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
        captured: Vec<usize>,
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

#[derive(Debug)]
pub struct DefValue {
    pub name: String,
    pub location: Option<Location>,
    pub expr: Expr,
    pub is_test: bool,
}

#[derive(Debug)]
pub enum DataCons {
    Record { fields: Vec<String> },
    Variant { constructors: Vec<String> },
    Enum { constructors: Vec<String> },
}

#[derive(Debug)]
pub struct DefDataType {
    pub name: String,
    pub location: Option<Location>,
    pub params: Vec<Binder>,
    pub cons: DataCons,
    pub is_serializable: bool,
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

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub self_ref: ModuleRef,
    pub values: FnvHashMap<String, DefValue>,
    templates: FnvHashMap<String, DefTemplate>,
    data_types: FnvHashMap<String, DefDataType>,
}

#[derive(Debug)]
pub struct Package {
    pub id: PackageId,
    pub modules: FnvHashMap<String, Module>,
}

#[derive(Debug)]
pub struct World {
    pub main: PackageId,
    packages: FnvHashMap<PackageId, Package>,
    pub map_entry_fields: Vec<String>,
    pub numeric_regex: Regex,
}

impl World {
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
