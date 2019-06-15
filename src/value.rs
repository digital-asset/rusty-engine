// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::FnvHashSet;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use crate::ast::{Builtin, Expr, TypeConRef};

#[derive(Clone, Debug)]
pub struct Env<'a> {
    pub stack: Vec<Rc<Value<'a>>>,
}

// NOTE(MH): Cloning this must remain cheap.
#[derive(Debug, Clone)]
pub enum Prim<'a> {
    Builtin(Builtin),
    RecCon(&'a TypeConRef, &'a Vec<String>),
    RecProj(&'a TypeConRef, &'a String),
    RecUpd(&'a TypeConRef, &'a String),
    VariantCon(&'a TypeConRef, &'a String),
    Lam(&'a Expr, Env<'a>),
    CreateCall(&'a TypeConRef),
    CreateCheckPrecondition(&'a TypeConRef),
    CreateExec(&'a TypeConRef),
    Fetch(&'a TypeConRef),
    ExerciseCall(&'a TypeConRef, &'a String),
    ExerciseExec(&'a TypeConRef, &'a String),
    Submit { should_succeed: bool },
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Party(String);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ContractId(i64);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Time(i64);

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Date(i64);

#[derive(Debug)]
pub enum Value<'a> {
    Unit,
    Bool(bool),
    Int64(i64),
    Text(String),
    Party(Party),
    ContractId(ContractId),
    Time(Time),
    Date(Date),
    RecCon(&'a TypeConRef, &'a Vec<String>, Vec<Rc<Value<'a>>>),
    VariantCon(&'a TypeConRef, &'a String, Rc<Value<'a>>),
    Nil,
    Cons(Rc<Value<'a>>, Rc<Value<'a>>),
    None,
    Some(Rc<Value<'a>>),
    Token, // The "real world" token for the `Update` monad.
    PAP(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
}

#[derive(Debug)]
pub struct ValueListIter<'a>(Option<(Rc<Value<'a>>, Rc<Value<'a>>)>);

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Env { stack: Vec::new() }
    }

    pub fn get(&self, idx: usize) -> &Rc<Value<'a>> {
        self.stack
            .get(self.stack.len() - idx)
            .expect("Bad de Bruijn index")
    }

    pub fn top(&self) -> &Rc<Value<'a>> {
        self.stack.last().expect("Top on empty stack")
    }

    pub fn push(&mut self, value: Rc<Value<'a>>) {
        self.stack.push(value);
    }

    pub fn push_many(&mut self, args: &[Rc<Value<'a>>]) {
        self.stack.extend_from_slice(args);
    }

    pub fn pop(&mut self) -> Rc<Value<'a>> {
        self.stack.pop().expect("Pop from empty stack")
    }

    pub fn pop_many(&mut self, count: usize) {
        let new_len = self.stack.len() - count;
        self.stack.truncate(new_len);
    }
}

impl FromStr for Party {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn is_party_char(c: char) -> bool {
            c.is_ascii_alphanumeric() || c == ' ' || c == '-' || c == '_' || c == ':'
        }
        if !s.is_empty() && s.chars().all(is_party_char) {
            Ok(Party(String::from(s)))
        } else {
            Err(format!("Invalid party name: '{}'", s))
        }
    }
}

impl fmt::Display for Party {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ContractId {
    pub fn new(id: i64) -> Self {
        Self(id)
    }
}

impl fmt::Display for ContractId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Time {
    pub const EPOCH: Self = Time(0);

    pub fn from_micros_since_epoch(ms: i64) -> Self {
        Self(ms)
    }

    pub fn to_micros_since_epoch(self) -> i64 {
        self.0
    }
}

impl fmt::Display for Time {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}ms SE", self.0)
    }
}

impl Date {
    pub fn from_days_since_epoch(days: i64) -> Self {
        Self(days)
    }

    pub fn to_days_since_epoch(self) -> i64 {
        self.0
    }
}

impl fmt::Display for Date {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}d SE", self.0)
    }
}

impl<'a> Value<'a> {
    pub fn from_list<I>(iter: I) -> Self
    where
        I: Iterator<Item = Rc<Value<'a>>> + DoubleEndedIterator,
    {
        iter.rev()
            .fold(Value::Nil, |tail, head| Value::Cons(head, Rc::new(tail)))
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            _ => panic!("Expected Bool, found {:?}", self),
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            Value::Int64(i) => *i,
            _ => panic!("Expected Int64, found {:?}", self),
        }
    }

    pub fn as_string(&self) -> &String {
        match self {
            Value::Text(s) => &s,
            _ => panic!("Expected Text, found {:?}", self),
        }
    }

    pub fn as_party(&self) -> &Party {
        match self {
            Value::Party(p) => &p,
            _ => panic!("Expected Party, found {:?}", self),
        }
    }

    pub fn as_contract_id(&self) -> &ContractId {
        match self {
            Value::ContractId(c) => &c,
            _ => panic!("Expected ContractId, found {:?}", self),
        }
    }

    pub fn as_time(&self) -> &Time {
        match self {
            Value::Time(t) => &t,
            _ => panic!("Expected Time, found {:?}", self),
        }
    }

    pub fn as_date(&self) -> &Date {
        match self {
            Value::Date(d) => &d,
            _ => panic!("Expected Date, found {:?}", self),
        }
    }

    pub fn as_list(&self) -> ValueListIter<'a> {
        ValueListIter(match self {
            Value::Nil => None,
            Value::Cons(head, tail) => Some((Rc::clone(head), Rc::clone(tail))),
            _ => panic!("Expected List, found {:?}", self),
        })
    }

    pub fn as_party_set(&self) -> FnvHashSet<Party> {
        self.as_list()
            .map(|value| value.as_party().clone())
            .collect()
    }
}

impl<'a> Iterator for ValueListIter<'a> {
    type Item = Rc<Value<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let this = std::mem::replace(&mut self.0, None);
        this.map(|(head, tail)| {
            *self = tail.as_list();
            head
        })
    }
}
