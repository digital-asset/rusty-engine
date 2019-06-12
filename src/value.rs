// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::borrow::Borrow;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use crate::ast::{Builtin, Expr, TypeCon};

#[derive(Clone, Debug)]
pub struct Env<'a> {
    stack: Vec<Rc<Value<'a>>>,
}

// NOTE(MH): Cloning this must remain cheap.
#[derive(Debug, Clone)]
pub enum Prim<'a> {
    Builtin(Builtin),
    RecCon(&'a TypeCon, &'a Vec<String>),
    RecProj(&'a TypeCon, &'a String),
    VariantCon(&'a TypeCon, &'a String),
    Lam(&'a Expr, Env<'a>),
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Party(String);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ContractId(i64);

#[derive(Debug)]
pub enum Value<'a> {
    Unit,
    Bool(bool),
    Int64(i64),
    Text(String),
    Party(Party),
    ContractId(ContractId),
    RecCon(&'a TypeCon, &'a Vec<String>, Vec<Rc<Value<'a>>>),
    VariantCon(&'a TypeCon, &'a String, Rc<Value<'a>>),
    Nil,
    Cons(Rc<Value<'a>>, Rc<Value<'a>>),
    None,
    Some(Rc<Value<'a>>),
    Token, // The "real world" token for the `Update` monad.
    PAP(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
}

#[derive(Debug)]
pub struct ValueListIter<'a> {
    value: Rc<Value<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Env { stack: Vec::new() }
    }

    pub fn get(&self, idx: usize) -> &Rc<Value<'a>> {
        self.stack
            .get(self.stack.len() - idx)
            .expect("Bad de Bruijn index")
    }

    pub fn push(&mut self, value: Rc<Value<'a>>) {
        self.stack.push(value);
    }

    pub fn push_many(&mut self, args: &[Rc<Value<'a>>]) {
        self.stack.extend_from_slice(args);
    }

    pub fn pop(&mut self, count: usize) {
        let new_len = self.stack.len() - count;
        self.stack.truncate(new_len);
    }
}

impl FromStr for Party {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // TODO(MH): Make sure `s` contains no forbidden characters.
        Ok(Party(String::from(s)))
    }
}

impl fmt::Display for Party {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> Value<'a> {
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

    pub fn make_list_iter(this: &Rc<Self>) -> ValueListIter<'a> {
        ValueListIter {
            value: Rc::clone(this),
        }
    }
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
