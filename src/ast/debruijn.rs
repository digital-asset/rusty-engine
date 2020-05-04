// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::borrow::Borrow;
use std::collections::HashMap;

use super::{Binder, PackageId};

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
