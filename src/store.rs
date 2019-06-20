// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::FnvHashMap;
use fnv::FnvHashSet;
use std::rc::Rc;

use crate::ast::*;
use crate::value::*;

#[derive(Clone, Debug)]
pub struct Contract<'a> {
    pub template_ref: &'a TypeConRef,
    pub payload: Rc<Value<'a>>,
    pub signatories: FnvHashSet<Party>,
    pub observers: FnvHashSet<Party>,
    // NOTE(MH): `witnesses` contains both, the parties that witnessed the
    // creation of this contract and the parties that witnessed a fetch or
    // exercise on it.
    pub witnesses: FnvHashSet<Party>,
}

#[derive(Clone, Debug)]
enum Entry<'a> {
    Active(Contract<'a>),
    Archived,
}

#[derive(Debug)]
pub struct Store<'a> {
    committed: FnvHashMap<ContractId, Entry<'a>>,
    pending: FnvHashMap<ContractId, Entry<'a>>,
    next_committed_contract_id: i64,
    next_pending_contract_id: i64,
}

impl<'a> Entry<'a> {
    fn get_typechecked(
        &mut self,
        expected_template_ref: &'a TypeConRef,
        contract_id: &ContractId,
    ) -> Result<&mut Contract<'a>, String> {
        match self {
            Entry::Active(contract) => {
                if contract.template_ref == expected_template_ref {
                    Ok(contract)
                } else {
                    panic!("type mismatch for contract id: {}", contract_id)
                }
            }
            Entry::Archived => Err(format!("contract id already archived: {}", contract_id)),
        }
    }
}

impl<'a> Store<'a> {
    pub fn new() -> Self {
        Self {
            committed: FnvHashMap::default(),
            pending: FnvHashMap::default(),
            next_committed_contract_id: 0,
            next_pending_contract_id: 0,
        }
    }

    pub fn create(&mut self, contract: Contract<'a>) -> ContractId {
        let contract_id = ContractId::new(self.next_pending_contract_id);
        self.pending
            .insert(contract_id.clone(), Entry::Active(contract));
        self.next_pending_contract_id += 1;
        contract_id
    }

    pub fn fetch(
        &mut self,
        submitter: &Party,
        witnesses: &FnvHashSet<Party>,
        template_ref: &'a TypeConRef,
        contract_id: &ContractId,
    ) -> Result<&Contract<'a>, String> {
        if !self.pending.contains_key(contract_id) {
            match self.committed.get(contract_id) {
                Some(entry) => {
                    self.pending.insert(contract_id.clone(), entry.clone());
                }
                None => {
                    return Err(format!("unknown contract id: {}", contract_id));
                }
            }
        }
        let entry = self.pending.get_mut(contract_id).unwrap();
        let contract = entry.get_typechecked(template_ref, contract_id)?;
        if !contract.witnesses.contains(submitter) {
            return Err(format!("undisclosed contract id: {}", contract_id));
        }
        contract.witnesses.extend(witnesses.iter().cloned());
        Ok(contract)
    }

    pub fn archive(
        &mut self,
        submitter: &Party,
        witnesses: &FnvHashSet<Party>,
        template_ref: &'a TypeConRef,
        contract_id: &ContractId,
    ) -> Result<(), String> {
        self.fetch(submitter, witnesses, template_ref, contract_id)?;
        // TODO(MH): There's no need to clone the contract id when it is
        // already present in the pending contracts.
        self.pending.insert(contract_id.clone(), Entry::Archived);
        Ok(())
    }

    pub fn commit(&mut self) {
        self.committed.extend(self.pending.drain());
        self.next_committed_contract_id = self.next_pending_contract_id;
    }

    pub fn rollback(&mut self) {
        self.pending.clear();
        self.next_pending_contract_id = self.next_committed_contract_id;
    }
}
