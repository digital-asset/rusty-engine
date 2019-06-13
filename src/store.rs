// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::FnvHashMap;
use fnv::FnvHashSet;
use std::rc::Rc;

use crate::ast::*;
use crate::value::*;

#[derive(Debug)]
pub struct Contract<'a> {
    pub template_ref: &'a TypeConRef,
    pub payload: Rc<Value<'a>>,
    pub signatories: FnvHashSet<Party>,
}

#[derive(Debug)]
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
        &self,
        expected_template_ref: &'a TypeConRef,
        contract_id: &ContractId,
    ) -> &Contract<'a> {
        match self {
            Entry::Active(contract) => {
                if contract.template_ref == expected_template_ref {
                    contract
                } else {
                    panic!("type mismatch for contract id: {}", contract_id)
                }
            }
            Entry::Archived => panic!("contract id already archived: {}", contract_id),
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

    pub fn create(
        &mut self,
        template_ref: &'a TypeConRef,
        payload: Rc<Value<'a>>,
        signatories: FnvHashSet<Party>,
    ) -> ContractId {
        let contract_id = ContractId::new(self.next_pending_contract_id);
        let contract = Contract {
            template_ref,
            payload,
            signatories,
        };
        self.pending
            .insert(contract_id.clone(), Entry::Active(contract));
        self.next_pending_contract_id += 1;
        contract_id
    }

    pub fn fetch(&self, template_ref: &'a TypeConRef, contract_id: &ContractId) -> &Contract<'a> {
        let entry = self
            .pending
            .get(contract_id)
            .or_else(|| self.committed.get(contract_id))
            .unwrap_or_else(|| panic!("unknown contract id: {}", contract_id));
        entry.get_typechecked(template_ref, contract_id)
    }

    pub fn archive(&mut self, template_ref: &'a TypeConRef, contract_id: &ContractId) {
        let _contract = self.fetch(template_ref, contract_id);
        // TODO(MH): There's no need to clone the contract id when it is
        // already present in the pending contracts.
        self.pending.insert(contract_id.clone(), Entry::Archived);
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
