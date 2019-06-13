// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::FnvHashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::value::{ContractId, Value};

#[derive(Debug)]
enum Contract<'a> {
    Active {
        template_ref: &'a TypeConRef,
        payload: Rc<Value<'a>>,
    },
    #[allow(dead_code)]
    Archived,
}

#[derive(Debug)]
pub struct Store<'a> {
    committed: FnvHashMap<ContractId, Contract<'a>>,
    pending: FnvHashMap<ContractId, Contract<'a>>,
    next_committed_contract_id: i64,
    next_pending_contract_id: i64,
}

impl<'a> Contract<'a> {
    fn get_payload_and_typecheck(
        &self,
        expected_template_ref: &'a TypeConRef,
        contract_id: &ContractId,
    ) -> &Rc<Value<'a>> {
        match self {
            Contract::Active {
                template_ref,
                payload,
            } => {
                if *template_ref == expected_template_ref {
                    payload
                } else {
                    panic!("type mismatch for contract id: {}", contract_id)
                }
            }
            Contract::Archived => panic!("contract id already archived: {}", contract_id),
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

    pub fn create(&mut self, template_ref: &'a TypeConRef, payload: Rc<Value<'a>>) -> ContractId {
        let contract_id = ContractId::new(self.next_pending_contract_id);
        let contract = Contract::Active {
            template_ref,
            payload,
        };
        self.pending.insert(contract_id.clone(), contract);
        self.next_pending_contract_id += 1;
        contract_id
    }

    pub fn fetch(&self, template_ref: &'a TypeConRef, contract_id: &ContractId) -> Rc<Value<'a>> {
        let contract = self
            .pending
            .get(contract_id)
            .or_else(|| self.committed.get(contract_id))
            .unwrap_or_else(|| panic!("unknown contract id: {}", contract_id));
        Rc::clone(contract.get_payload_and_typecheck(template_ref, contract_id))
    }

    pub fn archive(&mut self, template_ref: &'a TypeConRef, contract_id: &ContractId) {
        let _payload = self.fetch(template_ref, contract_id);
        // TODO(MH): There's no need to clone the contract id when it is
        // already present in the pending contracts.
        self.pending.insert(contract_id.clone(), Contract::Archived);
    }

    #[allow(dead_code)]
    pub fn commit(&mut self) {
        self.committed.extend(self.pending.drain());
        self.next_committed_contract_id = self.next_pending_contract_id;
    }

    #[allow(dead_code)]
    pub fn rollback(&mut self) {
        self.pending.clear();
        self.next_pending_contract_id = self.next_committed_contract_id;
    }
}
