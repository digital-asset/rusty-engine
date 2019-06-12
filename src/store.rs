// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use fnv::FnvHashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::value::{ContractId, Value};

#[derive(Debug)]
enum Contract<'a> {
    Active {
        template_ref: &'a TypeCon,
        payload: Rc<Value<'a>>,
    },
    #[allow(dead_code)]
    Archived,
}

#[derive(Debug)]
pub struct Store<'a> {
    contracts: FnvHashMap<ContractId, Contract<'a>>,
    next_contract_id: i64,
}

impl<'a> Store<'a> {
    pub fn new() -> Self {
        Self {
            contracts: FnvHashMap::default(),
            next_contract_id: 0,
        }
    }

    pub fn create(&mut self, template_ref: &'a TypeCon, payload: Rc<Value<'a>>) -> ContractId {
        let contract_id = ContractId::new(self.next_contract_id);
        let contract = Contract::Active {
            template_ref,
            payload,
        };
        self.contracts.insert(contract_id.clone(), contract);
        self.next_contract_id += 1;
        contract_id
    }

    pub fn fetch(&self, template_ref: &'a TypeCon, contract_id: &ContractId) -> Rc<Value<'a>> {
        let contract = self
            .contracts
            .get(contract_id)
            .unwrap_or_else(|| panic!("unknown contract id: {}", contract_id));
        match contract {
            Contract::Active {
                template_ref: stored_template_ref,
                payload,
            } => {
                if template_ref == *stored_template_ref {
                    Rc::clone(payload)
                } else {
                    panic!("type mismatch for contract id: {}", contract_id)
                }
            }
            Contract::Archived => panic!("contract id already archived: {}", contract_id),
        }
    }

    pub fn archive(&mut self, template_ref: &'a TypeCon, contract_id: &ContractId) {
        let contract = self
            .contracts
            .get_mut(contract_id)
            .unwrap_or_else(|| panic!("unknown contract id: {}", contract_id));
        match contract {
            Contract::Active {
                template_ref: stored_template_ref,
                ..
            } => {
                if template_ref == *stored_template_ref {
                    *contract = Contract::Archived
                } else {
                    panic!("type mismatch for contract id: {}", contract_id)
                }
            }
            Contract::Archived => panic!("contract id already archived: {}", contract_id),
        }
    }
}
