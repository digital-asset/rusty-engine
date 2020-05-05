// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::iter;

use super::*;

pub struct ChildrenMut<'a> {
    expr1: Option<&'a mut Expr>,
    expr2: Option<&'a mut Expr>,
    exprs: std::slice::IterMut<'a, Expr>,
    alts: std::slice::IterMut<'a, Alt>,
}

impl<'a> ChildrenMut<'a> {
    fn new(
        expr1: Option<&'a mut Expr>,
        expr2: Option<&'a mut Expr>,
        exprs: std::slice::IterMut<'a, Expr>,
    ) -> Self {
        let alts = [].iter_mut();
        ChildrenMut {
            expr1,
            expr2,
            exprs,
            alts,
        }
    }
}

impl<'a> Iterator for ChildrenMut<'a> {
    type Item = &'a mut Expr;

    fn next(&mut self) -> Option<Self::Item> {
        self.expr1
            .take()
            .or_else(|| self.expr2.take())
            .or_else(|| self.exprs.next())
            .or_else(|| self.alts.next().map(|alt| &mut alt.body))
    }
}

impl Expr {
    pub fn children_mut(&mut self) -> ChildrenMut {
        use Expr::*;
        match self {
            Var { .. }
            | Val { .. }
            | Builtin(_)
            | PrimLit(_)
            | EnumCon { .. }
            | GetTime
            | Unsupported(_) => ChildrenMut::new(None, None, [].iter_mut()),
            RecCon { exprs, .. } | TupleCon { exprs, .. } => {
                ChildrenMut::new(None, None, exprs.iter_mut())
            }
            RecProj { record: expr, .. }
            | VariantCon { arg: expr, .. }
            | TupleProj { tuple: expr, .. }
            | Lam { body: expr, .. }
            | Create { payload: expr, .. }
            | Fetch {
                contract_id: expr, ..
            }
            | AdvanceTime { delta: expr }
            | Located { expr, .. } => ChildrenMut::new(Some(expr), None, [].iter_mut()),
            RecUpd {
                record: expr1,
                value: expr2,
                ..
            }
            | Let {
                bound: expr1,
                body: expr2,
                ..
            }
            | Exercise {
                contract_id: expr1,
                arg: expr2,
                ..
            }
            | Submit {
                submitter: expr1,
                update: expr2,
                ..
            } => ChildrenMut::new(Some(expr1), Some(expr2), [].iter_mut()),
            App {
                fun: expr1,
                args: exprs,
            } => ChildrenMut::new(Some(expr1), None, exprs.iter_mut()),
            Case { scrut, alts } => ChildrenMut {
                expr1: Some(scrut),
                expr2: None,
                exprs: [].iter_mut(),
                alts: alts.iter_mut(),
            },
        }
    }
}

impl DefValue {
    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut Expr> {
        iter::once(&mut self.expr)
    }
}

impl Choice {
    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut Expr> {
        iter::once(&mut self.controllers).chain(iter::once(&mut self.consequence))
    }
}

impl DefTemplate {
    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut Expr> {
        iter::once(&mut self.precondtion)
            .chain(iter::once(&mut self.signatories))
            .chain(iter::once(&mut self.observers))
            .chain(self.choices.values_mut().flat_map(Choice::exprs_mut))
    }
}

impl Module {
    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut Expr> {
        self.values
            .values_mut()
            .flat_map(DefValue::exprs_mut)
            .chain(self.templates.values_mut().flat_map(DefTemplate::exprs_mut))
    }
}

impl Package {
    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut Expr> {
        self.modules.values_mut().flat_map(Module::exprs_mut)
    }
}

impl World {
    pub fn exprs_mut(&mut self) -> impl Iterator<Item = &mut Expr> {
        self.packages.values_mut().flat_map(Package::exprs_mut)
    }
}
