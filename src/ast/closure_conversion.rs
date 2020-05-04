// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::collections::{HashMap, HashSet};

use super::Expr;

fn rewrite_index(index: &mut usize, rewrite: &HashMap<usize, usize>, offset: usize) {
    if *index > offset {
        if let Some(new_index) = rewrite.get(&(*index - offset)) {
            *index = *new_index + offset;
        }
    }
}

fn rewrite_expr(expr: &mut Expr, rewrite: &HashMap<usize, usize>, offset: usize) {
    use Expr::*;
    match expr {
        Var { index, .. } => {
            rewrite_index(index, rewrite, offset);
        }
        Lam {
            params: _,
            captured,
            body: _,
        } => {
            for index in captured {
                rewrite_index(index, rewrite, offset);
            }
        }
        Let { bound, body, .. } => {
            rewrite_expr(bound, rewrite, offset);
            rewrite_expr(body, rewrite, offset + 1);
        }
        Case { scrut, alts } => {
            rewrite_expr(scrut, rewrite, offset);
            for alt in alts {
                rewrite_expr(&mut alt.body, rewrite, offset + alt.pattern.binders().len());
            }
        }
        _ => {
            for child in expr.children_mut() {
                rewrite_expr(child, rewrite, offset);
            }
        }
    }
}

fn unshift_fv(fv: HashSet<usize>, offset: usize) -> HashSet<usize> {
    fv.into_iter()
        .filter_map(|index| {
            if index > offset {
                Some(index - offset)
            } else {
                None
            }
        })
        .collect()
}

fn convert_fv(expr: &mut Expr) -> HashSet<usize> {
    use Expr::*;
    match expr {
        Var { index, .. } => [*index].iter().copied().collect(),
        Lam {
            params,
            captured,
            body,
        } => {
            let arity = params.len();
            let fv = unshift_fv(convert_fv(body), arity);
            let mut fv_vec: Vec<usize> = fv.iter().copied().collect();
            fv_vec.sort();
            let rewrite = fv_vec
                .iter()
                .enumerate()
                .map(|(i, old_index)| (*old_index, i + 1))
                .collect();
            rewrite_expr(body, &rewrite, arity);
            fv_vec.reverse();
            *captured = fv_vec;
            fv
        }
        Let { bound, body, .. } => {
            let mut fv = convert_fv(bound);
            fv.extend(unshift_fv(convert_fv(body), 1));
            fv
        }
        Case { scrut, alts } => {
            let mut fv = convert_fv(scrut);
            for alt in alts {
                fv.extend(unshift_fv(
                    convert_fv(&mut alt.body),
                    alt.pattern.binders().len(),
                ));
            }
            fv
        }
        _ => {
            let mut fv = HashSet::new();
            for child in expr.children_mut() {
                fv.extend(convert_fv(child));
            }
            fv
        }
    }
}

impl Expr {
    pub fn closure_convert(mut self) -> Self {
        convert_fv(&mut self);
        self
    }
}
