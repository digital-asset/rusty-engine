// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::env;
use std::rc::Rc;
use std::time::{Duration, Instant};

mod ast;
mod builtin;
mod cek;
mod protos;
mod value;

use crate::ast::*;
use crate::cek::State;
use crate::value::Value;

struct RunResult<'a> {
    count: i64,
    duration: Duration,
    value: Rc<Value<'a>>,
}

fn make_entry_point(world: &World) -> Expr {
    Expr::Val {
        module_ref: ModuleRef {
            package_id: world.main.clone(),
            module_name: DottedName {
                segments: vec![String::from("Main")],
            },
        },
        name: String::from("main"),
    }
}

fn run<'a>(world: &'a World, entry_point: &'a Expr) -> RunResult<'a> {
    let start = Instant::now();
    let mut state = State::init(&entry_point);
    let mut count = 0;
    while !state.is_final() {
        state.step(&world);
        count += 1;
    }
    let duration = start.elapsed();
    let value = state.get_result();

    RunResult {
        count,
        duration,
        value,
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let world = World::load(filename)?;

    let entry_point = make_entry_point(&world);
    let run_result = run(&world, &entry_point);

    println!(
        "Input:  {}\nSteps:  {}\nTime:   {:?}\nResult: {:?}",
        filename, run_result.count, run_result.duration, run_result.value,
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dar_test(path: &str) {
        let world = World::load(path).unwrap();
        let entry_point = make_entry_point(&world);
        let run_result = run(&world, &entry_point);
        match *run_result.value {
            Value::Unit => (),
            _ => panic!("expected unit result"),
        }
    }

    #[test]
    fn queens() {
        dar_test("test/Queens.dar");
    }

    #[test]
    fn sort() {
        dar_test("test/Sort.dar");
    }

    #[test]
    fn equal_list() {
        dar_test("test/EqualList.dar");
    }

    #[test]
    fn iou() {
        dar_test("test/Iou.dar");
    }
}
