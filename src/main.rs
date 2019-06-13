// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::env;
use std::rc::Rc;
use std::time::{Duration, Instant};

mod ast;
mod builtin;
mod cesk;
mod protos;
mod store;
mod value;

use crate::ast::*;
use crate::cesk::State;
use crate::store::Store;
use crate::value::Value;

struct RunResult<'a> {
    duration: Duration,
    value: Rc<Value<'a>>,
}

fn make_entry_point(world: &World) -> Expr {
    Expr::Val {
        module_ref: ModuleRef {
            package_id: world.main.clone(),
            module_name: String::from("Main"),
        },
        name: String::from("main"),
    }
}

fn run<'a>(world: &'a World, store: &mut Store<'a>, entry_point: &'a Expr) -> RunResult<'a> {
    let start = Instant::now();
    let state = State::init(&entry_point);
    let result = state.run(world, store);
    let duration = start.elapsed();

    RunResult {
        duration,
        value: result,
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let world = World::load(filename)?;
    let mut store = Store::new();

    let entry_point = make_entry_point(&world);
    let run_result = run(&world, &mut store, &entry_point);

    println!(
        "Input:  {}\nTime:   {:?}\nResult: {:?}",
        filename, run_result.duration, run_result.value,
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dar_test<F>(path: &str, check_value: F)
    where
        F: FnOnce(&Value) -> (),
    {
        let world = World::load(path).unwrap();
        let mut store = Store::new();
        let entry_point = make_entry_point(&world);
        let run_result = run(&world, &mut store, &entry_point);
        check_value(&*run_result.value);
    }

    fn expect_unit(value: &Value) {
        match value {
            Value::Unit => (),
            _ => panic!("expected Unit, found {:?}", value),
        }
    }

    #[test]
    fn queens() {
        dar_test("test/Queens.dar", expect_unit);
    }

    #[test]
    fn sort() {
        dar_test("test/Sort.dar", expect_unit);
    }

    #[test]
    fn equal_list() {
        dar_test("test/EqualList.dar", expect_unit);
    }

    #[test]
    fn iou() {
        dar_test("test/Iou.dar", |value| match value {
            Value::Int64(n) => assert_eq!(*n, 100),
            _ => panic!("expected Int64, found {:?}", value),
        });
    }
}
