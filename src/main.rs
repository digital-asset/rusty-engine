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
    count: i64,
    duration: Duration,
    store: Store<'a>,
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

fn run<'a>(world: &'a World, entry_point: &'a Expr) -> RunResult<'a> {
    let start = Instant::now();
    let store = Store::new();
    let mut state = State::init(&entry_point, store);
    let mut count = 0;
    while !state.is_final() {
        // state.print_debug();
        // println!("============================================================");
        state.step(&world);
        count += 1;
    }
    let duration = start.elapsed();
    let (value, store) = state.get_result();

    RunResult {
        count,
        duration,
        store,
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
        "Input:  {}\nSteps:  {}\nTime:   {:?}\nResult: {:?}\nStore:\n{:?}",
        filename, run_result.count, run_result.duration, run_result.value, run_result.store,
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
        let entry_point = make_entry_point(&world);
        let run_result = run(&world, &entry_point);
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
