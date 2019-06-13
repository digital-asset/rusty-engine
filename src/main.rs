// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::env;
use std::time::Instant;

mod ast;
mod builtin;
mod cesk;
mod protos;
mod store;
mod value;

use crate::ast::*;
use crate::cesk::State;
use crate::store::Store;

fn make_entry_point(world: &World) -> Expr {
    Expr::Val {
        module_ref: ModuleRef {
            package_id: world.main.clone(),
            module_name: String::from("Main"),
        },
        name: String::from("main"),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let world = World::load(filename)?;
    let mut store = Store::new();

    let entry_point = make_entry_point(&world);
    let start = Instant::now();
    let state = State::init(&entry_point);
    let result = state.run(&world, &mut store);
    let duration = start.elapsed();

    println!(
        "Input:  {}\nTime:   {:?}\nResult: {:?}",
        filename, duration, result,
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    fn dar_test<F>(path: &str, check_result: F)
    where
        F: FnOnce(Result<&Value, String>) -> (),
    {
        let world = World::load(path).unwrap();
        let mut store = Store::new();
        let entry_point = make_entry_point(&world);
        let state = State::init(&entry_point);
        let result = state.run(&world, &mut store);
        match result {
            Ok(value) => check_result(Ok(&*value)),
            Err(msg) => check_result(Err(msg)),
        }
    }

    fn expect_unit(result: Result<&Value, String>) {
        match result {
            Ok(Value::Unit) => (),
            _ => panic!("expected Unit, found {:?}", result),
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
        dar_test("test/Iou.dar", |result| match result {
            Ok(Value::Int64(n)) => assert_eq!(*n, 100),
            _ => panic!("expected Int64, found {:?}", result),
        });
    }

    #[test]
    fn error() {
        dar_test("test/Error.dar", |result| match result {
            Err(msg) => assert_eq!(msg, "BOOM"),
            _ => panic!("expected error \"BOOM\", found {:?}", result),
        });
    }
}
