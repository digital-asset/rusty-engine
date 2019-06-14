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

fn make_entry_point(world: &World, module_name: String, scenario_name: String) -> Expr {
    Expr::Val {
        module_ref: ModuleRef {
            package_id: world.main.clone(),
            module_name,
        },
        name: scenario_name,
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let module_filter: Option<&String> = args.get(2);
    let scenario_filter: Option<&String> = args.get(2);

    let use_module =
        |module: &&Module| module_filter.map_or(true, |module_name| module.name == *module_name);
    let use_value = |value: &&DefValue| {
        value.is_test && scenario_filter.map_or(true, |scenario_name| value.name == *scenario_name)
    };

    let world = World::load(filename)?;
    let main_package = world.main_package();

    for module in main_package.modules.values().filter(use_module) {
        for value in module.values.values().filter(use_value) {
            println!("Test:   {}:{}", module.name, value.name);
            let start = Instant::now();
            let mut store = Store::new();
            let entry_point = make_entry_point(&world, module.name.clone(), value.name.clone());
            let state = State::init(&entry_point);
            let result = state.run(&world, &mut store);
            let duration = start.elapsed();

            println!("Result: {:?}\nTime:   {:?}", result, duration);
        }
    }

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
        let entry_point = make_entry_point(&world, "Main".to_string(), "main".to_string());
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
    fn bond_trading() {
        dar_test("test/bond-trading.dar", expect_unit);
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
