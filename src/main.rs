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
    let scenario_filter: Option<&String> = args.get(3);

    let use_module =
        |module: &&Module| module_filter.map_or(true, |module_name| module.name == **module_name);
    let use_value = |value: &&DefValue| {
        value.is_test && scenario_filter.map_or(true, |scenario_name| value.name == *scenario_name)
    };

    let world = World::load(filename)?;
    let main_package = world.main_package();
    let mut failed_tests = Vec::new();

    for module in main_package.modules.values().filter(use_module) {
        for value in module.values.values().filter(use_value) {
            let test_name = format!("{}:{}", module.name, value.name);
            println!("Test:   {}", test_name);
            let start = Instant::now();
            let mut store = Store::new();
            let entry_point = make_entry_point(&world, module.name.clone(), value.name.clone());
            let state = State::init(&entry_point);
            let result = state.run(&world, &mut store);
            let duration = start.elapsed();

            if result.is_err() {
                failed_tests.push(test_name);
            }
            println!("Result: {:?}\nTime:   {:?}", result, duration);
        }
    }

    if !failed_tests.is_empty() {
        println!("Failed tests:");
        for test_name in failed_tests {
            println!("* {}", test_name);
        }
        panic!("Some tests failed");
    }

    println!("All tests passed");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use fnv::FnvHashMap;

    fn dar_test(path: &str, expected_failures: &FnvHashMap<(&str, &str), &str>) {
        let world = World::load(path).unwrap();
        let main_package = world.main_package();

        for module in main_package.modules.values() {
            for value in module.values.values().filter(|value| value.is_test) {
                let test_name = format!("{}:{}", module.name, value.name);
                println!("Test:   {}", test_name);
                let mut store = Store::new();
                let entry_point = make_entry_point(&world, module.name.clone(), value.name.clone());
                let state = State::init(&entry_point);
                let result = state.run(&world, &mut store);

                let expected_failure = expected_failures.get(&(&module.name, &value.name));
                match (result, expected_failure) {
                    (Ok(_), None) => (),
                    (Ok(_), Some(_)) => panic!("unexpected success in {}", test_name),
                    (Err(msg), None) => panic!("unexpected failure in {}: {}", test_name, msg),
                    (Err(msg), Some(pattern)) => {
                        if !msg.contains(pattern) {
                            panic!(
                                "expected failure for unexpected reason in {}: {}",
                                test_name, msg
                            )
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn damlc_tests() {
        let expected_failures: FnvHashMap<(&str, &str), &str> = [
            (("HelloWorld", "main"), "Hello World!"),
            (("PatError", "main"), "Non-exhaustive patterns in case"),
            (("PolymorphicTest", "main"), "boom"),
            (("Precondition", "test"), "Template pre-condition violated"),
            (
                ("Records", "main"),
                "B's Company is run by B and they are 3 years old",
            ),
            (("RightOfUse", "example"), "authorization missing"),
            (("Unicode", "main"), "⛄ ¯\\_(ツ)_/¯"),
            (("UnusedLet", "main"), "BOOM"),
        ]
        .iter()
        .cloned()
        .collect();
        dar_test("test/damlc-tests.dar", &expected_failures);
    }

    #[test]
    fn bond_trading() {
        dar_test("test/bond-trading.dar", &FnvHashMap::default());
    }
}
