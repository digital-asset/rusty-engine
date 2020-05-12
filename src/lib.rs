// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
#![allow(clippy::new_without_default)]
mod ast;
mod builtin;
mod cesk;
mod protos;
mod speedscope;
mod store;
mod value;

pub use crate::ast::{DefValue, Module, World};
pub use crate::cesk::{Event, State};
pub use crate::speedscope::events_to_speedscope_json;
pub use crate::store::Store;

#[cfg(test)]
mod tests {
    use super::*;
    use fnv::FnvHashMap;

    type ExpectedFailures =
        FnvHashMap<(&'static str, &'static str), (&'static str, Option<Vec<(i32, i32)>>)>;

    fn dar_test(path: &str, expected_failures: &ExpectedFailures) {
        let world = World::load(path).unwrap();
        let main_package = world.main_package();

        for module in main_package.modules.values() {
            for value in module.values.values().filter(|value| value.is_test) {
                let test_name = format!("{}:{}", module.name, value.name);
                println!("Test:   {}", test_name);
                let mut store = Store::new();
                let entry_point = world.entry_point(&module.name, &value.name);
                let state = State::new(&entry_point, &world, &mut store);
                let result = state.run();

                let expected_failure = expected_failures.get(&(&module.name, &value.name));
                match (result, expected_failure) {
                    (Ok(_), None) => (),
                    (Ok(_), Some(_)) => panic!("unexpected success in {}", test_name),
                    (Err(err), None) => panic!("unexpected failure in {}: {}", test_name, err),
                    (Err(ref err), Some((pattern, stack_trace))) => {
                        if !err.message.contains(pattern) {
                            panic!(
                                "expected failure for unexpected reason in {}: {:?}",
                                test_name, err
                            )
                        } else if let Some(stack_trace) = stack_trace {
                            let err_stack_trace: Vec<(i32, i32)> = err
                                .stack_trace
                                .iter()
                                .map(|loc| (loc.start_line, loc.start_col))
                                .collect();
                            if err_stack_trace != *stack_trace {
                                panic!(
                                    "expected failure at unexpected location in {}: {:?}",
                                    test_name, err
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn damlc_tests() {
        let expected_failures: ExpectedFailures = [
            (("BadCodePoint", "test"), ("invalid code point", None)),
            (
                ("EnumFromThenTo", "main"),
                ("enumFromThenTo: from == then", None),
            ),
            (("GetPartyError", "main"), ("Invalid party name:", None)),
            (("HelloWorld", "main"), ("Hello World!", None)),
            (("Lazy", "main"), ("Hello World!", None)),
            (
                ("PatError", "main"),
                ("Non-exhaustive patterns in case", None),
            ),
            (("PolymorphicTest", "main"), ("boom", None)),
            (
                ("Precondition", "test"),
                (
                    "Template pre-condition violated",
                    Some(vec![(21, 16), (23, 2)]),
                ),
            ),
            (
                ("Records", "main"),
                ("B's Company is run by B and they are 3 years old", None),
            ),
            (("RightOfUse", "example"), ("authorization missing", None)),
            (("TransientFailure", "testBio"), ("Assertion failed", None)),
            (("Unicode", "main"), ("⛄ ¯\\_(ツ)_/¯", None)),
            (("UnusedLet", "main"), ("BOOM", None)),
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

    #[test]
    fn collect_authority() {
        dar_test("test/collect-authority.dar", &FnvHashMap::default());
    }
}
