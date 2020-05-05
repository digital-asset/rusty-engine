// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::env;
use std::time::Instant;

use rusty_engine::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 || args.len() > 4 {
        println!(
            "usage: {} <path-to-dar> [<module-name> [<scenario-name>]]",
            &args[0]
        );
        std::process::exit(1);
    }
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
            println!("Test:      {}", test_name);
            let start = Instant::now();
            let mut store = Store::new();
            let entry_point = world.entry_point(&module.name, &value.name);
            let state = State::new(entry_point, &world, &mut store);
            let result = state.run();
            let duration = start.elapsed();
            let (active, archived) = store.stats();

            match result {
                Ok(val) => println!(
                    "Contracts: {} active / {} archived\nResult:    {:?}\nTime:      {:?}",
                    active, archived, val, duration
                ),
                Err(err) => {
                    failed_tests.push(test_name);
                    println!("Failed: {}", err)
                }
            }
        }
    }

    if !failed_tests.is_empty() {
        println!("Failed tests:");
        for test_name in failed_tests {
            println!("* {}", test_name);
        }
        println!("Some tests failed");
        std::process::exit(1);
    }

    println!("All tests passed");
    Ok(())
}
