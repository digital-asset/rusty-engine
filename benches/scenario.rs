// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

use rusty_engine::*;

const ADDRESS_VAR: &str = "RUSTY_BENCH_SCENARIO";

pub fn criterion_benchmark(c: &mut Criterion) {
    use std::env::VarError;
    let address = match std::env::var(ADDRESS_VAR) {
        Ok(val) => val,
        Err(VarError::NotPresent) => {
            String::from("test/collect-authority.dar:CollectAuthority:test")
        }
        Err(VarError::NotUnicode(_)) => panic!(
            "environment variable {} contains non-unicode characters",
            ADDRESS_VAR,
        ),
    };
    let address_parts: Vec<&str> = address.split(':').collect();
    if address_parts.len() != 3 {
        panic!("scenario address needs to be of the form 'path/to.dar:Module.Name:scenario'");
    }
    let dar = std::path::Path::new(address_parts[0]);
    let dar_file = dar.file_name().unwrap().to_str().unwrap();
    let module = address_parts[1];
    let scenario = address_parts[2];
    let world = World::load(dar).unwrap();

    c.bench_with_input(
        BenchmarkId::new("scenario", format!("{}:{}:{}", dar_file, module, scenario)),
        &(world, module, scenario),
        |b, (world, module, scenario)| {
            b.iter(|| {
                let mut store = Store::new();
                let entry_point = world.entry_point(module, scenario);
                let state = State::new(&entry_point, &world, &mut store);
                let _result = state.run();
            })
        },
    );
}

use std::time::Duration;

criterion_group! {
    name = benches;
    config = Criterion::default()
        .measurement_time(Duration::from_secs(30))
        .warm_up_time(Duration::from_secs(10))
        .sample_size(20)
        .configure_from_args();
    targets = criterion_benchmark
}
criterion_main!(benches);
