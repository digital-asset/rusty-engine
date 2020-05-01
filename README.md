[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![CircleCI](https://circleci.com/gh/hurryabit/rusty-engine.svg?style=svg&circle-token=a6e23bed2661e7e96a31d457da8d7b1887a59622)](https://circleci.com/gh/hurryabit/rusty-engine)

Copyright 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All Rights Reserved.
SPDX-License-Identifier: Apache-2.0

**This implementation is highly experimental and not supported by Digital Asset in any way.**

# Prototype of a DAML Engine written in Rust

This is an experimental implementation of a [DAML](https://daml.com/) scenario runner written in [Rust](https://www.rust-lang.org/). It is not intended for production use and clearly not ready for it:

- It can only run scenarios and does not produce transaction trees.
- The `Decimal` type from DAML-LF ≤ 1.6 is not supported at all.
- The `Numeric n` types from DAML-LF ≥ 1.7 are all interpreted by the same unbounded precision decimal type (similar to Java's `BigDecimal`) instead of fixed precision decimal types. Some operations on `Numeric` are not supported at all.
- Contract keys are not supported.
- The `Any` type and `TypeRep` are not supported.
- The conversion between `Text` and code points uses [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value) rather than [Unicode code points](http://www.unicode.org/glossary/#code_point).
- The performance of the `Map` type is atrocious.
- The `trace` primitive is not supported.
- Struct updates are not supported.
- There has been barely any performance tweaking.
- There has been at most very superficial code review and the test coverage is quite low.

# How to use this implementation

This implementation is developed using Rust stable, which is at version 1.43 at the time of this writing.

To run all scenarios in a `.dar` file, execute
```
$ cargo run <path-to-dar>
```
To run all scenarios in a specific module in a `.dar` file, execute
```
$ cargo run <path-to-dar> <module-name>
```
To run a specific scenario in a `.dar` file, execute
```
$ cargo run <path-to-dar> <module-name> <scenario-name>
```

# Benchmarks

There is a very simple benchmark which times the execution of the `CollectAuthority:test` scenario from the DAML project in `test/collect-authority`. To execute this benchmark, run
```shell
$ cargo bench
```
This will print some numbers in the terminal and produce some nice charts that can be found in `target/criterion/report/index.html` in the _collect-authority.dar:CollectAuthority:test_ section.

To execute this benchmark with different command line arguments, for instance with a different sample size, run
```shell
$ cargo bench --bench scenario -- --sample-size 50
```

To execute the benchmark runner on a different scenario, you can change the `RUSTY_BENCH_SCENARIO` environment variable to point to it, as in
```shell
$ RUSTY_BENCH_SCENARIO=/path/to.dar:Module.Name:scenarioName cargo bench
```
