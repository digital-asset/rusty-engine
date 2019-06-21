[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![CircleCI](https://circleci.com/gh/hurryabit/rusty-engine.svg?style=svg&circle-token=a6e23bed2661e7e96a31d457da8d7b1887a59622)](https://circleci.com/gh/hurryabit/rusty-engine)

Copyright 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All Rights Reserved.
SPDX-License-Identifier: Apache-2.0

**This implementation is highly experimental and not supported by Digital Asset in any way.**

# Prototype of a DAML Engine written in Rust

This is an experimental implementation of a [DAML](https://daml.com/) scenario runner written in [Rust](https://www.rust-lang.org/). It is not intended for production use and clearly not ready for it:

- It can only run scenarios and does not produce transaction trees.
- The `Decimal` type is not supported at all.
- The conversion between `Text` and code points uses [Unicode scalar values](http://www.unicode.org/glossary/#unicode_scalar_value) rather than [Unicode code points](http://www.unicode.org/glossary/#code_point).
- The performance of the `Map` type is atrocious.
- The `trace` primitive is not supported.
- There has been barely any performance tweaking.
- There has been no code review whatsoever.

# How to use this implementation

This implementation is developed using Rust stable, which is at version 1.35.0 at the time of this writing.

To run all scenarios in a `.dar` file, execute
```
$ cargo run <path-to-dar>
```
To run all scenario in a specific module in a `.dar` file, execute
```
$ cargo run <path-to-dar> <module-name>
```
To run a specific scenario in a `.dar` file, execute
```
$ cargo run <path-to-dar> <module-name> <scenario-name>
```
