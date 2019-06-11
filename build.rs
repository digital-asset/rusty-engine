// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
extern crate protobuf_codegen_pure;

use protobuf_codegen_pure::Customize;
use std::process::Command;

fn main() {
    let out_dir = "src/protos/da";
    let input = &[
        "protos/da/daml_lf.proto",
        "protos/da/daml_lf_0.proto",
        "protos/da/daml_lf_1.proto",
    ];

    std::fs::create_dir_all(out_dir).expect("mkdir -p");
    protobuf_codegen_pure::run(protobuf_codegen_pure::Args {
        out_dir,
        input,
        includes: &["protos"],
        customize: Customize {
            ..Default::default()
        },
    })
    .expect("protoc");

    // NOTE(MH): The protobuf codegen produces `#[allow(clippy)]` pragmas,
    // which cause a clippy warning. Thus we need to patch it.
    for file in input {
        Command::new("sed")
            .args(&["-i", "s/allow(clippy)/allow(clippy::all)/"])
            .arg(String::from("src/") + &file.replace(".proto", ".rs"))
            .output()
            .expect("sed");
    }
}
