// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
extern crate protobuf_codegen_pure;

use std::process::Command;

fn sed_in_place(file: &str, command: &str) {
    let mut cmd = Command::new("sed");
    cmd.arg("-i");
    if std::env::consts::OS == "macos" {
        cmd.arg("");
    }
    cmd.arg(command).arg(file).output().expect("sed -i");
}

fn main() {
    let out_dir = "src/protos/da";
    let inputs = &[
        "protos/da/daml_lf.proto",
        "protos/da/daml_lf_0.proto",
        "protos/da/daml_lf_1.proto",
    ];

    std::fs::create_dir_all(out_dir).expect("mkdir -p");
    extern crate protobuf_codegen_pure;

    protobuf_codegen_pure::Args::new()
        .out_dir(out_dir)
        .inputs(inputs)
        .include("protos")
        .customize(protobuf_codegen_pure::Customize {
            singular_field_option_box: Some(true),
            oneof_field_box: Some(true),
            ..Default::default()
        })
        .run()
        .expect("protoc");

    // NOTE(MH): Patch the generated Rust file to remedy an bug in the codegen
    // until we have a fix.
    sed_in_place(
        "src/protos/da/daml_lf_1.rs",
        "s/def_template::def_key::/def_key::/",
    );

    // NOTE(MH): The protobuf codegen produces `#[allow(clippy)]` pragmas,
    // which cause a clippy warning. Thus we need to patch it.
    for input in inputs {
        let output = "src/".to_string() + &input.replace(".proto", ".rs");
        sed_in_place(&output, "s/allow(clippy)/allow(clippy::all)/");
    }
}
