// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
extern crate protobuf_codegen_pure;

use std::process::Command;

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
    Command::new("sed")
        .arg("-i")
        .arg("s/def_template::def_key::/def_key::/")
        .arg("src/protos/da/daml_lf_1.rs")
        .output()
        .expect("sed");
}
