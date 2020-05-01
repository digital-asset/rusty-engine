// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

fn main() {
    let out_dir = "src/protos/da";
    let inputs = &[
        "protos/com/daml/daml_lf_dev/daml_lf.proto",
        "protos/com/daml/daml_lf_dev/daml_lf_0.proto",
        "protos/com/daml/daml_lf_dev/daml_lf_1.proto",
    ];

    std::fs::create_dir_all(out_dir).expect("mkdir -p");
    protobuf_codegen_pure::Codegen::new()
        .out_dir(out_dir)
        .inputs(inputs)
        .includes(&["protos"])
        .run()
        .expect("protoc");
}
