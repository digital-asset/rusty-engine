extern crate protoc_rust;

use protoc_rust::Customize;

fn main() {
    let out_dir = "src/protos/da";
    std::fs::create_dir_all(out_dir).expect("mkdir -p");
    protoc_rust::run(protoc_rust::Args {
        out_dir,
        input: &[
          "protos/da/daml_lf.proto",
          "protos/da/daml_lf_1.proto",
        ],
        includes: &["protos"],
        customize: Customize {
          ..Default::default()
        },
    }).expect("protoc");
}
