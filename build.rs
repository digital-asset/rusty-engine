extern crate protoc_rust;

use protoc_rust::Customize;

fn main() {
  if true {
    protoc_rust::run(protoc_rust::Args {
        out_dir: "src",
        input: &[
          "protos/daml_lf.proto",
          "protos/daml_lf_1.proto",
        ],
        includes: &["protos"],
        customize: Customize {
          ..Default::default()
        },
    }).expect("protoc");
  }
}
