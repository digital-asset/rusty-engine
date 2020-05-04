// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use regex::Regex;
use std::collections::HashMap;
use std::io::*;

use super::{Package, World};

impl Package {
    fn load<R: Read>(reader: &mut R) -> Result<Self> {
        let proto = protobuf::parse_from_reader(reader)?;
        let package = Package::from_proto(proto);
        Ok(package)
    }
}

impl World {
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> Result<Self> {
        use std::fs::File;
        let zip_file: File = File::open(path)?;
        let mut zip = zip::ZipArchive::new(zip_file)?;
        let manifest = read_manifest(zip.by_name("META-INF/MANIFEST.MF")?)?;
        let main_name = manifest
            .get("Main-Dalf")
            .expect("Missing Main-Dalf in manifest")
            .to_owned();
        let mut package_names = manifest
            .get("Dalfs")
            .expect("Missing Dalfs in manifest")
            .split(',')
            .map(|x| x.trim().to_owned())
            .collect::<Vec<_>>();
        let main_index = package_names.iter().position(|x| x == &main_name).unwrap();
        package_names.swap(0, main_index);
        let mut packages = Vec::new();
        for name in package_names {
            let mut file = zip.by_name(&name)?;
            let package = Package::load(&mut file)?;
            packages.push(package);
        }

        let main = packages[0].id.clone();
        let packages = packages
            .into_iter()
            .map(|package| (package.id.clone(), package))
            .collect();
        let map_entry_fields = vec!["key".to_string(), "value".to_string()];
        let numeric_regex = Regex::new(r"^[\+-]?\d+(\.\d+)?$").unwrap();
        let world = World {
            main,
            packages,
            map_entry_fields,
            numeric_regex,
        };
        Ok(world)
    }
}

fn read_manifest<R: Read>(reader: R) -> Result<HashMap<String, String>> {
    let buffered = BufReader::new(reader);
    let mut result = HashMap::new();
    let mut current_section_opt = None;
    let mut current_content = String::new();
    for line in buffered.lines() {
        let line = line?;
        if line.starts_with(' ') {
            if current_section_opt.is_some() {
                current_content += line.trim_start();
            } else {
                return Err(Error::from(ErrorKind::InvalidData));
            }
        } else if let Some(index) = line.find(':') {
            if let Some(current_section) = current_section_opt.take() {
                result.insert(
                    current_section,
                    std::mem::replace(&mut current_content, String::new()),
                );
            }
            let (section, content) = line.split_at(index);
            current_section_opt = Some(section.to_owned());
            current_content = content[1..].trim_start().to_owned();
        } else {
            return Err(Error::from(ErrorKind::InvalidData));
        }
    }
    if let Some(current_section) = current_section_opt {
        result.insert(current_section, current_content);
    }
    Ok(result)
}
