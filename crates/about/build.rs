use std::{collections::BTreeMap, env, fmt::Write as _, io::Write as _, path::Path, sync::Arc};

use anyhow::{bail, Result};
use cargo_about::licenses::{self, LicenseFileKind};
use krates::Utf8Path;

fn main() -> Result<()> {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../../about.toml");
    println!("cargo:rerun-if-changed=../../Cargo.lock");

    let thisdir = env::var("CARGO_MANIFEST_DIR")?;
    let xq_root = Utf8Path::new(&thisdir).parent().unwrap().parent().unwrap();
    let xq_toml = xq_root.join("Cargo.toml");
    let config = toml::de::from_str(include_str!("../../about.toml"))?;
    let output = Path::new(&env::var("OUT_DIR")?).join("about.txt");

    let krates = cargo_about::get_all_crates(&xq_toml, false, true, vec![], false, &config)?;
    let store = licenses::store_from_cache()?;
    let client = cd::client::Client::new();
    let licenses = cargo_about::licenses::Gatherer::with_store(Arc::new(store), client)
        .with_confidence_threshold(1.0)
        .gather(&krates, &config);

    let (_files, resolved) =
        licenses::resolution::resolve(&licenses, &config.accepted, &config.crates);

    struct License<'a> {
        name: &'a str,
        text: &'a str,
        deps: Vec<String>,
    }

    let mut everything = BTreeMap::<&str, _>::new();

    for (krate, resolved) in licenses
        .iter()
        .zip(resolved.iter())
        .filter_map(|(l, r)| r.as_ref().map(|r| (l, r)))
    {
        for diagnostic in &resolved.diagnostics {
            bail!(
                "Diagnostics about license of crate {}: {}",
                krate.krate,
                diagnostic.message
            );
        }
        for license in resolved.licenses.iter() {
            match license.license {
                spdx::LicenseItem::Spdx { id, .. } => {
                    let mut found = false;
                    for file in &krate.license_files {
                        if !file
                            .license_expr
                            .evaluate(|req| req.license.id() == Some(id))
                        {
                            continue;
                        }
                        match &file.kind {
                            LicenseFileKind::Text(text)
                            | LicenseFileKind::AddendumText(text, _) => {
                                found = true;
                                everything
                                    .entry(text)
                                    .or_insert_with(|| License {
                                        name: id.full_name,
                                        text,
                                        deps: vec![],
                                    })
                                    .deps
                                    .push(format!("{}", &krate.krate));
                            }
                            LicenseFileKind::Header => {}
                        }
                    }
                    if !found {
                        everything
                            .entry(id.text())
                            .or_insert_with(|| License {
                                name: id.full_name,
                                text: id.text(),
                                deps: vec![],
                            })
                            .deps
                            .push(format!("{}", &krate.krate));
                    }
                }
                _ => bail!("Don't know how to handle license {license:?}"),
            }
        }
    }

    let mut buf = String::new();
    writeln!(
        &mut buf,
        "Here is the list of licenses of the projects XQ uses."
    )?;
    for license in everything.values() {
        write!(&mut buf, "\n\n{}", license.name)?;
        write!(&mut buf, " used by ")?;
        for (i, dep) in license.deps.iter().enumerate() {
            if i == 0 {
                write!(&mut buf, "{dep}")?;
            } else {
                write!(&mut buf, ", {dep}")?;
            }
        }
        writeln!(&mut buf, "\n")?;
        writeln!(&mut buf, "{}", license.text)?;
    }
    let mut output_file = std::fs::File::create(output)?;
    output_file.write_all(buf.as_bytes())?;
    Ok(())
}
