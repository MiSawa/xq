use std::process::Command;

fn main() {
    lalrpop::process_root().unwrap();

    let package_version = env!("CARGO_PKG_VERSION");
    let git_revision = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.trim().to_string());
    if let Some(git_revision) = git_revision {
        println!("cargo:rustc-env=LONG_VERSION={package_version}-{git_revision}");
    } else {
        println!("cargo:rustc-env=LONG_VERSION={package_version}");
    }
}
