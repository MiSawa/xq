use std::process::Command;

fn main(){
    let package_version = env!("CARGO_PKG_VERSION");
    let git_output = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .expect("Failed to execute command `git rev-parse HEAD`")
        .stdout;
    let git_revision = String::from_utf8(git_output)
        .expect("Unable to interpret the output of `git rev-parse HEAD` as an UTF-8 string")
        .trim()
        .to_string();
    println!("cargo:rustc-env=LONG_VERSION={}-{}", package_version, git_revision);
}