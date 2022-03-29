#[test]
fn cli_test() {
    let t = trycmd::TestCases::new();
    t.default_bin_name("xq");
    t.case("tests/cli/*.toml");

    #[cfg(not(feature = "about"))]
    t.skip("tests/cli/about.toml");
}

mod verify_tests {
    use std::{path::PathBuf, process::Command};

    fn which(program: &str) -> Result<PathBuf, Box<dyn std::error::Error>> {
        let out = Command::new("which").arg(program).output()?.stdout;
        let path = PathBuf::from(String::from_utf8(out)?.trim());
        Ok(path)
    }

    #[ignore]
    #[test]
    fn test_jq() {
        if let Ok(bin) = which("jq") {
            let t = trycmd::TestCases::new();
            t.register_bin("jq", bin);
            t.default_bin_name("jq");
            t.case("tests/cli/*.toml");
            t.skip("tests/cli/*yaml*");
            t.skip("tests/cli/about.toml");
        }
    }

    #[ignore]
    #[test]
    fn test_gojq() {
        if let Ok(bin) = which("gojq") {
            let t = trycmd::TestCases::new();
            t.register_bin("gojq", bin);
            t.default_bin_name("gojq");
            t.case("tests/cli/*.toml");
            t.skip("tests/cli/*yaml*");
            t.skip("tests/cli/about.toml");
        }
    }
}
