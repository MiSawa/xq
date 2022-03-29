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

    #[test]
    #[ignore = "Only for testing test suit"]
    fn test_jq() {
        if let Ok(bin) = which("jq") {
            let t = trycmd::TestCases::new();
            t.register_bin("jq", bin);
            t.default_bin_name("jq");
            t.case("tests/cli/*.toml");

            // This is only for xq
            t.skip("tests/cli/about.toml");
            // No yaml functionality in jq
            t.skip("tests/cli/*yaml*");
        }
    }

    #[test]
    #[ignore = "Only for testing test suit"]
    fn test_gojq() {
        if let Ok(bin) = which("gojq") {
            let t = trycmd::TestCases::new();
            t.register_bin("gojq", bin);
            t.default_bin_name("gojq");
            t.case("tests/cli/*.toml");

            // This is only for xq
            t.skip("tests/cli/about.toml");
            // Yaml serialization format is different
            t.skip("tests/cli/*yaml_output.toml");
        }
    }
}
