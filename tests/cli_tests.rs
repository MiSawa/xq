#[test]
fn cli_test() {
    let t = trycmd::TestCases::new();
    t.default_bin_name("xq");
    t.case("tests/cli/*.toml");

    #[cfg(not(feature = "about"))]
    t.skip("tests/cli/about.toml");
}
