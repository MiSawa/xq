use crate::test;

test!(
    test1,
    r#"
    test("foo")
    "#,
    r#"
    "foo"
    "#,
    r#"
    true
    "#
);

test!(
    test2,
    r#"
    .[] | test("a b c # spaces are ignored"; "ix")
    "#,
    r#"
    ["xabcd", "ABC"]
    "#,
    r#"
    true
    true
    "#
);

test!(
    match1,
    r#"
    match("(abc)+"; "g")
    "#,
    r#"
    "abc abc"
    "#,
    r#"
    {"offset": 0, "length": 3, "string": "abc", "captures": [{"offset": 0, "length": 3, "string": "abc", "name": null}]}
    {"offset": 4, "length": 3, "string": "abc", "captures": [{"offset": 4, "length": 3, "string": "abc", "name": null}]}
    "#
);

test!(
    match2,
    r#"
    match("foo")
    "#,
    r#"
    "foo bar foo"
    "#,
    r#"
    {"offset": 0, "length": 3, "string": "foo", "captures": []}
    "#
);

test!(
    match3,
    r#"
    match(["foo", "ig"])
    "#,
    r#"
    "foo bar FOO"
    "#,
    r#"
    {"offset": 0, "length": 3, "string": "foo", "captures": []}
    {"offset": 8, "length": 3, "string": "FOO", "captures": []}
    "#
);

test!(
    match4,
    r#"
    match("foo (?<bar123>bar)? foo"; "ig")
    "#,
    r#"
    "foo bar foo foo  foo"
    "#,
    r#"
    {"offset": 0, "length": 11, "string": "foo bar foo", "captures": [{"offset": 4, "length": 3, "string": "bar", "name": "bar123"}]}
    {"offset": 12, "length": 8, "string": "foo  foo", "captures": [{"offset": -1, "length": 0, "string": null, "name": "bar123"}]}
    "#
);

test!(
    match5,
    r#"
    [ match("."; "g")] | length
    "#,
    r#"
    "abc"
    "#,
    r#"
    3
    "#
);

test!(
    capture1,
    r#"
    capture("(?<a>[a-z]+)-(?<n>[0-9]+)")
    "#,
    r#"
    "xyzzy-14"
    "#,
    r#"
    { "a": "xyzzy", "n": "14" }
    "#
);
