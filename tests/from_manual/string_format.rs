use crate::test;

test!(
    string_format1,
    r#"
    @uri "https://www.google.com/search?q=\(.search)"
    "#,
    r#"
    {"search":"what is jq?"}
    "#,
    r#"
    "https://www.google.com/search?q=what%20is%20jq%3F"
    "#
);

test!(
    string_format2,
    r#"
    @html
    "#,
    r#"
    "This works if x < y"
    "#,
    r#"
    "This works if x &lt; y"
    "#
);

test!(
    string_format3,
    r#"
    @sh "echo \(.)"
    "#,
    r#"
    "O'Hara's Ale"
    "#,
    r#"
    "echo 'O'\\''Hara'\\''s Ale'"
    "#
);

test!(
    string_format4,
    r#"
    @base64
    "#,
    r#"
    "This is a message"
    "#,
    r#"
    "VGhpcyBpcyBhIG1lc3NhZ2U="
    "#
);

test!(
    string_format5,
    r#"
    @base64d
    "#,
    r#"
    "VGhpcyBpcyBhIG1lc3NhZ2U="
    "#,
    r#"
    "This is a message"
    "#
);
