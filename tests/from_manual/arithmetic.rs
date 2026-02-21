use crate::test;

test!(
    add1,
    r#"
    .a + 1
    "#,
    r#"
    {"a": 7}
    "#,
    r#"
    8
    "#
);

test!(
    add2,
    r#"
    .a + .b
    "#,
    r#"
    {"a": [1,2], "b": [3,4]}
    "#,
    r#"
    [1,2,3,4]
    "#
);

test!(
    add3,
    r#"
    .a + null
    "#,
    r#"
    {"a": 1}
    "#,
    r#"
    1
    "#
);

test!(
    add4,
    r#"
    .a + 1
    "#,
    r#"
    {}
    "#,
    r#"
    1
    "#
);

test!(
    add5,
    r#"
    {a: 1} + {b: 2} + {c: 3} + {a: 42}
    "#,
    r#"
    null
    "#,
    r#"
    {"a": 42, "b": 2, "c": 3}
    "#
);

test!(
    subtract1,
    r#"
    4 - .a
    "#,
    r#"
    {"a":3}
    "#,
    r#"
    1
    "#
);

test!(
    subtract2,
    r#"
    . - ["xml", "yaml"]
    "#,
    r#"
    ["xml", "yaml", "json"]
    "#,
    r#"
    ["json"]
    "#
);

test!(
    muldivmod1,
    r#"
    10 / . * 3
    "#,
    r#"
    5
    "#,
    r#"
    6
    "#
);

test!(
    muldivmod2,
    r#"
    . / ", "
    "#,
    r#"
    "a, b,c,d, e"
    "#,
    r#"
    ["a","b,c,d","e"]
    "#
);

test!(
    muldivmod3,
    r#"
    {"k": {"a": 1, "b": 2}} * {"k": {"a": 0,"c": 3}}
    "#,
    r#"
    null
    "#,
    r#"
    {"k": {"a": 0, "b": 2, "c": 3}}
    "#
);

test!(
    muldivmod4,
    r#"
    .[] | (1 / .)?
    "#,
    r#"
    [1,0,-1]
    "#,
    r#"
    1
    -1
    "#
);

test!(
    multiply_string,
    r#"
    (0,3,-3,nan) * "abc", "abc" * (0,3,-3,nan)
    "#,
    r#"
    "abc"
    "#,
    r#"
    ""
    "abcabcabc"
    null
    null
    ""
    "abcabcabc"
    null
    null
    "#
);
