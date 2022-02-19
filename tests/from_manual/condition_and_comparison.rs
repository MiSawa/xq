use crate::test;

test!(
    equal1,
    r#"
    .[] == 1
    "#,
    r#"
    [1, 1.0, "1", "banana"]
    "#,
    r#"
    true
    true
    false
    false
    "#
);

test!(
    if_then_else1,
    r#"
    if . == 0 then "zero" elif . == 1 then "one" else "many" end
    "#,
    r#"
    2
    "#,
    r#"
    "many"
    "#
);

test!(
    comparison1,
    r#"
    . < 5
    "#,
    r#"
    2
    "#,
    r#"
    true
    "#
);

test!(
    and_or_not1,
    r#"
    42 and "a string"
    "#,
    r#"
    null
    "#,
    r#"
    true
    "#
);

test!(
    and_or_not2,
    r#"
    (true, false) or false
    "#,
    r#"
    null
    "#,
    r#"
    true
    false
    "#
);

test!(
    and_or_not3,
    r#"
    (true, true) and (true, false)
    "#,
    r#"
    null
    "#,
    r#"
    true
	false
	true
	false
    "#
);

test!(
    and_or_not4,
    r#"
    [true, false | not]
    "#,
    r#"
    null
    "#,
    r#"
    [false, true]
    "#
);

test!(
    alternative1,
    r#"
    .foo // 42
    "#,
    r#"
    {"foo": 19}
    "#,
    r#"
    19
    "#
);

test!(
    alternative2,
    r#"
    .foo // 42
    "#,
    r#"
    {}
    "#,
    r#"
    42
    "#
);

test!(
    try_catch1,
    r#"
    try .a catch ". is not an object"
    "#,
    r#"
    true
    "#,
    r#"
    ". is not an object"
    "#
);

test!(
    try_catch2,
    r#"
    [.[]|try .a]
    "#,
    r#"
    [{}, true, {"a":1}]
    "#,
    r#"
    [null, 1]
    "#
);

test!(
    try_catch3,
    r#"
    try error("some exception") catch .
    "#,
    r#"
    true
    "#,
    r#"
    "some exception"
    "#
);

test!(
    optional1,
    r#"
    [.[]|(.a)?]
    "#,
    r#"
    [{}, true, {"a":1}]
    "#,
    r#"
    [null, 1]
    "#
);
