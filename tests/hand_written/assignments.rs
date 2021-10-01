use crate::test;

test!(
    assignment_delete,
    r#"
    {a: [0, 1], b: 2} | .["a"] |= empty
    "#,
    r#"
    null
    "#,
    r#"
    {"b": 2}
    "#
);

test!(
    assignment_add_delete,
    r#"
    {a: [0, 1], b: 2} | .["a"].[0] += empty
    "#,
    r#"
    null
    "#,
    r#"
    "#
);

test!(
    assignment_sub,
    r#"
    {a: [0, 1], b: 2} | .["a"].[0] -= 1
    "#,
    r#"
    null
    "#,
    r#"
    {"a": [-1, 1], "b": 2}
    "#
);

test!(
    assignment_add_context,
    r#"
    [1] | .[0] += .[0]
    "#,
    r#"
    null
    "#,
    r#"
    [2]
    "#
);

test!(
    assignment_alt_context,
    r#"
    [null, 2, null] | (.[0], .[1], .[2]) //= (.[1], .[1] + 1)
    "#,
    r#"
    null
    "#,
    r#"
    [2, 2, 2]
    [3, 2, 3]
    "#
);
