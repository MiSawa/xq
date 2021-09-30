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
