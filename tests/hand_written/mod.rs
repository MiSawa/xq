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