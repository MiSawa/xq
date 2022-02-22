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

test!(
    slice_update1,
    r#"
    [1,2,3] | .[1:][:1] = [5]
    "#,
    r#"
    null
    "#,
    r#"
    [1,5,3]
    "#
);

test!(
    slice_update2,
    r#"
    [1,2,3] | .[0:3][1:][:1] = [5]
    "#,
    r#"
    null
    "#,
    r#"
    [1,5,3]
    "#
);

test!(
    slice_update3,
    r#"
    [1,2,3] | .[2:1][:1] = [5]
    "#,
    r#"
    null
    "#,
    r#"
    [1,2,5,3]
    "#
);

test!(
    slice_update4,
    r#"
    [1,2,3] | .[2:1][2] = 5
    "#,
    r#"
    null
    "#,
    r#"
    [1,2,null,null,5,3]
    "#
);
