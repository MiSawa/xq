use crate::test;

test!(
    assignment1,
    r#"
    {a:{b:{c:1}}} | (.a.b|=3), .
    "#,
    r#"
    null
    "#,
    r#"
    {"a":{"b":3}}
    {"a":{"b":{"c":1}}}
    "#
);

test!(
    update_assignment1,
    r#"
    (..|select(type=="boolean")) |= if . then 1 else 0 end
    "#,
    r#"
    [true,false,[5,true,[true,[false]],false]]
    "#,
    r#"
    [1,0,[5,1,[1,[0]],0]]
    "#
);

test!(
    arithmetic_update_assignment1,
    r#"
    .foo += 1
    "#,
    r#"
    {"foo": 42}
    "#,
    r#"
    {"foo": 43}
    "#
);

test!(
    plain_assignment1,
    r#"
    .a = .b
    "#,
    r#"
    {"a": {"b": 10}, "b": 20}
    "#,
    r#"
    {"a": 20, "b": 20}
    "#
);

test!(
    plain_assignment2,
    r#"
    .a |= .b
    "#,
    r#"
    {"a": {"b": 10}, "b": 20}
    "#,
    r#"
    {"a": 10, "b": 20}
    "#
);

test!(
    plain_assignment3,
    r#"
    null|(.a,.b)=range(3)
    "#,
    r#"
    null
    "#,
    r#"
    {"a":0,"b":0}
    {"a":1,"b":1}
    {"a":2,"b":2}
    "#
);

test!(
    plain_assignment4,
    r#"
    null|(.a,.b)|=range(3)
    "#,
    r#"
    null
    "#,
    r#"
    {"a":0,"b":0}
    "#
);
