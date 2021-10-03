use crate::test;

mod assignments;

test!(
    recursive,
    r#"
    def f: def g(x): ., x; g((. + 1) as $v | $v | if . > 2 then empty else f, $v end); f
    "#,
    r#"
    null
    "#,
    r#"
    null
    1
    2
    2
    1
    "#
);

test!(
    string1,
    r#"
    "foo\(1 + 2) \("b" + "a" + "r \([{x:"x"}])") \("\("\("baz")" + " qux")")"
    "#,
    r#"
    null
    "#,
    r#"
    "foo3.0 bar [{\"x\":\"x\"}] baz qux"
    "#
);
