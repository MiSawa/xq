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
