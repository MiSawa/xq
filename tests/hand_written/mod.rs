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
    "foo3 bar [{\"x\":\"x\"}] baz qux"
    "#
);

test!(
    int_to_string1,
    r#"
    "\(1 + 2.9) \(2147483647) \(2147483648)"
    "#,
    r#"
    null
    "#,
    r#"
    "3.9 2147483647 2147483648.0"
    "#
);

test!(
    boolean_comparison,
    r#"
    [
    false < false, false < true, true < false, false < false,
    false <= false, false <= true, true <= false, false <= false,
    false > false, false > true, true > false, false > false,
    false >= false, false >= true, true >= false, false >= false,
    false == false, false == true, true == false, false == false,
    false != false, false != true, true != false, false != false
    ]
    "#,
    r#"
    null
    "#,
    r#"
    [
    false, true, false, false,
    true, true, false, true,
    false, false, true, false,
    true, false, true, true,
    true, false, false, true,
    false, true, true, false
    ]
    "#
);

test!(
    a_lot_of_args,
    r#"
    def f($a; b; $c; d; $e): {$a, b: b, $c, d: d, $e}; f("a"; "b"; "c"; "d"; "e")
    "#,
    r#"
    null
    "#,
    r#"
    {"a": "a", "b": "b", "c": "c", "d": "d", "e": "e"}
    "#
);
