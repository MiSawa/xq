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

test!(
    length_on_number_yields_abs_value,
    r#"
    map(length)
    "#,
    r#"
    [0, 128, -128, -1.5]
    "#,
    r#"
    [0, 128, 128, 1.5]
    "#
);

test!(
    indices_with_empty_array,
    r#"
    indices([])
    "#,
    r#"
    [0]
    "#,
    r#"
    []
    "#
);

test!(
    limit_infinite_stream,
    r#"
    limit(3; repeat(0))
    "#,
    r#"
    null
    "#,
    r#"
    0
    0
    0
    "#
);

test!(
    isempty_infinite_stream,
    r#"
    isempty(repeat(0))
    "#,
    r#"
    null
    "#,
    r#"
    false
    "#
);

test!(
    any_infinite_stream,
    r#"
    any(repeat(true); .)
    "#,
    r#"
    null
    "#,
    r#"
    true
    "#
);

test!(
    all_infinite_stream,
    r#"
    all(repeat(false); .)
    "#,
    r#"
    null
    "#,
    r#"
    false
    "#
);

test!(
    ascii_downcase,
    r#"
    ascii_downcase
    "#,
    r#"
    "abCdEF123"
    "#,
    r#"
    "abcdef123"
    "#
);

test!(
    ascii_upcase,
    r#"
    ascii_upcase
    "#,
    r#"
    "abCdEF123"
    "#,
    r#"
    "ABCDEF123"
    "#
);

test!(
    tailrec_opt,
    r#"
    def walk2(f): if type | . == "array" or . == "object" then map_values(walk2(f)) | f else f end; walk2(.)
    "#,
    r#"
    [1,[2,3]]
    "#,
    r#"
    [1,[2,3]]
    "#
);

test!(
    split_empty_string,
    r#"
    . / ""
    "#,
    r#"
    "abcde"
    ""
    "#,
    r#"
    ["a","b","c","d","e"]
    []
    "#
);
