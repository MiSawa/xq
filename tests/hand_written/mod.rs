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
    contains_on_boolean,
    r#"
    .[] | [contains(.,not)?]
    "#,
    r#"
    [false,true]
    "#,
    r#"
    [true]
    [true]
    "#
);

test!(
    indices_with_empty_string,
    r#"
    indices("")
    "#,
    r#"
    "abcde"
    "#,
    r#"
    []
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
    indices_against_null,
    r#"
    indices(0)
    "#,
    r#"
    null
    "#,
    r#"
    null
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

test!(
    deep_nesting_string_interpolation,
    r#"
    "\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\("\(1+2)")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")")"
    "#,
    r#"
    null
    "#,
    r#"
    "3"
    "#
);

test!(
    index_with_keyword,
    r#"
    .true, .false, .def
    "#,
    r#"
    {"true": 1, "false": 2, "def": 3}
    "#,
    r#"
    1
    2
    3
    "#
);

test!(
    binding_after_comma,
    r#"
    [1, 2 as $x | $x + 1, $x + 2]
    "#,
    r#"
    null
    "#,
    r#"
    [1,3,4]
    "#
);

test!(
    key_value_bind,
    r#"
    . as {$a:[$b]} | {$a,$b}
    "#,
    r#"
    {"a":[1]}
    "#,
    r#"
    {"a":[1],"b":1}
    "#
);

test!(
    key_query_bind,
    r#"
    . as {$a, (.b):[$b]} | {$a,$b}
    "#,
    r#"
    {"a":[1], "b":"a"}
    "#,
    r#"
    {"a":[1],"b":1}
    "#
);

test!(
    evaluation_order1,
    r#"
    [range(4)][0,1:][0,2]
    "#,
    r#"
    null
    "#,
    r#"
    0
    1
    2
    3
    "#
);

test!(
    evaluation_order2,
    r#"
    .b[]."\(.a[])"
    "#,
    r#"
    {"a":[0,1],"b":[{"0":1,"1":2},{"0":3,"1":4}]}
    "#,
    r#"
    1
    3
    2
    4
    "#
);

test!(
    evaluation_order3,
    r#"
    [range(4)][0,1:2,3]
    "#,
    r#"
    null
    "#,
    r#"
    [0,1]
    [0,1,2]
    [1]
    [1,2]
    "#
);

test!(
    keyword_in_object,
    r#"
    {x: null, y: true, z: false, true: 1, false: 2, null: 3, try: 4, else: 5}
    "#,
    r#"
    null
    "#,
    r#"
    {"x": null, "y": true, "z": false, "true": 1, "false": 2, "null": 3, "try": 4, "else": 5}
    "#
);

test!(
    object_indexing_optional,
    r#"
    [1,{x:2},3,{x:4},5] | [.[].x?]
    "#,
    r#"
    null
    "#,
    r#"
    [2,4]
    "#
);

test!(
    multiple_optional_operators,
    r#"
    .x??, .
    "#,
    r#"
    0
    "#,
    r#"
    0
    "#
);

test!(
    error_against_null_backtrack,
    r#"
    [0, error, 1], (.x, error(null), .y) = 1
    "#,
    r#"
    null
    "#,
    r#"
    [0,1]
    {"x":1,"y":1}
    "#
);

test!(
    try_catch_value,
    r#"
    try error({ x: 0 }) catch .
    "#,
    r#"
    null
    "#,
    r#"
    {"x":0}
    "#
);

test!(
    recurse_label1,
    r#"
    isempty(isempty(empty))
    "#,
    r#"
    null
    "#,
    r#"
    false
    "#
);

test!(
    recurse_label2,
    r#"
    first((0, 0) | first(0))
    "#,
    r#"
    null
    "#,
    r#"
    0
    "#
);
