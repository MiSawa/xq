use crate::test;

mod assignments;
mod math;
mod regex;

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
    format_csv_tsv,
    r#"
    [1, "foo", null, 2, nan, "foo,\n\"bar\"\tbaz"] | @csv, @tsv
    "#,
    r#"
    null
    "#,
    r#"
    "1,\"foo\",,2,,\"foo,\n\"\"bar\"\"\tbaz\""
    "1\tfoo\t\t2\t\tfoo,\\n\"bar\"\\tbaz"
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
    join,
    r#"
    ([], {}, [0,null,false,true,"abc"], {a:0,b:null,c:false,d:true,e:"abc"}) | join(",")
    "#,
    r#"
    null
    "#,
    r#"
    ""
    ""
    "0,,false,true,abc"
    "0,,false,true,abc"
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

test!(
    label_prevent_tailrec,
    r#"
    def f: label $x | if . < 3 then ., (. + 1 | f, break $x, .) else . end; 1 | f
    "#,
    r#"
    null
    "#,
    r#"
    1
    2
    3
    "#
);

test!(
    index_and_slice_with_large_numbers,
    r#"
    .[1e300], .[-1e300], .[-1e300:], .[1e300:], .[:-1e300], .[:1e300], .[-1e300:1e300], .[1e300:-1e300]
    "#,
    r#"
    [0,1,2]
    "#,
    r#"
    null
    null
    [0,1,2]
    []
    []
    [0,1,2]
    [0,1,2]
    []
    "#
);

test!(
    modulo_floating_point_numbers,
    r#"
    (3.5, -6.5) % (1.0, -2.5, 8.5)
    "#,
    r#"
    null
    "#,
    r#"
    0
    0
    1
    0
    3
    -6
    "#
);

test!(
    delete1,
    r#"
    [1,2,3] | (del(.[1:2][0]), del(.[1:2][1], .[1]))
    "#,
    r#"
    null
    "#,
    r#"
    [1,3]
    [1,3]
    "#
);

test!(
    delete2,
    r#"
    {x:[1]} | del(.x[0],.x)
    "#,
    r#"
    null
    "#,
    r#"
    {}
    "#
);

test!(
    delete3,
    r#"
    try ({x:[1]} | del(.x.y, .x[0], .x)) catch "err"
    "#,
    r#"
    null
    "#,
    r#"
    "err"
    "#
);

test!(
    delete4,
    r#"
    try ({x:[1]} | del(.x[0], .x.y, .x)) catch "err"
    "#,
    r#"
    null
    "#,
    r#"
    "err"
    "#
);

test!(
    string_format_sh,
    r#"
    @sh "\(0, "a b c", [0,null,true,"a b c"])"
    "#,
    r#"
    null
    "#,
    r#"
    "0"
    "'a b c'"
    "0 null true 'a b c'"
    "#
);

test!(
    format,
    r#"
    format("html", "uri", "csv", "tsv", "sh", "base64")
    "#,
    r#"
    [1,2,3]
    "#,
    r#"
    "[1,2,3]"
    "%5B1%2C2%2C3%5D"
    "1,2,3"
    "1\t2\t3"
    "1 2 3"
    "WzEsMiwzXQ=="
    "#
);

test!(
    strptime_strftime,
    r#"
    strptime("%FT%TZ") | strftime("%c")
    "#,
    r#"
    "2015-03-05T23:51:47Z"
    "#,
    r#"
    "Thu Mar  5 23:51:47 2015"
    "#
);

test!(
    sort_by_min_by_max_by,
    r#"
    sort_by(.a), sort_by(.a,.b), min_by(.a), min_by(.a,.b), max_by(.a), max_by(.a,.b)
    "#,
    r#"
    [{"a":2,"b":3},{"a":0,"b":2},{"a":2,"b":2},{"a":0,"b":1},{"a":1,"b":1}]
    "#,
    r#"
    [{"a":0,"b":2},{"a":0,"b":1},{"a":1,"b":1},{"a":2,"b":3},{"a":2,"b":2}]
    [{"a":0,"b":1},{"a":0,"b":2},{"a":1,"b":1},{"a":2,"b":2},{"a":2,"b":3}]
    {"a":0,"b":2}
    {"a":0,"b":1}
    {"a":2,"b":2}
    {"a":2,"b":3}
    "#
);

test!(
    empty_flatten,
    r#"
    flatten(0, 1)
    "#,
    r#"
    []
    [[], []]
    "#,
    r#"
    []
    []
    [[],[]]
    []
    "#
);

test!(
    input,
    r#"
    [., input]
    "#,
    r#"
    1
    2
    3
    4
    "#,
    r#"
    [1, 2]
    [3, 4]
    "#
);

test!(
    inputs,
    r#"
    [., inputs]
    "#,
    r#"
    1
    2
    3
    4
    "#,
    r#"
    [1, 2, 3, 4]
    "#
);

test!(
    flatten_fractional,
    r#"
    flatten(1.1)
    "#,
    r#"
    [[[]]]
    [[[[[[]]]]]]
    "#,
    r#"
    []
    []
    "#
);

test!(
    surrogate_pair,
    r#"
    "\uD83E\uDD14", "\ud83e\udd14"
    "#,
    r#"
    null
    "#,
    r#"
    "🤔"
    "🤔"
    "#
);
