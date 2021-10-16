use crate::test;

test!(
    length1,
    r#"
    .[] | length
    "#,
    r#"
    [[1,2], "string", {"a":2}, null]
    "#,
    r#"
    2
    6
    1
    0
    "#
);

test!(
    utf8bytelength1,
    r#"
    utf8bytelength
    "#,
    "
    \"\u{03bc}\"
    ",
    r#"
    2
    "#
);

test!(
    keys1,
    r#"
    keys
    "#,
    r#"
    {"abc": 1, "abcd": 2, "Foo": 3}
    "#,
    r#"
    ["Foo", "abc", "abcd"]
    "#
);

test!(
    keys2,
    r#"
    keys
    "#,
    r#"
    [42,3,35]
    "#,
    r#"
    [0,1,2]
    "#
);

test!(
    has1,
    r#"
    map(has("foo"))
    "#,
    r#"
    [{"foo": 42}, {}]
    "#,
    r#"
    [true, false]
    "#
);

test!(
    has2,
    r#"
    map(has(2))
    "#,
    r#"
    [[0,1], ["a","b","c"]]
    "#,
    r#"
    [false, true]
    "#
);

test!(
    in1,
    r#"
    .[] | in({"foo": 42})
    "#,
    r#"
    ["foo", "bar"]
    "#,
    r#"
    true
    false
    "#
);

test!(
    in2,
    r#"
    map(in([0,1]))
    "#,
    r#"
    [2, 0]
    "#,
    r#"
    [false, true]
    "#
);

test!(
    map1,
    r#"
    map(.+1)
    "#,
    r#"
    [1,2,3]
    "#,
    r#"
    [2,3,4]
    "#
);

test!(
    map2,
    r#"
    map_values(.+1)
    "#,
    r#"
    {"a": 1, "b": 2, "c": 3}
    "#,
    r#"
    {"a": 2, "b": 3, "c": 4}
    "#
);

test!(
    path1,
    r#"
    path(.a[0].b)
    "#,
    r#"
    null
    "#,
    r#"
    ["a",0,"b"]
    "#
);

test!(
    path2,
    r#"
    [path(..)]
    "#,
    r#"
    {"a":[{"b":1}]}
    "#,
    r#"
    [[],["a"],["a",0],["a",0,"b"]]
    "#
);

test!(
    del1,
    r#"
    del(.foo)
    "#,
    r#"
    {"foo": 42, "bar": 9001, "baz": 42}
    "#,
    r#"
    {"bar": 9001, "baz": 42}
    "#
);

test!(
    del2,
    r#"
    del(.[1, 2])
    "#,
    r#"
    ["foo", "bar", "baz"]
    "#,
    r#"
    ["foo"]
    "#
);

test!(
    getpath1,
    r#"
    getpath(["a","b"])
    "#,
    r#"
    null
    "#,
    r#"
    null
    "#
);

test!(
    getpath2,
    r#"
    [getpath(["a","b"], ["a","c"])]
    "#,
    r#"
    {"a":{"b":0, "c":1}}
    "#,
    r#"
    [0, 1]
    "#
);

test!(
    setpath1,
    r#"
    setpath(["a","b"]; 1)
    "#,
    r#"
    null
    "#,
    r#"
    {"a": {"b": 1}}
    "#
);

test!(
    setpath2,
    r#"
    setpath(["a","b"]; 1)
    "#,
    r#"
    {"a":{"b":0}}
    "#,
    r#"
    {"a": {"b": 1}}
    "#
);

test!(
    setpath3,
    r#"
    setpath([0,"a"]; 1)
    "#,
    r#"
    null
    "#,
    r#"
    [{"a":1}]
    "#
);

test!(
    delpaths1,
    r#"
    delpaths([["a","b"]])
    "#,
    r#"
    {"a":{"b":1},"x":{"y":2}}
    "#,
    r#"
    {"a":{},"x":{"y":2}}
    "#
);

test!(
    entries1,
    r#"
    to_entries
    "#,
    r#"
    {"a": 1, "b": 2}
    "#,
    r#"
    [{"key":"a", "value":1}, {"key":"b", "value":2}]
    "#
);

test!(
    entries2,
    r#"
    from_entries
    "#,
    r#"
    [{"key":"a", "value":1}, {"key":"b", "value":2}]
    "#,
    r#"
    {"a": 1, "b": 2}
    "#
);

test!(
    entries3,
    r#"
    with_entries(.key |= "KEY_" + .)
    "#,
    r#"
    {"a": 1, "b": 2}
    "#,
    r#"
    {"KEY_a": 1, "KEY_b": 2}
    "#
);

test!(
    select1,
    r#"
    map(select(. >= 2))
    "#,
    r#"
    [1,5,3,0,7]
    "#,
    r#"
    [5,3,7]
    "#
);

test!(
    select2,
    r#"
    .[] | select(.id == "second")
    "#,
    r#"
    [{"id": "first", "val": 1}, {"id": "second", "val": 2}]
    "#,
    r#"
    {"id": "second", "val": 2}
    "#
);

test!(
    typefilter1,
    r#"
    .[]|numbers
    "#,
    r#"
    [[],{},1,"foo",null,true,false]
    "#,
    r#"
    1
    "#
);

test!(
    empty1,
    r#"
    1, empty, 2
    "#,
    r#"
    null
    "#,
    r#"
    1
    2
    "#
);

test!(
    empty2,
    r#"
    [1,2,empty,3]
    "#,
    r#"
    null
    "#,
    r#"
    [1,2,3]
    "#
);

/*
TODO: Support $__loc__
test!(
    loc1,
    r#"
    try error("\($__loc__)") catch .
    "#,
    r#"
    null
    "#,
    r#"
    "{\"file\":\"<top-level>\",\"line\":1}"
    "#
);
 */

test!(
    paths1,
    r#"
    [paths]
    "#,
    r#"
    [1,[[],{"a":2}]]
    "#,
    r#"
    [[0],[1],[1,0],[1,1],[1,1,"a"]]
    "#
);

test!(
    paths2,
    r#"
    [paths(scalars)]
    "#,
    r#"
    [1,[[],{"a":2}]]
    "#,
    r#"
    [[0],[1,1,"a"]]
    "#
);

test!(
    add1,
    r#"
    add
    "#,
    r#"
    ["a","b","c"]
    "#,
    r#"
    "abc"
    "#
);

test!(
    add2,
    r#"
    add
    "#,
    r#"
    [1, 2, 3]
    "#,
    r#"
    6
    "#
);

test!(
    add3,
    r#"
    add
    "#,
    r#"
    []
    "#,
    r#"
    null
    "#
);

test!(
    any1,
    r#"
    any
    "#,
    r#"
    [true, false]
    "#,
    r#"
    true
    "#
);

test!(
    any2,
    r#"
    any
    "#,
    r#"
    [false, false]
    "#,
    r#"
    false
    "#
);

test!(
    any3,
    r#"
    any
    "#,
    r#"
    []
    "#,
    r#"
    false
    "#
);

test!(
    all1,
    r#"
    all
    "#,
    r#"
    [true, false]
    "#,
    r#"
    false
    "#
);

test!(
    all2,
    r#"
    all
    "#,
    r#"
    [true, true]
    "#,
    r#"
    true
    "#
);

test!(
    all3,
    r#"
    all
    "#,
    r#"
    []
    "#,
    r#"
    true
    "#
);

test!(
    flatten1,
    r#"
    flatten
    "#,
    r#"
    [1, [2], [[3]]]
    "#,
    r#"
    [1, 2, 3]
    "#
);

test!(
    flatten2,
    r#"
    flatten(1)
    "#,
    r#"
    [1, [2], [[3]]]
    "#,
    r#"
    [1, 2, [3]]
    "#
);

test!(
    flatten3,
    r#"
    flatten
    "#,
    r#"
    [[]]
    "#,
    r#"
    []
    "#
);

test!(
    range1,
    r#"
    range(2;4)
    "#,
    r#"
    null
    "#,
    r#"
    2
    3
    "#
);

test!(
    range2,
    r#"
    [range(2;4)]
    "#,
    r#"
    null
    "#,
    r#"
    [2,3]
    "#
);

test!(
    range3,
    r#"
    [range(4)]
    "#,
    r#"
    null
    "#,
    r#"
    [0,1,2,3]
    "#
);

test!(
    range4,
    r#"
    [range(0;10;3)]
    "#,
    r#"
    null
    "#,
    r#"
    [0,3,6,9]
    "#
);

test!(
    range5,
    r#"
    [range(0;10;-1)]
    "#,
    r#"
    null
    "#,
    r#"
    []
    "#
);

test!(
    range6,
    r#"
    [range(0;-5;-1)]
    "#,
    r#"
    null
    "#,
    r#"
    [0,-1,-2,-3,-4]
    "#
);

test!(
    floor1,
    r#"
    floor
    "#,
    r#"
    3.14159
    "#,
    r#"
    3
    "#
);

test!(
    sqrt1,
    r#"
    sqrt
    "#,
    r#"
    9
    "#,
    r#"
    3
    "#
);

test!(
    tonumber1,
    r#"
    .[] | tonumber
    "#,
    r#"
    [1, "1"]
    "#,
    r#"
    1
    1
    "#
);

test!(
    tostring1,
    r#"
    .[] | tostring
    "#,
    r#"
    [1, "1", [1]]
    "#,
    r#"
    "1"
    "1"
    "[1]"
    "#
);

test!(
    type1,
    r#"
    map(type)
    "#,
    r#"
    [0, false, [], {}, null, "hello"]
    "#,
    r#"
    ["number", "boolean", "array", "object", "null", "string"]
    "#
);

test!(
    floating_point1,
    r#"
    .[] | (infinite * .) < 0
    "#,
    r#"
    [-1, 1]
    "#,
    r#"
    true
    false
    "#
);

test!(
    floating_point2,
    r#"
    infinite, nan | type
    "#,
    r#"
    null
    "#,
    r#"
    "number"
    "number"
    "#
);

test!(
    sort1,
    r#"
    sort
    "#,
    r#"
    [8,3,null,6]
    "#,
    r#"
    [null,3,6,8]
    "#
);

test!(
    sort2,
    r#"
    sort_by(.foo)
    "#,
    r#"
    [{"foo":4, "bar":10}, {"foo":3, "bar":100}, {"foo":2, "bar":1}]
    "#,
    r#"
    [{"foo":2, "bar":1}, {"foo":3, "bar":100}, {"foo":4, "bar":10}]
    "#
);

test!(
    group_by1,
    r#"
    group_by(.foo)
    "#,
    r#"
    [{"foo":1, "bar":10}, {"foo":3, "bar":100}, {"foo":1, "bar":1}]
    "#,
    r#"
    [[{"foo":1, "bar":10}, {"foo":1, "bar":1}], [{"foo":3, "bar":100}]]
    "#
);

test!(
    min_max1,
    r#"
    min
    "#,
    r#"
    [5,4,2,7]
    "#,
    r#"
    2
    "#
);

test!(
    min_max2,
    r#"
    max_by(.foo)
    "#,
    r#"
    [{"foo":1, "bar":14}, {"foo":2, "bar":3}]
    "#,
    r#"
    {"foo":2, "bar":3}
    "#
);

test!(
    unique1,
    r#"
    unique
    "#,
    r#"
    [1,2,5,3,5,3,1,3]
    "#,
    r#"
    [1,2,3,5]
    "#
);

test!(
    unique2,
    r#"
    unique_by(.foo)
    "#,
    r#"
    [{"foo": 1, "bar": 2}, {"foo": 1, "bar": 3}, {"foo": 4, "bar": 5}]
    "#,
    r#"
    [{"foo": 1, "bar": 2}, {"foo": 4, "bar": 5}]
    "#
);

test!(
    unique3,
    r#"
    unique_by(length)
    "#,
    r#"
    ["chunky", "bacon", "kitten", "cicada", "asparagus"]
    "#,
    r#"
    ["bacon", "chunky", "asparagus"]
    "#
);

test!(
    reverse1,
    r#"
    reverse
    "#,
    r#"
    [1,2,3,4]
    "#,
    r#"
    [4,3,2,1]
    "#
);

test!(
    contains1,
    r#"
    contains("bar")
    "#,
    r#"
    "foobar"
    "#,
    r#"
    true
    "#
);

test!(
    contains2,
    r#"
    contains(["baz", "bar"])
    "#,
    r#"
    ["foobar", "foobaz", "blarp"]
    "#,
    r#"
    true
    "#
);

test!(
    contains3,
    r#"
    contains(["bazzzzz", "bar"])
    "#,
    r#"
    ["foobar", "foobaz", "blarp"]
    "#,
    r#"
    false
    "#
);

test!(
    contains4,
    r#"
    contains({foo: 12, bar: [{barp: 12}]})
    "#,
    r#"
    {"foo": 12, "bar":[1,2,{"barp":12, "blip":13}]}
    "#,
    r#"
    true
    "#
);

test!(
    contains5,
    r#"
    contains({foo: 12, bar: [{barp: 15}]})
    "#,
    r#"
    {"foo": 12, "bar":[1,2,{"barp":12, "blip":13}]}
    "#,
    r#"
    false
    "#
);

test!(
    indices1,
    r#"
    indices(", ")
    "#,
    r#"
    "a,b, cd, efg, hijk"
    "#,
    r#"
    [3,7,12]
    "#
);

test!(
    indices2,
    r#"
    indices(1)
    "#,
    r#"
    [0,1,2,1,3,1,4]
    "#,
    r#"
    [1,3,5]
    "#
);

test!(
    indices3,
    r#"
    indices([1,2])
    "#,
    r#"
    [0,1,2,3,1,4,2,5,1,2,6,7]
    "#,
    r#"
    [1,8]
    "#
);

test!(
    index1,
    r#"
    index(", ")
    "#,
    r#"
    "a,b, cd, efg, hijk"
    "#,
    r#"
    3
    "#
);

test!(
    index2,
    r#"
    rindex(", ")
    "#,
    r#"
    "a,b, cd, efg, hijk"
    "#,
    r#"
    12
    "#
);

test!(
    inside1,
    r#"
    inside("foobar")
    "#,
    r#"
    "bar"
    "#,
    r#"
    true
    "#
);

test!(
    inside2,
    r#"
    inside(["foobar", "foobaz", "blarp"])
    "#,
    r#"
    ["baz", "bar"]
    "#,
    r#"
    true
    "#
);

test!(
    inside3,
    r#"
    inside(["foobar", "foobaz", "blarp"])
    "#,
    r#"
    ["bazzzzz", "bar"]
    "#,
    r#"
    false
    "#
);

test!(
    inside4,
    r#"
    inside({"foo": 12, "bar":[1,2,{"barp":12, "blip":13}]})
    "#,
    r#"
    {"foo": 12, "bar": [{"barp": 12}]}
    "#,
    r#"
    true
    "#
);

test!(
    inside5,
    r#"
    inside({"foo": 12, "bar":[1,2,{"barp":12, "blip":13}]})
    "#,
    r#"
    {"foo": 12, "bar": [{"barp": 15}]}
    "#,
    r#"
    false
    "#
);

test!(
    startswith1,
    r#"
    [.[]|startswith("foo")]
    "#,
    r#"
    ["fo", "foo", "barfoo", "foobar", "barfoob"]
    "#,
    r#"
    [false, true, false, true, false]
    "#
);

test!(
    endswith1,
    r#"
    [.[]|endswith("foo")]
    "#,
    r#"
    ["foobar", "barfoo"]
    "#,
    r#"
    [false, true]
    "#
);

test!(
    combinations1,
    r#"
    combinations
    "#,
    r#"
    [[1,2], [3, 4]]
    "#,
    r#"
    [1, 3]
    [1, 4]
    [2, 3]
    [2, 4]
    "#
);

// TODO: Add more
