use crate::test;

test!(
    identity,
    r#"
    .
    "#,
    r#"
    "Hello, world!"
    "#,
    r#"
    "Hello, world!"
    "#
);

test!(
    object_ident_index1,
    r#"
    .foo
    "#,
    r#"
    {"foo": 42, "bar": "less interesting data"}
    "#,
    r#"
    42
    "#
);

test!(
    object_ident_index2,
    r#"
    .foo
    "#,
    r#"
    {"notfoo": true, "alsonotfoo": false}
    "#,
    r#"
    null
    "#
);

test!(
    optional_object_identifier_index1,
    r#"
    .foo?
    "#,
    r#"
    {"foo": 42, "bar": "less interesting data"}
    "#,
    r#"
    42
    "#
);

test!(
    optional_object_identifier_index2,
    r#"
    .foo?
    "#,
    r#"
    {"notfoo": true, "alsonotfoo": false}
    "#,
    r#"
    null
    "#
);

test!(
    optional_object_identifier_index3,
    r#"
    .["foo"]?
    "#,
    r#"
    {"foo": 42}
    "#,
    r#"
    42
    "#
);

test!(
    optional_object_identifier_index4,
    r#"
    [.foo?]
    "#,
    r#"
    [1,2]
    "#,
    r#"
    []
    "#
);

test!(
    array_index1,
    r#"
    .[0]
    "#,
    r#"
    [{"name":"JSON", "good":true}, {"name":"XML", "good":false}]
    "#,
    r#"
    {"name":"JSON", "good":true}
    "#
);

test!(
    array_index2,
    r#"
    .[2]
    "#,
    r#"
    [{"name":"JSON", "good":true}, {"name":"XML", "good":false}]
    "#,
    r#"
    null
    "#
);

test!(
    array_index3,
    r#"
    .[-2]
    "#,
    r#"
    [1,2,3]
    "#,
    r#"
    2
    "#
);

test!(
    array_string_slice1,
    r#"
    .[2:4]
    "#,
    r#"
    ["a","b","c","d","e"]
    "#,
    r#"
    ["c", "d"]
    "#
);

test!(
    array_string_slice2,
    r#"
    .[2:4]
    "#,
    r#"
    "abcdefghi"
    "#,
    r#"
    "cd"
    "#
);

test!(
    array_string_slice3,
    r#"
    .[:3]
    "#,
    r#"
    ["a","b","c","d","e"]
    "#,
    r#"
    ["a", "b", "c"]
    "#
);

test!(
    array_string_slice4,
    r#"
    .[-2:]
    "#,
    r#"
    ["a","b","c","d","e"]
    "#,
    r#"
    ["d", "e"]
    "#
);

test!(
    array_object_value_iterator1,
    r#"
    .[]
    "#,
    r#"
    [{"name":"JSON", "good":true}, {"name":"XML", "good":false}]
    "#,
    r#"
    {"name":"JSON", "good":true}
    {"name":"XML", "good":false}
    "#
);

test!(
    array_object_value_iterator2,
    r#"
    .[]
    "#,
    r#"
    []
    "#,
    r#"
    "#
);

test!(
    array_object_value_iterator3,
    r#"
    .[]
    "#,
    r#"
    {"a": 1, "b": 1}
    "#,
    r#"
    1
    1
    "#
);

test!(
    comma1,
    r#"
    .foo, .bar
    "#,
    r#"
    {"foo": 42, "bar": "something else", "baz": true}
    "#,
    r#"
    42
    "something else"
    "#
);

test!(
    comma2,
    r#"
    .user, .projects[]
    "#,
    r#"
    {"user":"stedolan", "projects": ["jq", "wikiflow"]}
    "#,
    r#"
    "stedolan"
    "jq"
    "wikiflow"
    "#
);

test!(
    comma3,
    r#"
    .[4,2]
    "#,
    r#"
    ["a","b","c","d","e"]
    "#,
    r#"
    "e"
    "c"
    "#
);

test!(
    pipe1,
    r#"
    .[] | .name
    "#,
    r#"
    [{"name":"JSON", "good":true}, {"name":"XML", "good":false}]
    "#,
    r#"
   	"JSON"
    "XML"
    "#
);

test!(
    parenthesis1,
    r#"
    (. + 2) * 5
    "#,
    r#"
    1
    "#,
    r#"
   	15
    "#
);
