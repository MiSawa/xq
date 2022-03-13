use crate::test;

test!(
    capture_multi_byte,
    r#"
    match("(.)"; "g")
    "#,
    r#"
    "あaいうえお"
    "#,
    r#"
    {"offset":0,"length":1,"string":"あ","captures":[{"offset":0,"length":1,"string":"あ","name":null}]}
    {"offset":1,"length":1,"string":"a","captures":[{"offset":1,"length":1,"string":"a","name":null}]}
    {"offset":2,"length":1,"string":"い","captures":[{"offset":2,"length":1,"string":"い","name":null}]}
    {"offset":3,"length":1,"string":"う","captures":[{"offset":3,"length":1,"string":"う","name":null}]}
    {"offset":4,"length":1,"string":"え","captures":[{"offset":4,"length":1,"string":"え","name":null}]}
    {"offset":5,"length":1,"string":"お","captures":[{"offset":5,"length":1,"string":"お","name":null}]}
    "#
);

test!(
    capture_same_name,
    r#"
    match("(?<x>.)(?<x>.)"; "g") | .captures
    "#,
    r#"
    "aあb"
    "#,
    r#"
    [{"offset":0,"length":1,"string":"a","name":"x"},{"offset":1,"length":1,"string":"あ","name":"x"}]
    "#
);

test!(
    nonmatched_group,
    r#"
    match("う|(い)|(.)"; "g")
    "#,
    r#"
    "あaいう"
    "#,
    r#"
    {"offset":0,"length":1,"string":"あ","captures":[{"offset":-1,"string":null,"length":0,"name":null},{"offset":0,"length":1,"string":"あ","name":null}]}
    {"offset":1,"length":1,"string":"a","captures":[{"offset":-1,"string":null,"length":0,"name":null},{"offset":1,"length":1,"string":"a","name":null}]}
    {"offset":2,"length":1,"string":"い","captures":[{"offset":2,"length":1,"string":"い","name":null},{"offset":-1,"string":null,"length":0,"name":null}]}
    {"offset":3,"length":1,"string":"う","captures":[{"offset":-1,"string":null,"length":0,"name":null},{"offset":-1,"string":null,"length":0,"name":null}]}
    "#
);

test!(
    capture_global,
    r#"
    capture("(?<a>.)-(?<b>.)(\\S*)"; "g")
    "#,
    r#"
    "a-1 b-234 c"
    "#,
    r#"
    {"a":"a","b":"1"}
    {"a":"b","b":"2"}
    "#
);

test!(
    scan,
    r#"
    scan("(?<a>.)-(?<b>.)(\\S*)")
    "#,
    r#"
    "a-1 b-234 c"
    "#,
    r#"
    ["a","1",""]
    ["b","2","34"]
    "#
);

test!(
    scan_no_capture,
    r#"
    scan("(?:.)-(?:.)(?:\\S*)")
    "#,
    r#"
    "a-1 b-234 c"
    "#,
    r#"
    "a-1"
    "b-234"
    "#
);
