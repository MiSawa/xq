use crate::test;

test!(
    fromdate1,
    r#"
    fromdate
    "#,
    r#"
    "2015-03-05T23:51:47Z"
    "#,
    r#"
    1425599507
    "#
);

test!(
    strptime1,
    r#"
    strptime("%Y-%m-%dT%H:%M:%SZ")
    "#,
    r#"
    "2015-03-05T23:51:47Z"
    "#,
    r#"
    [2015,2,5,23,51,47,4,63]
    "#
);

test!(
    mktime1,
    r#"
    strptime("%Y-%m-%dT%H:%M:%SZ")|mktime
    "#,
    r#"
    "2015-03-05T23:51:47Z"
    "#,
    r#"
    1425599507
    "#
);
