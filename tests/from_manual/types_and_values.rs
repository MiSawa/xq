use crate::test;

test!(
    array1,
    r#"
    [.user, .projects[]]
    "#,
    r#"
    {"user":"stedolan", "projects": ["jq", "wikiflow"]}
    "#,
    r#"
    ["stedolan", "jq", "wikiflow"]
    "#
);

test!(
    array2,
    r#"
    [ .[] | . * 2]
    "#,
    r#"
    [1, 2, 3]
    "#,
    r#"
    [2, 4, 6]
    "#
);

test!(
    object1,
    r#"
    {user, title: .titles[]}
    "#,
    r#"
    {"user":"stedolan","titles":["JQ Primer", "More JQ"]}
    "#,
    r#"
    {"user":"stedolan", "title": "JQ Primer"}
	{"user":"stedolan", "title": "More JQ"}
    "#
);

test!(
    object2,
    r#"
    {(.user): .titles}
    "#,
    r#"
    {"user":"stedolan","titles":["JQ Primer", "More JQ"]}
    "#,
    r#"
    {"stedolan": ["JQ Primer", "More JQ"]}
    "#
);

test!(
    object3,
    r#"
    "f o o" as $foo | "b a r" as $bar | {$foo, $bar:$foo}
    "#,
    r#"
    null
    "#,
    r#"
    {"f o o":"f o o","b a r":"f o o"}
    "#
);

test!(
    recursive_descent,
    r#"
    ..|.a?
    "#,
    r#"
    [[{"a":1}]]
    "#,
    r#"
    1
    "#
);
