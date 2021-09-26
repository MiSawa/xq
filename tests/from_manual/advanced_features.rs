use crate::test;

test!(
    bind1,
    r#"
    .bar as $x | .foo | . + $x
    "#,
    r#"
    {"foo":10, "bar":200}
    "#,
    r#"
    210
    "#
);

test!(
    bind2,
    r#"
    . as $i|[(.*2|. as $i| $i), $i]
    "#,
    r#"
    5
    "#,
    r#"
    [10,5]
    "#
);

test!(
    bind3,
    r#"
    . as [$a, $b, {c: $c}] | $a + $b + $c
    "#,
    r#"
    [2, 3, {"c": 4, "d": 5}]
    "#,
    r#"
    9
    "#
);

test!(
    bind4,
    r#"
    .[] as [$a, $b] | {a: $a, b: $b}
    "#,
    r#"
    [[0], [0, 1], [2, 1, 0]]
    "#,
    r#"
    {"a":0,"b":null}
    {"a":0,"b":1}
    {"a":2,"b":1}
    "#
);

test!(
    destructing_alt1,
    r#"
    .[] as {$a, $b, c: {$d, $e}} ?// {$a, $b, c: [{$d, $e}]} | {$a, $b, $d, $e}
    "#,
    r#"
    [{"a": 1, "b": 2, "c": {"d": 3, "e": 4}}, {"a": 1, "b": 2, "c": [{"d": 3, "e": 4}]}]
    "#,
    r#"
    {"a":1,"b":2,"d":3,"e":4}
    {"a":1,"b":2,"d":3,"e":4}
    "#
);

test!(
    destructing_alt2,
    r#"
    .[] as {$a, $b, c: {$d}} ?// {$a, $b, c: [{$e}]} | {$a, $b, $d, $e}
    "#,
    r#"
    [{"a": 1, "b": 2, "c": {"d": 3, "e": 4}}, {"a": 1, "b": 2, "c": [{"d": 3, "e": 4}]}]
    "#,
    r#"
    {"a":1,"b":2,"d":3,"e":null}
    {"a":1,"b":2,"d":null,"e":4}
    "#
);

test!(
    destructing_alt3,
    r#"
    .[] as [$a] ?// [$b] | if $a != null then error("err: \($a)") else {$a,$b} end
    "#,
    r#"
    [[3]]
    "#,
    r#"
    {"a":null,"b":3}
    "#
);

test!(
    functions1,
    r#"
    def addvalue(f): . + [f]; map(addvalue(.[0]))
    "#,
    r#"
    [[1,2],[10,20]]
    "#,
    r#"
    [[1,2,1], [10,20,10]]
    "#
);

test!(
    functions2,
    r#"
    def addvalue(f): f as $x | map(. + $x); addvalue(.[0])
    "#,
    r#"
    [[1,2],[10,20]]
    "#,
    r#"
    [[1,2,1,2], [10,20,1,2]]
    "#
);

test!(
    reduce1,
    r#"
    reduce .[] as $item (0; . + $item)
    "#,
    r#"
    [10,2,5,3]
    "#,
    r#"
    20
    "#
);

test!(
    isempty1,
    r#"
    isempty(empty)
    "#,
    r#"
    null
    "#,
    r#"
    true
    "#
);

test!(
    limit1,
    r#"
    [limit(3;.[])]
    "#,
    r#"
    [0,1,2,3,4,5,6,7,8,9]
    "#,
    r#"
    [0,1,2]
    "#
);

test!(
    first_last_nth1,
    r#"
    [first(range(.)), last(range(.)), nth(./2; range(.))]
    "#,
    r#"
    10
    "#,
    r#"
    [0,9,5]
    "#
);

test!(
    first_last_nth2,
    r#"
    [range(.)]|[first, last, nth(5)]
    "#,
    r#"
    10
    "#,
    r#"
    [0,9,5]
    "#
);

test!(
    foreach1,
    r#"
    [foreach .[] as $item ([[],[]]; if $item == null then [[],.[0]] else [(.[0] + [$item]),[]] end; if $item == null then .[1] else empty end)]
    "#,
    r#"
    [1,2,3,4,null,"a","b",null]
    "#,
    r#"
    [[1,2,3,4],["a","b"]]
    "#
);

test!(
    generator_and_iterator1,
    r#"
    def range(init; upto; by): def _range: if (by > 0 and . < upto) or (by < 0 and . > upto) then ., ((.+by)|_range) else . end; if by == 0 then init else init|_range end | select((by > 0 and . < upto) or (by < 0 and . > upto)); range(0; 10; 3)
    "#,
    r#"
    null
    "#,
    r#"
    0
    3
    6
    9
    "#
);

test!(
    recursion1,
    r#"
    def while(cond; update): def _while: if cond then ., (update | _while) else empty end; _while; [while(.<100; .*2)]
    "#,
    r#"
    1
    "#,
    r#"
    [1,2,4,8,16,32,64]
    "#
);
