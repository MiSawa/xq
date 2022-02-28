use crate::test;

test!(
    isfinite_isinfinite,
    r#"
    0, nan, infinite, -infinite | [isfinite, isinfinite]
    "#,
    r#"
    null
    "#,
    r#"
    [true,false]
    [true,false]
    [false,true]
    [false,true]
    "#
);

test!(
    trigonometric_functions,
    r#"
    map(sin), map(cos), map(tan) | map(. * 1000000000 | floor / 1000000000)
    "#,
    r#"
    [0,1,2]
    "#,
    r#"
    [0,0.841470984, 0.909297426]
    [1,0.540302305,-0.416146837]
    [0,1.557407724,-2.185039864]
    "#
);

test!(
    inverse_trigonometric_functions,
    r#"
    map(asin), map(acos), map(atan) | map(. * 1000000000 | floor / 1000000000)
    "#,
    r#"
    [0,0.5,1]
    "#,
    r#"
    [0,0.523598775,1.570796326]
    [1.570796326,1.047197551,0]
    [0,0.463647609,0.785398163]
    "#
);

test!(
    hyperbolic_functions,
    r#"
    map(sinh), map(cosh), map(tanh) | map(. * 1000000000 | floor / 1000000000)
    "#,
    r#"
    [0,1,2]
    "#,
    r#"
    [0,1.175201193,3.626860407]
    [1,1.543080634,3.762195691]
    [0,0.761594155,0.964027580]
    "#
);

test!(
    inverse_hyperbolic_functions,
    r#"
    map(asinh), map(.+1 | acosh), map(./3 | atanh) | map(. * 1000000000 | floor / 1000000000)
    "#,
    r#"
    [0,1,2]
    "#,
    r#"
    [0,0.881373587,1.443635475]
    [0,1.316957896,1.762747174]
    [0,0.346573590,0.804718956]
    "#
);
