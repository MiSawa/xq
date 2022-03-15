use crate::test_no_panic;

test_no_panic!(
    lexer_leave_match,
    r#"
    "n3\fbr\uffbr\uffb3333fffbr
    "#,
    r#""#
);

test_no_panic!(
    todate,
    r#"
    todate#.749;011111111111111
    "#,
    r#"
    1111111111111111111111111111111{4:fltena\u{0}
    e
    "#
);
