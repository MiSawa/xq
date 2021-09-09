use crate::ast::{
    BinaryOp, Comparator, Identifier, Query, StringFragment, Term, UnaryOp, UpdateOp,
};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::one_of,
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0},
    combinator::{map, map_res, opt, recognize, success, value},
    error::ParseError,
    multi::fold_many0,
    multi::{many0, separated_list0, separated_list1},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};

pub type ParseResult<'a, T> = IResult<&'a str, T>;

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn binop_chain_left_assoc<'a, F, G, C, T, O, E>(
    operator_parser: F,
    term_parser: G,
    combiner: C,
) -> impl FnMut(&'a str) -> IResult<&'a str, T, E>
where
    F: Parser<&'a str, O, E>,
    G: Parser<&'a str, T, E> + Copy,
    C: Fn(T, O, T) -> T + 'a,
    E: ParseError<&'a str>,
{
    map(
        pair(term_parser, many0(pair(operator_parser, term_parser))),
        move |(first, rest)| {
            rest.into_iter()
                .fold(first, |lhs, (op, rhs)| combiner(lhs, op, rhs))
        },
    )
}

fn binop_chain_right_assoc<'a, F, G, C, T, O, E>(
    operator_parser: F,
    term_parser: G,
    combiner: C,
) -> impl FnMut(&'a str) -> IResult<&'a str, T, E>
where
    F: Parser<&'a str, O, E>,
    G: Parser<&'a str, T, E> + Copy,
    C: Fn(T, O, T) -> T + 'a,
    E: ParseError<&'a str>,
{
    map(
        pair(term_parser, many0(pair(operator_parser, term_parser))),
        move |(first, rest)| {
            if let Some((op, rhs)) = rest
                .into_iter()
                .rev()
                .reduce(|(op, rhs), (next_op, lhs)| (next_op, combiner(lhs, op, rhs)))
            {
                combiner(first, op, rhs)
            } else {
                first
            }
        },
    )
}

fn identifier(input: &str) -> ParseResult<Identifier> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s| Identifier(s),
    )(input)
}

fn variable(input: &str) -> ParseResult<Identifier> {
    preceded(char('$'), identifier)(input)
}

fn identifier_or_module_identifier(input: &str) -> ParseResult<Identifier> {
    map(recognize(separated_list1(tag("::"), identifier)), |s| {
        Identifier(s)
    })(input)
}

fn variable_or_module_variable(input: &str) -> ParseResult<Identifier> {
    preceded(char('$'), identifier_or_module_identifier)(input)
}

fn format(input: &str) -> ParseResult<Identifier> {
    preceded(
        char('@'),
        map(recognize(many0(alt((alphanumeric1, tag("_"))))), |s| {
            Identifier(s)
        }),
    )(input)
}

fn string_fragment(input: &str) -> ParseResult<StringFragment> {
    alt((
        map(is_not("\\\""), |s| StringFragment::String(s)),
        preceded(
            char('\\'),
            alt((
                map(one_of("\"\\/"), |c| StringFragment::Char(c)),
                map(char('b'), |_| StringFragment::Char('\x08')),
                map(char('f'), |_| StringFragment::Char('\x0C')),
                map(char('n'), |_| StringFragment::Char('\n')),
                map(char('r'), |_| StringFragment::Char('\r')),
                map(char('t'), |_| StringFragment::Char('\t')),
                delimited(
                    char('('),
                    map(ws(query), |q| StringFragment::Query(q)),
                    char(')'),
                ),
            )),
        ),
    ))(input)
}

fn string(input: &str) -> ParseResult<Vec<StringFragment>> {
    delimited(
        char('"'),
        fold_many0(string_fragment, Vec::new, |mut vec, s| {
            vec.push(s);
            vec
        }),
        char('"'),
    )(input)
}

fn object_term_entry(input: &str) -> ParseResult<(Query, Option<Query>)> {
    pair(
        alt((
            map(alt((identifier, variable)), |ident| {
                Query::Term(Box::new(Term::String(vec![StringFragment::String(
                    ident.0,
                )])))
            }),
            map(string, |t| Query::Term(Box::new(Term::String(t)))),
            delimited(char('('), ws(query), char(')')),
        )),
        alt((
            map(
                preceded(ws(char(':')), separated_list1(ws(char('|')), term)),
                |terms| {
                    Some(Query::Pipe(
                        terms
                            .into_iter()
                            .map(|t| Query::Term(Box::new(t)))
                            .collect(),
                    ))
                },
            ),
            success(None),
        )),
    )(input)
}

pub fn term_inner(input: &str) -> ParseResult<Term> {
    alt((
        value(Term::Null, tag("null")),
        value(Term::False, tag("false")),
        value(Term::True, tag("true")),
        map_res(digit1, |s: &str| {
            s.parse().map(|n: i64| Term::Number(n.into()))
        }),
        map(double, |x| Term::Number(x.into())),
        map(
            delimited(
                terminated(char('('), multispace0),
                query,
                preceded(multispace0, char(')')),
            ),
            |q| Term::Query(Box::new(q)),
        ),
        map(
            delimited(
                terminated(char('['), multispace0),
                opt(query),
                preceded(multispace0, char(']')),
            ),
            |q| Term::Array(q.map(Box::new)),
        ),
        map(
            delimited(
                terminated(char('{'), multispace0),
                separated_list0(char(','), ws(object_term_entry)),
                preceded(multispace0, char('}')),
            ),
            |entries| Term::Object(entries),
        ),
        map(
            preceded(pair(tag("break"), multispace0), variable),
            |ident| Term::Break(ident),
        ),
        map(
            alt((
                pair(
                    identifier_or_module_identifier,
                    opt(delimited(
                        preceded(multispace0, char('(')),
                        separated_list1(char(';'), ws(query)),
                        char(')'),
                    )),
                ),
                pair(variable_or_module_variable, success(None)),
            )),
            |(name, args)| Term::FunctionCall(name, args.unwrap_or_else(|| vec![])),
        ),
        map(format, |name| Term::Format(name)),
        map(string, |s| Term::String(s)),
        value(Term::Recurse, tag("..")),
        value(Term::Identity, char('.')),
    ))(input)
}

pub fn term(input: &str) -> ParseResult<Term> {
    alt((
        map(preceded(pair(char('-'), multispace0), term), |t| {
            Term::Unary(UnaryOp::Minus, Box::new(t))
        }),
        map(preceded(pair(char('+'), multispace0), term), |t| {
            Term::Unary(UnaryOp::Plus, Box::new(t))
        }),
        term_inner,
    ))(input)
}

fn query9(input: &str) -> ParseResult<Query> {
    map(term, |t| Query::Term(Box::new(t)))(input)
}

fn query8(input: &str) -> ParseResult<Query> {
    binop_chain_left_assoc(
        ws(alt((
            value(BinaryOp::Multiply, char('*')),
            value(BinaryOp::Divide, char('/')),
            value(BinaryOp::Modulo, char('%')),
        ))),
        query9,
        |lhs, op, rhs| Query::Operate(Box::new(lhs), op, Box::new(rhs)),
    )(input)
}

fn query7(input: &str) -> ParseResult<Query> {
    binop_chain_left_assoc(
        ws(alt((
            value(BinaryOp::Add, char('+')),
            value(BinaryOp::Subtract, char('-')),
        ))),
        query8,
        |lhs, op, rhs| Query::Operate(Box::new(lhs), op, Box::new(rhs)),
    )(input)
}

fn query6(input: &str) -> ParseResult<Query> {
    fn comparator(input: &str) -> ParseResult<Comparator> {
        alt((
            value(Comparator::Eq, tag("==")),
            value(Comparator::Neq, tag("!=")),
            value(Comparator::Ge, tag(">=")),
            value(Comparator::Le, tag("<=")),
            value(Comparator::Gt, tag(">")),
            value(Comparator::Lt, tag("<")),
        ))(input)
    }
    map(
        pair(query7, opt(pair(comparator, query7))),
        |(head, tail)| {
            if let Some((op, rhs)) = tail {
                Query::Compare(Box::new(head), op, Box::new(rhs))
            } else {
                head
            }
        },
    )(input)
}

fn query5(input: &str) -> ParseResult<Query> {
    binop_chain_left_assoc(ws(tag("and")), query6, |lhs, _, rhs| {
        Query::Operate(Box::new(lhs), BinaryOp::And, Box::new(rhs))
    })(input)
}

fn query4(input: &str) -> ParseResult<Query> {
    binop_chain_left_assoc(ws(tag("or")), query5, |lhs, _, rhs| {
        Query::Operate(Box::new(lhs), BinaryOp::Or, Box::new(rhs))
    })(input)
}

fn query3(input: &str) -> ParseResult<Query> {
    fn update_op(input: &str) -> ParseResult<UpdateOp> {
        alt((
            value(UpdateOp::Assign, tag("=")), // TODO: not immediately followed by =
            value(UpdateOp::Modify, tag("|=")),
            value(UpdateOp::Alt, tag("//=")),
            value(UpdateOp::Add, tag("+=")),
            value(UpdateOp::Subtract, tag("-=")),
            value(UpdateOp::Multiply, tag("*=")),
            value(UpdateOp::Divide, tag("/=")),
            value(UpdateOp::Modulo, tag("%=")),
        ))(input)
    }
    map(
        pair(query4, opt(pair(update_op, query4))),
        |(head, tail)| {
            if let Some((op, rhs)) = tail {
                Query::Update(Box::new(head), op, Box::new(rhs))
            } else {
                head
            }
        },
    )(input)
}

fn query2(input: &str) -> ParseResult<Query> {
    binop_chain_right_assoc(ws(tag("//")), query3, |lhs, _, rhs| {
        Query::Operate(Box::new(lhs), BinaryOp::Alt, Box::new(rhs))
    })(input)
}

fn query1(input: &str) -> ParseResult<Query> {
    map(separated_list1(ws(char(',')), query2), |mut qs| {
        if qs.len() == 1 {
            qs.pop().unwrap()
        } else {
            Query::Concat(qs)
        }
    })(input)
}

pub fn query(input: &str) -> ParseResult<Query> {
    map(separated_list1(ws(char('|')), query1), |mut qs| {
        if qs.len() == 1 {
            qs.pop().unwrap()
        } else {
            Query::Pipe(qs)
        }
    })(input)
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{BinaryOp, Identifier, StringFragment, Term, UnaryOp},
        parser::{format, identifier, string, term, variable},
    };

    #[test]
    fn test_identifier() {
        assert_eq!(identifier("ab_c"), Ok(("", Identifier("ab_c"))));
        assert_eq!(identifier("ab+c"), Ok(("+c", Identifier("ab"))));
        assert!(identifier("123abc").is_err());
    }

    #[test]
    fn test_variable() {
        assert_eq!(variable("$ab12+c"), Ok(("+c", Identifier("ab12"))));
        assert!(variable("$12abc").is_err());
        assert!(variable("$ abc").is_err());
    }

    #[test]
    fn test_format() {
        assert_eq!(format("@tsv"), Ok(("", Identifier("tsv"))));
        assert_eq!(format("@123"), Ok(("", Identifier("123"))));
    }

    #[test]
    fn test_term() {
        assert_eq!(term("true"), Ok(("", Term::True)));
        assert_eq!(term("false"), Ok(("", Term::False)));
        assert_eq!(term("null"), Ok(("", Term::Null)));
        assert_eq!(term("[ ]"), Ok(("", Term::Array(None))));
        assert_eq!(
            term("-123"),
            Ok((
                "",
                Term::Unary(UnaryOp::Minus, Box::new(Term::Number(123.into())))
            ))
        );
    }

    #[test]
    fn test_string() {
        use crate::ast;
        use StringFragment::*;
        assert_eq!(
            string(r#""\rabc\"\n\\""#),
            Ok((
                "",
                vec![Char('\r'), String("abc"), Char('"'), Char('\n'), Char('\\')]
            ))
        );
        assert_eq!(
            string(r#""abc\( 1 + 2 )def""#),
            Ok((
                "",
                vec![
                    String("abc"),
                    Query(ast::Query::Operate(
                        Box::new(ast::Query::Term(Box::new(Term::Number(1.into())))),
                        BinaryOp::Add,
                        Box::new(ast::Query::Term(Box::new(Term::Number(2.into()))))
                    )),
                    String("def")
                ]
            ))
        );
    }
}
