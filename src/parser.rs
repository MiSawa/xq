use crate::ast::{
    BinaryOp, BindPattern, Comparator, FuncDef, Identifier, ObjectBindPatternEntry, Query,
    StringFragment, Suffix, Term, UnaryOp, UpdateOp,
};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, one_of},
    combinator::{map, map_res, opt, recognize, success, value},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0, separated_list1},
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated, tuple},
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

fn binop_no_assoc<'a, F, G, C, T, O, E>(
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
        pair(term_parser, opt(pair(operator_parser, term_parser))),
        move |(head, tail)| {
            if let Some((op, rhs)) = tail {
                combiner(head, op, rhs)
            } else {
                head
            }
        },
    )
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
    map(recognize(preceded(char('$'), identifier)), Identifier)(input)
}

fn identifier_or_module_identifier(input: &str) -> ParseResult<Identifier> {
    map(
        recognize(separated_list1(tag("::"), identifier)),
        Identifier,
    )(input)
}

fn variable_or_module_variable(input: &str) -> ParseResult<Identifier> {
    map(
        recognize(preceded(char('$'), identifier_or_module_identifier)),
        Identifier,
    )(input)
}

fn format(input: &str) -> ParseResult<Identifier> {
    preceded(
        char('@'),
        map(recognize(many0(alt((alphanumeric1, tag("_"))))), |s| {
            Identifier(s)
        }),
    )(input)
}

fn string(input: &str) -> ParseResult<Vec<StringFragment>> {
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

    delimited(
        char('"'),
        fold_many0(string_fragment, Vec::new, |mut vec, s| {
            vec.push(s);
            vec
        }),
        char('"'),
    )(input)
}

pub fn term(input: &str) -> ParseResult<Term> {
    fn suffix(input: &str) -> ParseResult<Suffix> {
        delimited(
            terminated(char('['), multispace0),
            alt((
                success(Suffix::Explode),
                map(preceded(terminated(char(':'), multispace0), query), |q| {
                    Suffix::Slice(None, Some(Box::new(q)))
                }),
                map(
                    pair(query, opt(preceded(ws(char(':')), opt(query)))),
                    |(lower, upper)| match upper {
                        None => Suffix::Query(Box::new(lower)),
                        Some(None) => Suffix::Slice(Some(Box::new(lower)), None),
                        Some(Some(upper)) => {
                            Suffix::Slice(Some(Box::new(lower)), Some(Box::new(upper)))
                        }
                    },
                ),
            )),
            preceded(multispace0, char(']')),
        )(input)
    }
    fn object_term_entry(input: &str) -> ParseResult<(Query, Option<Query>)> {
        pair(
            alt((
                map(identifier, |ident| {
                    Term::String(vec![StringFragment::String(ident.0)]).into()
                }),
                map(variable, |ident| {
                    Term::FunctionCall {
                        name: ident,
                        args: vec![],
                    }
                    .into()
                }),
                map(string, |t| Term::String(t).into()),
                delimited(char('('), ws(query), char(')')),
            )),
            alt((
                map(
                    preceded(ws(char(':')), separated_list1(ws(char('|')), term)),
                    |terms| {
                        terms
                            .into_iter()
                            .map(Term::into)
                            .reduce(|lhs, rhs| Query::Pipe {
                                lhs: Box::new(lhs),
                                rhs: Box::new(rhs),
                            })
                    },
                ),
                success(None),
            )),
        )(input)
    }
    fn term_inner(input: &str) -> ParseResult<Term> {
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
                Query::into,
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
                |(name, args)| Term::FunctionCall {
                    name,
                    args: args.unwrap_or_default(),
                },
            ),
            map(format, |name| Term::Format(name)),
            map(string, |s| Term::String(s)),
            preceded(
                char('.'),
                preceded(
                    multispace0,
                    map(
                        alt((
                            map(identifier, Suffix::Index),
                            map(string, |s| Suffix::Query(Box::new(Term::String(s).into()))),
                            suffix,
                        )),
                        |suffix| Term::Suffix(Box::new(Term::Identity), vec![suffix]),
                    ),
                ),
            ),
            value(Term::Recurse, tag("..")),
            value(Term::Identity, char('.')),
        ))(input)
    }
    fn term_with_suffix(input: &str) -> ParseResult<Term> {
        map(
            pair(
                term_inner,
                many0(preceded(
                    multispace0,
                    alt((
                        value(Suffix::Optional, char('?')),
                        preceded(
                            terminated(char('.'), multispace0),
                            alt((
                                map(identifier, Suffix::Index),
                                map(string, |s| Suffix::Query(Box::new(Term::String(s).into()))),
                                suffix,
                            )),
                        ),
                        suffix,
                    )),
                )),
            ),
            |(term, suffixes)| {
                if suffixes.is_empty() {
                    term
                } else if let Term::Suffix(term, mut original) = term {
                    original.extend(suffixes);
                    Term::Suffix(term, original)
                } else {
                    Term::Suffix(Box::new(term), suffixes)
                }
            },
        )(input)
    }

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

fn funcdef(input: &str) -> ParseResult<FuncDef> {
    map(
        tuple((
            preceded(tag("def"), ws(identifier)),
            opt(delimited(
                char('('),
                separated_list1(char(';'), ws(alt((identifier, variable)))),
                terminated(char(')'), multispace0),
            )),
            delimited(char(':'), ws(query), char(';')),
        )),
        |(name, args, body)| FuncDef {
            name,
            args: args.unwrap_or_default(),
            body: Box::new(body),
        },
    )(input)
}

pub fn query(input: &str) -> ParseResult<Query> {
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
    fn bind_pattern(input: &str) -> ParseResult<BindPattern> {
        fn entry(input: &str) -> ParseResult<ObjectBindPatternEntry> {
            alt((
                map(
                    pair(
                        alt((
                            map(identifier, |ident| {
                                Term::String(vec![StringFragment::String(ident.0)]).into()
                            }),
                            map(variable, |name| {
                                Term::FunctionCall { name, args: vec![] }.into()
                            }),
                            map(string, |t| Term::String(t).into()),
                            delimited(char('('), ws(query), char(')')),
                        )),
                        preceded(ws(char(':')), bind_pattern),
                    ),
                    |(key, value)| ObjectBindPatternEntry::KeyValue(Box::new(key), Box::new(value)),
                ),
                map(variable, ObjectBindPatternEntry::KeyOnly),
            ))(input)
        }
        alt((
            map(variable, BindPattern::Variable),
            delimited(
                char('['),
                map(
                    separated_list1(char(','), ws(bind_pattern)),
                    BindPattern::Array,
                ),
                char(']'),
            ),
            delimited(
                char('{'),
                map(separated_list1(char(','), ws(entry)), BindPattern::Object),
                char('}'),
            ),
        ))(input)
    }

    fn query100(input: &str) -> ParseResult<Query> {
        map(term, Term::into)(input)
    }
    fn query10(input: &str) -> ParseResult<Query> {
        alt((
            map(
                pair(funcdef, preceded(multispace0, query)),
                |(function, query)| Query::WithFunc {
                    function,
                    query: Box::new(query),
                },
            ),
            map(
                pair(
                    preceded(tag("try"), preceded(multispace0, query)),
                    opt(preceded(ws(tag("catch")), query10)),
                ),
                |(lhs, rhs)| Query::Try {
                    body: Box::new(lhs),
                    catch: rhs.map(Box::new),
                },
            ),
            map(
                tuple((
                    terminated(term, ws(tag("as"))),
                    separated_list1(ws(tag("?//")), bind_pattern),
                    preceded(ws(char('|')), query10),
                )),
                |(term, patterns, query)| Query::Bind {
                    source: Box::new(term),
                    patterns,
                    body: Box::new(query),
                },
            ),
            map(
                tuple((
                    delimited(tag("reduce"), ws(term), tag("as")),
                    ws(bind_pattern),
                    delimited(char('('), ws(query), char(';')),
                    terminated(ws(query), char(')')),
                )),
                |(term, pattern, q1, q2)| Query::Reduce {
                    source: Box::new(term),
                    pattern,
                    initial: Box::new(q1),
                    accumulator: Box::new(q2),
                },
            ),
            map(
                tuple((
                    delimited(tag("foreach"), ws(term), tag("as")),
                    ws(bind_pattern),
                    delimited(char('('), ws(query), char(';')),
                    terminated(
                        pair(ws(query), opt(preceded(char(';'), ws(query)))),
                        char(')'),
                    ),
                )),
                |(term, pattern, q1, (q2, q3))| Query::ForEach {
                    source: Box::new(term),
                    pattern,
                    initial: Box::new(q1),
                    update: Box::new(q2),
                    extract: q3.map(Box::new),
                },
            ),
            map(
                tuple((
                    delimited(terminated(tag("if"), multispace0), query, ws(tag("then"))),
                    query,
                    many0(pair(
                        preceded(ws(tag("elif")), query),
                        preceded(ws(tag("then")), query),
                    )),
                    opt(preceded(ws(tag("else")), query)),
                )),
                |(cond, body, chain, other)| {
                    let mut o = other.map(Box::new);
                    for (c, b) in chain.into_iter().rev() {
                        o = Some(Box::new(Query::If {
                            cond: Box::new(c),
                            positive: Box::new(b),
                            negative: o,
                        }));
                    }
                    Query::If {
                        cond: Box::new(cond),
                        positive: Box::new(body),
                        negative: o,
                    }
                },
            ),
            map(
                pair(
                    delimited(tag("label"), ws(variable), char('|')),
                    preceded(multispace0, query),
                ),
                |(var, query)| Query::Label {
                    label: var,
                    body: Box::new(query),
                },
            ),
            query100,
        ))(input)
    }
    fn query9(input: &str) -> ParseResult<Query> {
        map(
            pair(query10, opt(preceded(multispace0, char('?')))),
            |(q, opt)| {
                if opt.is_some() {
                    Term::Suffix(Box::new(q.into()), vec![Suffix::Optional]).into()
                } else {
                    q
                }
            },
        )(input)
    }
    fn query8(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(
            ws(alt((
                value(BinaryOp::Multiply, char('*')),
                value(BinaryOp::Divide, char('/')),
                value(BinaryOp::Modulo, char('%')),
            ))),
            query9,
            |lhs, operator, rhs| Query::Operate {
                lhs: Box::new(lhs),
                operator,
                rhs: Box::new(rhs),
            },
        )(input)
    }
    fn query7(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(
            ws(alt((
                value(BinaryOp::Add, char('+')),
                value(BinaryOp::Subtract, char('-')),
            ))),
            query8,
            |lhs, operator, rhs| Query::Operate {
                lhs: Box::new(lhs),
                operator,
                rhs: Box::new(rhs),
            },
        )(input)
    }
    fn query6(input: &str) -> ParseResult<Query> {
        binop_no_assoc(ws(comparator), query7, |lhs, operator, rhs| {
            Query::Compare {
                lhs: Box::new(lhs),
                operator,
                rhs: Box::new(rhs),
            }
        })(input)
    }
    fn query5(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(ws(tag("and")), query6, |lhs, _, rhs| Query::Operate {
            lhs: Box::new(lhs),
            operator: BinaryOp::And,
            rhs: Box::new(rhs),
        })(input)
    }
    fn query4(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(ws(tag("or")), query5, |lhs, _, rhs| Query::Operate {
            lhs: Box::new(lhs),
            operator: BinaryOp::Or,
            rhs: Box::new(rhs),
        })(input)
    }
    fn query3(input: &str) -> ParseResult<Query> {
        binop_no_assoc(ws(update_op), query4, |lhs, operator, rhs| Query::Update {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        })(input)
    }
    fn query2(input: &str) -> ParseResult<Query> {
        binop_chain_right_assoc(ws(tag("//")), query3, |lhs, _, rhs| Query::Operate {
            lhs: Box::new(lhs),
            operator: BinaryOp::Alt,
            rhs: Box::new(rhs),
        })(input)
    }
    fn query1(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(ws(char(',')), query2, |lhs, _, rhs| Query::Concat {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })(input)
    }
    fn query0(input: &str) -> ParseResult<Query> {
        binop_chain_right_assoc(ws(char('|')), query1, |lhs, _, rhs| Query::Pipe {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })(input)
    }

    query0(input)
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
        assert_eq!(variable("$ab12+c"), Ok(("+c", Identifier("$ab12"))));
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
                    Query(ast::Query::Operate {
                        lhs: Box::new(Term::Number(1.into()).into()),
                        operator: BinaryOp::Add,
                        rhs: Box::new(Term::Number(2.into()).into())
                    }),
                    String("def")
                ]
            ))
        );
    }
}
