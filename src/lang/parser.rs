use crate::{
    lang::ast::{
        BinaryArithmeticOp, BinaryOp, BindPattern, Comparator, ConstantArray, ConstantObject,
        ConstantPrimitive, ConstantValue, FuncArg, FuncDef, Identifier, Import,
        ObjectBindPatternEntry, Program, Query, StringFragment, Suffix, Term, UnaryOp, UpdateOp,
    },
    Number, Value,
};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_while_m_n},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1},
    combinator::{eof, map, map_res, opt, recognize, success, value, verify},
    error::{Error, ParseError},
    multi::{fold_many0, many0, separated_list0, separated_list1},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish, IResult, Parser,
};
use std::convert::TryFrom;

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

const KEYWORDS: [&str; 21] = [
    "or", "and", "module", "import", "include", "def", "as", "label", "break", "null", "true",
    "false", "if", "then", "elif", "else", "end", "try", "catch", "reduce", "foreach",
];

pub fn keyword(keyword: &'static str) -> impl Fn(&str) -> ParseResult<()> {
    move |input: &str| {
        map(
            verify(
                pair(tag(keyword), opt(alt((alphanumeric1, tag("_"))))),
                |(_, x)| x.is_none(),
            ),
            |_| (),
        )(input)
    }
}

fn identifier(input: &str) -> ParseResult<Identifier> {
    map(
        verify(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            |s| !KEYWORDS.contains(s),
        ),
        Identifier::from,
    )(input)
}

fn identifier_allow_keyword(input: &str) -> ParseResult<Identifier> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        Identifier::from,
    )(input)
}

fn variable(input: &str) -> ParseResult<Identifier> {
    map(
        preceded(char('$'), identifier_allow_keyword),
        Identifier::from,
    )(input)
}

fn identifier_or_module_identifier(input: &str) -> ParseResult<Identifier> {
    alt((
        identifier,
        map(
            recognize(tuple((
                identifier_allow_keyword,
                tag("::"),
                separated_list1(tag("::"), identifier_allow_keyword),
            ))),
            Identifier::from,
        ),
    ))(input)
}

fn variable_or_module_variable(input: &str) -> ParseResult<Identifier> {
    map(
        preceded(
            char('$'),
            recognize(separated_list1(tag("::"), identifier_allow_keyword)),
        ),
        Identifier::from,
    )(input)
}

fn format(input: &str) -> ParseResult<Term> {
    map(
        pair(
            preceded(
                char('@'),
                map(
                    recognize(many0(alt((alphanumeric1, tag("_"))))),
                    Identifier::from,
                ),
            ),
            opt(preceded(multispace0, string)),
        ),
        |(ident, str)| Term::Format(ident, str),
    )(input)
}

fn number(input: &str) -> ParseResult<Number> {
    alt((
        map(double, |x| x.into()),
        map_res(digit1, |s: &str| s.parse().map(|n: i64| n.into())),
    ))(input)
}

fn escaped_char(input: &str) -> ParseResult<std::primitive::char> {
    fn codepoint(input: &str) -> ParseResult<std::primitive::char> {
        preceded(
            char('u'),
            map(
                take_while_m_n(4, 4, |c: std::primitive::char| c.is_ascii_hexdigit()),
                |digits: &str| char::try_from(u32::from_str_radix(digits, 16).unwrap()).unwrap(),
            ),
        )(input)
    }
    preceded(
        char('\\'),
        alt((
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
            codepoint,
        )),
    )(input)
}

fn literal_string_fragment1(input: &str) -> ParseResult<&str> {
    let not_quote_slash = is_not("\"\\");
    verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

fn constant_string(input: &str) -> ParseResult<String> {
    enum Fragment<'a> {
        Str(&'a str),
        Char(char),
    }
    fn fragment(input: &str) -> ParseResult<Fragment> {
        alt((
            map(escaped_char, Fragment::Char),
            map(literal_string_fragment1, Fragment::Str),
        ))(input)
    }

    delimited(
        char('"'),
        fold_many0(fragment, String::new, |mut acc: String, f| {
            match f {
                Fragment::Char(c) => acc.push(c),
                Fragment::Str(s) => acc.push_str(s),
            }
            acc
        }),
        char('"'),
    )(input)
}

fn constant_value(input: &str) -> ParseResult<ConstantValue> {
    fn primitive(input: &str) -> ParseResult<ConstantPrimitive> {
        alt((
            value(ConstantPrimitive::Null, keyword("null")),
            value(ConstantPrimitive::True, keyword("true")),
            value(ConstantPrimitive::False, keyword("false")),
            map(number, ConstantPrimitive::Number),
            map(constant_string, ConstantPrimitive::String),
        ))(input)
    }
    alt((
        delimited(
            terminated(char('['), multispace0),
            map(separated_list0(ws(char(',')), constant_value), |v| {
                ConstantValue::Array(ConstantArray(v))
            }),
            preceded(multispace0, char(']')),
        ),
        map(constant_object, ConstantValue::Object),
        map(primitive, ConstantValue::Primitive),
    ))(input)
}
fn constant_object(input: &str) -> ParseResult<ConstantObject> {
    fn entry(input: &str) -> ParseResult<(String, ConstantValue)> {
        separated_pair(
            alt((
                constant_string,
                map(identifier_allow_keyword, |ident| ident.0),
            )),
            ws(char(':')),
            constant_value,
        )(input)
    }
    delimited(
        terminated(char('{'), multispace0),
        map(separated_list0(ws(char(',')), entry), ConstantObject),
        preceded(multispace0, char('}')),
    )(input)
}

fn string(input: &str) -> ParseResult<Vec<StringFragment>> {
    enum Fragment<'a> {
        Str(&'a str),
        Char(char),
        Query(Query),
    }
    fn string_fragment(input: &str) -> ParseResult<Fragment> {
        alt((
            map(literal_string_fragment1, Fragment::Str),
            map(escaped_char, Fragment::Char),
            delimited(tag("\\("), map(ws(query), Fragment::Query), char(')')),
        ))(input)
    }

    delimited(
        char('"'),
        fold_many0(string_fragment, Vec::new, |mut vec, s| {
            match s {
                Fragment::Char(c) => {
                    if let Some(StringFragment::String(last)) = vec.last_mut() {
                        last.push(c)
                    } else {
                        vec.push(StringFragment::String(c.to_string()))
                    }
                }
                Fragment::Str(s) => {
                    if let Some(StringFragment::String(last)) = vec.last_mut() {
                        last.push_str(s)
                    } else {
                        vec.push(StringFragment::String(s.to_string()))
                    }
                }
                Fragment::Query(q) => vec.push(StringFragment::Query(q)),
            }
            vec
        }),
        char('"'),
    )(input)
}

fn term(input: &str) -> ParseResult<Term> {
    fn suffix(input: &str) -> ParseResult<Suffix> {
        delimited(
            terminated(char('['), multispace0),
            alt((
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
                success(Suffix::Iterate),
            )),
            preceded(multispace0, char(']')),
        )(input)
    }
    fn object_term_entry(input: &str) -> ParseResult<(Query, Option<Query>)> {
        pair(
            alt((
                map(identifier_allow_keyword, |ident| {
                    Term::String(vec![StringFragment::String(ident.0)]).into()
                }),
                map(variable, |ident| Term::Variable(ident).into()),
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
            value(Term::Constant(Value::Null), keyword("null")),
            value(Term::Constant(Value::Boolean(false)), keyword("false")),
            value(Term::Constant(Value::Boolean(true)), keyword("true")),
            map(number, |n| Term::Constant(Value::number(n))),
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
                Term::Object,
            ),
            map(
                preceded(pair(keyword("break"), multispace0), variable),
                Term::Break,
            ),
            map(
                pair(
                    identifier_or_module_identifier,
                    opt(delimited(
                        preceded(multispace0, char('(')),
                        separated_list1(char(';'), ws(query)),
                        char(')'),
                    )),
                ),
                |(name, args)| Term::FunctionCall {
                    name,
                    args: args.unwrap_or_default(),
                },
            ),
            map(variable_or_module_variable, Term::Variable),
            format,
            map(string, Term::String),
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
                        |suffix| Term::Suffix(Box::new(Term::Identity), suffix),
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
                suffixes
                    .into_iter()
                    .fold(term, |t, s| Term::Suffix(Box::new(t), s))
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
        term_with_suffix,
    ))(input)
}

fn funcdef(input: &str) -> ParseResult<FuncDef> {
    map(
        tuple((
            preceded(keyword("def"), ws(identifier)),
            opt(delimited(
                char('('),
                separated_list1(
                    char(';'),
                    ws(alt((
                        map(identifier, FuncArg::Closure),
                        map(variable, FuncArg::Variable),
                    ))),
                ),
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

fn query(input: &str) -> ParseResult<Query> {
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
            value(UpdateOp::Arithmetic(BinaryArithmeticOp::Add), tag("+=")),
            value(
                UpdateOp::Arithmetic(BinaryArithmeticOp::Subtract),
                tag("-="),
            ),
            value(
                UpdateOp::Arithmetic(BinaryArithmeticOp::Multiply),
                tag("*="),
            ),
            value(UpdateOp::Arithmetic(BinaryArithmeticOp::Divide), tag("/=")),
            value(UpdateOp::Arithmetic(BinaryArithmeticOp::Modulo), tag("%=")),
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
                            map(variable, |name| Term::Variable(name).into()),
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
                    preceded(keyword("try"), preceded(multispace0, query)),
                    opt(preceded(ws(keyword("catch")), query10)),
                ),
                |(lhs, rhs)| Query::Try {
                    body: Box::new(lhs),
                    catch: rhs.map(Box::new),
                },
            ),
            map(
                tuple((
                    terminated(term, ws(keyword("as"))),
                    separated_list1(ws(tag("?//")), bind_pattern),
                    preceded(ws(char('|')), query),
                )),
                |(term, patterns, query)| Query::Bind {
                    source: Box::new(term),
                    patterns,
                    body: Box::new(query),
                },
            ),
            map(
                tuple((
                    delimited(keyword("reduce"), ws(term), keyword("as")),
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
                    delimited(keyword("foreach"), ws(term), keyword("as")),
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
                terminated(
                    tuple((
                        delimited(
                            terminated(keyword("if"), multispace1),
                            query,
                            ws(keyword("then")),
                        ),
                        query,
                        many0(pair(
                            preceded(ws(keyword("elif")), query),
                            preceded(ws(keyword("then")), query),
                        )),
                        opt(preceded(ws(keyword("else")), query)),
                    )),
                    preceded(multispace0, keyword("end")),
                ),
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
                    delimited(keyword("label"), ws(variable), char('|')),
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
                    Term::Suffix(Box::new(q.into()), Suffix::Optional).into()
                } else {
                    q
                }
            },
        )(input)
    }
    fn query8(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(
            ws(alt((
                value(
                    BinaryOp::Arithmetic(BinaryArithmeticOp::Multiply),
                    char('*'),
                ),
                value(BinaryOp::Arithmetic(BinaryArithmeticOp::Divide), char('/')),
                value(BinaryOp::Arithmetic(BinaryArithmeticOp::Modulo), char('%')),
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
                value(BinaryOp::Arithmetic(BinaryArithmeticOp::Add), char('+')),
                value(
                    BinaryOp::Arithmetic(BinaryArithmeticOp::Subtract),
                    char('-'),
                ),
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
                comparator: operator,
                rhs: Box::new(rhs),
            }
        })(input)
    }
    fn query5(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(ws(keyword("and")), query6, |lhs, _, rhs| Query::Operate {
            lhs: Box::new(lhs),
            operator: BinaryOp::And,
            rhs: Box::new(rhs),
        })(input)
    }
    fn query4(input: &str) -> ParseResult<Query> {
        binop_chain_left_assoc(ws(keyword("or")), query5, |lhs, _, rhs| Query::Operate {
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

fn import(input: &str) -> ParseResult<Import> {
    map(
        alt((
            preceded(
                keyword("import"),
                tuple((
                    ws(constant_string),
                    preceded(keyword("as"), ws(map(alt((identifier, variable)), Some))),
                    terminated(opt(terminated(constant_object, multispace0)), char(':')),
                )),
            ),
            preceded(
                keyword("include"),
                tuple((
                    ws(constant_string),
                    success(None),
                    terminated(opt(terminated(constant_object, multispace0)), char(':')),
                )),
            ),
        )),
        |(path, alias, meta)| Import { path, alias, meta },
    )(input)
}

fn program(input: &str) -> ParseResult<Program> {
    map(
        tuple((
            opt(delimited(
                keyword("module"),
                ws(constant_object),
                terminated(char(';'), multispace0),
            )),
            many0(terminated(import, multispace0)),
            alt((
                pair(success(vec![]), query),
                pair(
                    // This eats the following spaces... It's fine but all the other places are not made in that way.
                    // This is because `separated_list0` doesn't behave well with `multispace0`.
                    many0(terminated(funcdef, multispace0)),
                    success(Term::Identity.into()),
                ),
            )),
        )),
        |(module_header, imports, (functions, query))| Program {
            module_header,
            imports,
            functions,
            query,
        },
    )(input)
}

pub fn parse_query(input: &str) -> Result<Program, Error<String>> {
    terminated(ws(program), eof)(input)
        .finish()
        .map(|(rest, program)| {
            assert!(
                rest.is_empty(),
                "Failed to parse to the end of the file. \nParsed: {:?}\nRest: {}",
                program,
                rest
            );
            program
        })
        .map_err(|e| Error::new(e.input.into(), e.code))
}

#[cfg(test)]
mod test {
    use crate::{
        lang::{
            ast::{BinaryArithmeticOp, BinaryOp, StringFragment, Suffix, Term, UnaryOp},
            parser::{format, identifier, string, term, variable},
        },
        Value,
    };

    fn string_term(s: &str) -> Term {
        Term::String(vec![StringFragment::String(s.to_string())])
    }

    #[test]
    fn test_identifier() {
        assert_eq!(identifier("ab_c"), Ok(("", "ab_c".into())));
        assert_eq!(identifier("ab+c"), Ok(("+c", "ab".into())));
        assert!(identifier("123abc").is_err());
    }

    #[test]
    fn test_variable() {
        assert_eq!(variable("$ab12+c"), Ok(("+c", "ab12".into())));
        assert!(variable("$12abc").is_err());
        assert!(variable("$ abc").is_err());
    }

    #[test]
    fn test_format() {
        assert_eq!(format("@tsv"), Ok(("", Term::Format("tsv".into(), None))));
        assert_eq!(format("@123"), Ok(("", Term::Format("123".into(), None))));
        assert_eq!(
            format(r#"@tsv"hoge\($foo)""#),
            Ok((
                "",
                Term::Format(
                    "tsv".into(),
                    Some(vec![
                        StringFragment::String("hoge".to_string()),
                        StringFragment::Query(Term::Variable("foo".into()).into())
                    ])
                )
            ))
        );
    }

    #[test]
    fn test_term() {
        assert_eq!(term("true"), Ok(("", Term::Constant(Value::Boolean(true)))));
        assert_eq!(
            term("false"),
            Ok(("", Term::Constant(Value::Boolean(false))))
        );
        assert_eq!(term("null"), Ok(("", Term::Constant(Value::Null))));
        assert_eq!(
            term("-123"),
            Ok((
                "",
                Term::Unary(UnaryOp::Minus, Box::new(Term::Constant(Value::number(123))))
            ))
        );
        assert_eq!(term("3.14"), Ok(("", Term::Constant(Value::number(3.14)))));
        assert_eq!(term("[ ]"), Ok(("", Term::Array(None))));
        assert_eq!(
            term(". [ ]"),
            Ok(("", Term::Suffix(Box::new(Term::Identity), Suffix::Iterate)))
        );
        assert_eq!(
            term(". \"foo\""),
            Ok((
                "",
                Term::Suffix(
                    Box::new(Term::Identity),
                    Suffix::Query(Box::new(string_term("foo").into()))
                )
            ))
        );
        assert_eq!(
            term(". [ \"foo\" ]"),
            Ok((
                "",
                Term::Suffix(
                    Box::new(Term::Identity),
                    Suffix::Query(Box::new(string_term("foo").into()))
                )
            ))
        );
        assert_eq!(
            term(". foo"),
            Ok((
                "",
                Term::Suffix(Box::new(Term::Identity), Suffix::Index("foo".into()))
            ))
        );
        assert_eq!(
            term(". foo [ ]"),
            Ok((
                "",
                Term::Suffix(
                    Box::new(Term::Suffix(
                        Box::new(Term::Identity),
                        Suffix::Index("foo".into())
                    )),
                    Suffix::Iterate
                )
            ))
        );
        assert_eq!(
            term(". foo [ 4 ]"),
            Ok((
                "",
                Term::Suffix(
                    Box::new(Term::Suffix(
                        Box::new(Term::Identity),
                        Suffix::Index("foo".into())
                    )),
                    Suffix::Query(Box::new(Term::Constant(Value::number(4)).into()))
                )
            ))
        );
    }

    #[test]
    fn test_string() {
        use crate::lang::ast;
        use StringFragment::*;
        assert_eq!(
            string(r#""\rabc\"\n\\""#),
            Ok(("", vec![String("\rabc\"\n\\".to_string())]))
        );
        assert_eq!(
            string(r#""abc\( 1 + 2 )def""#),
            Ok((
                "",
                vec![
                    String("abc".to_string()),
                    Query(ast::Query::Operate {
                        lhs: Box::new(Term::Constant(Value::number(1)).into()),
                        operator: BinaryOp::Arithmetic(BinaryArithmeticOp::Add),
                        rhs: Box::new(Term::Constant(Value::number(2)).into())
                    }),
                    String("def".to_string())
                ]
            ))
        );
    }
}
