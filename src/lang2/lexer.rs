use lexgen::lexer;
use thiserror::Error;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Keyword {
    Or,
    And,
    Module,
    Import,
    Include,
    Def,
    As,
    Label,
    Break,
    Null,
    True,
    False,
    If,
    Then,
    Elif,
    Else,
    End,
    Try,
    Catch,
    Reduce,
    Foreach,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum StringFragment<'input> {
    String(&'input str),
    Char(char),
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Token<'input> {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    SlashSlashEq,
    PipeEq,

    EqEq,
    LtEq,
    GtEq,
    Lt,
    Gt,

    Comma,
    Dot,
    Semicolon,
    Colon,
    DotDot,

    Pipe,
    Question,
    QuestionSlashSlash,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    StringStart,
    StringFragment(StringFragment<'input>),
    InterpolationStart,
    InterpolationEnd,
    StringEnd,

    Number(&'input str),
    Keyword(Keyword),
    Identifier(&'input str),
    Variable(&'input str),
    Format(&'input str),
}

struct LexerState {
    paren_nest: Vec<usize>,
}
impl Default for LexerState {
    fn default() -> Self {
        Self {
            paren_nest: vec![0],
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub enum LexicalError {
    #[error("Found too many close parentheses")]
    TooManyCloseParen,
    #[error("Invalid unicode scalar value: `{0}`")]
    InvalidUnicodeScalar(u32),
    #[error("Something went wrong")]
    InvalidState,
}

lexer! {
    Lexer(LexerState) -> Token<'input>;
    type Error = LexicalError;

    let ws = [' ' '\t' '\n'] | "\r\n";
    let ident_start = ['a'-'z' 'A'-'Z' '_'];
    let digit = ['0'-'9'];
    let hex_digit = $digit | ['a'-'f' 'A'-'F'];
    let ident_follow = $ident_start | $digit;

    rule Init {
        $ws,
        "+" = Token::Plus,
        "-" = Token::Minus,
        "*" = Token::Star,
        "/" = Token::Slash,
        "%" = Token::Percent,

        "=" = Token::Eq,
        "+=" = Token::PlusEq,
        "-=" = Token::MinusEq,
        "*=" = Token::StarEq,
        "/=" = Token::SlashEq,
        "//=" = Token::SlashSlashEq,
        "|=" = Token::PipeEq,

        "==" = Token::EqEq,
        "<=" = Token::LtEq,
        ">=" = Token::GtEq,
        "<" = Token::Lt,
        ">" = Token::Gt,

        "," = Token::Comma,
        "." = Token::Dot,
        ";" = Token::Semicolon,
        ":" = Token::Colon,
        ".." = Token::DotDot,

        "|" = Token::Pipe,
        "?" = Token::Question,
        "?//" = Token::QuestionSlashSlash,

        "(" =? |lexer| {
            match lexer.state().paren_nest.last_mut() {
                Some(x) => {
                    *x += 1;
                    lexer.return_(Ok(Token::LParen))
                },
                None => {
                    lexer.return_(Err(LexicalError::InvalidState))
                }
            }
        },
        ")" =? |lexer| {
            match lexer.state().paren_nest.last_mut() {
                Some(0) => {
                    lexer.state().paren_nest.pop();
                    if lexer.state().paren_nest.is_empty() {
                        lexer.return_(Err(LexicalError::TooManyCloseParen))
                    } else {
                        lexer.switch_and_return(LexerRule::InString, Ok(Token::InterpolationEnd))
                    }
                },
                Some(x) => {
                    *x -= 1;
                    lexer.return_(Ok(Token::RParen))
                },
                None => {
                    lexer.return_(Err(LexicalError::TooManyCloseParen))
                }
            }
        },
        "{" = Token::LBrace,
        "}" = Token::RBrace,
        "[" = Token::LBracket,
        "]" = Token::RBracket,

        "or" = Token::Keyword(Keyword::Or),
        "and" = Token::Keyword(Keyword::And),
        "module" = Token::Keyword(Keyword::Module),
        "import" = Token::Keyword(Keyword::Import),
        "include" = Token::Keyword(Keyword::Include),
        "def" = Token::Keyword(Keyword::Def),
        "as" = Token::Keyword(Keyword::As),
        "label" = Token::Keyword(Keyword::Label),
        "break" = Token::Keyword(Keyword::Break),
        "null" = Token::Keyword(Keyword::Null),
        "true" = Token::Keyword(Keyword::True),
        "false" = Token::Keyword(Keyword::False),
        "if" = Token::Keyword(Keyword::If),
        "then" = Token::Keyword(Keyword::Then),
        "elif" = Token::Keyword(Keyword::Elif),
        "else" = Token::Keyword(Keyword::Else),
        "end" = Token::Keyword(Keyword::End),
        "try" = Token::Keyword(Keyword::Try),
        "catch" = Token::Keyword(Keyword::Catch),
        "reduce" = Token::Keyword(Keyword::Reduce),
        "foreach" = Token::Keyword(Keyword::Foreach),

        $ident_start $ident_follow* ("::" $ident_start $ident_follow*)* => |lexer| {
            lexer.return_(Token::Identifier(lexer.match_()))
        },
        "$" $ident_start $ident_follow* ("::" $ident_start $ident_follow*)* => |lexer| {
            lexer.return_(Token::Variable(&lexer.match_()[1..]))
        },
        "@" $ident_start $ident_follow* => |lexer| {
            lexer.return_(Token::Format(&lexer.match_()[1..]))
        },
        "\"" => |lexer| {
            lexer.switch_and_return(LexerRule::InString, Token::StringStart)
        },
    }
    rule InString {
        "\\n" = Token::StringFragment(StringFragment::Char('\n')),
        "\\r" = Token::StringFragment(StringFragment::Char('\r')),
        "\\t" = Token::StringFragment(StringFragment::Char('\t')),
        "\\b" = Token::StringFragment(StringFragment::Char('\u{08}')),
        "\\f" = Token::StringFragment(StringFragment::Char('\u{0C}')),
        "\\\\" = Token::StringFragment(StringFragment::Char('\\')),
        "\\/" = Token::StringFragment(StringFragment::Char('/')),
        "\\\"" = Token::StringFragment(StringFragment::Char('"')),
        "\\u" $hex_digit $hex_digit $hex_digit $hex_digit =? |lexer| {
            let value = u32::from_str_radix(&lexer.match_()[2..], 16).unwrap();
            match char::try_from(value) {
                Ok(c) => {
                    lexer.return_(Ok(Token::StringFragment(StringFragment::Char(c))))
                }
                Err(_) => lexer.return_(Err(LexicalError::InvalidUnicodeScalar(value)))
            }
        },
        "\\(" => |lexer| {
            lexer.state().paren_nest.push(0);
            lexer.switch_and_return(LexerRule::Init, Token::InterpolationStart)
        },
        "\"" =? |lexer| {
            lexer.switch_and_return(LexerRule::Init, Ok(Token::StringEnd))
        },
        (_ # ['\\' '"'])+ => |lexer| {
            lexer.return_(Token::StringFragment(StringFragment::String(lexer.match_())))
        },
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, StringFragment, Token};

    fn string_fragment(s: &str) -> Token {
        Token::StringFragment(StringFragment::String(s))
    }
    fn assert_lex(q: &str, expected_tokens: &[Token]) {
        let tokens: Vec<_> = Lexer::new(q)
            .map(Result::unwrap)
            .map(|(_, token, _)| token)
            .collect();
        assert_eq!(&tokens[..], expected_tokens);
    }

    #[test]
    fn test_ident() {
        assert_lex(r#"abc"#, &[Token::Identifier("abc")]);
        assert_lex(r#"abc12"#, &[Token::Identifier("abc12")]);
        assert_lex(
            r#"ab ab12"#,
            &[Token::Identifier("ab"), Token::Identifier("ab12")],
        );
        assert_lex(
            r#"ab_ ab_12"#,
            &[Token::Identifier("ab_"), Token::Identifier("ab_12")],
        );
    }

    #[test]
    fn test_string() {
        assert_lex(
            r#""abc""#,
            &[Token::StringStart, string_fragment("abc"), Token::StringEnd],
        );
    }

    #[test]
    fn test_string_interpolation() {
        assert_lex(
            r#"(ab"(\(a"\()")))")"#,
            &[
                Token::LParen,
                Token::Identifier("ab"),
                Token::StringStart,
                string_fragment("("),
                Token::InterpolationStart,
                Token::Identifier("a"),
                Token::StringStart,
                Token::InterpolationStart,
                Token::InterpolationEnd,
                Token::StringEnd,
                Token::InterpolationEnd,
                string_fragment("))"),
                Token::StringEnd,
                Token::RParen,
            ],
        );
    }
}
