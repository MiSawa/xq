use lexgen::lexer;
use thiserror::Error;

pub type Loc = lexgen_util::Loc;
pub type LexerError = lexgen_util::LexerError<LexicalError>;

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
    False,
    True,
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
    PercentEq,
    SlashSlashEq,
    PipeEq,

    EqEq,
    NotEq,
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

    Keyword(&'input str, Keyword),
    Field(&'input str),
    Identifier(&'input str),
    ModuleIdentifier(&'input str),
    Variable(&'input str),
    ModuleVariable(&'input str),
    Format(&'input str),
    Number(crate::Number),
}

pub struct LexerState {
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
    #[error("Unable to parse number: `{0}`")]
    InvalidNumber(String),
    #[error("Something went wrong")]
    InvalidState,
}

lexer! {
    pub Lexer(LexerState) -> Token<'input>;
    type Error = LexicalError;

    let ws = [' ' '\t' '\n'] | "\r\n";
    let ident_start = ['a'-'z' 'A'-'Z' '_'];
    let digit = ['0'-'9'];
    let hex_digit = $digit | ['a'-'f' 'A'-'F'];
    let ident_follow = $ident_start | $digit;

    rule Init {
        $ws,
        '+' = Token::Plus,
        '-' = Token::Minus,
        '*' = Token::Star,
        '/' = Token::Slash,
        '%' = Token::Percent,

        '=' = Token::Eq,
        "+=" = Token::PlusEq,
        "-=" = Token::MinusEq,
        "*=" = Token::StarEq,
        "/=" = Token::SlashEq,
        "//=" = Token::SlashSlashEq,
        "|=" = Token::PipeEq,

        "==" = Token::EqEq,
        "!=" = Token::NotEq,
        "<=" = Token::LtEq,
        ">=" = Token::GtEq,
        '<' = Token::Lt,
        '>' = Token::Gt,

        ',' = Token::Comma,
        '.' = Token::Dot,
        ';' = Token::Semicolon,
        ':' = Token::Colon,
        ".." = Token::DotDot,

        '|' = Token::Pipe,
        '?' = Token::Question,
        "?//" = Token::QuestionSlashSlash,

        '(' =? |lexer| {
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
        ')' =? |lexer| {
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
        '{' = Token::LBrace,
        '}' = Token::RBrace,
        '[' = Token::LBracket,
        ']' = Token::RBracket,

        "or"      => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Or)),
        "and"     => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::And)),
        "module"  => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Module)),
        "import"  => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Import)),
        "include" => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Include)),
        "def"     => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Def)),
        "as"      => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::As)),
        "label"   => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Label)),
        "break"   => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Break)),
        "null"    => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Null)),
        "false"   => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::False)),
        "true"    => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::True)),
        "if"      => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::If)),
        "then"    => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Then)),
        "elif"    => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Elif)),
        "else"    => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Else)),
        "end"     => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::End)),
        "try"     => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Try)),
        "catch"   => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Catch)),
        "reduce"  => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Reduce)),
        "foreach" => |lexer| lexer.return_(Token::Keyword(lexer.match_(), Keyword::Foreach)),

        $ident_start $ident_follow* => |lexer| {
            lexer.return_(Token::Identifier(lexer.match_()))
        },
        $ident_start $ident_follow* ("::" $ident_start $ident_follow*)+ => |lexer| {
            lexer.return_(Token::ModuleIdentifier(lexer.match_()))
        },
        '$' $ident_start $ident_follow* => |lexer| {
            lexer.return_(Token::Variable(&lexer.match_()[1..]))
        },
        '$' $ident_start $ident_follow* ("::" $ident_start $ident_follow*)+ => |lexer| {
            lexer.return_(Token::ModuleVariable(&lexer.match_()[1..]))
        },
        '@' $ident_start $ident_follow* => |lexer| {
            lexer.return_(Token::Format(&lexer.match_()[1..]))
        },
        (['+' '-'] ?) ($digit+ | $digit+ '.' $digit* | $digit* '.' $digit+) (['e' 'E'] (['+' '-']? $digit+)) =? |lexer| {
            use std::str::FromStr;
            let parsed = crate::Number::from_str(lexer.match_())
                .map_err(|_| LexicalError::InvalidNumber(lexer.match_().to_string()))
                .map(Token::Number);
            lexer.return_(parsed)
        },
        '"' => |lexer| {
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
        '"' => |lexer| {
            lexer.switch_and_return(LexerRule::Init, Token::StringEnd)
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
