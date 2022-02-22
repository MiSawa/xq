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
    TryNoCatch,
    Catch,
    Reduce,
    Foreach,
}
impl Keyword {
    pub fn to_str(&self) -> &'static str {
        match self {
            Keyword::Or => "or",
            Keyword::And => "and",
            Keyword::Module => "module",
            Keyword::Import => "import",
            Keyword::Include => "include",
            Keyword::Def => "def",
            Keyword::As => "as",
            Keyword::Label => "label",
            Keyword::Break => "break",
            Keyword::Null => "null",
            Keyword::False => "false",
            Keyword::True => "true",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Elif => "elif",
            Keyword::Else => "else",
            Keyword::End => "end",
            Keyword::Try => "try",
            Keyword::TryNoCatch => "try",
            Keyword::Catch => "catch",
            Keyword::Reduce => "reduce",
            Keyword::Foreach => "foreach",
        }
    }
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
    SlashSlash,
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

    Keyword(Keyword),
    Field(&'input str),
    Identifier(&'input str),
    ModuleIdentifier(&'input str),
    Variable(&'input str),
    ModuleVariable(&'input str),
    Format(&'input str),
    Number(crate::Number),

    // Post-processed
    DefScopeEnd,
    LabelScopeEnd,
    BindScopeEnd,
}

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub enum LexicalError {
    #[error("Unmatching open {0:?} and close {1:?}")]
    UnmatchingOpenClose(OpenCloseType, OpenCloseType),
    #[error("Expected `{0}` but got `{1}`")]
    UnexpectedToken(String, String),
    #[error("Expected token `{0}`")]
    OrphanToken(String),
    #[error("No matching open for close {0:?}")]
    TooManyClose(OpenCloseType),
    #[error("Invalid unicode scalar value: `{0}`")]
    InvalidUnicodeScalar(u32),
    #[error("Unable to parse number: `{0}`")]
    InvalidNumber(String),
    #[error("Something went wrong")]
    InvalidState,
}

enum ContextType<'input> {
    /// parenthesis, braces, brackets, interpolation, if-end, etc.
    Balancing(Token<'input>),
    /// def to the end of the scope, catch to the end of the following query, etc.
    AutoCloseAndEmit(Token<'input>),
    /// try but haven't get catch yet
    Try(usize),
}
pub struct Lexer<'input> {
    input: &'input str,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self { input }
    }
}

impl<'input> IntoIterator for Lexer<'input> {
    type Item = Result<(Loc, Token<'input>, Loc), LexerError>;

    type IntoIter = <Vec<Self::Item> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[derive(Default)]
        struct State<'input> {
            ret: Vec<Result<(Loc, Token<'input>, Loc), LexerError>>,
            pos: Loc,
            stack: Vec<ContextType<'input>>,
        }
        impl<'input> State<'input> {
            fn track_pos(&mut self, pos: Loc) {
                self.pos = pos;
            }
            fn open(&mut self, ty: ContextType<'input>) {
                self.stack.push(ty);
            }
            fn open_balancing(&mut self, token: Token<'input>) {
                self.open(ContextType::Balancing(token));
            }
            fn close_to_try(&mut self) -> Result<(), LexicalError> {
                while let Some(item) = self.stack.last() {
                    match item {
                        ContextType::Balancing(token) => {
                            return Err(LexicalError::UnexpectedToken(
                                format!("{token:?}"),
                                "catch".to_string(),
                            ))
                        }
                        ContextType::AutoCloseAndEmit(term) => {
                            self.ret.push(Ok((self.pos, term.clone(), self.pos)));
                            self.stack.pop();
                        }
                        ContextType::Try(_) => {
                            self.stack.pop();
                            return Ok(());
                        }
                    }
                }
                Err(LexicalError::OrphanToken("catch".to_string()))
            }
            fn close_autoclose(&mut self) -> Option<Token<'input>> {
                while let Some(item) = self.stack.last() {
                    match item {
                        ContextType::Balancing(token) => return Some(token.clone()),
                        ContextType::AutoCloseAndEmit(term) => {
                            self.ret.push(Ok((self.pos, term.clone(), self.pos)));
                            self.stack.pop();
                        }
                        ContextType::Try(i) => {
                            if let Some(Ok((_, Token::Keyword(ref mut keyword), _))) =
                                self.ret.get_mut(*i).map(Result::as_mut)
                            {
                                *keyword = Keyword::TryNoCatch;
                                self.stack.pop();
                            } else {
                                panic!("Something went wrong with parsing try catch");
                            }
                        }
                    }
                }
                None
            }
            fn close_balancing(&mut self, token: &Token<'input>) -> Result<(), LexicalError> {
                self.close_autoclose();
                match self.stack.last() {
                    Some(ContextType::Balancing(expected)) => {
                        if token == expected {
                            self.stack.pop();
                            Ok(())
                        } else {
                            Err(LexicalError::UnexpectedToken(
                                format!("{expected:?}"),
                                format!("{token:?}"),
                            ))
                        }
                    }
                    Some(ContextType::AutoCloseAndEmit(_)) => unreachable!(),
                    Some(ContextType::Try(_)) => unreachable!(),
                    None => Err(LexicalError::OrphanToken(format!("{token:?}"))),
                }
            }
            fn flush_or_close(&mut self, token: &Token<'input>) -> bool {
                self.close_autoclose();
                match self.stack.last() {
                    Some(ContextType::Balancing(expected)) => {
                        if token == expected {
                            self.stack.pop();
                            return true;
                        }
                    }
                    Some(ContextType::AutoCloseAndEmit(_)) => unreachable!(),
                    Some(ContextType::Try(_)) => unreachable!(),
                    None => {}
                }
                false
            }
            fn try_close_without_flush(&mut self, token: &Token<'input>) -> bool {
                if let Some(ContextType::Balancing(expected)) = self.stack.last() {
                    if token == expected {
                        self.stack.pop();
                        return true;
                    }
                }
                false
            }

            fn handle_token(&mut self, token: &Token<'input>) -> Result<(), LexicalError> {
                match token {
                    Token::LParen => self.open_balancing(Token::RParen),
                    Token::LBrace => self.open_balancing(Token::RBrace),
                    Token::LBracket => self.open_balancing(Token::RBracket),
                    Token::Keyword(Keyword::If) => {
                        self.open_balancing(Token::Keyword(Keyword::End))
                    }
                    Token::RParen
                    | Token::RBrace
                    | Token::RBracket
                    | Token::Keyword(Keyword::End) => {
                        self.close_balancing(token)?;
                    }
                    Token::Semicolon
                    | Token::Colon
                    | Token::Keyword(Keyword::Then | Keyword::Elif | Keyword::Else) => {
                        self.flush_or_close(token);
                    }
                    Token::Keyword(Keyword::Def) => {
                        // def _ (_; _; ...): _; _
                        self.open(ContextType::AutoCloseAndEmit(Token::DefScopeEnd));
                        self.open_balancing(Token::Semicolon);
                        self.open_balancing(Token::Colon);
                    }
                    Token::Keyword(Keyword::Try) => {
                        self.open(ContextType::Try(self.ret.len()));
                    }
                    Token::Keyword(Keyword::Catch) => {
                        self.close_to_try()?;
                    }
                    Token::Keyword(Keyword::Reduce | Keyword::Foreach) => {
                        self.open_balancing(Token::Keyword(Keyword::As));
                    }
                    Token::Keyword(Keyword::As) => {
                        if !self.try_close_without_flush(token) {
                            self.open(ContextType::AutoCloseAndEmit(Token::BindScopeEnd));
                        }
                    }
                    Token::Keyword(Keyword::Label) => {
                        self.open(ContextType::AutoCloseAndEmit(Token::LabelScopeEnd));
                    }
                    _ => {}
                }
                Ok(())
            }
            fn handle_item(&mut self, item: Result<(Loc, Token<'input>, Loc), LexerError>) {
                match item {
                    Ok((l, token, r)) => {
                        self.track_pos(l);
                        let to_push =
                            self.handle_token(&token)
                                .map(|_| (l, token, r))
                                .map_err(|e| LexerError {
                                    kind: lexgen_util::LexerErrorKind::Custom(e),
                                    location: l,
                                });
                        self.ret.push(to_push);
                    }
                    Err(_) => {
                        self.ret.push(item);
                    }
                }
            }
            fn finish(mut self) -> Vec<Result<(Loc, Token<'input>, Loc), LexerError>> {
                if let Some(token) = self.close_autoclose() {
                    self.ret.push(Err(LexerError {
                        kind: lexgen_util::LexerErrorKind::Custom(LexicalError::UnexpectedToken(
                            format!("{token:?}"),
                            "EOF".to_string(),
                        )),
                        location: self.pos,
                    }));
                }
                self.ret
            }
        }

        let lexer = LexerImpl::new(self.input);
        let mut state = State::default();

        for item in lexer {
            state.handle_item(item);
        }
        state.finish().into_iter()
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum OpenCloseType {
    /// `(` and `)`
    Parenthesis,
    /// `[` and `]`
    Bracket,
    /// `{` and `}`
    Brace,
    /// `\(` and `)`
    Interpolation,
}

#[derive(Debug, Default)]
struct LexerState {
    stack: Vec<OpenCloseType>,
}
impl LexerState {
    fn open(&mut self, ty: OpenCloseType) {
        self.stack.push(ty);
    }
    fn current_type(&self) -> Option<OpenCloseType> {
        self.stack.last().cloned()
    }
    fn close(&mut self, ty: OpenCloseType) -> Result<(), LexicalError> {
        if let Some(open) = self.current_type() {
            if open == ty {
                self.stack.pop();
                Ok(())
            } else {
                Err(LexicalError::UnmatchingOpenClose(open, ty))
            }
        } else {
            Err(LexicalError::TooManyClose(ty))
        }
    }
}

macro_rules! handle_keyword {
    ($lexer: expr, $keyword: expr) => {
        if $lexer.state().current_type() == Some(OpenCloseType::Brace) {
            // If here is a direct child of braces, it is used as an identifier.
            $lexer.return_(Token::Identifier($lexer.match_()))
        } else {
            $lexer.return_(Token::Keyword($keyword))
        }
    };
}

lexer! {
    LexerImpl(LexerState) -> Token<'input>;
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
        "//" = Token::SlashSlash,
        "?//" = Token::QuestionSlashSlash,

        '(' => |lexer| {
            lexer.state().open(OpenCloseType::Parenthesis);
            lexer.return_(Token::LParen)
        },
        ')' =? |lexer| {
            if lexer.state().current_type() == Some(OpenCloseType::Interpolation) {
                let token = lexer.state().close(OpenCloseType::Interpolation).map(|_| Token::InterpolationEnd);
                lexer.switch_and_return(LexerImplRule::InString, token)
            } else {
                let token = lexer.state().close(OpenCloseType::Parenthesis).map(|_| Token::RParen);
                lexer.return_(token)
            }
        },
        '{' => |lexer| {
            lexer.state().open(OpenCloseType::Brace);
            lexer.return_(Token::LBrace)
        },
        '}' =? |lexer| {
            let token = lexer.state().close(OpenCloseType::Brace).map(|_| Token::RBrace);
            lexer.return_(token)
        },
        '[' => |lexer| {
            lexer.state().open(OpenCloseType::Bracket);
            lexer.return_(Token::LBracket)
        },
        ']' =? |lexer| {
            let token = lexer.state().close(OpenCloseType::Bracket).map(|_| Token::RBracket);
            lexer.return_(token)
        },

        "or"      => |lexer| handle_keyword!(lexer, Keyword::Or),
        "and"     => |lexer| handle_keyword!(lexer, Keyword::And),
        "module"  => |lexer| handle_keyword!(lexer, Keyword::Module),
        "import"  => |lexer| handle_keyword!(lexer, Keyword::Import),
        "include" => |lexer| handle_keyword!(lexer, Keyword::Include),
        "def"     => |lexer| handle_keyword!(lexer, Keyword::Def),
        "as"      => |lexer| handle_keyword!(lexer, Keyword::As),
        "label"   => |lexer| handle_keyword!(lexer, Keyword::Label),
        "break"   => |lexer| handle_keyword!(lexer, Keyword::Break),
        "null"    => |lexer| handle_keyword!(lexer, Keyword::Null),
        "false"   => |lexer| handle_keyword!(lexer, Keyword::False),
        "true"    => |lexer| handle_keyword!(lexer, Keyword::True),
        "if"      => |lexer| handle_keyword!(lexer, Keyword::If),
        "then"    => |lexer| handle_keyword!(lexer, Keyword::Then),
        "elif"    => |lexer| handle_keyword!(lexer, Keyword::Elif),
        "else"    => |lexer| handle_keyword!(lexer, Keyword::Else),
        "end"     => |lexer| handle_keyword!(lexer, Keyword::End),
        "try"     => |lexer| handle_keyword!(lexer, Keyword::Try),
        "catch"   => |lexer| handle_keyword!(lexer, Keyword::Catch),
        "reduce"  => |lexer| handle_keyword!(lexer, Keyword::Reduce),
        "foreach" => |lexer| handle_keyword!(lexer, Keyword::Foreach),

        '.' $ident_start $ident_follow* => |lexer| {
            lexer.return_(Token::Field(&lexer.match_()[1..]))
        },
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
        ($digit+ | $digit+ '.' $digit* | $digit* '.' $digit+) (['e' 'E'] (['+' '-']? $digit+))? =? |lexer| {
            use std::str::FromStr;
            let parsed = crate::Number::from_str(lexer.match_())
                .map_err(|_| LexicalError::InvalidNumber(lexer.match_().to_string()))
                .map(Token::Number);
            lexer.return_(parsed)
        },
        '"' => |lexer| {
            lexer.switch_and_return(LexerImplRule::InString, Token::StringStart)
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
            lexer.state().open(OpenCloseType::Interpolation);
            lexer.switch_and_return(LexerImplRule::Init, Token::InterpolationStart)
        },
        '"' => |lexer| {
            lexer.switch_and_return(LexerImplRule::Init, Token::StringEnd)
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
            .into_iter()
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

    #[test]
    fn test_number() {
        assert_lex(
            r#"2 12 1e3 1.5 .2 .3e-1"#,
            &[
                Token::Number(2.into()),
                Token::Number(12.into()),
                Token::Number(1000.into()),
                Token::Number(1.5.into()),
                Token::Number(0.2.into()),
                Token::Number(0.03.into()),
            ],
        );
    }
}
