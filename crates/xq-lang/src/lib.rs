pub mod ast;
#[allow(clippy::manual_range_contains)]
pub mod lexer;

use lalrpop_util::lalrpop_mod;
use ordered_float::OrderedFloat;
use thiserror::Error;

lalrpop_mod!(#[allow(clippy::all, unused_imports)] pub parser, "/jq.rs");

pub(crate) type Number = OrderedFloat<f64>;
pub type ParseResult<T> = Result<T, ParseError>;
type Loc = lexer::Loc;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Got an invalid token at {0:?}")]
    InvalidToken(Loc),
    #[error("Got an EOF at {0:?}, but expected one of {1:?}")]
    UnrecognizedEOF(Loc, Vec<String>),
    #[error("Got a token {1} at {0:?}, but expected one of {2:?}")]
    UnrecognizedToken(Loc, String, Vec<String>),
    #[error("Got an unexpected additional token {0}")]
    ExtraToken(String),
    #[error("{0}")]
    LexerError(String),
}
impl<'a> From<lalrpop_util::ParseError<lexgen_util::Loc, lexer::Token<'a>, lexer::LexerError>>
    for ParseError
{
    fn from(
        e: lalrpop_util::ParseError<lexgen_util::Loc, lexer::Token<'a>, lexer::LexerError>,
    ) -> Self {
        match e {
            lalrpop_util::ParseError::InvalidToken { location } => Self::InvalidToken(location),
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                Self::UnrecognizedEOF(location, expected)
            }
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (loc, token, _),
                expected,
            } => Self::UnrecognizedToken(loc, format!("{token:?}"), expected),
            lalrpop_util::ParseError::ExtraToken { token } => {
                Self::ExtraToken(format!("{token:?}"))
            }
            lalrpop_util::ParseError::User { error } => Self::LexerError(format!("{error:?}")),
        }
    }
}

pub fn parse_program(input: &str) -> ParseResult<ast::Program> {
    let lexer = lexer::Lexer::new(input);
    parser::ProgramParser::new()
        .parse(input, lexer.into_iter())
        .map_err(|e| e.into())
}

#[cfg(test)]
mod test {
    use super::{ast, lexer, parser, ParseResult};

    fn parse_query(q: &str) -> ParseResult<ast::Query> {
        let lexer = lexer::Lexer::new(q);
        parser::QueryParser::new()
            .parse(q, lexer.into_iter())
            .map_err(|e| e.into())
    }

    #[test]
    fn test_try() -> ParseResult<()> {
        assert_eq!(parse_query("try . | .")?, parse_query("(try .) | .")?);
        assert_eq!(parse_query("try try .")?, parse_query("try (try .)")?);
        assert_eq!(
            parse_query("try . catch try .")?,
            parse_query("try . catch (try .)")?
        );
        assert_eq!(
            parse_query("try . catch try . catch .")?,
            parse_query("try . catch (try . catch .)")?
        );
        assert_eq!(
            parse_query("try try try . catch try . catch . catch .")?,
            parse_query("try (try (try . catch (try . catch .)) catch .)")?
        );
        assert_eq!(
            parse_query("try . catch . | .")?,
            parse_query("(try . catch .) | .")?
        );
        assert!(parse_query("try try . | . catch .").is_err());
        assert!(parse_query("try try . | . catch . | .").is_err());
        assert_eq!(
            parse_query("try . catch try . | .")?,
            parse_query("(try . catch (try .)) | .")?
        );
        Ok(())
    }

    #[test]
    fn test_precedences() -> ParseResult<()> {
        assert_eq!(parse_query(". | . | .")?, parse_query(". | (. | .)")?,);
        assert_eq!(parse_query("try . | .")?, parse_query("(try .) | .")?);
        assert_eq!(
            parse_query("def f: .; . | .")?,
            parse_query("def f: .; (. | .)")?
        );
        assert_eq!(
            parse_query(". as $a | . | .")?,
            parse_query(". as $a | (. | .)")?,
        );
        assert_eq!(
            parse_query(". as $a | . | .")?,
            parse_query(". as $a | (. | .)")?,
        );
        assert_eq!(
            parse_query("reduce . as $a (.; .) | . | .")?,
            parse_query("(reduce . as $a (.; .)) | (. | .)")?,
        );
        assert_eq!(
            parse_query(". + reduce . as $a (.; .) | . | .")?,
            parse_query("(. + (reduce . as $a (.; .))) | (. | .)")?,
        );
        assert_eq!(
            parse_query(". + def f:.; f + f")?,
            parse_query(". + (def f:.; (f + f))")?,
        );
        assert_eq!(
            parse_query(". * def f:.; f + f")?,
            parse_query(". * (def f:.; (f + f))")?,
        );
        // except rhs of pipe
        assert_eq!(
            parse_query(". | def f:.; f + f")?,
            parse_query(". | (def f:.; (f + f))")?,
        );
        assert_eq!(
            parse_query(". | def f:.; f | f | . + .")?,
            parse_query(". | (def f:.; (f | (f | (.+.))))")?,
        );
        assert_eq!(
            parse_query(". * . as $a | .")?,
            parse_query(". * (. as $a | .)")?,
        );
        assert_eq!(
            parse_query(". * . as $a | .")?,
            parse_query(". * (. as $a | .)")?,
        );
        assert_eq!(
            parse_query("[1, 2 as $x | $x + 1, $x + 2]")?,
            parse_query("[1, (2 as $x | ($x + 1, $x + 2))]")?,
        );
        Ok(())
    }

    #[test]
    fn test_query_opt() -> ParseResult<()> {
        assert_eq!(
            parse_query("foreach . as $a (.; .)?")?,
            parse_query("(foreach . as $a (.; .))?")?
        );
        assert_eq!(
            parse_query("try foreach . as $a (.; .)?")?,
            parse_query("try ((foreach . as $a (.; .))?)")?
        );
        parse_query(".a??")?;
        parse_query("foreach . as $a (.; .)??")?;
        Ok(())
    }

    #[test]
    fn test_other() -> ParseResult<()> {
        parse_query("2")?;
        parse_query(".[]")?;
        parse_query("f(.; .)")?;
        parse_query("f(.; .)[]")?;
        parse_query("{}")?;
        parse_query("{a: 1}")?;
        parse_query("{a: 1,}")?;
        assert!(parse_query("{,}").is_err());
        Ok(())
    }
}
