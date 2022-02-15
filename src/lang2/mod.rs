pub mod ast;
#[allow(clippy::manual_range_contains)]
pub mod lexer;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(#[allow(clippy::all, unused_imports)]pub parser, "/lang2/jq.rs");

pub type ParseError<'a> =
    lalrpop_util::ParseError<lexgen_util::Loc, lexer::Token<'a>, lexer::LexerError>;
pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

#[cfg(test)]
mod test {
    use super::{ast::Query, lexer::Lexer, parser::QueryParser, ParseResult};

    // use lalrpop_util::lalrpop_mod;
    // lalrpop_mod!(#[allow(unused_imports)] pub tmp, "/lang2/tmp.rs");
    // #[test]
    // fn test_tmp() {
    //     fn try_parse_query(q: &str) -> Option<String> {
    //         tmp::QueryParser::new().parse(q).ok()
    //     }
    //     fn parse_query(q: &str) -> String {
    //         println!("{q}");
    //         tmp::QueryParser::new()
    //             .parse(q)
    //             .unwrap()
    //             .replace("tryo", "try")
    //             .replace("tryc", "try")
    //             .replace("catcho", "catch")
    //             .replace("catchc", "catch")
    //             .replace("|o", "|")
    //             .replace("|c", "|")
    //     }
    //     assert_eq!(parse_query("try . | ."), parse_query("(try .) | ."));
    //     assert_eq!(parse_query("try try ."), parse_query("try (try .)"));
    //     assert_eq!(
    //         parse_query("try . catch try ."),
    //         parse_query("try . catch (try .)")
    //     );
    //     assert_eq!(
    //         parse_query("try . catch try . catch ."),
    //         parse_query("try . catch (try . catch .)")
    //     );
    //     assert_eq!(
    //         parse_query("try try try . catch try . catch . catch ."),
    //         parse_query("try (try (try . catch (try . catch .)) catch .)")
    //     );
    //     assert_eq!(
    //         parse_query("try . catch . | ."),
    //         parse_query("(try . catch .) | .")
    //     );
    //     assert!(try_parse_query("try try . | . catch .").is_none());
    //     assert!(try_parse_query("try try . | . catch . | .").is_none());
    //     assert_eq!(
    //         parse_query("try . catch try . | ."),
    //         parse_query("(try . catch (try .)) | .")
    //     );
    // }

    fn parse_query(q: &str) -> ParseResult<Query> {
        println!("{q}");
        let lexer = Lexer::new(q);
        QueryParser::new().parse(q, lexer)
    }

    #[test]
    fn test_try() -> ParseResult<'static, ()> {
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
    fn test_precedences() -> ParseResult<'static, ()> {
        assert_eq!(
            parse_query(". | . | .")?,
            parse_query(". | (. | .)")?,
        );
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
        Ok(())
    }
}
