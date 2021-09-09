use crate::Number;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Identifier<'a>(pub &'a str);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ModuleIdent<'a>(pub &'a str);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    Plus,
    Minus,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Alt,
    And,
    Or,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UpdateOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Alt,
    Modify,
    Assign,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Comparator {
    Eq,
    Neq,
    Gt,
    Ge,
    Lt,
    Le,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Index<'a> {
    /// '[' ']'
    Explode,
    /// '.' <ident> | '.' <string>
    Index(&'a str),
    /// '[' query ']'
    Query(Box<Query<'a>>),
    /// '[' (<query>)? ':' (<query>)? ']' except '[' ':' ']'
    Slice(Option<Box<Query<'a>>>, Option<Box<Query<'a>>>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StringFragment<'a> {
    String(&'a str),
    Char(char),
    Query(Query<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StringWithInterpolation<'a>(pub Vec<StringFragment<'a>>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term<'a> {
    /// 'null'
    Null,
    /// 'true'
    True,
    /// 'false'
    False,
    /// <number>
    Number(Number),
    /// <string>
    String(Vec<StringFragment<'a>>),

    /// '.'
    Identity,
    /// '..'
    Recurse,
    /// '.' '[' ( | <query> | (<query>)? ':' (<query>)? ) ']'
    /// '.' (<ident> | <string>)
    /// <term> '[' ( | <query> | (<query>)? ':' (<query>)? ) ']'
    /// <term> '.' '[' ( | <query> | (<query>)? ':' (<query>)? ) ']'
    /// <term> '?'
    /// <term> '.' (<ident> | <string>)
    Index(Box<Term<'a>>, Vec<Index<'a>>),

    /// (<ident> | <moduleident>) ( '(' query (';' query)* ')' )? | (<var> | <modulevar>)
    FunctionCall(Identifier<'a>, Vec<Query<'a>>),
    /// '@' <ident-allowing-num-prefix> (<string>)?
    Format(Identifier<'a>),
    /// '(' <query> ')'
    Query(Box<Query<'a>>),
    /// ('+' | '-') <term>
    Unary(UnaryOp, Box<Term<'a>>),
    /// '{' (<ident> | <variable> | <keyword> | <string> | '(' <query> ')') (':' <term> ('|' <term>)*)? (',' ....)* '}'
    Object(Vec<(Query<'a>, Option<Query<'a>>)>),
    /// '[' (<query>)? ']'
    Array(Option<Box<Query<'a>>>),
    /// 'break' <variable>
    Break(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Query<'a> {
    /// <term>
    Term(Box<Term<'a>>),
    /// 'def' <ident> ':' <query> ';' <query>
    WithFunc(Box<Query<'a>>),
    /// <query> ('|' <query>)+
    Pipe(Vec<Query<'a>>),
    /// <query> (',' <query>)+
    Concat(Vec<Query<'a>>),
    /// <term> 'as' <pattern> ('?//' <pattern>)* '|' <query>
    Bind(Box<Query<'a>>),
    /// 'reduce' <term> 'as' <pattern> '(' <query> ';' <query> ')'
    Reduce(),
    /// 'foreach' <term> 'as' <pattern> '(' <query> ';' <query> (';' <query>)? ')'
    ForEach(),
    /// 'if' <query> 'then' <query> ('elif' <query> 'then' <query>)* ('else' <query>)? 'end'
    If(),
    /// 'try' <query> ('catch' <query>)?
    Try(),
    /// 'label' <variable> '|' <query>
    Label(),
    /// <query> '?'
    Optional(),

    /// <query> ('//' | '+' | '-' | '*' | '/' | '%' | 'and' | 'or') <query>
    Operate(Box<Query<'a>>, BinaryOp, Box<Query<'a>>),
    /// <query> ('=' | '|=' | '//=' | '+=' | '-=' | '*=' | '/=' | '%=') <query>
    Update(Box<Query<'a>>, UpdateOp, Box<Query<'a>>),
    /// <query> <comparator> <query>
    Compare(Box<Query<'a>>, Comparator, Box<Query<'a>>),
}
