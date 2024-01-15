use std::fmt::{Display, Formatter};

use derive_more::From;

use crate::Number;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier(pub String);

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    Plus,
    Minus,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    Arithmetic(BinaryArithmeticOp),
    Alt,
    And,
    Or,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UpdateOp {
    Arithmetic(BinaryArithmeticOp),
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
pub enum Suffix {
    /// `'?'`
    Optional,
    /// `'[' ']'`
    Iterate,
    /// `'.' <ident>`
    Index(Identifier),
    /// `'[' query ']' | '.' <string>`
    Query(Box<Query>),
    /// `'[' (<query>)? ':' (<query>)? ']' except '[' ':' ']'`
    Slice(Option<Box<Query>>, Option<Box<Query>>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StringFragment {
    String(String),
    Query(Query),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjectBindPatternEntry {
    /// `<variable>`
    KeyOnly(Identifier),
    /// `(<ident> | <keyword> | <string> | '(' <query> ')') ':' pattern`
    ValueOnly(Box<Query>, Box<BindPattern>),
    /// `<variable> ':' pattern`
    KeyAndValue(Identifier, Box<BindPattern>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BindPattern {
    /// `<variable>`
    Variable(Identifier),
    /// `'[' <pattern> (',' <pattern>)* ']'`
    Array(Vec<BindPattern>),
    /// `'{' <object pattern elem> (',' <object pattern elem>)* '}'`
    Object(Vec<ObjectBindPatternEntry>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FuncArg {
    Variable(Identifier),
    Closure(Identifier),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FuncDef {
    pub name: Identifier,
    pub args: Vec<FuncArg>,
    pub body: Box<Query>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term {
    /// `'null' | 'true' | 'false' | <number>`
    Constant(ConstantPrimitive),
    /// `<string>`
    String(Vec<StringFragment>),

    /// `'.'`
    Identity,
    /// `'..'`
    Recurse,
    /// ```text
    /// '.' '[' ( | <query> | (<query>)? ':' (<query>)? ) ']'
    /// '.' (<ident> | <string>)
    /// <term> '[' ( | <query> | (<query>)? ':' (<query>)? ) ']'
    /// <term> '.' '[' ( | <query> | (<query>)? ':' (<query>)? ) ']'
    /// <term> '?'
    /// <term> '.' (<ident> | <string>)
    /// ```
    Suffix(Box<Term>, Suffix),

    /// `(<var> | <modulevar>)`
    Variable(Identifier),
    /// `(<ident> | <moduleident>) ( '(' query (';' query)* ')' )? | (<var> | <modulevar>)`
    FunctionCall { name: Identifier, args: Vec<Query> },
    /// `'@' <ident-allowing-num-prefix> (<string>)?`
    Format(Identifier, Option<Vec<StringFragment>>),
    /// `'(' <query> ')'`
    Query(Box<Query>),
    /// `('+' | '-') <term>`
    Unary(UnaryOp, Box<Term>),
    /// `'{' (<ident> | <variable> | <keyword> | <string> | '(' <query> ')') (':' <term> ('|' <term>)*)? (',' ....)* ','? '}'`
    Object(Vec<(Query, Option<Query>)>),
    /// `'[' (<query>)? ']'`
    Array(Option<Box<Query>>),
    /// `'break' <variable>`
    Break(Identifier),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Query {
    /// `<term> | <query> '?'`
    Term(Box<Term>),
    /// `'def' <ident> ( '(' <ident> | <var> (';' <ident> | <var>)* ')' )? ':' <query> ';' <query>`
    WithFunc {
        function: FuncDef,
        query: Box<Query>,
    },
    /// `<query> ('|' <query>)+`
    Pipe { lhs: Box<Query>, rhs: Box<Query> },
    /// `<query> (',' <query>)+`
    Concat { lhs: Box<Query>, rhs: Box<Query> },
    /// `<term> 'as' <pattern> ('?//' <pattern>)* '|' <query>`
    Bind {
        source: Box<Term>,
        patterns: Vec<BindPattern>,
        body: Box<Query>,
    },
    /// `'reduce' <term> 'as' <pattern> '(' <query> ';' <query> ')'`
    Reduce {
        source: Box<Term>,
        pattern: BindPattern,
        initial: Box<Query>,
        accumulator: Box<Query>,
    },
    /// `'foreach' <term> 'as' <pattern> '(' <query> ';' <query> (';' <query>)? ')'`
    ForEach {
        source: Box<Term>,
        pattern: BindPattern,
        initial: Box<Query>,
        update: Box<Query>,
        extract: Option<Box<Query>>,
    },
    /// `'if' <query> 'then' <query> ('elif' <query> 'then' <query>)* ('else' <query>)? 'end'`
    If {
        cond: Box<Query>,
        positive: Box<Query>,
        negative: Option<Box<Query>>,
    },
    /// `'try' <query> ('catch' <query>)?`
    Try {
        body: Box<Query>,
        catch: Option<Box<Query>>,
    },
    /// `'label' <variable> '|' <query>`
    Label { label: Identifier, body: Box<Query> },

    /// `<query> ('//' | '+' | '-' | '*' | '/' | '%' | 'and' | 'or') <query>`
    Operate {
        lhs: Box<Query>,
        operator: BinaryOp,
        rhs: Box<Query>,
    },
    /// `<query> ('=' | '|=' | '//=' | '+=' | '-=' | '*=' | '/=' | '%=') <query>`
    Update {
        lhs: Box<Query>,
        operator: UpdateOp,
        rhs: Box<Query>,
    },
    /// `<query> <comparator> <query>`
    Compare {
        lhs: Box<Query>,
        comparator: Comparator,
        rhs: Box<Query>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ConstantPrimitive {
    Null,
    False,
    True,
    Number(Number),
    String(String),
}

#[derive(Debug, Clone, Eq, PartialEq, From)]
pub enum ConstantValue {
    Primitive(ConstantPrimitive),
    Array(ConstantArray),
    Object(ConstantObject),
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct ConstantArray(pub Vec<ConstantValue>);

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct ConstantObject(pub Vec<(String, ConstantValue)>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Import {
    pub path: String,
    pub alias: Option<Identifier>,
    pub meta: Option<ConstantObject>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Program {
    pub module_header: Option<ConstantObject>,
    pub imports: Vec<Import>,
    pub functions: Vec<FuncDef>,
    pub query: Query,
}

impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<Term> for Query {
    fn from(term: Term) -> Self {
        if let Term::Query(query) = term {
            *query
        } else {
            Query::Term(Box::new(term))
        }
    }
}

impl From<Query> for Term {
    fn from(query: Query) -> Self {
        if let Query::Term(term) = query {
            *term
        } else {
            Term::Query(Box::new(query))
        }
    }
}
