use crate::Number;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier(pub String);

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
pub enum Suffix {
    /// `'?'`
    Optional,
    /// `'[' ']'`
    Explode,
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
    /// `(<ident> | <variable> | <keyword> | <string> | '(' <query> ')') ':' pattern`
    KeyValue(Box<Query>, Box<BindPattern>),
    /// `<variable>`
    KeyOnly(Identifier),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BindPattern {
    /// `<variable>`
    Variable(Identifier),
    /// `'[' <patten> (',' <pattern>)* ']'`
    Array(Vec<BindPattern>),
    /// `'{' <object pattern elem> (',' <object pattern elem>)* '}'`
    Object(Vec<ObjectBindPatternEntry>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FuncDef {
    pub name: Identifier,
    pub args: Vec<Identifier>,
    pub body: Box<Query>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term {
    /// `'null'`
    Null,
    /// `'true'`
    True,
    /// `'false'`
    False,
    /// `<number>`
    Number(Number),
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
    Suffix(Box<Term>, Vec<Suffix>),

    /// `(<ident> | <moduleident>) ( '(' query (';' query)* ')' )? | (<var> | <modulevar>)`
    FunctionCall { name: Identifier, args: Vec<Query> },
    /// `'@' <ident-allowing-num-prefix> (<string>)?`
    Format(Identifier),
    /// `'(' <query> ')'`
    Query(Box<Query>),
    /// `('+' | '-') <term>`
    Unary(UnaryOp, Box<Term>),
    /// `'{' (<ident> | <variable> | <keyword> | <string> | '(' <query> ')') (':' <term> ('|' <term>)*)? (',' ....)* '}'`
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
    /// `'def' <ident> ':' <query> ';' <query>`
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
        operator: Comparator,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ConstantJson {
    Primitive(ConstantPrimitive),
    Array(ConstantArray),
    Object(ConstantObject),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstantArray(pub Vec<ConstantJson>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstantObject(pub Vec<(String, ConstantJson)>);

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
