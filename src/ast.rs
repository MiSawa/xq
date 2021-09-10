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
pub enum Suffix<'a> {
    /// `'?'`
    Optional,
    /// `'[' ']'`
    Explode,
    /// `'.' <ident>`
    Index(Identifier<'a>),
    /// `'[' query ']' | '.' <string>`
    Query(Box<Query<'a>>),
    /// `'[' (<query>)? ':' (<query>)? ']' except '[' ':' ']'`
    Slice(Option<Box<Query<'a>>>, Option<Box<Query<'a>>>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StringFragment<'a> {
    String(&'a str),
    Char(char),
    Query(Query<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjectBindPatternEntry<'a> {
    /// `(<ident> | <variable> | <keyword> | <string> | '(' <query> ')') ':' pattern`
    KeyValue(Box<Query<'a>>, Box<BindPattern<'a>>),
    /// `<variable>`
    KeyOnly(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BindPattern<'a> {
    /// `<variable>`
    Variable(Identifier<'a>),
    /// `'[' <patten> (',' <pattern>)* ']'`
    Array(Vec<BindPattern<'a>>),
    /// `'{' <object pattern elem> (',' <object pattern elem>)* '}'`
    Object(Vec<ObjectBindPatternEntry<'a>>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FuncDef<'a> {
    pub name: Identifier<'a>,
    pub args: Vec<Identifier<'a>>,
    pub body: Box<Query<'a>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term<'a> {
    /// `'null'`
    Null,
    /// `'true'`
    True,
    /// `'false'`
    False,
    /// `<number>`
    Number(Number),
    /// `<string>`
    String(Vec<StringFragment<'a>>),

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
    Suffix(Box<Term<'a>>, Vec<Suffix<'a>>),

    /// `(<ident> | <moduleident>) ( '(' query (';' query)* ')' )? | (<var> | <modulevar>)`
    FunctionCall {
        name: Identifier<'a>,
        args: Vec<Query<'a>>,
    },
    /// `'@' <ident-allowing-num-prefix> (<string>)?`
    Format(Identifier<'a>),
    /// `'(' <query> ')'`
    Query(Box<Query<'a>>),
    /// `('+' | '-') <term>`
    Unary(UnaryOp, Box<Term<'a>>),
    /// `'{' (<ident> | <variable> | <keyword> | <string> | '(' <query> ')') (':' <term> ('|' <term>)*)? (',' ....)* '}'`
    Object(Vec<(Query<'a>, Option<Query<'a>>)>),
    /// `'[' (<query>)? ']'`
    Array(Option<Box<Query<'a>>>),
    /// `'break' <variable>`
    Break(Identifier<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Query<'a> {
    /// `<term> | <query> '?'`
    Term(Box<Term<'a>>),
    /// `'def' <ident> ':' <query> ';' <query>`
    WithFunc {
        function: FuncDef<'a>,
        query: Box<Query<'a>>,
    },
    /// `<query> ('|' <query>)+`
    Pipe {
        lhs: Box<Query<'a>>,
        rhs: Box<Query<'a>>,
    },
    /// `<query> (',' <query>)+`
    Concat {
        lhs: Box<Query<'a>>,
        rhs: Box<Query<'a>>,
    },
    /// `<term> 'as' <pattern> ('?//' <pattern>)* '|' <query>`
    Bind {
        source: Box<Term<'a>>,
        patterns: Vec<BindPattern<'a>>,
        body: Box<Query<'a>>,
    },
    /// `'reduce' <term> 'as' <pattern> '(' <query> ';' <query> ')'`
    Reduce {
        source: Box<Term<'a>>,
        pattern: BindPattern<'a>,
        initial: Box<Query<'a>>,
        accumulator: Box<Query<'a>>,
    },
    /// `'foreach' <term> 'as' <pattern> '(' <query> ';' <query> (';' <query>)? ')'`
    ForEach {
        source: Box<Term<'a>>,
        pattern: BindPattern<'a>,
        initial: Box<Query<'a>>,
        update: Box<Query<'a>>,
        extract: Option<Box<Query<'a>>>,
    },
    /// `'if' <query> 'then' <query> ('elif' <query> 'then' <query>)* ('else' <query>)? 'end'`
    If {
        cond: Box<Query<'a>>,
        positive: Box<Query<'a>>,
        negative: Option<Box<Query<'a>>>,
    },
    /// `'try' <query> ('catch' <query>)?`
    Try {
        body: Box<Query<'a>>,
        catch: Option<Box<Query<'a>>>,
    },
    /// `'label' <variable> '|' <query>`
    Label {
        label: Identifier<'a>,
        body: Box<Query<'a>>,
    },

    /// `<query> ('//' | '+' | '-' | '*' | '/' | '%' | 'and' | 'or') <query>`
    Operate {
        lhs: Box<Query<'a>>,
        operator: BinaryOp,
        rhs: Box<Query<'a>>,
    },
    /// `<query> ('=' | '|=' | '//=' | '+=' | '-=' | '*=' | '/=' | '%=') <query>`
    Update {
        lhs: Box<Query<'a>>,
        operator: UpdateOp,
        rhs: Box<Query<'a>>,
    },
    /// `<query> <comparator> <query>`
    Compare {
        lhs: Box<Query<'a>>,
        operator: Comparator,
        rhs: Box<Query<'a>>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum ConstantPrimitive {
    Null,
    False,
    True,
    Number(Number),
    String(String),
}
#[derive(Debug, Clone, Eq, PartialEq)]
enum ConstantJson {
    Primitive(ConstantPrimitive),
    Array(ConstantArray),
    Object(ConstantObject),
}
#[derive(Debug, Clone, Eq, PartialEq)]
struct ConstantArray(Vec<ConstantJson>);
#[derive(Debug, Clone, Eq, PartialEq)]
struct ConstantObject(Vec<(String, ConstantJson)>);

#[derive(Debug, Clone, Eq, PartialEq)]
struct Import<'a> {
    path: String,
    alias: Identifier<'a>,
    meta: ConstantObject,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Program<'a> {
    module_header: Option<ConstantObject>,
    imports: Vec<Import<'a>>,
    functions: Vec<FuncDef<'a>>,
    query: Query<'a>,
}

impl<'a> Into<Term<'a>> for Query<'a> {
    fn into(self) -> Term<'a> {
        if let Query::Term(term) = self {
            *term
        } else {
            Term::Query(Box::new(self))
        }
    }
}

impl<'a> Into<Query<'a>> for Term<'a> {
    fn into(self) -> Query<'a> {
        if let Term::Query(query) = self {
            *query
        } else {
            Query::Term(Box::new(self))
        }
    }
}
