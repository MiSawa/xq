use crate::{
    ast::{FuncDef, Identifier, Program, Query, StringFragment, Suffix, Term},
    Number,
};
use num::ToPrimitive;
use serde::{
    de::{MapAccess, SeqAccess, Visitor},
    ser::{SerializeMap, SerializeSeq},
    Deserialize, Deserializer, Serialize, Serializer,
};
use std::{borrow::Borrow, collections::HashMap, fmt::Formatter, rc::Rc};

#[derive(Clone, Debug)]
pub enum Json {
    Null,
    True,
    False,
    Number(Number),
    String(String),
    Array(im_rc::Vector<Rc<Json>>),
    Object(im_rc::HashMap<Rc<String>, Rc<Json>>),
    Err,
}

impl Serialize for Json {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Json::Null => serializer.serialize_none(),
            Json::True => serializer.serialize_bool(true),
            Json::False => serializer.serialize_bool(false),
            Json::Number(Number::Real(v)) => serializer.serialize_f64(*v),
            Json::Number(Number::Integer(v)) => serializer.serialize_i64(*v),
            Json::String(s) => serializer.serialize_str(s),
            Json::Array(v) => {
                let mut seq = serializer.serialize_seq(Some(v.len()))?;
                for e in v {
                    seq.serialize_element::<Json>(e.borrow())?;
                }
                seq.end()
            }
            Json::Object(values) => {
                let mut map = serializer.serialize_map(Some(values.len()))?;
                for (k, v) in values {
                    map.serialize_entry::<String, Json>(k.borrow(), v.borrow())?;
                }
                map.end()
            }
            Json::Err => todo!(),
        }
    }
}

impl<'de> Deserialize<'de> for Json {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct V;
        impl<'de> Visitor<'de> for V {
            type Value = Json;

            fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
                write!(
                    formatter,
                    "null, boolean, number, string, array, or map keyed with string"
                )
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v {
                    Ok(Json::True)
                } else {
                    Ok(Json::False)
                }
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Json::Number(Number::Integer(v)))
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if let Some(v) = v.to_i64() {
                    Ok(Json::Number(Number::Integer(v)))
                } else {
                    Ok(Json::Number(Number::Real(v.to_f64().unwrap())))
                }
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Json::Number(Number::Real(v)))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Json::String(v.to_string()))
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Json::String(v))
            }

            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(Json::Null)
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: SeqAccess<'de>,
            {
                let mut v = if let Some(n) = seq.size_hint() {
                    Vec::with_capacity(n)
                } else {
                    vec![]
                };
                while let Some(elem) = seq.next_element::<Json>()? {
                    v.push(Rc::new(elem));
                }
                Ok(Json::Array(im_rc::Vector::from(v)))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'de>,
            {
                let mut m = if let Some(n) = map.size_hint() {
                    HashMap::with_capacity(n)
                } else {
                    HashMap::new()
                };
                while let Some((key, value)) = map.next_entry::<String, Json>()? {
                    m.insert(Rc::new(key), Rc::new(value));
                }
                Ok(Json::Object(im_rc::HashMap::from(m)))
            }
        }
        deserializer.deserialize_any(V)
    }
}

#[derive(Clone, Debug)]
pub enum Index {
    Array(usize),
    Object(Rc<String>),
}

#[derive(Clone, Default, Debug)]
pub struct Env {
    pub root_object: Option<Rc<Json>>,
    pub current_object: Option<Rc<Json>>,
    pub path: im_rc::Vector<Index>,
    pub functions: im_rc::HashMap<Identifier, Rc<FuncDef>>,
}

impl Env {
    pub fn object_changed(&self, json: &Rc<Json>) -> Self {
        Env {
            root_object: Some(json.clone()),
            current_object: Some(json.clone()),
            path: im_rc::Vector::default(),
            functions: self.functions.clone(),
        }
    }

    fn indexed(&self, index: Index) -> Self {
        let mut new_path = self.path.clone();
        new_path.push_back(index.clone());
        if let Some(obj) = self.current_object.clone() {
            let new_obj = match index {
                Index::Array(i) => match obj.borrow() {
                    Json::Array(arr) => arr.get(i).cloned(),
                    _ => None,
                },
                Index::Object(key) => match obj.borrow() {
                    Json::Object(map) => map.get(&*key).cloned(),
                    _ => None,
                },
            };
            Env {
                path: new_path,
                current_object: new_obj,
                ..self.clone()
            }
        } else {
            Env {
                path: new_path,
                ..self.clone()
            }
        }
    }

    fn func_defined(&self, func: &FuncDef) -> Self {
        let mut new_functions = self.functions.clone();
        new_functions.insert(func.name.clone(), Rc::new(func.clone()));
        Env {
            functions: new_functions,
            ..self.clone()
        }
    }
}

pub trait Consumer {
    fn consume(&mut self, env: &Env);
}

impl<F: FnMut(&Env)> Consumer for F {
    fn consume(&mut self, env: &Env) {
        self(env)
    }
}
impl<'a> Consumer for Box<&mut (dyn Consumer + 'a)> {
    fn consume(&mut self, env: &Env) {
        (**self).consume(env)
    }
}

fn run_term(env: &Env, term: &Term, consumer: &mut dyn Consumer) {
    match term {
        Term::Null => {
            consumer.consume(&env.object_changed(&Rc::new(Json::Null)));
        }
        Term::True => {
            consumer.consume(&env.object_changed(&Rc::new(Json::True)));
        }
        Term::False => {
            consumer.consume(&env.object_changed(&Rc::new(Json::False)));
        }
        Term::Number(v) => {
            consumer.consume(&env.object_changed(&Rc::new(Json::Number(*v))));
        }
        Term::String(s) => {
            struct ConcatenatedStr<'a, C>(&'a String, &'a mut C);
            impl<'a, C: Consumer> Consumer for ConcatenatedStr<'a, C> {
                fn consume(&mut self, env: &Env) {
                    if let Some(obj) = &env.current_object {
                        match obj.borrow() {
                            Json::String(s) => self.1.consume(
                                &env.object_changed(&Rc::new(Json::String(s.to_string() + self.0))),
                            ),
                            _ => todo!(),
                        }
                    }
                }
            }
            let mut v = s.clone();
            if let Some(fragment) = v.pop() {
                match fragment {
                    StringFragment::String(s) => run_term(
                        env,
                        &Term::String(v),
                        &mut ConcatenatedStr(&s, &mut Box::new(consumer)),
                    ),
                    StringFragment::Query(q) => {
                        struct ConcatenatedQuery<'a, C>(&'a Vec<StringFragment>, &'a mut C);
                        impl<'a, C: Consumer> Consumer for ConcatenatedQuery<'a, C> {
                            fn consume(&mut self, env: &Env) {
                                if let Some(obj) = &env.current_object {
                                    match obj.borrow() {
                                        Json::String(s) => {
                                            let mut new_consumer = ConcatenatedStr(s, self.1);
                                            run_term(
                                                env,
                                                &Term::String(self.0.clone()),
                                                &mut new_consumer,
                                            )
                                        }
                                        _ => todo!(),
                                    }
                                }
                            }
                        }
                        let mut c = Box::new(consumer);
                        let mut new_consumer = ConcatenatedQuery(&v, &mut c);
                        run_query(env, &q, &mut new_consumer)
                    }
                }
            } else {
                consumer.consume(&env.object_changed(&Rc::new(Json::String("".to_string()))))
            }
        }
        Term::Identity => consumer.consume(env),
        Term::Recurse => {
            if let Some(obj) = &env.current_object {
                consumer.consume(env);
                match obj.borrow() {
                    Json::Array(v) => {
                        for i in 0..v.len() {
                            run_term(&env.indexed(Index::Array(i)), term, consumer)
                        }
                    }
                    Json::Object(map) => {
                        for key in map.keys() {
                            run_term(&env.indexed(Index::Object(key.clone())), term, consumer)
                        }
                    }
                    _ => {}
                }
            }
        }
        Term::Suffix(term, suffixes) => {
            struct S<'a, C>(&'a [Suffix], &'a mut C);
            impl<'a, C: Consumer> Consumer for S<'a, C> {
                fn consume(&mut self, env: &Env) {
                    suffix(env, self.0, self.1)
                }
            }
            fn suffix(env: &Env, suffixes: &[Suffix], consumer: &mut dyn Consumer) {
                if let Some((last, other)) = suffixes.split_last() {
                    match last {
                        Suffix::Optional => {
                            todo!()
                        }
                        Suffix::Explode => suffix(env, other, &mut |e: &Env| {
                            if let Some(v) = &e.current_object {
                                match v.borrow() {
                                    Json::Array(v) => {
                                        for i in 0..v.len() {
                                            consumer.consume(&e.indexed(Index::Array(i)))
                                        }
                                    }
                                    Json::Object(map) => {
                                        for key in map.keys() {
                                            consumer.consume(&e.indexed(Index::Object(key.clone())))
                                        }
                                    }
                                    _ => todo!(),
                                }
                            }
                        }),
                        Suffix::Index(ident) => {
                            suffix(env, other, &mut |e: &Env| {
                                consumer
                                    .consume(&e.indexed(Index::Object(Rc::new(ident.0.clone()))))
                            });
                        }
                        Suffix::Query(query) => {
                            run_query(env, query, &mut |e: &Env| {
                                if let Some(obj) = &e.current_object {
                                    match obj.borrow() {
                                        Json::Number(v) => {
                                            suffix(env, other, &mut |e: &Env| {
                                                let i = match v {
                                                    Number::Integer(v) => v.to_usize(),
                                                    Number::Real(v) => v.round().to_usize(),
                                                };
                                                if let Some(i) = i {
                                                    consumer.consume(&e.indexed(Index::Array(i)))
                                                }
                                            });
                                        }
                                        Json::String(s) => {
                                            suffix(env, other, &mut |e: &Env| {
                                                consumer.consume(
                                                    &e.indexed(Index::Object(Rc::new(s.clone()))),
                                                )
                                            });
                                        }
                                        _ => todo!(),
                                    }
                                }
                            });
                        }
                        Suffix::Slice(_, _) => todo!(),
                    }
                } else {
                    consumer.consume(env)
                }
            }
            run_term(env, term, &mut S(suffixes, &mut Box::new(consumer)))
        }
        Term::FunctionCall { .. } => {}
        Term::Format(_) => {}
        Term::Query(q) => run_query(env, q, consumer),
        Term::Unary(_, _) => {}
        Term::Object(_) => {}
        Term::Array(_) => {}
        Term::Break(_) => {}
    }
}

fn run_query(env: &Env, query: &Query, consumer: &mut dyn Consumer) {
    match query {
        Query::Term(term) => run_term(env, term, consumer),
        Query::WithFunc { function, query } => {
            run_query(&env.func_defined(function), query, consumer)
        }
        Query::Pipe { lhs, rhs } => {
            struct Piped<'a, C>(&'a Query, &'a mut C);
            impl<'a, C: Consumer> Consumer for Piped<'a, C> {
                fn consume(&mut self, env: &Env) {
                    run_query(env, self.0, self.1)
                }
            }
            let mut c = Box::new(consumer);
            run_query(env, lhs, &mut Piped(rhs, &mut c))
        }
        Query::Concat { lhs, rhs } => {
            run_query(env, lhs, consumer);
            run_query(env, rhs, consumer);
        }
        Query::Bind { .. } => {
            unimplemented!()
        }
        Query::Reduce { .. } => {
            unimplemented!()
        }
        Query::ForEach { .. } => {
            unimplemented!()
        }
        Query::If { .. } => {
            unimplemented!()
        }
        Query::Try { .. } => {
            unimplemented!()
        }
        Query::Label { .. } => {
            unimplemented!()
        }
        Query::Operate { .. } => {
            unimplemented!()
        }
        Query::Update { .. } => {
            unimplemented!()
        }
        Query::Compare { .. } => {
            unimplemented!()
        }
    }
}

pub fn run_with_env<C: Consumer>(env: &Env, ast: &Program, consumer: &mut C) {
    run_query(env, &ast.query, consumer)
}

pub fn run<C: Consumer>(ast: &Program, consumer: &mut C) {
    run_with_env(&Env::default(), ast, consumer)
}