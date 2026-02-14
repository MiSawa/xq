use std::rc::Rc;

use onig::{Regex, RegexOptions, Syntax};

use crate::{
    vm::{QueryExecutionError, Result},
    Array, Object, Value,
};

fn to_string(func: &'static str, v: Value) -> Result<Rc<String>> {
    match v {
        Value::String(s) => Ok(s),
        _ => Err(QueryExecutionError::InvalidArgType(func, v)),
    }
}

fn compile_regex(pattern: &str, flags: &str) -> Result<(Regex, bool)> {
    let mut opt = RegexOptions::REGEX_OPTION_CAPTURE_GROUP;
    let mut global = false;
    for c in flags.chars() {
        match c {
            'g' => global = true,
            'i' => opt |= RegexOptions::REGEX_OPTION_IGNORECASE,
            'm' => opt |= RegexOptions::REGEX_OPTION_MULTILINE,
            'n' => opt |= RegexOptions::REGEX_OPTION_FIND_NOT_EMPTY,
            'p' => {
                opt |= RegexOptions::REGEX_OPTION_SINGLELINE | RegexOptions::REGEX_OPTION_MULTILINE
            }
            's' => opt |= RegexOptions::REGEX_OPTION_SINGLELINE,
            'l' => opt |= RegexOptions::REGEX_OPTION_FIND_LONGEST,
            'x' => opt |= RegexOptions::REGEX_OPTION_EXTEND,
            _ => return Err(QueryExecutionError::InvalidRegexFlag(c)),
        }
    }
    Ok((
        Regex::with_options(pattern, opt, Syntax::default())?,
        global,
    ))
}

/// Returns an array consists of interleaving results of "splits" and "match".
/// string between matches, match info, string between matches, match info, ..., string between matches
pub(crate) fn split_match_impl(context: Value, pattern: Value, flags: Value) -> Result<Value> {
    let s = to_string("match", context)?;
    let pattern = to_string("match", pattern)?;
    let flags = if flags == Value::Null {
        Rc::new("".into())
    } else {
        to_string("match", flags)?
    };
    let (regex, global) = compile_regex(&pattern, &flags)?;

    let mut b2c: Vec<usize> = std::iter::repeat_n(0, s.len() + 1).collect();
    for i in 0..s.len() {
        if s.is_char_boundary(i) {
            b2c[i + 1] = b2c[i] + 1;
        } else {
            b2c[i + 1] = b2c[i]
        }
    }
    let mut capture_names = vec![Value::Null; regex.captures_len() + 1]; // +1 for the whole match
    regex.foreach_name(|name, groups| {
        let name = Rc::new(name.to_owned());
        for group in groups {
            capture_names[*group as usize] = Value::String(name.clone());
        }
        true
    });
    let mut v = vec![];
    let mut last_end = 0;
    for c in regex
        .captures_iter(&s)
        .take(if global { usize::MAX } else { 1 })
    {
        let (start, end) = c.pos(0).unwrap();
        v.push(Value::string(s[last_end..start].into()));
        last_end = end;

        let captures: Array = c
            .iter_pos()
            .enumerate()
            .skip(1)
            .map(|(i, g)| {
                let entries: Object = if let Some((start, end)) = g {
                    [
                        (Rc::new("offset".into()), Value::number(b2c[start])),
                        (
                            Rc::new("length".into()),
                            Value::number(b2c[end] - b2c[start]),
                        ),
                        (
                            Rc::new("string".into()),
                            Value::string(s[start..end].into()),
                        ),
                        (Rc::new("name".into()), capture_names[i].clone()),
                    ]
                } else {
                    [
                        (Rc::new("offset".into()), Value::number(-1)),
                        (Rc::new("length".into()), Value::number(0)),
                        (Rc::new("string".into()), Value::Null),
                        (Rc::new("name".into()), capture_names[i].clone()),
                    ]
                }
                .into_iter()
                .collect();
                entries.into()
            })
            .collect();
        let entries: Object = [
            (Rc::new("offset".into()), Value::number(b2c[start])),
            (
                Rc::new("length".into()),
                Value::number(b2c[end] - b2c[start]),
            ),
            (
                Rc::new("string".into()),
                Value::string(s[start..end].into()),
            ),
            (Rc::new("captures".into()), captures.into()),
        ]
        .into_iter()
        .collect();
        v.push(entries.into());
    }
    v.push(Value::string(s[last_end..].into()));
    Ok(Array::from_vec(v).into())
}
