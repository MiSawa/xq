use std::cmp::Ordering;

use itertools::Itertools;
use num::Float;
use xq_lang::ast::Comparator;

use crate::{vm::bytecode::NamedFn1, Value};

pub(crate) fn comparator(comparator: &Comparator) -> NamedFn1 {
    // NOTE: Because of the evaluation order, lhs and rhs are flipped here.
    match comparator {
        Comparator::Eq => NamedFn1 {
            name: "Equal",
            func: |rhs, lhs| Ok((PartialValue(&lhs) == PartialValue(&rhs)).into()),
        },
        Comparator::Neq => NamedFn1 {
            name: "NotEqual",
            func: |rhs, lhs| Ok((PartialValue(&lhs) != PartialValue(&rhs)).into()),
        },
        Comparator::Gt => NamedFn1 {
            name: "GreaterThan",
            func: |rhs, lhs| Ok((compare(lhs, rhs).map(Ordering::is_gt).unwrap_or(false)).into()),
        },
        Comparator::Ge => NamedFn1 {
            name: "GreaterOrEqual",
            func: |rhs, lhs| Ok((compare(lhs, rhs).map(Ordering::is_ge).unwrap_or(false)).into()),
        },
        Comparator::Lt => NamedFn1 {
            name: "LessThan",
            func: |rhs, lhs| Ok((compare(lhs, rhs).map(Ordering::is_lt).unwrap_or(false)).into()),
        },
        Comparator::Le => NamedFn1 {
            name: "LessOrEqual",
            func: |rhs, lhs| Ok((compare(lhs, rhs).map(Ordering::is_le).unwrap_or(false)).into()),
        },
    }
}

fn compare(lhs: Value, rhs: Value) -> Option<Ordering> {
    PartialOrd::partial_cmp(&PartialValue(&lhs), &PartialValue(&rhs))
}

/// A wrapper of [Value] to treat it as a [PartialEq] and a [PartialOrd].
struct PartialValue<'a>(&'a Value);

impl<'a> PartialEq<Self> for PartialValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self.0, other.0) {
            (Null, Null) => true,
            (Boolean(lhs), Boolean(rhs)) => lhs == rhs,
            (Number(lhs), Number(rhs)) => lhs == rhs && !lhs.is_nan(),
            (String(lhs), String(rhs)) => lhs == rhs,
            (Array(lhs), Array(rhs)) => lhs
                .iter()
                .map(PartialValue)
                .eq(rhs.iter().map(PartialValue)),
            (Object(lhs), Object(rhs)) => {
                if lhs.len() != rhs.len() {
                    return false;
                }
                let lhs_keys = lhs.keys().sorted().collect_vec();
                let rhs_keys = rhs.keys().sorted().collect_vec();
                if lhs_keys != rhs_keys {
                    return false;
                }
                for key in lhs_keys {
                    if lhs.get(key) != rhs.get(key) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}

impl<'a> PartialOrd<Self> for PartialValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Ordering::*;
        use Value::*;
        fn type_ord(value: &Value) -> u8 {
            match value {
                Null => 0,
                Boolean(_) => 1,
                Number(_) => 2,
                String(_) => 3,
                Array(_) => 4,
                Object(_) => 5,
            }
        }
        if let res @ (Less | Greater) = type_ord(self.0).cmp(&type_ord(other.0)) {
            return Some(res);
        }
        let ret = match (&self.0, &other.0) {
            (Null, Null) => Equal,
            (Boolean(lhs), Boolean(rhs)) => Ord::cmp(lhs, rhs),
            (Number(lhs), Number(rhs)) => {
                if lhs.is_nan() || rhs.is_nan() {
                    return None;
                }
                Ord::cmp(&lhs, &rhs)
            }
            (String(lhs), String(rhs)) => Ord::cmp(&lhs, &rhs),
            (Array(lhs), Array(rhs)) => {
                return Iterator::partial_cmp(
                    lhs.iter().map(PartialValue),
                    rhs.iter().map(PartialValue),
                )
            }
            (Object(lhs), Object(rhs)) => {
                let lhs_keys = lhs.keys().sorted().collect_vec();
                let rhs_keys = rhs.keys().sorted().collect_vec();
                if let res @ (Less | Greater) = Iterator::cmp(lhs_keys.iter(), rhs_keys.iter()) {
                    return Some(res);
                }
                for key in lhs_keys {
                    if let res @ Some(Less | Greater) = PartialOrd::partial_cmp(
                        &lhs.get(key).map(PartialValue),
                        &rhs.get(key).map(PartialValue),
                    ) {
                        return res;
                    }
                }
                Equal
            }
            (Null | Boolean(_) | Number(_) | String(_) | Array(_) | Object(_), _) => {
                unreachable!()
            }
        };
        Some(ret)
    }
}
