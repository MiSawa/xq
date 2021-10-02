use crate::{lang::ast::Comparator, vm::bytecode::NamedFn1, Value};
use itertools::Itertools;
use std::cmp::Ordering;

pub(crate) fn comparator(comparator: &Comparator) -> NamedFn1 {
    // NOTE: Because of the evaluation order, lhs and rhs are flipped here.
    match comparator {
        Comparator::Eq => NamedFn1 {
            name: "Equal",
            func: |rhs, lhs| Ok((compare(lhs, rhs).is_eq()).into()),
        },
        Comparator::Neq => NamedFn1 {
            name: "NotEqual",
            func: |rhs, lhs| Ok((compare(lhs, rhs).is_ne()).into()),
        },
        Comparator::Gt => NamedFn1 {
            name: "GreaterThan",
            func: |rhs, lhs| Ok((compare(lhs, rhs).is_gt()).into()),
        },
        Comparator::Ge => NamedFn1 {
            name: "GreaterOrEqual",
            func: |rhs, lhs| Ok((compare(lhs, rhs).is_ge()).into()),
        },
        Comparator::Lt => NamedFn1 {
            name: "LessThan",
            func: |rhs, lhs| Ok((compare(lhs, rhs).is_lt()).into()),
        },
        Comparator::Le => NamedFn1 {
            name: "LessOrEqual",
            func: |rhs, lhs| Ok((compare(lhs, rhs).is_le()).into()),
        },
    }
}

fn compare(lhs: Value, rhs: Value) -> Ordering {
    Ord::cmp(&ComparableValue(&lhs), &ComparableValue(&rhs))
}

struct ComparableValue<'a>(&'a Value);

impl<'a> Eq for ComparableValue<'a> {}

impl<'a> PartialEq<Self> for ComparableValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        Ord::cmp(self, other).is_eq()
    }
}

impl<'a> PartialOrd<Self> for ComparableValue<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Ord::cmp(self, other))
    }
}

impl<'a> Ord for ComparableValue<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
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
            return res;
        }
        match (&self.0, &other.0) {
            (Null, Null) | (Boolean(true), Boolean(true)) | (Boolean(false), Boolean(false)) => {
                Equal
            }
            (Number(lhs), Number(rhs)) => Ord::cmp(&lhs, &rhs),
            (String(lhs), String(rhs)) => Ord::cmp(&lhs, &rhs),
            (Array(lhs), Array(rhs)) => Iterator::cmp(
                lhs.as_ref().into_iter().map(ComparableValue),
                rhs.as_ref().into_iter().map(ComparableValue),
            ),
            (Object(lhs), Object(rhs)) => {
                let lhs_keys = lhs.keys().sorted().collect_vec();
                let rhs_keys = rhs.keys().sorted().collect_vec();
                if let res @ (Less | Greater) = Iterator::cmp(lhs_keys.iter(), rhs_keys.iter()) {
                    return res;
                }
                for key in lhs_keys {
                    if let res @ (Less | Greater) = Ord::cmp(
                        &lhs.get(key).map(ComparableValue),
                        &rhs.get(key).map(ComparableValue),
                    ) {
                        return res;
                    }
                }
                Equal
            }
            (Null | Boolean(_) | Number(_) | String(_) | Array(_) | Object(_), _) => {
                unreachable!()
            }
        }
    }
}
