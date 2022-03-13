use std::iter::Once;

use xq::{util::SharedIterator, Array, InputError, Value};

type ResultValue = Result<Value, InputError>;

pub(crate) trait Input {
    type SingleInputIterator: Iterator<Item = ResultValue>;
    type ContextsIterator: Iterator<Item = ResultValue>;
    type InputIterator: Iterator<Item = ResultValue>;

    fn into_input_iterator(self) -> Self::SingleInputIterator;
    fn into_iterators(self) -> (Self::ContextsIterator, Self::InputIterator);

    fn null_input(self) -> Null<Self>
    where
        Self: Sized,
    {
        Null(self)
    }
}

pub(crate) struct Tied<I: Iterator<Item = ResultValue>>(I);
impl<I: Iterator<Item = ResultValue>> Tied<I> {
    pub(crate) fn new(inner: I) -> Self {
        Self(inner)
    }
    pub(crate) fn slurp(self) -> impl Input {
        let v = self
            .0
            .collect::<Result<Array, InputError>>()
            .map(Into::into);
        Slurp(v)
    }
}

impl<I: Iterator<Item = ResultValue>> Input for Tied<I> {
    type SingleInputIterator = I;
    type ContextsIterator = SharedIterator<I>;
    type InputIterator = SharedIterator<I>;

    fn into_input_iterator(self) -> Self::SingleInputIterator {
        self.0
    }

    fn into_iterators(self) -> (Self::ContextsIterator, Self::InputIterator) {
        let shared = SharedIterator::from(self.0);
        (shared.clone(), shared)
    }
}

pub(crate) struct Null<I>(I);
impl<I: Input> Input for Null<I> {
    type SingleInputIterator = I::SingleInputIterator;
    type ContextsIterator = Once<ResultValue>;
    type InputIterator = I::SingleInputIterator;

    fn into_input_iterator(self) -> Self::SingleInputIterator {
        self.0.into_input_iterator()
    }

    fn into_iterators(self) -> (Self::ContextsIterator, Self::InputIterator) {
        (
            std::iter::once(Ok(Value::Null)),
            self.0.into_input_iterator(),
        )
    }
}

struct Slurp(ResultValue);
impl Input for Slurp {
    type SingleInputIterator = Once<ResultValue>;
    type ContextsIterator = SharedIterator<Once<ResultValue>>;
    type InputIterator = SharedIterator<Once<ResultValue>>;

    fn into_input_iterator(self) -> Self::SingleInputIterator {
        std::iter::once(self.0)
    }

    fn into_iterators(self) -> (Self::ContextsIterator, Self::InputIterator) {
        let shared = SharedIterator::from(std::iter::once(self.0));
        (shared.clone(), shared)
    }
}
