use std::{cell::RefCell, rc::Rc};

pub(crate) fn make_owned<T: Clone>(v: Rc<T>) -> T {
    match Rc::try_unwrap(v) {
        Ok(v) => v,
        Err(v) => (*v).clone(),
    }
}

pub struct SharedIterator<I>(Rc<RefCell<I>>);
impl<I> From<I> for SharedIterator<I> {
    fn from(it: I) -> Self {
        Self(Rc::new(RefCell::new(it)))
    }
}
impl<I> Clone for SharedIterator<I> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T, I: Iterator<Item = T>> Iterator for SharedIterator<I> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.borrow_mut().next()
    }
}
