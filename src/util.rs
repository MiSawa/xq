use std::rc::Rc;

pub fn make_owned<T: Clone>(v: Rc<T>) -> T {
    match Rc::try_unwrap(v) {
        Ok(v) => v,
        Err(v) => (*v).clone(),
    }
}
