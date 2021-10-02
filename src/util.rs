use std::rc::Rc;

// TODO: Is there a better way?
pub fn make_owned<T: Clone + Default>(mut v: Rc<T>) -> T {
    let ret = Rc::make_mut(&mut v);
    std::mem::take(ret)
}
