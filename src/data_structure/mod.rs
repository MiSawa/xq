use std::{borrow::Borrow, rc::Rc};

pub type PVector<T> = im_rc::Vector<T>;
pub type PHashMap<K, V> = im_rc::HashMap<K, V>;

#[derive(Clone, Debug)]
pub(crate) struct PStack<T, B = [T; 32]> {
    prev: Option<Rc<PStack<T, B>>>,
    current: sized_chunks::InlineArray<T, B>,
}

impl<T, B> Default for PStack<T, B> {
    fn default() -> Self {
        Self {
            prev: None,
            current: sized_chunks::InlineArray::new(),
        }
    }
}

impl<T: Clone, B> PStack<T, B> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push(&mut self, value: T) {
        if self.current.is_full() {
            let built = std::mem::take(self);
            self.prev = Some(Rc::new(built));
        }
        self.current.push(value)
    }

    pub fn pop(&mut self) -> Option<T> {
        let ret = self.current.pop();
        if ret.is_some() {
            ret
        } else if let Some(p) = self.prev.take() {
            let p: &PStack<_, _> = p.borrow();
            self.prev = p.prev.clone();
            self.current.clone_from(&p.current);
            self.current.pop()
        } else {
            None
        }
    }

    pub fn top_mut(&mut self) -> Option<&mut T> {
        if self.current.is_empty() {
            if let Some(p) = self.prev.take() {
                let p: &PStack<_, _> = p.borrow();
                self.prev = p.prev.clone();
                self.current.clone_from(&p.current);
            } else {
                return None;
            }
        }
        self.current.last_mut()
    }
}

#[cfg(test)]
mod test {
    use super::PStack;

    #[test]
    fn test_stack() {
        let mut v = vec![];
        let mut s = PStack::<usize, [usize; 4]>::new();
        for i in 0..10 {
            s.push(i);
            v.push(s.clone());
        }
        for i in 0..10 {
            assert_eq!(Some(9 - i), s.pop());
        }
        assert_eq!(None, s.pop());
        for i in 0..10 {
            let mut s = v[i].clone();
            for j in 0..=i {
                assert_eq!(Some(i - j), s.pop());
            }
            assert_eq!(None, s.pop());
        }
    }
}
