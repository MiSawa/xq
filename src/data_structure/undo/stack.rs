use crate::data_structure::undo::Undo;
use derive_more::Display;
use itertools::Itertools;

#[derive(Debug, Eq, PartialEq)]
pub struct UStackToken(usize);

/// This span info corresponds to `undoes[undo_start_index .. (undo_start_index + len)]`
/// where `len` is `stack_initial - stack_bottom`.
/// The span of the `undoes` corresponds to `stack[stack_bottom .. stack_initial].rev()`
#[derive(Debug)]
struct UndoSpanInfo {
    id: UStackToken,
    undo_start: usize,
    stack_initial: usize,
    stack_bottom: usize,
}

#[derive(Debug, Display)]
#[display(fmt = "{}", stack)]
pub struct UStack<T> {
    stack: Vec<T>,
    undoes: Vec<T>,
    spans: Vec<UndoSpanInfo>,
    current: UndoSpanInfo,
    next_token_id: usize,
}

impl<T> Default for UStack<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> UStack<T> {
    fn new() -> Self {
        Self {
            stack: vec![],
            undoes: vec![],
            spans: vec![],
            current: UndoSpanInfo {
                id: UStackToken(0),
                undo_start: 0,
                stack_initial: 0,
                stack_bottom: 0,
            },
            next_token_id: 0,
        }
    }
}

impl<T> Undo for UStack<T> {
    type UndoToken = UStackToken;

    fn save(&mut self) -> Self::UndoToken {
        self.next_token_id += 1;
        let next_id = UStackToken(self.next_token_id);
        let ret = UStackToken(self.next_token_id);

        let old = std::mem::replace(
            &mut self.current,
            UndoSpanInfo {
                id: next_id,
                undo_start: self.undoes.len(),
                stack_initial: self.stack.len(),
                stack_bottom: self.stack.len(),
            },
        );
        self.spans.push(old);
        ret
    }

    fn undo(&mut self, token: Self::UndoToken) {
        let old = std::mem::replace(
            &mut self.current,
            UndoSpanInfo {
                id: UStackToken(0),
                undo_start: 0,
                stack_initial: 0,
                stack_bottom: 0,
            },
        );
        self.spans.push(old);

        let i = self.spans.iter().enumerate().rfind(|(_, t)| t.id == token)
            .expect("This undo token may have been expired already, or maybe you got the token from different stack?")
            .0;
        let tokens = self.spans.split_off(i);
        let stack_ranges = tokens
            .iter()
            .map(|token| token.stack_bottom)
            .scan(tokens[0].stack_initial, |filled, bottom| {
                let min = Ord::min(bottom, *filled);
                let ret = min..*filled;
                *filled = min;
                Some(ret)
            })
            .collect_vec();
        self.stack.truncate(stack_ranges.last().unwrap().start);

        for (token, range) in tokens
            .into_iter()
            .rev()
            .zip_eq(stack_ranges.into_iter().rev())
        {
            if range.is_empty() {
                continue;
            }
            let undoes = self
                .undoes
                .drain(token.undo_start..)
                .take(token.stack_initial - token.stack_bottom)
                .rev();
            // undoes corresponds to stack_bottom .. stack_initial
            let undoes = undoes.take(range.len());
            self.stack.extend(undoes);
        }

        self.current = self
            .spans
            .pop()
            .expect("There should be at least the base span left in the span stack.");
    }
}

impl<T> UStack<T> {
    pub fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }

    pub fn top(&self) -> Option<&T> {
        self.stack.last()
    }

    pub fn push(&mut self, value: T) {
        self.stack.push(value);
    }

    pub fn pop_and_discard(&mut self) -> bool {
        if let Some(item) = self.stack.pop() {
            if self.current.stack_bottom > self.stack.len() {
                self.undoes.push(item);
                self.current.stack_bottom -= 1;
            }
            true
        } else {
            false
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.stack.iter()
    }

    pub fn into_vec(self) -> Vec<T> {
        self.stack
    }
}

impl<T: Clone> UStack<T> {
    pub fn pop(&mut self) -> Option<T> {
        if let Some(item) = self.stack.pop() {
            if self.current.stack_bottom > self.stack.len() {
                self.undoes.push(item.clone());
                self.current.stack_bottom -= 1;
            }
            Some(item)
        } else {
            None
        }
    }

    pub fn dup(&mut self) {
        self.stack
            .push(self.stack.last().expect("Dup on empty stack").clone())
    }

    pub fn top_mut(&mut self) -> Option<&mut T> {
        let len = self.stack.len();
        if let Some(item) = self.stack.last_mut() {
            if self.current.stack_bottom >= len {
                self.undoes.push(item.clone());
                self.current.stack_bottom -= 1;
            }
            Some(item)
        } else {
            None
        }
    }

    pub fn swap(&mut self) {
        let last = self.pop().expect("Swap was called on an empty stack");
        let penultimate = self.pop().expect("Swap was called on a stack of size one");
        self.push(last);
        self.push(penultimate);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_simple() {
        let mut s = UStack::new();
        let token = s.save();
        s.push(1);
        s.undo(token);
        assert_eq!(s.pop(), None);

        s.push(1);
        s.push(2);
        assert_eq!(s.iter().cloned().collect_vec(), vec![1, 2]);
        let token = s.save();
        assert_eq!(s.pop(), Some(2));
        s.push(3);
        s.undo(token);
        assert_eq!(s.iter().cloned().collect_vec(), vec![1, 2]);
        s.push(3);
        assert_eq!(s.pop(), Some(3));
        assert_eq!(s.pop(), Some(2));
    }

    #[test]
    fn test_simple2() {
        let mut s = UStack::new();
        s.push(0);
        let token1 = s.save();
        s.pop();
        s.push(1);
        let _token2 = s.save();
        assert_eq!(s.pop(), Some(1));
        s.undo(token1);
        assert_eq!(s.iter().cloned().collect_vec(), vec![0]);
    }
}
