#[allow(dead_code)]
pub mod stack;

pub trait Undo {
    type UndoToken;
    fn save(&mut self) -> Self::UndoToken;
    fn undo(&mut self, token: Self::UndoToken);
}
