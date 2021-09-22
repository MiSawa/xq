use thiserror::Error;

pub type Result<T, E = QueryExecutionError> = std::result::Result<T, E>;

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub enum QueryExecutionError {
    #[error("Error encountered while running a query: '{0:?}'")]
    Recoverable(RecoverableErrorWrapper),
    #[error("You found a bug of xq!: '{0:?}'")]
    UnRecoverable(UnrecoverableErrorWrapper),
}

#[derive(Debug, Clone, Eq, PartialEq, Error)]
#[error(transparent)]
pub struct RecoverableErrorWrapper(#[from] RecoverableError);

#[derive(Debug, Clone, Eq, PartialEq, Error)]
#[error(transparent)]
pub struct UnrecoverableErrorWrapper(#[from] UnrecoverableError);

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub(crate) enum RecoverableError {}

#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub(crate) enum UnrecoverableError {
    #[error("tried to pop an empty stack")]
    PopEmptyStack,
    #[error("tried to use an uninitialized scope")]
    UninitializedScope,
    #[error("tried to load from an uninitialized slot of a scope")]
    UnknownSlot,
    #[error("tried to load from an uninitialized slot of a scope")]
    UninitializedSlot,
    #[error("type mismatch: {0:}")]
    TypeMismatch(&'static str),
    #[error("tried to pop a scope but there was no scope to pop")]
    PopEmptyScope,
    #[error("tried to restore an unknown scope")]
    PopUnknownScope,
    #[error("reached to an unreachable state")]
    ReachedUnreachable,
    #[error("this byte code should be replaced during compilation")]
    ReachedPlaceholder,
}

impl From<RecoverableError> for QueryExecutionError {
    fn from(e: RecoverableError) -> Self {
        Self::Recoverable(RecoverableErrorWrapper(e))
    }
}

impl From<UnrecoverableError> for QueryExecutionError {
    fn from(e: UnrecoverableError) -> Self {
        Self::UnRecoverable(UnrecoverableErrorWrapper(e))
    }
}
