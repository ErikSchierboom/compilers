use crate::interpreter::RuntimeError;
use crate::location::Position;

// TODO: add diagnostic struct
// TODO: impl Display for diagnostic

#[derive(Debug)]
pub struct Diagnostic {
    pub title: String,
    pub text: String,
    pub position: Position,
}

impl From<RuntimeError> for Diagnostic {
    fn from(value: RuntimeError) -> Self {
        todo!()
    }
}