use crate::interpreter::RuntimeError;
use crate::location::{LineEndings, Position, Spanned};
use std::fmt::{Display, Formatter};

// TODO: add level to support warnings
// TODO: support file name
pub struct Diagnostic {
    pub message: String,
    pub position: Position,
}

impl Diagnostic {
    pub fn from_errors(source: &str, errors: &Vec<Spanned<RuntimeError>>) -> Vec<Diagnostic> {
        let line_endings = LineEndings::new(source);

        errors.iter()
            .map(|spanned_error| {
                let Spanned { value: error, span } = spanned_error;
                let message = error.to_string();
                let position = line_endings.position(span);
                Diagnostic { message, position }
            })
            .collect()
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} - {}", self.position, self.message)
    }
}
