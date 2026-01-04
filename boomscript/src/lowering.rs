use crate::location::{Span, Spanned};
use crate::parser::Word;

pub fn lower(words: Vec<Spanned<Word>>) -> Vec<Spanned<Word>> {
    let mut lowered = Vec::with_capacity(words.len());

    for Spanned { value, span } in words {
        match value {
            Word::Word(name) if name.len() > 1 && name.starts_with(&['@', '$', '%']) => {
                lowered.push(Spanned::new(Word::QuotedWord(name[1..].into()), Span { start: span.start + 1, end: span.end }));
                lowered.push(Spanned::new(Word::Word(name[0..1].into()), Span { start: span.start, end: span.start + 1 }));
            }
            Word::Array(words) => lowered.push(Spanned::new(Word::Array(lower(words)), span)),
            Word::Block(words) => lowered.push(Spanned::new(Word::Block(lower(words)), span)),
            _ => lowered.push(Spanned { value, span }),
        }
    }

    lowered
}