use crate::location::{Span, Spanned};
use crate::parser::Word;

pub fn lower(words: Vec<Spanned<Word>>) -> Vec<Spanned<Word>> {
    let mut lowered = Vec::with_capacity(words.len());

    for spanned_word in words {
        match spanned_word.value {
            Word::Name(name) if name.starts_with(&['@', '$', '%']) => {
                lowered.push(Spanned::new(Word::Quote(name[1..].into()),Span { start: spanned_word.span.start + 1, end: spanned_word.span.end }));
                lowered.push(Spanned::new(Word::Name(name[0..1].into()), Span { start: spanned_word.span.start, end: spanned_word.span.start + 1, }));
            }
            _ => lowered.push( spanned_word),
        }
    }

    lowered
}