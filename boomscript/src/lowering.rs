use crate::parser::Word;

pub fn lower(words: Vec<Word>) -> Vec<Word> {
    let mut lowered = Vec::with_capacity(words.len());
    let mut words = words.into_iter().peekable();

    while let Some(word) = words.next() {
        if matches!(word, Word::Read { .. } | Word::Write { .. } | Word::Execute { .. }) {
            if let Some(Word::Identifier { name, location }) = words.next_if(|w| matches!(w, Word::Identifier { .. })) {
                lowered.push(Word::Quote { name, location });
            }
        }

        lowered.push(word);
    }

    lowered
}