use crate::parser::Word;

pub fn lower(words: Vec<Word>) -> Vec<Word> {
    let mut lowered = Vec::with_capacity(words.len());
    let mut words = words.into_iter().peekable();

    while let Some(word) = words.next() {
        if matches!(&word, Word::Identifier { name, .. } if matches!(name.as_str(), "@" | "$" | "%")) {
            if let Some(Word::Identifier { name, location }) = words.next_if(|next| word.location().is_contiguous_with(&next.location()) && matches!(next, Word::Identifier { .. })) {
                lowered.push(Word::Quote { name, location })
            }
        }

        lowered.push(word)
    }

    lowered
}
