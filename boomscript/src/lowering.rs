use crate::location::Span;
use crate::parser::Word;

pub fn lower(words: Vec<Word>) -> Vec<Word> {
    let mut lowered = Vec::with_capacity(words.len());

    for word in words {
        match word {
            Word::Name { name, location } if name.starts_with(&['@', '$', '%']) => {
                lowered.push(Word::Quote {
                    name: name[1..].into(),
                    location: Span { start: location.start + 1, end: location.end },
                });
                lowered.push(Word::Name {
                    name: name[0..1].into(),
                    location: Span {
                        start: location.start,
                        end: location.start + 1,
                    },
                });
            }
            word => lowered.push(word),
        }
    }

    lowered
}