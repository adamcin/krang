use std::io::Error;

use crate::{common::err_invalid_input, read::TryingIterator, scan::Ch};

/// Each instance of a backslash character (\) immediately followed by a new-line
/// character is deleted, splicing physical source lines to form logical source lines.
/// Only the last backslash on any physical source line shall be eligible for being part
/// of such a splice.
///
/// NOTE: A source file that is not empty shall end in a new-line character,
/// which shall not be immediately preceded by a backslash character before any such
/// splicing takes place.
pub struct NlSpliceFilter<'a> {
    iter: Box<dyn TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>,
    last: Option<Ch<'a>>,
}

impl<'a> NlSpliceFilter<'a> {
    pub fn new<T: TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>(
        first: Option<Ch<'a>>,
        iter: T,
    ) -> Self {
        Self {
            iter: Box::new(iter),
            last: first,
        }
    }

    pub fn try_filter<T: TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>(
        mut iter: T,
    ) -> Result<Self, Error> {
        let first = iter.try_next()?;
        Ok(Self::new(first, iter))
    }

    fn push(&mut self, next: Option<Ch<'a>>) -> Option<Ch<'a>> {
        let temp = self.last.as_ref().cloned();
        self.last = next;
        temp
    }
}

impl<'a> TryingIterator for NlSpliceFilter<'a> {
    type OkItem = Ch<'a>;

    fn try_next(&mut self) -> Result<Option<Self::OkItem>, std::io::Error> {
        match (self.last, self.iter.try_next()?) {
            (Some(last_pos), Some(pos)) if last_pos.chat() == '\\' && pos.chat() == '\n' => {
                let next = self.iter.try_next()?;
                // A source file that is not empty shall end in a new-line character,
                // which shall not be immediately preceded by a backslash character before any such
                // splicing takes place.
                if next.is_none() {
                    Err(err_invalid_input(
                        "source ends with newline preceded by backslash",
                    ))
                } else {
                    self.push(next);
                    Ok(Some(pos))
                }
            }
            (Some(last_pos), None) if last_pos.chat() == '\\' => {
                // A source file that is not empty shall end in a new-line character,
                // which shall not be immediately preceded by a backslash character before any such
                // splicing takes place.
                Err(err_invalid_input("source ends with a backslash"))
            }
            // (Some(last_pos), None) if last_pos.chat() != '\n' => {
            //     // if source file does not end in a newline, inject one.
            //     Ok(self.push(Some(last_pos.next('\n'))))
            // }
            (_, next) => Ok(self.push(next)),
        }
    }
}
