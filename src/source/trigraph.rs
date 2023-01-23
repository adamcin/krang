use std::io::Error;

use crate::{read::TryingIterator, scan::Ch};

/// Physical source file multibyte characters are mapped, in an implementation defined manner,
/// to the source character set (introducing new-line characters for
/// end-of-line indicators) if necessary. Trigraph sequences are replaced by
/// corresponding single-character internal representations.
pub struct TrigraphFilter<'a> {
    iter: Box<dyn TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>,
    buff: [Option<Ch>; 3],
    idx: usize,
}

impl<'a> TrigraphFilter<'a> {
    pub fn new<T: TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>(
        e0: Option<<Self as TryingIterator>::OkItem>,
        e1: Option<<Self as TryingIterator>::OkItem>,
        e2: Option<<Self as TryingIterator>::OkItem>,
        iter: T,
    ) -> Self {
        let buff = [e0, e1, e2];
        Self {
            iter: Box::new(iter),
            buff,
            idx: 0,
        }
    }

    pub fn try_filter<T: TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>(
        mut iter: T,
    ) -> Result<Self, Error> {
        let e0 = iter.try_next()?;
        let e1 = iter.try_next()?;
        let e2 = iter.try_next()?;
        Ok(Self::new(e0, e1, e2, iter))
    }

    fn check_trigraph(&self) -> Option<Ch> {
        self.buff[self.i0()]
            .as_ref()
            .filter(|pos| pos.chat() == '?')
            .and_then(|_| {
                self.buff[self.i1()]
                    .as_ref()
                    .filter(|pos| pos.chat() == '?')
            })
            .and_then(|_| {
                Self::map_graph(self.buff[self.i2()].as_ref().map(|pos| pos.chat()))
                    .and_then(|chat| self.buff[self.i0()].as_ref().map(|pos| pos.as_char(chat)))
            })
    }

    fn map_graph(input: Option<char>) -> Option<char> {
        match input {
            Some('=') => Some('#'),
            Some('(') => Some('['),
            Some(')') => Some(']'),
            Some('<') => Some('{'),
            Some('>') => Some('}'),
            Some('!') => Some('|'),
            Some('-') => Some('~'),
            Some('\'') => Some('^'),
            Some('/') => Some('\\'),
            _ => None,
        }
    }

    fn i0(&self) -> usize {
        self.idx % 3
    }

    fn i1(&self) -> usize {
        (self.idx + 1) % 3
    }

    fn i2(&self) -> usize {
        (self.idx + 2) % 3
    }

    fn push(&mut self, next: Option<Ch>) -> Option<Ch> {
        let temp = self.buff[self.i0()].as_ref().cloned();
        self.buff[self.i0()] = next;
        self.idx += 1;
        temp
    }
}

impl<'a> TryingIterator for TrigraphFilter<'a> {
    type OkItem = Ch;

    fn try_next(&mut self) -> Result<Option<Self::OkItem>, std::io::Error> {
        match self.check_trigraph() {
            Some(pos) => {
                let next = Some(pos);
                let e0 = self.iter.try_next()?;
                self.push(e0);
                let e1 = self.iter.try_next()?;
                self.push(e1);
                let e2 = self.iter.try_next()?;
                self.push(e2);
                Ok(next)
            }
            None => {
                let next = self.iter.try_next();
                next.map(|pos| self.push(pos))
            }
        }
    }
}
