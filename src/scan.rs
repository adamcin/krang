use std::{fmt::Debug, io::Error, path::Path};

use crate::read::{CharReaderIter, TryingIterator};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Loc<'p> {
    path: &'p Path,
    byte: usize,
    line: usize,
    col: usize,
}

impl<'p> Loc<'p> {
    fn init(path: &'p Path) -> Self {
        Loc {
            path,
            byte: 0,
            line: 0,
            col: 0,
        }
    }

    pub fn next(&self, ch: char) -> Self {
        let (line, col) = if ch == '\n' {
            (self.line + 1, 0)
        } else {
            (self.line, self.col + 1)
        };
        Self {
            path: self.path,
            byte: self.byte + ch.len_utf8(),
            line,
            col,
        }
    }
}

/// represents a lexable character
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Ch<'p> {
    loc: Loc<'p>,
    chat: char,
}

impl<'p> Ch<'p> {
    pub fn new(loc: Loc<'p>, chat: char) -> Self {
        Self { loc, chat }
    }

    pub fn loc(&self) -> &Loc<'p> {
        &self.loc
    }

    pub fn chat(&self) -> char {
        self.chat
    }

    pub fn next(&self, chat: char) -> Self {
        let next_loc = self.loc.next(self.chat);
        Self::new(next_loc, chat)
    }

    pub fn as_string(poss: &[Self]) -> String {
        String::from_iter(poss.iter().map(|pos| pos.chat()))
    }

    pub fn as_space(&self) -> Self {
        self.as_char(' ')
    }

    pub fn as_char(&self, chat: char) -> Self {
        Self::new(self.loc, chat)
    }
}

impl Debug for Ch<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.chat())
    }
}

pub struct ChIter<'a> {
    next: Loc<'a>,
    chars: CharReaderIter<'a>,
}

impl<'a> ChIter<'a> {
    pub fn new<'p>(path: &'p Path, chars: CharReaderIter<'a>) -> Self
    where
        'p: 'a,
    {
        Self {
            next: Loc::init(path),
            chars,
        }
    }
}

impl<'a> TryingIterator for ChIter<'a> {
    type OkItem = Ch<'a>;

    fn try_next(&mut self) -> Result<Option<Self::OkItem>, Error> {
        match self.chars.try_next() {
            Ok(Some(ch)) => {
                let pos = Ch::new(self.next, ch);
                self.next = self.next.next(ch);
                Ok(Some(pos))
            }
            Err(err) => Err(err),
            _ => Ok(None),
        }
    }
}
