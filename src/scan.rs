use std::{fmt::Debug, io::Error, path::Path};

use crate::read::CharReaderIter;

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

    fn next(&self, ch: char) -> Self {
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
pub struct SrcPos<'p> {
    loc: Loc<'p>,
    chat: char,
}

impl<'p> SrcPos<'p> {
    fn new(loc: Loc<'p>, chat: char) -> Self {
        Self { loc, chat }
    }

    pub fn chat(&self) -> char {
        self.chat
    }

    pub fn as_string(poss: &[Self]) -> String {
        String::from_iter(poss.iter().map(|pos| pos.chat()))
    }

    pub fn as_space(&self) -> Self {
        Self::new(self.loc, ' ')
    }
}

impl Debug for SrcPos<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.chat())
    }
}

pub struct SrcPosIter<'a> {
    next: Loc<'a>,
    chars: CharReaderIter<'a>,
}

impl<'a> SrcPosIter<'a> {
    pub fn new<'p>(path: &'p Path, chars: CharReaderIter<'a>) -> Self
    where
        'p: 'a,
    {
        Self {
            next: Loc::init(path),
            chars,
        }
    }

    pub fn try_next(&mut self) -> Result<Option<SrcPos<'a>>, Error> {
        match self.next() {
            Some(Ok(pos)) => Ok(Some(pos)),
            Some(Err(err)) => Err(err),
            None => Ok(None),
        }
    }

    pub fn into_ignore_comments(mut self) -> Result<CCommentFilteringIter<'a>, Error> {
        let left = self.try_next()?;
        let right = self.try_next()?;
        Ok(CCommentFilteringIter::new(left, right, self))
    }
}

impl<'a> Iterator for SrcPosIter<'a> {
    type Item = Result<SrcPos<'a>, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            Some(Ok(ch)) => {
                let pos = SrcPos::new(self.next, ch);
                self.next = self.next.next(ch);
                Some(Ok(pos))
            }
            Some(Err(err)) => Some(Err(err)),
            _ => None,
        }
    }
}

enum CommentState {
    Line,
    Block,
    None,
}

pub struct CCommentFilteringIter<'a> {
    iter: SrcPosIter<'a>,
    buff: [Option<SrcPos<'a>>; 2],
    flip: bool,
    state: CommentState,
    is_err: bool,
}

impl<'a> CCommentFilteringIter<'a> {
    pub fn new(left: Option<SrcPos<'a>>, right: Option<SrcPos<'a>>, iter: SrcPosIter<'a>) -> Self {
        let buff = [left, right];
        Self {
            iter,
            buff,
            flip: false,
            state: if Self::is_comment_line(left.as_ref(), right.as_ref()) {
                CommentState::Line
            } else if Self::is_comment_block(left.as_ref(), right.as_ref()) {
                CommentState::Block
            } else {
                CommentState::None
            },
            is_err: false,
        }
    }

    fn left(&self) -> Option<&SrcPos<'a>> {
        self.buff[self.lidx()].as_ref()
    }

    fn right(&self) -> Option<&SrcPos<'a>> {
        self.buff[self.ridx()].as_ref()
    }

    fn lidx(&self) -> usize {
        self.flip as usize
    }

    fn ridx(&self) -> usize {
        (!self.flip) as usize
    }

    fn char_at(pos: Option<&SrcPos<'a>>) -> Option<char> {
        pos.map(|p| p.chat())
    }

    fn is_comment_line(left: Option<&SrcPos<'a>>, right: Option<&SrcPos<'a>>) -> bool {
        Self::char_at(left) == Some('/') && Self::char_at(right) == Some('/')
    }

    fn is_comment_block(left: Option<&SrcPos<'a>>, right: Option<&SrcPos<'a>>) -> bool {
        Self::char_at(left) == Some('/') && Self::char_at(right) == Some('*')
    }

    fn iter_until_newline(&mut self) -> Result<Option<SrcPos<'a>>, Error> {
        let next = self.left().map(|pos| pos.as_space());
        while let Some(pos) = self.iter.try_next()? {
            if pos.chat() == '\n' {
                // keep the newline for the first index
                self.set_next_buf(Some(pos));
                // pop another for the second index
                let second = self.iter.try_next()?;
                self.set_next_buf(second);
                return Ok(next);
            }
        }
        Ok(None)
    }

    fn iter_until_blockend(&mut self) -> Result<Option<SrcPos<'a>>, Error> {
        let next = self.left().map(|pos| pos.as_space());
        while let Some(pos) = self.iter.try_next()? {
            if pos.chat() == '*' {
                if let Some(peek) = self.iter.try_next()? {
                    if peek.chat() == '/' {
                        let first = self.iter.try_next()?;
                        self.set_next_buf(first);
                        let second = self.iter.try_next()?;
                        self.set_next_buf(second);
                        return Ok(next);
                    }
                }
            }
        }
        Ok(None)
    }

    fn check_state(&mut self) {
        let left = self.left();
        let right = self.right();
        self.state = if Self::is_comment_line(left, right) {
            self.buff[self.lidx()] = left.map(|pos| pos.as_space());
            CommentState::Line
        } else if Self::is_comment_block(left, right) {
            self.buff[self.lidx()] = left.map(|pos| pos.as_space());
            CommentState::Block
        } else {
            CommentState::None
        }
    }

    fn set_next_buf(&mut self, next: Option<SrcPos<'a>>) -> Option<SrcPos<'a>> {
        let temp = self.left().cloned();
        self.buff[self.lidx()] = next;
        self.flip = !self.flip;
        self.check_state();
        temp
    }

    fn try_next(&mut self) -> Result<Option<SrcPos<'a>>, Error> {
        match self.state {
            CommentState::Line => self.iter_until_newline(),
            CommentState::Block => self.iter_until_blockend(),
            CommentState::None => {
                let next = self.iter.try_next();
                next.map(|pos| self.set_next_buf(pos))
            }
        }
    }
}

impl<'a> Iterator for CCommentFilteringIter<'a> {
    type Item = Result<SrcPos<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_err {
            None
        } else {
            let result = self.try_next();
            match result {
                Ok(next) => next.map(|pos| Ok(pos)),
                Err(err) => {
                    self.is_err = true;
                    Some(Err(err))
                }
            }
        }
    }
}
