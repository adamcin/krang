use std::io::Error;

use crate::{read::TryingIterator, scan::Ch};

enum CommentState {
    Line,
    Block,
    None,
}

pub struct CommentFilter<'a> {
    iter: Box<dyn TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>,
    buff: [Option<Ch>; 2],
    flip: bool,
    state: CommentState,
}

impl<'a> CommentFilter<'a> {
    pub fn new<T: TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>(
        left: Option<Ch>,
        right: Option<Ch>,
        iter: T,
    ) -> Self {
        let buff = [left, right];
        let state = if Self::is_comment_line(buff[0].as_ref(), buff[1].as_ref()) {
            CommentState::Line
        } else if Self::is_comment_block(buff[0].as_ref(), buff[1].as_ref()) {
            CommentState::Block
        } else {
            CommentState::None
        };
        Self {
            iter: Box::new(iter),
            buff,
            flip: false,
            state,
        }
    }

    pub fn try_filter<T: TryingIterator<OkItem = <Self as TryingIterator>::OkItem> + 'a>(
        mut iter: T,
    ) -> Result<Self, Error> {
        let left = iter.try_next()?;
        let right = iter.try_next()?;
        Ok(Self::new(left, right, iter))
    }

    fn left(&self) -> Option<&Ch> {
        self.buff[self.lidx()].as_ref()
    }

    fn right(&self) -> Option<&Ch> {
        self.buff[self.ridx()].as_ref()
    }

    fn lidx(&self) -> usize {
        self.flip as usize
    }

    fn ridx(&self) -> usize {
        (!self.flip) as usize
    }

    fn char_at(pos: Option<&Ch>) -> Option<char> {
        pos.map(|p| p.chat())
    }

    fn is_comment_line(left: Option<&Ch>, right: Option<&Ch>) -> bool {
        Self::char_at(left) == Some('/') && Self::char_at(right) == Some('/')
    }

    fn is_comment_block(left: Option<&Ch>, right: Option<&Ch>) -> bool {
        Self::char_at(left) == Some('/') && Self::char_at(right) == Some('*')
    }

    fn iter_until_newline(&mut self) -> Result<Option<Ch>, Error> {
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

    fn iter_until_blockend(&mut self) -> Result<Option<Ch>, Error> {
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

    fn set_next_buf(&mut self, next: Option<Ch>) -> Option<Ch> {
        let temp = self.left().cloned();
        self.buff[self.lidx()] = next;
        self.flip = !self.flip;
        self.check_state();
        temp
    }
}

impl<'a> TryingIterator for CommentFilter<'a> {
    type OkItem = Ch;

    fn try_next(&mut self) -> Result<Option<Ch>, Error> {
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
