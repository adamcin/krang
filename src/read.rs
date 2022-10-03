//

use std::io::{Error, Read};

use crate::common::err_invalid_input;

const BUF_SIZE: usize = 64;

/// wrap a Read impl to produce unicode chars over utf8-encoded bytes
pub struct CharReader<'read> {
    reader: Box<dyn Read + 'read>,
    // must store at least 4 bytes for multibyte chars
    buff: [u8; BUF_SIZE],
    bufi: usize,
    bufn: usize,
}

impl<'read> CharReader<'read> {
    pub fn new(reader: Box<dyn Read + 'read>) -> Self {
        Self {
            reader,
            buff: [0; BUF_SIZE],
            bufi: 0,
            bufn: 0,
        }
    }

    /// advances a reader to consume the next unicode character, if available
    pub fn read(&mut self) -> Result<Option<char>, Error> {
        // if buf index exceeds buf count, reset the buffer and indices with fresh read
        if self.bufi >= self.bufn {
            let n_read = self.reader.read(&mut self.buff)?;
            if n_read == 0 {
                Ok(None)
            } else {
                self.bufn = n_read;
                self.bufi = 0;
                self.read()
            }
        } else {
            let ret = self.buff[self.bufi] as char;
            if ret.is_ascii() {
                self.bufi += 1;
                Ok(Some(ret))
            } else {
                // dealing with unicode, so rotate left if necessary...
                if self.bufn - self.bufi < 4 {
                    self.buff.rotate_left(self.bufi);
                    self.bufn -= self.bufi;
                    self.bufi = 0;
                }
                // then attempt to fill the buffer if necessary
                if self.bufn < 4 {
                    let n_read = self.reader.read(&mut self.buff[self.bufn..BUF_SIZE])?;
                    self.bufn += n_read;
                }

                if let Some(uni) = core::str::from_utf8(&self.buff[self.bufi..self.bufn])
                    .map_err(err_invalid_input)?
                    .chars()
                    .next()
                {
                    self.bufi += uni.len_utf8();
                    Ok(Some(uni))
                } else {
                    Ok(None)
                }
            }
        }
    }

    pub fn into_chars(self) -> CharReaderIter<'read> {
        CharReaderIter::new(self)
    }
}

pub struct CharReaderIter<'read> {
    reader: CharReader<'read>,
}

impl<'read> CharReaderIter<'read> {
    fn new(reader: CharReader<'read>) -> Self {
        Self { reader }
    }
}

impl<'a> Iterator for CharReaderIter<'a> {
    type Item = Result<char, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.reader.read() {
            Ok(Some(next)) => Some(Ok(next)),
            Err(err) => Some(Err(err)),
            _ => None,
        }
    }
}

impl<'a> IntoIterator for CharReader<'a> {
    type Item = Result<char, Error>;
    type IntoIter = CharReaderIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        self.into_chars()
        //SrcPosIter::new().into_ignore_comments()
    }
}

#[cfg(test)]
mod tests {
    use std::io::Error;

    use super::CharReader;

    #[test]
    fn hello_world() -> Result<(), Error> {
        let teststr = "// 😀 ".as_bytes();
        let mut reader = CharReader::new(Box::new(teststr));

        let first = reader.read()?;
        assert_eq!(Some('/'), first);
        let second = reader.read()?;
        assert_eq!(Some('/'), second);
        let third = reader.read()?;
        assert_eq!(Some(' '), third);
        let fourth = reader.read()?;
        assert_eq!(Some('😀'), fourth);
        let fifth = reader.read()?;
        assert_eq!(Some(' '), fifth);
        let sixth = reader.read()?;
        assert_eq!(None, sixth);
        Ok(())
    }
}
