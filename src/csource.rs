use std::{fmt::Display, fs::File, io::Error, path::Path};

use crate::{
    read::CharReader,
    scan::{SrcPos, SrcPosIter},
};

pub struct CSource<'p> {
    path: Box<dyn AsRef<Path> + 'p>,
    src: Vec<SrcPos<'p>>,
}

const PATH_INLINE: &str = "<inline>";

impl<'p> CSource<'p> {
    fn new(path: Box<dyn AsRef<Path> + 'p>, src: Vec<SrcPos<'p>>) -> Self {
        Self { path, src }
    }

    pub fn path(&self) -> &Path {
        self.path.as_ref().as_ref()
    }

    pub fn inline(input: &'p str) -> Result<Self, Error> {
        let path = Path::new(PATH_INLINE);
        let chars = CharReader::new(Box::new(input.as_bytes())).into_iter();
        let src: Result<Vec<SrcPos>, Error> = SrcPosIter::new(path, chars)
            .into_ignore_comments()?
            .collect();
        Ok(Self::new(Box::new(path), src?))
    }

    pub fn open(path: &'p Path) -> Result<Self, Error> {
        let chars = CharReader::new(Box::new(File::open(path)?)).into_iter();
        let src: Result<Vec<SrcPos>, Error> = SrcPosIter::new(path, chars)
            .into_ignore_comments()?
            .collect();
        Ok(Self::new(Box::new(path), src?))
    }

    pub fn stream(&self) -> &[SrcPos<'p>] {
        self.src.as_slice()
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }
}

impl<'p> std::ops::Index<std::ops::Range<usize>> for CSource<'p> {
    type Output = [SrcPos<'p>];

    fn index(&self, index: std::ops::Range<usize>) -> &Self::Output {
        &self.src[index]
    }
}

impl<'p> std::ops::Index<std::ops::RangeTo<usize>> for CSource<'p> {
    type Output = [SrcPos<'p>];

    fn index(&self, index: std::ops::RangeTo<usize>) -> &Self::Output {
        &self.src[index]
    }
}

impl<'p> std::ops::Index<std::ops::RangeFrom<usize>> for CSource<'p> {
    type Output = [SrcPos<'p>];

    fn index(&self, index: std::ops::RangeFrom<usize>) -> &Self::Output {
        &self.src[index]
    }
}

impl<'p> std::ops::Index<std::ops::RangeFull> for CSource<'p> {
    type Output = [SrcPos<'p>];

    fn index(&self, _index: std::ops::RangeFull) -> &Self::Output {
        self.src.as_slice()
    }
}

impl Display for CSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", SrcPos::as_string(self.src.as_slice()))
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Error, path::Path};

    use super::CSource;

    #[test]
    fn no_comment() -> Result<(), Error> {
        let file = CSource::open(Path::new("tests/hello_world.c"))?;
        let contents: String = file.to_string();

        println!("{}", contents);
        Ok(())
    }
}
