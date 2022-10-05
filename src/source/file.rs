use std::{fmt::Display, fs::File, io::Error, path::Path};

use crate::{
    read::{CharReader, CharReaderIter, TryingIteratorAdaptor},
    scan::{Ch, ChIter},
    source::{nlsplice::NlSpliceFilter, trigraph::TrigraphFilter},
};

use super::comment::CommentFilter;

/// Represents stream of unicode characters that have been read from a C physical source file.
/// Successful construction indicates that these translation phases have been performed:
/// * 1: trigraph replacement
/// * 2: splicing around escaped newlines
pub struct SourceFile<'p> {
    path: Box<dyn AsRef<Path> + 'p>,
    src: Vec<Ch<'p>>,
}

const PATH_INLINE: &str = "<inline>";

impl<'p> SourceFile<'p> {
    fn new(path: Box<dyn AsRef<Path> + 'p>, src: Vec<Ch<'p>>) -> Self {
        Self { path, src }
    }

    pub fn path(&self) -> &Path {
        self.path.as_ref().as_ref()
    }

    pub fn inline(input: &'p str) -> Result<Self, Error> {
        let path = Path::new(PATH_INLINE);
        let chars = CharReader::new(Box::new(input.as_bytes())).into_chars();
        let src: Result<Vec<Ch>, Error> = Self::filter_source(path, chars)?.collect();
        Ok(Self::new(Box::new(path), src?))
    }

    pub fn open(path: &'p Path) -> Result<Self, Error> {
        let chars = CharReader::new(Box::new(File::open(path)?)).into_chars();
        let src: Result<Vec<Ch>, Error> = Self::filter_source(path, chars)?.collect();
        Ok(Self::new(Box::new(path), src?))
    }

    fn filter_source(
        path: &'p Path,
        chars: CharReaderIter<'p>,
    ) -> Result<TryingIteratorAdaptor<'p, Ch<'p>>, Error> {
        Ok(TryingIteratorAdaptor::new(CommentFilter::try_filter(
            NlSpliceFilter::try_filter(TrigraphFilter::try_filter(ChIter::new(path, chars))?)?,
        )?))
    }

    pub fn stream(&self) -> &[Ch<'p>] {
        self.src.as_slice()
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }
}

impl<'p> std::ops::Index<std::ops::Range<usize>> for SourceFile<'p> {
    type Output = [Ch<'p>];

    fn index(&self, index: std::ops::Range<usize>) -> &Self::Output {
        &self.src[index]
    }
}

impl<'p> std::ops::Index<std::ops::RangeTo<usize>> for SourceFile<'p> {
    type Output = [Ch<'p>];

    fn index(&self, index: std::ops::RangeTo<usize>) -> &Self::Output {
        &self.src[index]
    }
}

impl<'p> std::ops::Index<std::ops::RangeFrom<usize>> for SourceFile<'p> {
    type Output = [Ch<'p>];

    fn index(&self, index: std::ops::RangeFrom<usize>) -> &Self::Output {
        &self.src[index]
    }
}

impl<'p> std::ops::Index<std::ops::RangeFull> for SourceFile<'p> {
    type Output = [Ch<'p>];

    fn index(&self, _index: std::ops::RangeFull) -> &Self::Output {
        self.src.as_slice()
    }
}

impl Display for SourceFile<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Ch::as_string(self.src.as_slice()))
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Error, path::Path};

    use super::SourceFile;

    #[test]
    fn no_comment() -> Result<(), Error> {
        let file = SourceFile::open(Path::new("tests/hello_world.c"))?;
        let contents: String = file.to_string();

        println!("{}", contents);
        Ok(())
    }
}
