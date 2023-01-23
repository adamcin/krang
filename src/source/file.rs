use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    fs::File,
    io::Error,
    path::Path,
    rc::Rc,
};

use crate::{
    common::err_invalid_input,
    read::{CharReader, CharReaderIter, TryingIteratorAdaptor},
    scan::{Ch, ChIter},
    source::{nlsplice::NlSpliceFilter, trigraph::TrigraphFilter},
};

use super::comment::CommentFilter;

/// Represents stream of unicode characters that have been read from a C physical source file.
/// Successful construction indicates that these translation phases have been performed:
/// * 1: trigraph replacement
/// * 2: splicing around escaped newlines
#[derive(Clone, Eq)]
pub struct SourceFile {
    path: Rc<String>,
    src: Rc<Vec<Ch>>,
}

impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path
    }
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("path", &self.path)
            .field(
                "src",
                &String::from_iter(self.src.iter().map(|ch| ch.chat())),
            )
            .finish()
    }
}

const PATH_INLINE: &str = "<inline>";

impl SourceFile {
    fn new(path: Rc<String>, src: Rc<Vec<Ch>>) -> Self {
        Self { path, src }
    }

    pub fn path(&self) -> &str {
        self.path.as_str()
    }

    pub fn inline(input: &str) -> Result<Self, Error> {
        let path = Rc::new(PATH_INLINE.to_owned());
        let chars = CharReader::new(Box::new(input.as_bytes())).into_chars();
        Self::read(path.clone(), chars).map(|chs| Self::new(path, Rc::new(chs)))
    }

    fn read(path: Rc<String>, chars: CharReaderIter<'_>) -> Result<Vec<Ch>, Error> {
        Self::filter_source(path, chars)?.collect::<Result<Vec<_>, Error>>()
    }

    pub fn open(path: &Path) -> Result<Self, Error> {
        let path_str = Rc::new(path.display().to_string());
        let chars = CharReader::new(Box::new(File::open(path)?)).into_chars();
        Self::read(path_str.clone(), chars).map(|chs| Self::new(path_str, Rc::new(chs)))
    }

    fn filter_source(
        path: Rc<String>,
        chars: CharReaderIter<'_>,
    ) -> Result<TryingIteratorAdaptor<'_, Ch>, Error> {
        Ok(TryingIteratorAdaptor::new(CommentFilter::try_filter(
            NlSpliceFilter::try_filter(TrigraphFilter::try_filter(ChIter::new(path, chars))?)?,
        )?))
    }

    pub fn stream(&self) -> &[Ch] {
        self.src.as_slice()
    }

    pub fn len(&self) -> usize {
        self.src.len()
    }
}

impl std::ops::Index<std::ops::Range<usize>> for SourceFile {
    type Output = [Ch];

    fn index(&self, index: std::ops::Range<usize>) -> &Self::Output {
        &self.stream()[index]
    }
}

impl std::ops::Index<std::ops::RangeTo<usize>> for SourceFile {
    type Output = [Ch];

    fn index(&self, index: std::ops::RangeTo<usize>) -> &Self::Output {
        &self.stream()[index]
    }
}

impl std::ops::Index<std::ops::RangeFrom<usize>> for SourceFile {
    type Output = [Ch];

    fn index(&self, index: std::ops::RangeFrom<usize>) -> &Self::Output {
        &self.stream()[index]
    }
}

impl std::ops::Index<std::ops::RangeFull> for SourceFile {
    type Output = [Ch];

    fn index(&self, _index: std::ops::RangeFull) -> &Self::Output {
        self.stream()
    }
}

impl Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", Ch::as_string(self.stream()))
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
