use std::fmt::Display;
use std::rc::Rc;

use crate::parse::*;
use crate::scan::*;

use super::keyword::Keyword;
use super::pptoken::Ucn;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id(pub String);

impl Id {
    pub fn new(value: String) -> Self {
        Self(value)
    }

    fn new_internal(value: String) -> Self {
        Self(value)
    }

    pub fn try_new(value: &str) -> Result<Id, &str> {
        if Keyword::is_reserved(value) {
            Err(value)
        } else {
            Ok(Self::new_internal(value.to_owned()))
        }
    }

    pub fn copy(&self) -> Self {
        Self::new(self.0.to_owned())
    }

    pub fn is_ident_lead(c: &char) -> bool {
        c.is_ascii_alphabetic() || c == &'_'
    }

    pub fn is_ident(c: &char) -> bool {
        Self::is_ident_lead(c) || c.is_ascii_digit()
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for Id {
    fn from(value: &str) -> Self {
        Self::new(value.to_owned())
    }
}

impl<'a> Parses<'a> for Id {
    type Context = Rc<bool>;
    type Input = &'a [Ch];
    fn parse_into(ctx: Self::Context, input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        msg(
            map(
                map(
                    pair(
                        pred(any_char, Self::is_ident_lead),
                        range(
                            0..,
                            max(
                                map(Ucn::parse_into, |ucn| ucn.as_char()),
                                pred(any_char, Self::is_ident),
                            ),
                        ),
                    ),
                    |(l, cs)| vec![vec![l], cs].concat(),
                ),
                |chars| Self::new(chars.into_iter().collect()),
            ),
            "Id not matched",
        )
        .parse(ctx, input)
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[cfg(test)]
mod tests {
    use std::io::Error;

    use crate::source::file::SourceFile;

    use super::*;
    #[test]
    fn ids() -> Result<(), Error> {
        let class1 = SourceFile::inline("Class1")?;
        let one_class = SourceFile::inline("1Class")?;
        let one2345 = SourceFile::inline("12345")?;
        let my_class = SourceFile::inline("MyClass")?;
        let parser = Id::parse_into;

        assert_eq!(
            Ok((&my_class[my_class.len()..], Id("MyClass".to_owned()))),
            parser.parse(Rc::new(true), my_class.stream())
        );
        assert_eq!(
            Err(("Id not matched".to_owned(), one2345.stream())),
            parser.parse(Rc::new(true), one2345.stream())
        );
        assert_eq!(
            Err(("Id not matched".to_owned(), one_class.stream())),
            parser.parse(Rc::new(true), one_class.stream())
        );
        assert_eq!(
            Ok((&class1.stream()[class1.len()..], Id("Class1".to_owned()))),
            parser.parse(Rc::new(true), class1.stream())
        );
        Ok(())
    }
}
