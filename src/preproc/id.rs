use std::fmt::Display;

use crate::common::err_invalid_input;
use crate::parse::*;
use crate::source::token::{any_char, Atoms};

use super::keyword::Keyword;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id(String);

impl Id {
    pub fn new(value: String) -> Self {
        if Keyword::is_reserved(value.as_str()) {
            panic!("Id is reserved keyword: {}", value.as_str());
        }
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

    fn is_ident_lead(c: &char) -> bool {
        c.is_ascii_alphabetic() || c == &'_'
    }

    fn is_ident(c: &char) -> bool {
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
    type Input = &'a Atoms<'a>;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        and_then(
            msg(
                map(
                    map(
                        pair(
                            pred(any_char, Self::is_ident_lead),
                            range(pred(any_char, Self::is_ident), 0..),
                        ),
                        |(l, cs)| vec![vec![l], cs].concat(),
                    ),
                    |chars| chars.into_iter().collect(),
                ),
                "Id not matched",
            ),
            |s: String| {
                if Keyword::is_reserved(s.as_str()) {
                    Err(format!("{} is a reserved keyword", s))
                } else {
                    Ok(Self::new(s))
                }
            },
        )
        .parse(input)
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
            parser.parse(my_class.stream())
        );
        assert_eq!(
            Err(("Id not matched".to_owned(), one2345.stream())),
            parser.parse(one2345.stream())
        );
        assert_eq!(
            Err(("Id not matched".to_owned(), one_class.stream())),
            parser.parse(one_class.stream())
        );
        assert_eq!(
            Ok((&class1.stream()[class1.len()..], Id("Class1".to_owned()))),
            parser.parse(class1.stream())
        );
        Ok(())
    }
}
