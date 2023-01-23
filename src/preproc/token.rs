use std::fmt::Display;

use crate::{parse::*, scan::*};

use super::{id::Id, keyword::Keyword, pptoken::PPToken};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntConst(i16);
impl IntConst {
    pub fn new(value: i16) -> Result<Self, String> {
        if value < 0 {
            Err("value must be >= 0".to_owned())
        } else {
            Ok(IntConst(value))
        }
    }

    pub fn value(&self) -> i16 {
        self.0
    }

    pub fn zero() -> Self {
        Self(0)
    }

    pub fn one() -> Self {
        Self(1)
    }

    pub fn copy(&self) -> Self {
        Self(self.0)
    }
}
impl<'a> Parses<'a> for IntConst {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        and_then(
            map(range(1.., digit_char()), |chars| -> String {
                chars.into_iter().collect()
            }),
            |token| {
                token
                    .as_str()
                    .parse::<i16>()
                    .map_err(|err| err.to_string())
                    .and_then(Self::new)
            },
        )
        .parse(input)
    }
}

impl TryFrom<i16> for IntConst {
    type Error = String;
    fn try_from(value: i16) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringConst(Vec<u8>);
impl StringConst {
    pub fn new(value: String) -> Self {
        Self(
            value
                .chars()
                .filter(|c| c.is_ascii())
                .map(|c| c as u8)
                .collect(),
        )
    }
    pub fn copy(&self) -> Self {
        Self(self.0.to_owned())
    }

    pub fn value(&self) -> String {
        self.0.iter().map(|i| *i as char).collect()
    }

    pub fn len(&self) -> i16 {
        (self.0).len() as i16
    }

    pub fn chars(&self) -> Vec<i16> {
        (self.0).iter().map(|u| *u as i16).collect()
    }
}
impl<'a> Parses<'a> for StringConst {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self> {
        map(
            right(
                match_literal("\""),
                left(
                    map(until(non_nl_char(), 0.., "\""), |chars| -> String {
                        chars.into_iter().collect()
                    }),
                    match_literal("\""),
                ),
            ),
            Self::new,
        )
        .parse(input)
    }
}

impl From<&str> for StringConst {
    fn from(item: &str) -> Self {
        Self::new(item.to_owned())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Keyword(Loc, Keyword),
    Identifier(Loc, Id),
}
impl Token {
    pub fn key(input: &[Self], filter: Keyword) -> ParseResult<'_, &[Self], Keyword> {
        match input.split_first() {
            Some((Self::Keyword(loc, value), rem)) if value == &filter => Ok((rem, *value)),
            _ => Err((("Token.key not matched".to_owned(), input), Vec::new())),
        }
    }

    pub fn id(input: &[Self]) -> ParseResult<'_, &[Self], Id> {
        match input.split_first() {
            Some((Self::Identifier(loc, value), rem)) => Ok((rem, value.copy())),
            _ => Err((("Token.id not matched".to_owned(), input), Vec::new())),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenStream {
    tokens: Vec<Token>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }
}

impl TokenStream {
    pub fn tokens(&self) -> &[Token] {
        self.tokens.as_slice()
    }
}

// impl<'a> Parses<'a> for TokenStream<'a> {
//     type Input = &'a [PPToken<'a>];
//     fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
//     where
//         Self::Input: 'a,
//     {
//         map(
//             range(0.., right(ok(space0()), Token::parse_into)),
//             Self::new,
//         )
//         .parse(input)
//     }
// }

impl Display for TokenStream {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn string_const() {
    //     assert_eq!(
    //         Ok(("", StringConst::new("a string".to_owned()))),
    //         StringConst::parse_into("\"a string\"")
    //     );
    //     assert_eq!(Err("a string"), StringConst::parse_into("a string"));
    // }

    // #[test]
    // fn class_tokens() {
    //     assert_eq!(
    //         Ok((
    //             " ",
    //             TokenStream::new(vec![
    //                 Keyword::Class.into(),
    //                 Id::from("MyClass").into(),
    //                 Sym::LCurly.into(),
    //                 Keyword::Static.into(),
    //                 Keyword::Int.into(),
    //                 Id::from("count").into(),
    //                 Sym::Semi.into(),
    //                 Sym::RCurly.into(),
    //                 Id::from("extra").into(),
    //             ])
    //         )),
    //         TokenStream::parse_into(
    //             r"class MyClass {
    //             static int count// some inline comment
    //             /*
    //             some multiline comment;
    //              */
    //             ;

    //         } extra "
    //         )
    //     );
    // }

    // #[test]
    // fn int_const() {
    //     assert_eq!(Ok(("", IntConst(0))), IntConst::parse_into("0"));
    //     assert_eq!(Ok(("", IntConst(12345))), IntConst::parse_into("12345"));
    //     assert_eq!(Ok(("", IntConst(32767))), IntConst::parse_into("32767"));
    //     assert_eq!(Err("-1"), IntConst::parse_into("-1"));
    //     assert_eq!(Err("32768"), IntConst::parse_into("32768"));
    //     assert_eq!(Err("123456"), IntConst::parse_into("123456"));
    // }
}
