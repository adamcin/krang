use std::fmt::Display;

use crate::{
    parse::*,
    source::token::{match_literal, Atoms},
};

use super::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    _Alignas,
    _Alignof,
    _Atomic,
    _Bool,
    _Complex,
    _Generic,
    _Imaginary,
    _Noreturn,
    _StaticAssert,
    _ThreadLocal,
}
impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Auto => "auto",
            Self::Break => "break",
            Self::Case => "case",
            Self::Char => "char",
            Self::Const => "const",
            Self::Continue => "continue",
            Self::Default => "default",
            Self::Do => "do",
            Self::Double => "double",
            Self::Else => "else",
            Self::Enum => "enum",
            Self::Extern => "extern",
            Self::Float => "float",
            Self::For => "for",
            Self::Goto => "goto",
            Self::If => "if",
            Self::Inline => "inline",
            Self::Int => "int",
            Self::Long => "long",
            Self::Register => "register",
            Self::Restrict => "restrict",
            Self::Return => "return",
            Self::Short => "short",
            Self::Signed => "signed",
            Self::Sizeof => "sizeof",
            Self::Static => "static",
            Self::Struct => "struct",
            Self::Switch => "switch",
            Self::Typedef => "typedef",
            Self::Union => "union",
            Self::Unsigned => "unsigned",
            Self::Void => "void",
            Self::Volatile => "volatile",
            Self::While => "while",
            Self::_Alignas => "_Alignas",
            Self::_Alignof => "_Alignof",
            Self::_Atomic => "_Atomic",
            Self::_Bool => "_Bool",
            Self::_Complex => "_Complex",
            Self::_Generic => "_Generic",
            Self::_Imaginary => "_Imaginary",
            Self::_Noreturn => "_Noreturn",
            Self::_StaticAssert => "_StaticAssert",
            Self::_ThreadLocal => "_ThreadLocal",
        }
    }

    pub fn all() -> Vec<Self> {
        use Keyword::*;
        vec![
            Auto,
            Break,
            Case,
            Char,
            Const,
            Continue,
            Default,
            Do,
            Double,
            Else,
            Enum,
            Extern,
            Float,
            For,
            Goto,
            If,
            Inline,
            Int,
            Long,
            Register,
            Restrict,
            Return,
            Short,
            Signed,
            Sizeof,
            Static,
            Struct,
            Switch,
            Typedef,
            Union,
            Unsigned,
            Void,
            Volatile,
            While,
            _Alignas,
            _Alignof,
            _Atomic,
            _Bool,
            _Complex,
            _Generic,
            _Imaginary,
            _Noreturn,
            _StaticAssert,
            _ThreadLocal,
        ]
    }

    pub fn is_reserved(token: &str) -> bool {
        Self::all().iter().any(|k| k.as_str() == token)
    }

    pub fn as_token(self) -> Token {
        Token::Keyword(self)
    }
}

impl<'a> Parser<'a, &'a Atoms<'a>, Keyword> for Keyword {
    fn parse(&self, input: &'a Atoms<'a>) -> ParseResult<'a, &'a Atoms<'a>, Keyword> {
        map(match_literal(self.as_str()), |()| *self).parse(input)
    }
}

impl<'a> Parser<'a, &'a [Token], Keyword> for Keyword {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, &'a [Token], Keyword> {
        match input.split_first() {
            Some((Token::Keyword(value), rem)) if value == self => Ok((rem, *self)),
            _ => Err(("Keyword not matched".to_owned(), input)),
        }
    }
}

impl<'a> Parses<'a> for Keyword {
    type Input = &'a Atoms<'a>;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        let init_parser: Box<dyn Parser<'a, &'a Atoms<'a>, Self>> =
            Box::new(none("unexpected error in Keyword::parse_into"));

        Self::all()
            .iter()
            .fold(
                init_parser,
                |acc, keyword| -> Box<dyn Parser<&Atoms<'a>, Self>> {
                    Box::new(or_else(
                        move |input| keyword.parse(input),
                        move |input| acc.parse(input),
                    ))
                },
            )
            .parse(input)
    }
}

impl Display for Keyword {
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
    fn keywords() -> Result<(), Error> {
        let str_auto = SourceFile::inline("auto")?;
        assert_eq!(
            Ok((&str_auto[str_auto.len()..], Keyword::Auto)),
            Keyword::Auto.parse(str_auto.stream())
        );
        Ok(())
    }
}
