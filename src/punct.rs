use std::rc::Rc;

use crate::{parse::*, scan::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punctuator {
    /// [
    LSqu,
    /// ]
    RSqu,
    /// (
    LRnd,
    /// )
    RRnd,
    /// {
    LCur,
    /// }
    RCur,
    /// .
    Dot,
    /// ->
    RArrow2,
    /// ++
    Inc2,
    /// --
    Dec2,
    /// &
    And,
    /// *
    Mult,
    /// +
    Add,
    /// -
    Sub,
    /// ~
    Tilde,
    /// !
    Not,
    /// /
    FSlash,
    /// %
    Mod,
    /// <<
    LShift2,
    /// >>
    RShift2,
    /// <
    LAng,
    /// >
    RAng,
    /// <=
    Lte2,
    /// >=
    Gte2,
    /// ==
    Eq2,
    /// !=
    NotEq2,
    /// ^
    Xor,
    /// |
    Or,
    /// &&
    And2,
    /// ||
    Or2,
    /// ?
    Quest,
    /// :
    Colon,
    /// ;
    Semi,
    /// ...
    Elips3,
    /// =
    Eq,
    /// *=
    MultEq2,
    /// /=
    DivEq2,
    /// %=
    ModEq2,
    /// +=
    AddEq2,
    /// -=
    SubEq2,
    /// <<=
    LShiftEq3,
    /// >>=
    RShiftEq3,
    /// &=
    AndEq2,
    /// ^=
    XorEq2,
    /// |=
    OrEq2,
    /// ,
    Comma,
    /// #
    Sharp,
    /// ##
    Sharps2,
    /// <: equiv [
    LSquAlt2,
    /// :> equiv ]
    RSquAlt2,
    /// <% equiv {
    LCurAlt2,
    /// %> equiv }
    RCurAlt2,
    /// %: equiv #
    SharpAlt2,
    /// %:%: equiv ##
    SharpsAlt4,
}

impl Punctuator {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::LSqu => "[",
            Self::RSqu => "]",
            Self::LRnd => "(",
            Self::RRnd => ")",
            Self::LCur => "{",
            Self::RCur => "}",
            Self::Dot => ".",
            Self::RArrow2 => "->",
            Self::Inc2 => "++",
            Self::Dec2 => "--",
            Self::And => "&",
            Self::Mult => "*",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Tilde => "~",
            Self::Not => "!",
            Self::FSlash => "/",
            Self::Mod => "%",
            Self::LShift2 => "<<",
            Self::RShift2 => ">>",
            Self::LAng => "<",
            Self::RAng => ">",
            Self::Lte2 => "<=",
            Self::Gte2 => ">=",
            Self::Eq2 => "==",
            Self::NotEq2 => "!=",
            Self::Xor => "^",
            Self::Or => "|",
            Self::And2 => "&&",
            Self::Or2 => "||",
            Self::Quest => "?",
            Self::Colon => ":",
            Self::Semi => ";",
            Self::Elips3 => "...",
            Self::Eq => "=",
            Self::MultEq2 => "*=",
            Self::DivEq2 => "/=",
            Self::ModEq2 => "%=",
            Self::AddEq2 => "+=",
            Self::SubEq2 => "-=",
            Self::LShiftEq3 => "<<=",
            Self::RShiftEq3 => ">>=",
            Self::AndEq2 => "&=",
            Self::XorEq2 => "^=",
            Self::OrEq2 => "|=",
            Self::Comma => ",",
            Self::Sharp => "#",
            Self::Sharps2 => "##",
            Self::LSquAlt2 => "<:",
            Self::RSquAlt2 => ":>",
            Self::LCurAlt2 => "<%",
            Self::RCurAlt2 => "%>",
            Self::SharpAlt2 => "%:",
            Self::SharpsAlt4 => "%:%:",
        }
    }
    pub fn singles() -> Vec<Self> {
        vec![
            Self::LSqu,
            Self::RSqu,
            Self::LRnd,
            Self::RRnd,
            Self::LCur,
            Self::RCur,
            Self::Dot,
            Self::And,
            Self::Mult,
            Self::Add,
            Self::Sub,
            Self::Tilde,
            Self::Not,
            Self::FSlash,
            Self::Mod,
            Self::LAng,
            Self::RAng,
            Self::Xor,
            Self::Or,
            Self::Quest,
            Self::Colon,
            Self::Semi,
            Self::Eq,
            Self::Comma,
            Self::Sharp,
        ]
    }
    pub fn doubles() -> Vec<Self> {
        vec![
            Self::RArrow2,
            Self::Inc2,
            Self::Dec2,
            Self::LShift2,
            Self::RShift2,
            Self::Lte2,
            Self::Gte2,
            Self::Eq2,
            Self::NotEq2,
            Self::And2,
            Self::Or2,
            Self::MultEq2,
            Self::DivEq2,
            Self::ModEq2,
            Self::AddEq2,
            Self::SubEq2,
            Self::AndEq2,
            Self::XorEq2,
            Self::OrEq2,
            Self::Sharps2,
            Self::LSquAlt2,
            Self::RSquAlt2,
            Self::LCurAlt2,
            Self::RCurAlt2,
            Self::SharpAlt2,
        ]
    }
    pub fn triples() -> Vec<Self> {
        vec![Self::Elips3, Self::LShiftEq3, Self::RShiftEq3]
    }
    pub fn fourples() -> Vec<Self> {
        vec![Self::SharpsAlt4]
    }
    pub fn all() -> Vec<Self> {
        vec![
            Self::LSqu,
            Self::RSqu,
            Self::LRnd,
            Self::RRnd,
            Self::LCur,
            Self::RCur,
            Self::Dot,
            Self::RArrow2,
            Self::Inc2,
            Self::Dec2,
            Self::And,
            Self::Mult,
            Self::Add,
            Self::Sub,
            Self::Tilde,
            Self::Not,
            Self::FSlash,
            Self::Mod,
            Self::LShift2,
            Self::RShift2,
            Self::LAng,
            Self::RAng,
            Self::Lte2,
            Self::Gte2,
            Self::Eq2,
            Self::NotEq2,
            Self::Xor,
            Self::Or,
            Self::And2,
            Self::Or2,
            Self::Quest,
            Self::Colon,
            Self::Semi,
            Self::Elips3,
            Self::Eq,
            Self::MultEq2,
            Self::DivEq2,
            Self::ModEq2,
            Self::AddEq2,
            Self::SubEq2,
            Self::LShiftEq3,
            Self::RShiftEq3,
            Self::AndEq2,
            Self::XorEq2,
            Self::OrEq2,
            Self::Comma,
            Self::Sharp,
            Self::Sharps2,
            Self::LSquAlt2,
            Self::RSquAlt2,
            Self::LCurAlt2,
            Self::RCurAlt2,
            Self::SharpAlt2,
            Self::SharpsAlt4,
        ]
    }

    pub fn is_digraph(&self) -> bool {
        self != &self.to_unigraph()
    }

    pub fn to_unigraph(&self) -> Self {
        match self {
            Self::LSquAlt2 => Self::LSqu,
            Self::RSquAlt2 => Self::RSqu,
            Self::LCurAlt2 => Self::LCur,
            Self::RCurAlt2 => Self::RCur,
            Self::SharpAlt2 => Self::Sharp,
            Self::SharpsAlt4 => Self::Sharps2,
            _ => *self,
        }
    }

    pub fn is_equiv(&self, other: &Self) -> bool {
        self.to_unigraph() == other.to_unigraph()
    }
}

impl<'a> Parser<'a, &'a [Ch], Punctuator, Rc<bool>> for Punctuator {
    fn parse(&self, ctx: Rc<bool>, input: &'a [Ch]) -> ParseResult<'a, &'a [Ch], Punctuator> {
        map(match_literal(self.as_str()), |()| *self).parse(ctx, input)
    }
}

impl<'a> Parses<'a> for Punctuator {
    type Context = Rc<bool>;
    type Input = &'a [Ch];

    fn parse_into(
        ctx: Self::Context,
        input: Self::Input,
    ) -> crate::parse::ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        msg(
            |c, inp| {
                let init_parser: Box<dyn Parser<'a, &'a [Ch], Self, Self::Context>> =
                    Box::new(none("unexpected error in Punctuator::parse_into"));
                vec![
                    Self::singles(),
                    Self::doubles(),
                    Self::triples(),
                    Self::fourples(),
                ]
                .concat()
                .iter()
                .fold(
                    init_parser,
                    |acc, punct| -> Box<dyn Parser<&[Ch], Self, Self::Context>> {
                        Box::new(or_else(
                            move |ctx, input| punct.parse(ctx, input),
                            // acc on the right-hand side means [doubles, singles], [triples, doubles], [fourples, triples]
                            move |ctx, input| acc.parse(ctx, input),
                        ))
                    },
                )
                .parse(c, inp)
            },
            "Punctuator not matched",
        )
        .parse(ctx, input)
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Error, rc::Rc};

    use crate::{parse::*, source::file::SourceFile};

    use super::Punctuator;

    #[test]
    fn check_lengths() {
        for punct in Punctuator::fourples() {
            assert_eq!(4, punct.as_str().len());
        }
        for punct in Punctuator::triples() {
            assert_eq!(3, punct.as_str().len());
        }
        for punct in Punctuator::doubles() {
            assert_eq!(2, punct.as_str().len());
        }
        for punct in Punctuator::singles() {
            assert_eq!(1, punct.as_str().len());
        }
    }

    #[test]
    fn parse_precedence() -> Result<(), Error> {
        let alt_sharps = SourceFile::inline("%:%:")?;
        let alt_sharp = SourceFile::inline("%:")?;
        let parser = Punctuator::parse_into;
        assert_eq!(
            Ok((&alt_sharp[alt_sharp.len()..], Punctuator::SharpAlt2)),
            parser.parse(Rc::new(true), alt_sharp.stream())
        );
        assert_eq!(
            Ok((&alt_sharps[alt_sharps.len()..], Punctuator::SharpsAlt4)),
            parser.parse(Rc::new(true), alt_sharps.stream())
        );
        Ok(())
    }
}
