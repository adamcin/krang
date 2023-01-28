use crate::{error::KrangError, parse::*, scan::Loc};

use super::{
    directive::*,
    expression::PPExpression,
    id::Id,
    ppgroup::*,
    pptoken::*,
    preprocessor::{PPAble, PPContext},
};

#[derive(Debug, Clone)]
pub struct PPIf(pub PPIfLine, pub Box<Option<PPGroup>>);
impl<'a> PPIf {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(line, _) => line.loc(),
        }
    }

    pub fn group(&'a self) -> &'a Option<PPGroup> {
        match self {
            Self(_, group) => group.as_ref(),
        }
    }

    pub fn eval_condition(&self, ctx: &PPContext) -> Result<bool, KrangError> {
        match self {
            Self(line, _) => line.eval_condition(ctx),
        }
    }
}
impl<'a> Parses<'a> for PPIf {
    type Input = &'a [PPLine];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(
                parse_next!(PPLine::If(loc, if_line) => if_line.clone(), "#if line not parsed"),
                ok(PPGroup::parse_into),
            ),
            |(line, group)| Self(line, Box::new(group)),
        )
        .parse(input)
    }
}

impl PPAble for PPIf {}

#[derive(Debug, Clone)]
pub struct PPElIf(pub PPElIfLine, pub Box<Option<PPGroup>>);
impl<'a> Parses<'a> for PPElIf {
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(PPElIfLine::parse_into, ok(PPGroup::parse_into)),
            |(line, group)| Self(line, Box::new(group)),
        )
        .parse(input)
    }
}
impl<'a> PPElIf {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(line, _) => line.loc(),
        }
    }

    pub fn group(&'a self) -> &'a Option<PPGroup> {
        match self {
            Self(_, group) => group.as_ref(),
        }
    }

    /// evaluate the controlling condition and return true if the nested group should be processed
    pub fn eval_condition(&self, ctx: &PPContext) -> Result<bool, KrangError> {
        Ok(false)
    }
}
impl PPAble for PPElIf {}

#[derive(Debug, Clone)]
pub struct PPElse(pub Loc, Box<Option<PPGroup>>);
impl<'a> Parses<'a> for PPElse {
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(
                parse_next!(PPLine::Else(loc) => loc.clone(), "#else not parsed"),
                ok(PPGroup::parse_into),
            ),
            |(loc, group)| Self(loc, Box::new(group)),
        )
        .parse(input)
    }
}
impl<'a> PPElse {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(loc, _) => loc,
        }
    }

    pub fn group(&'a self) -> &'a Option<PPGroup> {
        match self {
            Self(_, group) => group.as_ref(),
        }
    }
}
impl PPAble for PPElse {}
