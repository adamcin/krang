use crate::{error::KrangError, parse::*, scan::Loc};

use super::{
    directive::{match_directive, PPGroup, ParsesPPLine},
    expression::PPExpression,
    id::Id,
    pptoken::*,
    preprocessor::{PPAble, PPContext},
};

#[derive(Debug, Clone)]
pub enum PPIfLine {
    If(Loc, PPExpression),
    IfDef(Loc, Id),
    IfNotDef(Loc, Id),
}
impl<'a> PPIfLine {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self::If(loc, ..) | Self::IfDef(loc, ..) | Self::IfNotDef(loc, ..) => loc,
        }
    }

    /// evaluate the controlling condition and return true if the group should be processed
    pub fn eval_condition(&self, ctx: &PPContext) -> Result<bool, KrangError> {
        match self {
            Self::If(..) => Ok(true),
            Self::IfDef(..) => Ok(true),
            Self::IfNotDef(..) => Ok(false),
        }
    }

    pub fn parse_if(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(match_directive("if"), PPTokens::parse_into),
            |(pos, expr)| Self::If(pos.clone(), PPExpression::Unparsed(expr)),
        )
        .parse(input)
    }

    pub fn parse_ifdef(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(
                match_directive("ifdef"),
                parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"),
            ),
            |(pos, id)| Self::IfDef(pos.clone(), id.clone()),
        )
        .parse(input)
    }

    pub fn parse_ifndef(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(
                match_directive("ifndef"),
                parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"),
            ),
            |(pos, id)| Self::IfNotDef(pos.clone(), id.clone()),
        )
        .parse(input)
    }
}

impl<'a> Parses<'a> for PPIfLine {
    type Input = &'a [PPToken];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        max(Self::parse_ifndef, max(Self::parse_ifdef, Self::parse_if)).parse(input)
    }
}
impl<'a> ParsesPPLine<'a> for PPIfLine {}

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
        none("").parse(input)
    }
}

impl PPAble for PPIf {}

#[derive(Debug, Clone)]
pub struct PPElIfLine(Loc, PPExpression);
impl<'a> PPElIfLine {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(loc, _) => loc,
        }
    }

    /// evaluate the controlling condition and return true if the nested group should be processed
    pub fn eval_condition(&self, ctx: &PPContext) -> Result<bool, KrangError> {
        Ok(false)
    }
}
impl<'a> Parses<'a> for PPElIfLine {
    type Input = &'a [PPToken];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(match_directive("elif"), PPTokens::parse_into),
            |(pos, expr)| Self(pos.clone(), PPExpression::Unparsed(expr)),
        )
        .parse(input)
    }
}
impl<'a> ParsesPPLine<'a> for PPElIfLine {}

#[derive(Debug, Clone)]
pub struct PPElIf(pub PPElIfLine, pub Box<Option<PPGroup>>);
impl<'a> Parses<'a> for PPElIf {
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(PPElIfLine::parse_line, ok(PPGroup::parse_into)),
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
pub struct PPElseLine(Loc);
impl<'a> PPElseLine {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(loc) => loc,
        }
    }
}

impl<'a> Parses<'a> for PPElseLine {
    type Input = &'a [PPToken];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            left(match_directive("else"), ok(match_next!(PPToken::HSpace(_)))),
            |pos| Self(pos.clone()),
        )
        .parse(input)
    }
}
impl<'a> ParsesPPLine<'a> for PPElseLine {}

#[derive(Debug, Clone)]
pub struct PPElse(pub PPElseLine, Box<Option<PPGroup>>);
impl<'a> Parses<'a> for PPElse {
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(PPElseLine::parse_line, ok(PPGroup::parse_into)),
            |(line, group)| Self(line, Box::new(group)),
        )
        .parse(input)
    }
}
impl<'a> PPElse {
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
}
impl PPAble for PPElse {}

#[derive(Debug, Clone)]
pub struct PPEndIf(Loc);
impl<'a> Parses<'a> for PPEndIf {
    type Input = &'a [PPToken];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        free(
            map(
                left(
                    match_directive("endif"),
                    ok(match_next!(PPToken::HSpace(_))),
                ),
                |loc| Self(loc.clone()),
            ),
            "@ PPEndIf",
        )
        .parse(input)
    }
}

impl<'a> PPEndIf {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(loc) => loc,
        }
    }
}

impl<'a> ParsesPPLine<'a> for PPEndIf {}
