use std::rc::Rc;

use crate::{error::KrangError, parse::*, scan::Loc};

use super::{
    expression::PPExpression, id::Id, ppcontext::PPContext, ppgroup::*, ppline::*, pptoken::*,
    preprocessor::PPAble,
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
    type Context = Rc<bool>;
    type Input = &'a [PPLine];
    fn parse_into(ctx: Self::Context, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(
                or_else(
                    parse_next!(PPLine::Sharp(PPDirective::If, lead, PPTokens(_, tokens)) => PPIfLine::If(lead.loc().clone(), tokens.to_vec()), "#if line not parsed"),
                    or_else(
                        parse_next!(PPLine::Sharp(PPDirective::IfDef, lead, PPTokens(_, tokens)) => PPIfLine::IfDef(lead.loc().clone(), tokens.to_vec()), "#ifdef line not parsed"),
                        parse_next!(PPLine::Sharp(PPDirective::IfNotDef, lead, PPTokens(_, tokens)) => PPIfLine::IfNotDef(lead.loc().clone(), tokens.to_vec()), "#ifndef line not parsed"),
                    ),
                ),
                ok(PPGroup::parse_into),
            ),
            |(line, group)| Self(line, Box::new(group)),
        )
        .parse(ctx, input)
    }
}

impl PPAble for PPIf {}

#[derive(Debug, Clone)]
pub struct PPElIf(pub PPElIfLine, pub Box<Option<PPGroup>>);
impl<'a> Parses<'a> for PPElIf {
    type Context = Rc<bool>;
    type Input = &'a [PPLine];

    fn parse_into(ctx: Self::Context, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(PPElIfLine::parse_into, ok(PPGroup::parse_into)),
            |(line, group)| Self(line, Box::new(group)),
        )
        .parse(ctx, input)
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
    type Context = Rc<bool>;
    type Input = &'a [PPLine];

    fn parse_into(ctx: Self::Context, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(
                parse_next!(PPLine::Sharp(PPDirective::Else, lead, ..) => lead.loc().clone(), "#else not parsed"),
                ok(PPGroup::parse_into),
            ),
            |(loc, group)| Self(loc, Box::new(group)),
        )
        .parse(ctx, input)
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

#[derive(Debug, Clone)]
pub enum PPIfLine {
    If(Loc, Vec<PPToken>),
    IfDef(Loc, Vec<PPToken>),
    IfNotDef(Loc, Vec<PPToken>),
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
}

#[derive(Debug, Clone)]
pub struct PPElIfLine(Loc, Vec<PPToken>);
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
    type Context = Rc<bool>;
    type Input = &'a [PPLine];
    fn parse_into(ctx: Self::Context, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        parse_next!(PPLine::Sharp(PPDirective::ElseIf, lead, PPTokens(_, tokens)) => Self(lead.loc().clone(), tokens.to_vec()), "#elif not parsed").parse(ctx, input)
    }
}

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
    type Context = Rc<bool>;
    type Input = &'a [PPLine];
    fn parse_into(ctx: Self::Context, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        parse_next!(PPLine::Sharp(PPDirective::Else, lead, ..) => Self(lead.loc().clone()), "#else not parsed").parse(ctx, input)
    }
}

#[derive(Debug, Clone)]
pub struct PPEndIf(Loc);
impl<'a> PPEndIf {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(loc) => loc,
        }
    }
}
impl<'a> Parses<'a> for PPEndIf {
    type Context = Rc<bool>;
    type Input = &'a [PPLine];
    fn parse_into(ctx: Self::Context, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        parse_next!(PPLine::Sharp(PPDirective::EndIf, lead, ..) => Self(lead.loc().clone()), "#endif not parsed").parse(ctx, input)
    }
}
