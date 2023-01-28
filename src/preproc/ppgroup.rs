use std::fmt::Debug;

use crate::{error::KrangError, parse::*, punct::Punctuator, scan::*};

use super::{
    directive::*,
    id::Id,
    ppif::*,
    ppinclude::PPInclude,
    ppmacro::{PPDefineMatch, PPDefineReplace},
    pptoken::*,
    preprocessor::{PPAble, PPContext},
};

#[derive(Debug, Clone)]
pub struct PPGroup(pub PPGroupPart, pub Vec<PPGroupPart>);
impl PPAble for PPGroup {
    fn pass(&self, ctx: &PPContext) -> Result<Self, KrangError> {
        match self {
            PPGroup(head, tail) => {
                let h = head.pass(ctx)?;
                let t = tail
                    .iter()
                    .map(|p| p.pass(ctx))
                    .collect::<Result<Vec<_>, KrangError>>()?;
                Ok(Self(h, t))
            }
        }
    }
}

impl<'a> Parses<'a> for PPGroup {
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(PPGroupPart::parse_into, range(0.., PPGroupPart::parse_into)),
            |(head, tail)| Self(head, tail),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone)]
pub enum PPGroupPart {
    /// if-endif, if-else-endif, if-elif-endif, if-elif-else-endif, etc.
    IfSection(Loc, PPIf, Vec<PPElIf>, Option<PPElse>, PPEndIf),
    /// a resolved if-section for which any necessary conditions have been eval'd.
    ResolvedIf(Loc, Box<Option<PPGroup>>),
    /// control-line
    ControlLine(Loc, PPControlLine),
    /// # pp-tokens new-line
    /// The execution of a non-directive preprocessing directive results in undefined behavior.
    NonDirective(Loc, PPTokens),

    TextLine(Loc, PPTokens),
    /// variant of text line with no tokens
    EmptyLine,
}

impl<'a> PPGroupPart {
    pub fn parse_if_section(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses<'a>>::Input, Self> {
        map(
            pair(
                PPIf::parse_into,
                pair(
                    range(0.., PPElIf::parse_into),
                    pair(ok(PPElse::parse_into), PPEndIf::parse_into),
                ),
            ),
            |(ppif, (ppelifs, (ppelse, ppendif)))| {
                Self::IfSection(ppif.loc().clone(), ppif, ppelifs, ppelse, ppendif)
            },
        )
        .parse(input)
    }

    pub fn parse_control_line(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses<'a>>::Input, Self> {
        map(PPControlLine::parse_into, |dir| {
            Self::ControlLine(dir.loc().clone(), dir)
        })
        .parse(input)
    }

    pub fn parse_text_line(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses<'a>>::Input, Self> {
        or_else(
            parse_next!(PPLine::Empty => Self::EmptyLine, "<empty> not parsed"),
            parse_next!(PPLine::Text(loc, tokens) => Self::TextLine(loc.clone(), tokens.clone()), "<text> not parsed"),
        )
        .parse(input)
    }

    pub fn parse_non_directive(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses<'a>>::Input, Self> {
        parse_next!(PPLine::NonDirective(loc, tokens) => Self::NonDirective(loc.clone(), tokens.clone()), "#<non> not parsed").parse(input)
    }
}
impl<'a> Parses<'a> for PPGroupPart {
    type Input = &'a [PPLine];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            Self::parse_if_section,
            or_else(
                Self::parse_control_line,
                or_else(Self::parse_non_directive, Self::parse_text_line),
            ),
        )
        .parse(input)
    }
}

impl PPAble for PPGroupPart {
    fn pass(&self, ctx: &PPContext) -> Result<Self, KrangError> {
        match self {
            Self::IfSection(_, ppif, ppelifs, ppelse, ppendif) => {
                let ppif2 = ppif.pass(ctx)?;
                if ppif2.eval_condition(ctx)? {
                    return Ok(Self::ResolvedIf(
                        ppif2.loc().clone(),
                        Box::new(ppif2.group().clone()),
                    ));
                }
                for ppelif in ppelifs.iter() {
                    let ppelif2 = ppelif.pass(ctx)?;
                    if ppelif2.eval_condition(ctx)? {
                        return Ok(Self::ResolvedIf(
                            ppelif2.loc().clone(),
                            Box::new(ppelif2.group().clone()),
                        ));
                    }
                }
                if let Some(ppelse) = ppelse {
                    let ppelse2 = ppelse.pass(ctx)?;
                    return Ok(Self::ResolvedIf(
                        ppelse2.loc().clone(),
                        Box::new(ppelse2.group().clone()),
                    ));
                }
                return Ok(Self::ResolvedIf(ppendif.loc().clone(), Box::new(None)));
            }
            Self::ControlLine(loc, line) => {
                let line2 = line.pass(ctx)?;
                Ok(Self::ControlLine(loc.clone(), line2))
            }
            _ => Ok(self.clone()),
        }
    }
}
