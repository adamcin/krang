use std::fmt::Debug;

use crate::{error::KrangError, parse::*, punct::Punctuator, scan::*};

use super::{
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
                    pair(ok(PPElse::parse_into), PPEndIf::parse_line),
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
        map(PPControlLine::parse_line, |dir| {
            Self::ControlLine(dir.loc().clone(), dir)
        })
        .parse(input)
    }

    pub fn parse_text_line(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses<'a>>::Input, Self> {
        free(
            map(single(), |line: &'a PPLine| match line.as_tokens() {
                Some(tokens) => Self::TextLine(tokens.loc().clone(), tokens),
                None => Self::EmptyLine,
            }),
            "text",
        )
        .parse(input)
    }

    pub fn parse_non_directive(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses<'a>>::Input, Self> {
        and_then_input(
            single(),
            |PPLine(line), next_input| match Self::parse_non_directive_tokens(line) {
                Ok((rem, ret)) if rem.is_empty() => Ok(ret),
                Ok((rem, ret)) => Err(format!("line not fully matched {:?} => {:?}", rem, ret)),
                Err(((err, err_i), _)) => Err(format!("line not matched ({}): {:?}", err, err_i)),
            },
        )
        .parse(input)
    }

    pub fn parse_non_directive_tokens(
        input: &'a [PPToken],
    ) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(left(parse_next!(PPToken::Punct(pos, Punctuator::Sharp) => pos, "non-directive not matched"), 
                match_next!(PPToken::HSpace(_))), PPTokens::parse_into),
            |(pos, tokens)| Self::NonDirective(pos.clone(), tokens),
        )
        .parse(input)
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

pub fn match_directive<'a>(name: &'static str) -> impl Parser<'a, &'a [PPToken], &'a Loc> {
    move |input| {
        free(left(
            left(
                left(parse_next!(PPToken::Punct(pos, Punctuator::Sharp) => pos, "directive not matched"), ok(match_next!(PPToken::HSpace(_)))),
                match_next!(PPToken::Id(_, Id(value)) if value.as_str() == name),
            ),
            ok(match_next!(PPToken::HSpace(_))),
        ), name)
        .parse(input)
    }
}

pub trait ParsesPPLine<'a> {
    fn parse_line(input: &'a [PPLine]) -> ParseResult<'a, &'a [PPLine], Self>
    where
        Self: Parses<'a, Input = &'a [PPToken]> + Debug,
    {
        and_then_input(
            single(),
            |PPLine(line), next_input| match Self::parse_into(line) {
                Ok((rem, ret)) if rem.is_empty() => Ok(ret),
                Ok((rem, ret)) => Err(format!("line not fully matched {:?} => {:?}", rem, ret)),
                Err(((err, err_i), _)) => Err(format!("line not matched ({}): {:?}", err, err_i)),
            },
        )
        .parse(input)
    }
}

#[derive(Debug, Clone)]
pub enum PPControlLine {
    Include(Loc, PPInclude),
    Define(Loc, PPDefineMatch, PPDefineReplace),
    Undef(Loc, Id),
    Line(Loc, PPLineForm),
    Error(Loc, Option<PPTokens>),
    Pragma(Loc, PPPragmaForm),
    Null(Loc),
}
impl<'a> PPControlLine {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self::Include(loc, ..)
            | Self::Define(loc, ..)
            | Self::Undef(loc, ..)
            | Self::Line(loc, ..)
            | Self::Error(loc, ..)
            | Self::Pragma(loc, ..)
            | Self::Null(loc) => loc,
        }
    }

    fn parse_include(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(match_directive("include"), PPTokens::parse_into),
            |(pos, name)| Self::Include(pos.clone(), PPInclude::resolve(name)),
        )
        .parse(input)
    }

    fn parse_define(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(
                match_directive("define"),
                pair(PPDefineMatch::parse_into, ok(PPTokens::parse_into)),
            ),
            |(pos, (def_match, def_replace))| {
                Self::Define(pos.clone(), def_match, PPDefineReplace(def_replace))
            },
        )
        .parse(input)
    }

    fn parse_undef(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(
                match_directive("undef"),
                parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"),
            ),
            |(pos, id)| Self::Undef(pos.clone(), id.clone()),
        )
        .parse(input)
    }

    fn parse_line_dir(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(match_directive("line"), PPTokens::parse_into),
            |(pos, tokens)| Self::Line(pos.clone(), PPLineForm::Unparsed(tokens)),
        )
        .parse(input)
    }

    fn parse_error(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(match_directive("error"), ok(PPTokens::parse_into)),
            |(pos, tokens)| Self::Error(pos.clone(), tokens),
        )
        .parse(input)
    }

    fn parse_pragma(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            pair(
                match_directive("pragma"),
                or_else(
                    map(
                        right(
                            left(
                                match_next!(PPToken::Id(_, Id(value)) if value.as_str() == "STDC"),
                                ok(match_next!(PPToken::HSpace(_))),
                            ),
                            pair(
                                parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"),
                                PPOnOffSwitch::parse_into,
                            ),
                        ),
                        |(id, switch)| PPPragmaForm::Stdc(id.clone(), switch),
                    ),
                    map(ok(PPTokens::parse_into), PPPragmaForm::Tokens),
                ),
            ),
            |(pos, form)| Self::Pragma(pos.clone(), form),
        )
        .parse(input)
    }

    fn parse_null(input: &'a [PPToken]) -> ParseResult<'_, &'a [PPToken], Self> {
        map(
            left(
                parse_next!(PPToken::Punct(pos, Punctuator::Sharp) => pos, "directive not matched"),
                ok(match_next!(PPToken::HSpace(_))),
            ),
            |pos| Self::Null(pos.clone()),
        )
        .parse(input)
    }
}

impl<'a> Parses<'a> for PPControlLine {
    type Input = &'a [PPToken];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            Self::parse_include,
            or_else(
                Self::parse_define,
                or_else(
                    Self::parse_undef,
                    or_else(
                        Self::parse_line_dir,
                        or_else(
                            Self::parse_error,
                            or_else(Self::parse_pragma, Self::parse_null),
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

impl<'a> ParsesPPLine<'a> for PPControlLine {}

impl PPAble for PPControlLine {
    fn pass(&self, ctx: &PPContext) -> Result<Self, KrangError> {
        match self {
            Self::Error(loc, tokens) => {
                Err(KrangError::ErrorDirective(loc.clone(), tokens.clone()))
            }
            Self::Pragma(loc, PPPragmaForm::Stdc(id, switch)) => ctx
                .stdc()
                .flip(id, switch)
                .map(|_| self.clone())
                .map_err(|err| KrangError::PPUnrecognizedId(loc.clone(), err.clone())),
            Self::Include(loc, inc) => {
                let inc2 = inc.pass(ctx)?;
                if matches!(&inc, PPInclude::HName(..) | PPInclude::QName(..)) {
                    ctx.include(loc, inc).map(|group| {
                        Self::Include(loc.clone(), PPInclude::Included(Box::new(group)))
                    })
                } else {
                    Err(KrangError::ParseError(vec![(
                        Some(loc.clone()),
                        format!("failed to resolve include directive {:?}", inc2),
                    )]))
                }
            }
            _ => Ok(self.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PPOnOffSwitch {
    On,
    Off,
    Default,
}

impl<'a> Parses<'a> for PPOnOffSwitch {
    type Input = &'a [PPToken];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(parse_next!(PPToken::Id(_, Id(value)) if value.as_str() == "DEFAULT" => PPOnOffSwitch::Default, "DEFAULT not parsed"), 
        or_else(parse_next!(PPToken::Id(_, Id(value)) if value.as_str() == "OFF" => PPOnOffSwitch::Off, "OFF not parsed"), 
        parse_next!(PPToken::Id(_, Id(value)) if value.as_str() == "ON" => PPOnOffSwitch::On, "ON not parsed"))).parse(input)
    }
}

#[derive(Debug, Clone)]
pub enum PPPragmaForm {
    Stdc(Id, PPOnOffSwitch),
    Tokens(Option<PPTokens>),
}

#[derive(Debug, Clone)]
pub enum PPLineForm {
    Unparsed(PPTokens),
    Resolved(PPNum, Option<Vec<SChar>>),
}
impl PPAble for PPLineForm {
    fn pass(&self, ctx: &PPContext) -> Result<Self, KrangError> {
        match self {
            Self::Unparsed(tokens) => unimplemented!("PPLineForm Unparsed"),
            _ => Ok(self.clone()),
        }
    }
}
