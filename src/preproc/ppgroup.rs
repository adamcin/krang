use std::{fmt::Debug, rc::Rc};

use crate::{error::KrangError, parse::*, punct::Punctuator, scan::*};

use super::{
    id::Id,
    ppif::*,
    ppinclude::PPInclude,
    ppline::*,
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
    type Context = bool;
    type Input = &'a [PPLine];

    fn parse_into(ctx: Rc<Self::Context>, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(PPGroupPart::parse_into, range(0.., PPGroupPart::parse_into)),
            |(head, tail)| Self(head, tail),
        )
        .parse(ctx, input)
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
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'a, <Self as Parses<'a>>::Input, Self> {
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
        .parse(ctx, input)
    }

    pub fn parse_control_line(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'a, <Self as Parses<'a>>::Input, Self> {
        map(PPControlLine::parse_into, |dir| {
            Self::ControlLine(dir.loc().clone(), dir)
        })
        .parse(ctx, input)
    }

    pub fn parse_text_line(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'a, <Self as Parses<'a>>::Input, Self> {
        or_else(
            parse_next!(PPLine::Empty => Self::EmptyLine, "<empty> not parsed"),
            parse_next!(text@PPLine::Text(tokens) => Self::TextLine(tokens.loc().clone(), tokens.clone()), "<text> not parsed"),
        )
        .parse(ctx, input)
    }

    pub fn parse_non_directive(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'a, <Self as Parses<'a>>::Input, Self> {
        parse_next!(sharp@PPLine::Sharp(PPDirective::Unknown(id), _, tokens) => Self::NonDirective(tokens.loc().clone(), tokens.clone()), "#<non> not parsed").parse(ctx, input)
    }
}
impl<'a> Parses<'a> for PPGroupPart {
    type Context = bool;
    type Input = &'a [PPLine];
    fn parse_into(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: Self::Input,
    ) -> ParseResult<'a, Self::Input, Self>
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
        .parse(ctx, input)
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

#[derive(Debug, Clone)]
pub enum PPControlLine {
    Include(Loc),
    Define(Loc),
    Undef(Loc),
    Line(Loc),
    Error(Loc),
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

    fn parse_include(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: &'a [PPLine],
    ) -> ParseResult<'a, &'a [PPLine], Self> {
        parse_next!(PPLine::Sharp(PPDirective::Include, lead, ..) => Self::Include(lead.loc().clone()), "#include not parsed").parse(ctx, input)
    }

    fn parse_define(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: &'a [PPLine],
    ) -> ParseResult<'a, &'a [PPLine], Self> {
        parse_next!(PPLine::Sharp(PPDirective::Define, lead, ..) => Self::Define(lead.loc().clone()), "#define not parsed").parse(ctx, input)
    }

    fn parse_undef(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: &'a [PPLine],
    ) -> ParseResult<'a, &'a [PPLine], Self> {
        parse_next!(PPLine::Sharp(PPDirective::Undef, lead, ..) => Self::Undef(lead.loc().clone()), "#undef not parsed").parse(ctx, input)
    }

    fn parse_sharp_line(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: &'a [PPLine],
    ) -> ParseResult<'a, &'a [PPLine], Self> {
        parse_next!(PPLine::Sharp(PPDirective::Line, lead, ..) => Self::Line(lead.loc().clone()), "#line not parsed").parse(ctx, input)
    }

    fn parse_sharp_error(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: &'a [PPLine],
    ) -> ParseResult<'a, &'a [PPLine], Self> {
        parse_next!(PPLine::Sharp(PPDirective::Error, lead, ..) => Self::Error(lead.loc().clone()), "#error not parsed").parse(ctx, input)
    }

    fn parse_pragma(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: &'a [PPLine],
    ) -> ParseResult<'a, &'a [PPLine], Self> {
        parse_next!(PPLine::Sharp(PPDirective::Pragma, lead, tokens) => Self::Pragma(lead.loc().clone(), PPPragmaForm::Tokens(Some(tokens.clone()))), "#pragma not parsed").parse(ctx, input)
    }

    fn parse_sharp_null(
        ctx: Rc<<Self as Parses<'a>>::Context>,
        input: &'a [PPLine],
    ) -> ParseResult<'a, &'a [PPLine], Self> {
        parse_next!(PPLine::SharpNull(lead, ..) => Self::Null(lead.loc().clone()), "#<null> not parsed")
            .parse(ctx, input)
    }
}

impl<'a> Parses<'a> for PPControlLine {
    type Context = bool;
    type Input = &'a [PPLine];
    fn parse_into(ctx: Rc<Self::Context>, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
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
                        Self::parse_sharp_line,
                        or_else(
                            Self::parse_sharp_error,
                            or_else(Self::parse_pragma, Self::parse_sharp_null),
                        ),
                    ),
                ),
            ),
        )
        .parse(ctx, input)
    }
}

impl PPAble for PPControlLine {
    fn pass(&self, ctx: &PPContext) -> Result<Self, KrangError> {
        match self {
            // Self::Error(loc) => Err(KrangError::ErrorDirective(loc.clone(), None)),
            // Self::Pragma(loc, PPPragmaForm::Stdc(id, switch)) => ctx
            //     .stdc()
            //     .flip(id, switch)
            //     .map(|_| self.clone())
            //     .map_err(|err| KrangError::PPUnrecognizedId(loc.clone(), err.clone())),
            // Self::Include(loc, inc) => {
            //     let inc2 = inc.pass(ctx)?;
            //     if matches!(&inc, PPInclude::HName(..) | PPInclude::QName(..)) {
            //         log::info!("would include {:?}", inc);
            //         ctx.include(loc, inc).map(|group| {
            //             Some(inc2)
            //             //Self::Include(loc.clone(), PPInclude::Included(Box::new(group)))
            //         })
            //     } else {
            //         Err(KrangError::ParseError(vec![(
            //             Some(loc.clone()),
            //             format!("failed to resolve include directive {:?}", inc2),
            //         )]))
            //     }
            // }
            _ => Ok(self.clone()),
        }
    }
}
