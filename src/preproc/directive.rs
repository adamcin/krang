use std::fmt::Debug;

use crate::{error::KrangError, parse::*, punct::Punctuator, scan::*};

use super::{
    expression::PPExpression,
    id::Id,
    ppif::*,
    ppinclude::PPInclude,
    ppmacro::{PPDefineMatch, PPDefineReplace},
    pptoken::*,
    preprocessor::{PPAble, PPContext},
};



#[derive(Debug, Clone)]
pub enum PPDirective {
    If,
    IfDef,
    IfNotDef,
    ElseIf,
    Else,
    EndIf,
    Include,
    Define,
    Undef,
    Line,
    Error,
    Pragma,
    Unknown(Id),
}

impl PPDirective {
    pub fn as_str(&self) -> &str {
        match self {
            Self::If => "if",
            Self::IfDef => "ifdef",
            Self::IfNotDef => "ifndef",
            Self::ElseIf => "elif",
            Self::Else => "else",
            Self::EndIf => "endif",
            Self::Include => "include",
            Self::Define => "define",
            Self::Undef => "undef",
            Self::Line => "line",
            Self::Error => "error",
            Self::Pragma => "pragma",
            Self::Unknown(id) => id.as_str(),
        }
    }

    pub fn all_known() -> Vec<Self> {
        vec![
            Self::If,
            Self::IfDef,
            Self::IfNotDef,
            Self::ElseIf,
            Self::Else,
            Self::EndIf,
            Self::Include,
            Self::Define,
            Self::Undef,
            Self::Line,
            Self::Error,
            Self::Pragma,
        ]
    }

    pub fn from_id(id: Id) -> Self {
        Self::all_known().into_iter().find(|d| d.as_str() == id.as_str()).unwrap_or_else(|| Self::Unknown(id))
    }

    pub fn parse_sharp<'a>(self, sharp: &Loc, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        use PPDirective::*;
        match self {
            If | IfDef | IfNotDef => PPIfLine::parse_sharp(sharp, self, input),
            ElseIf => PPElIfLine::parse_sharp(sharp, self, input),
            Else => PPElseLine::parse_sharp(sharp, self, input),
            EndIf => PPEndIf::parse_sharp(sharp, self, input),
            Include => PPInclude::parse_sharp(sharp, self, input),
            Define => PPDefineLine::parse_sharp(sharp, self, input),
            Undef => PPUndefLine::parse_sharp(sharp, self, input),
            Line => PPLineForm::parse_sharp(sharp, self, input),
            Error => PPErrorLine::parse_sharp(sharp, self, input),
            Pragma => PPPragmaForm::parse_sharp(sharp, self, input),
            Unknown(id) => map(PPTokens::parse_into, |tokens| PPLine::NonDirective(sharp.clone(), tokens)).parse(input),
        }
    }
}

impl<'a> Parses<'a> for PPDirective {
    type Input = &'a [PPToken];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
        where
            Self::Input: 'a {
        parse_next!(PPToken::Id(_, id) => Self::from_id(id.clone()), "identifier not parsed").parse(input)
    }
}



trait ParsesSharpLine<'a> {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine>;
}

#[derive(Debug, Clone)]
pub struct PPDefineLine(pub PPDefineMatch, pub PPDefineReplace);
impl<'a> ParsesSharpLine<'a> for PPDefineLine {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::Define => map(pair(PPDefineMatch::parse_into, 
                left(ok(PPTokens::parse_into), match_next!(PPToken::LineEnd))), 
                |(def_match, def_replace)| PPLine::Define(sharp.clone(), Self(def_match, PPDefineReplace(def_replace)))).parse(input),
            _ => none("").parse(input),
        }   
    }
}

#[derive(Debug, Clone)]
pub struct PPUndefLine(pub Id);
impl<'a> ParsesSharpLine<'a> for PPUndefLine {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::Undef => {
                right(ok(match_next!(PPToken::HSpace(_))), 
                left(map(parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"), |id| PPLine::Undef(sharp.clone(), id.clone())), 
                right(ok(match_next!(PPToken::HSpace(_))), match_next!(PPToken::LineEnd)))).parse(input)
            },
            _ => none("").parse(input),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PPErrorLine(pub Option<PPTokens>);
impl<'a> ParsesSharpLine<'a> for PPErrorLine {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::Error => left(map(ok(PPTokens::parse_into), |expr| PPLine::SharpError(sharp.clone(), expr)), match_next!(PPToken::LineEnd)).parse(input),
            _ => none("").parse(input),
        }
    }
}


#[derive(Debug, Clone)]
pub enum PPControlLine {
    Include(Loc, PPInclude),
    Define(Loc, PPDefineLine),
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

    fn parse_include(input: &'a [PPLine]) -> ParseResult<'_, &'a [PPLine], Self> {
        parse_next!(PPLine::Include(pos, inc) => Self::Include(pos.clone(), inc.clone()), "#include not parsed").parse(input)
    }

    fn parse_define(input: &'a [PPLine]) -> ParseResult<'_, &'a [PPLine], Self> {
        parse_next!(PPLine::Define(pos, define_line) => Self::Define(pos.clone(), define_line.clone()), "#define not parsed").parse(input)
    }

    fn parse_undef(input: &'a [PPLine]) -> ParseResult<'_, &'a [PPLine], Self> {
        parse_next!(PPLine::Undef(pos, id) => Self::Undef(pos.clone(), id.clone()), "#undef not parsed").parse(input)
    }

    fn parse_sharp_line(input: &'a [PPLine]) -> ParseResult<'_, &'a [PPLine], Self> {
        parse_next!(PPLine::SharpLine(pos, line_form) => Self::Line(pos.clone(), line_form.clone()), "#line not parsed").parse(input)
    }

    fn parse_sharp_error(input: &'a [PPLine]) -> ParseResult<'_, &'a [PPLine], Self> {
        parse_next!(PPLine::SharpError(pos, expr) => Self::Error(pos.clone(), expr.clone()), "#error not parsed").parse(input)
    }

    fn parse_pragma(input: &'a [PPLine]) -> ParseResult<'_, &'a [PPLine], Self> {
        parse_next!(PPLine::Pragma(pos, pragma_form) => Self::Pragma(pos.clone(), pragma_form.clone()), "#pragma not parsed").parse(input)
    }

    fn parse_sharp_null(input: &'a [PPLine]) -> ParseResult<'_, &'a [PPLine], Self> {
        parse_next!(PPLine::SharpNull(pos) => Self::Null(pos.clone()), "#<null> not parsed").parse(input)
    }
}

impl<'a> Parses<'a> for PPControlLine {
    type Input = &'a [PPLine];
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
                        Self::parse_sharp_line,
                        or_else(
                            Self::parse_sharp_error,
                            or_else(Self::parse_pragma, Self::parse_sharp_null),
                        ),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

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
impl<'a> ParsesSharpLine<'a> for PPPragmaForm {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::Pragma => {
                map(left(or_else(
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
                ), match_next!(PPToken::LineEnd)), |form| PPLine::Pragma(sharp.clone(), form)).parse(input) }
            _ => none("").parse(input),
        }
    }
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
impl<'a> ParsesSharpLine<'a> for PPLineForm {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::Line => left(map(PPTokens::parse_into, |expr| PPLine::SharpLine(sharp.clone(), Self::Unparsed(expr))), match_next!(PPToken::LineEnd)).parse(input),
            _ => none("").parse(input),
        }
    }
}



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
}

impl<'a> Parses<'a> for PPIfLine {
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {   
        pass(
            parse_next!(PPLine::If(loc, if_line) => if_line.clone(), "#if not matched"),
            "IfLine::parse_into",
        )
        .parse(input)
    }
}

impl<'a> ParsesSharpLine<'a> for PPIfLine {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::If => left(map(pass(PPTokens::parse_into, "tokens!"), |expr| PPLine::If(sharp.clone(), Self::If(sharp.clone(), PPExpression::Unparsed(expr)))), match_next!(PPToken::LineEnd)).parse(input),
            PPDirective::IfDef => left(map(parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"), |id| PPLine::If(sharp.clone(), Self::IfDef(sharp.clone(), id.clone()))), match_next!(PPToken::LineEnd)).parse(input),
            PPDirective::IfNotDef => left(map(parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"), |id| PPLine::If(sharp.clone(), Self::IfNotDef(sharp.clone(), id.clone()))), match_next!(PPToken::LineEnd)).parse(input),
            _ => none("").parse(input),
        }
    }
}



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
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        parse_next!(PPLine::ElseIf(loc, expr) => Self(loc.clone(), expr.clone()), "#elif not matched").parse(input)
    }
}
impl<'a> ParsesSharpLine<'a> for PPElIfLine {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::ElseIf => left(map(PPTokens::parse_into, |expr| PPLine::ElseIf(sharp.clone(), PPExpression::Unparsed(expr))), match_next!(PPToken::LineEnd)).parse(input),
            _ => none("").parse(input),
        }
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
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        parse_next!(PPLine::Else(loc) => Self(loc.clone()), "#else not matched").parse(input)
    }
}
impl<'a> ParsesSharpLine<'a> for PPElseLine {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::Else => right(ok(match_next!(PPToken::HSpace(_))), map(match_next!(PPToken::LineEnd), |_| PPLine::Else(sharp.clone()))).parse(input),
            _ => none("").parse(input),
        }
    }
}


#[derive(Debug, Clone)]
pub struct PPEndIf(Loc);
impl<'a> Parses<'a> for PPEndIf {
    type Input = &'a [PPLine];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        
        pass(
            parse_next!(PPLine::EndIf(loc) => Self(loc.clone()), "#endif not matched"),
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

impl<'a> ParsesSharpLine<'a> for PPEndIf {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::EndIf => right(ok(match_next!(PPToken::HSpace(_))), map(match_next!(PPToken::LineEnd), |_| PPLine::EndIf(sharp.clone()))).parse(input),
            _ => none("").parse(input),
        }
    }
}

impl<'a> ParsesSharpLine<'a> for PPInclude {
    fn parse_sharp(sharp: &Loc, directive: PPDirective, input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], PPLine> {
        match directive {
            PPDirective::Include => left(map(PPTokens::parse_into, |tokens| PPLine::Include(sharp.clone(), PPInclude::Unparsed(tokens))), match_next!(PPToken::LineEnd)).parse(input),
            _ => none("").parse(input),
        }
    }
}


#[derive(Debug, Clone)]
pub struct PPLineStream {
    stream: PPTokenStream,
    lines: Vec<PPLine>,
}

impl<'a> PPLineStream {
    pub fn path(&self) -> &str {
        self.stream.path()
    }

    pub fn lines(&self) -> &[PPLine] {
        &self.lines
    }

    pub fn new(stream: PPTokenStream) -> Result<Self, KrangError> {
        let mapper = |tok: &PPToken| tok.loc().cloned();
        let lines = Self::parse(stream.tokens())
            .map(|(rem, group)| group)
            .map_err(|err| KrangError::ParseError(trace_parse_errors(mapper)(err)))?;

        Ok(Self { stream, lines })
    }

    pub fn parse(input: &'a [PPToken]) -> ParseResult<'a, &'a [PPToken], Vec<PPLine>> {
        range(0.., PPLine::parse_into).parse(input)
    }
}

#[derive(Debug, Clone)]
pub enum PPLine {
    If(Loc, PPIfLine),
    ElseIf(Loc, PPExpression),
    Else(Loc),
    EndIf(Loc),
    Include(Loc, PPInclude),
    Define(Loc, PPDefineLine),
    Undef(Loc, Id),
    SharpLine(Loc, PPLineForm),
    SharpError(Loc, Option<PPTokens>),
    Pragma(Loc, PPPragmaForm),
    SharpNull(Loc),
    NonDirective(Loc, PPTokens),
    Text(Loc, PPTokens),
    Empty,
}
impl PPLine {
    pub fn loc(&self) -> Option<&Loc> {
        use PPLine::*;
        match self {
            If(loc, _) |
            ElseIf(loc, _) |
            Else(loc) |
            EndIf(loc) |
            Include(loc, _) |
            Define(loc, _) |
            Undef(loc, _) |
            SharpLine(loc, _) |
            SharpError(loc, _) |
            Pragma(loc, _) |
            SharpNull(loc) |
            NonDirective(loc, _) |
            Text(loc, _) => Some(loc),
            Empty => None,
        }
    }

    pub fn parse_directive(sharp: &Loc, input: &[PPToken]) -> Result<Self, String> {
        match right(ok(match_next!(PPToken::HSpace(_))), 
            or_else(map(match_next!(PPToken::LineEnd), |_| Self::SharpNull(sharp.clone())), 
                or_else(left(
                    map(pair(not_next!(PPToken::Id(_, _)), range(0.., map(not_next!(PPToken::LineEnd), 
                    |token| token.clone()))), |(head, tail)| Self::NonDirective(sharp.clone(), PPTokens(head.clone(), tail))),
                    match_next!(PPToken::LineEnd)),
                and_then_input(pair(PPDirective::parse_into, map(pair(range(0.., map(not_next!(PPToken::LineEnd), |token| token.clone())), match_next!(PPToken::LineEnd)), |(nonnls, nl)| vec![nonnls, vec![nl.clone()]].concat())), 
                |(directive, tokens), input| {
                    match directive.parse_sharp(sharp, &tokens) {
                        Ok((rem, ret)) if rem.is_empty() => Ok(ret),
                        Ok((rem, ret)) => Err(format!("line not fully matched {:?} => {:?}", rem, ret)),
                        Err(((err, err_i), _)) => Err(format!("line not matched ({}): {:?}", err, err_i)),
                    }
                })))
        ).parse(input) {
            Ok((rem, ret)) if rem.is_empty() => Ok(ret),
            Ok((rem, ret)) => Err(format!("line not fully matched {:?} => {:?}", rem, ret)),
            Err(((err, err_i), _)) => Err(format!("line not matched ({}): {:?}", err, err_i)),
        }
    }

    pub fn parse_text(input: &[PPToken]) -> Result<Self, String> {
        match left(map(ok(PPTokens::parse_into), |o_tokens| o_tokens.map(|tokens| Self::Text(tokens.loc().clone(), tokens)).unwrap_or(Self::Empty)), match_next!(PPToken::LineEnd)).parse(input) {
            Ok((rem, ret)) if rem.is_empty() => Ok(ret),
            Ok((rem, ret)) => Err(format!("line not fully matched {:?} => {:?}", rem, ret)),
            Err(((err, err_i), _)) => Err(format!("line not matched ({}): {:?}", err, err_i)),
        }
    }
}
impl<'a> Parses<'a> for PPLine {
    type Input = &'a [PPToken];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        right(
            ok(match_next!(PPToken::HSpace(_))),
            or_else(map(match_next!(PPToken::LineEnd), |_| Self::Empty), 
            and_then_input(pair(ok(parse_next!(PPToken::Punct(sharp, Punctuator::Sharp) => sharp, "# not matched")), 
            map(pair(range(0.., map(not_next!(PPToken::LineEnd), |token| token.clone())), match_next!(PPToken::LineEnd)), 
            |(nonnls, nl)| vec![nonnls, vec![nl.clone()]].concat())),
            |(o_sharp, tokens), input| match o_sharp {
                Some(sharp) => Self::parse_directive(sharp, &tokens),
                None => Self::parse_text(&tokens),
            }),
        ))
        .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{error::KrangError, preproc::{file::PPFileCalculator, file::PathCalculator}};

    #[test]
    fn ifendif() -> Result<(), KrangError> {
        let calc = PPFileCalculator::new();
        let file = calc.calculate(Path::new("tests/ifendif.h"))?;
        
        print!("{:#?}", file);
        Ok(())
    }
}