use std::{fmt::Debug, rc::Rc};

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
        Self::all_known()
            .into_iter()
            .find(|d| d.as_str() == id.as_str())
            .unwrap_or_else(|| Self::Unknown(id))
    }
}

impl<'a> Parses<'a> for PPDirective {
    type Context = bool;
    type Input = &'a [PPToken];
    fn parse_into(ctx: Rc<Self::Context>, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        parse_next!(PPToken::Id(_, id) => Self::from_id(id.clone()), "directive id not parsed")
            .parse(ctx, input)
    }
}

trait ParsesSharpLine<'a> {
    fn parse_sharp(
        sharp: &Loc,
        directive: PPDirective,
        input: &'a [PPToken],
    ) -> ParseResult<'a, &'a [PPToken], PPLine>;
}

#[derive(Debug, Clone)]
pub struct PPDefineLine(pub PPDefineMatch, pub PPDefineReplace);
// impl<'a> ParsesSharpLine<'a> for PPDefineLine {
//     fn parse_sharp(
//         sharp: &Loc,
//         directive: PPDirective,
//         input: &'a [PPToken],
//     ) -> ParseResult<'a, &'a [PPToken], PPLine> {
//         match directive {
//             PPDirective::Define => map(
//                 pair(
//                     PPDefineMatch::parse_into,
//                     left(ok(PPTokens::parse_into), match_next!(PPToken::LineEnd)),
//                 ),
//                 |(def_match, def_replace)| {
//                     PPLine::Define(sharp.clone(), Self(def_match, PPDefineReplace(def_replace)))
//                 },
//             )
//             .parse(input),
//             _ => none("").parse(input),
//         }
//     }
// }

#[derive(Debug, Clone)]
pub struct PPUndefLine(pub Id);

#[derive(Debug, Clone)]
pub struct PPErrorLine(pub Option<PPTokens>);

#[derive(Debug, Clone)]
pub enum PPOnOffSwitch {
    On,
    Off,
    Default,
}

impl<'a> Parses<'a> for PPOnOffSwitch {
    type Context = bool;
    type Input = &'a [PPToken];
    fn parse_into(ctx: Rc<Self::Context>, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(parse_next!(PPToken::Id(_, Id(value)) if value.as_str() == "DEFAULT" => PPOnOffSwitch::Default, "DEFAULT not parsed"), 
        or_else(parse_next!(PPToken::Id(_, Id(value)) if value.as_str() == "OFF" => PPOnOffSwitch::Off, "OFF not parsed"), 
        parse_next!(PPToken::Id(_, Id(value)) if value.as_str() == "ON" => PPOnOffSwitch::On, "ON not parsed"))).parse(ctx, input)
    }
}

#[derive(Debug, Clone)]
pub enum PPPragmaForm {
    Stdc(Id, PPOnOffSwitch),
    Tokens(Option<PPTokens>),
}
impl<'a> PPPragmaForm {
    // fn parse_sharp(
    //     sharp: &Loc,
    //     directive: PPDirective,
    //     input: &'a [PPToken],
    // ) -> ParseResult<'a, &'a [PPToken], PPLine> {
    //     match directive {
    //         PPDirective::Pragma => {
    //             map(left(or_else(
    //                 map(
    //                     right(
    //                         left(
    //                             match_next!(PPToken::Id(_, Id(value)) if value.as_str() == "STDC"),
    //                             ok(match_next!(PPToken::HSpace(..))),
    //                         ),
    //                         pair(
    //                             parse_next!(PPToken::Id(_, id) => id, "identifier not parsed"),
    //                             PPOnOffSwitch::parse_into,
    //                         ),
    //                     ),
    //                     |(id, switch)| PPPragmaForm::Stdc(id.clone(), switch),
    //                 ),
    //                 map(ok(PPTokens::parse_into), PPPragmaForm::Tokens),
    //             ), match_next!(PPToken::LineEnd)), |form| PPLine::Pragma(sharp.clone(), form)).parse(input) }
    //         _ => none("").parse(input),
    //     }
    // }
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
// impl<'a> ParsesSharpLine<'a> for PPLineForm {
//     fn parse_sharp(
//         sharp: &Loc,
//         directive: PPDirective,
//         input: &'a [PPToken],
//     ) -> ParseResult<'a, &'a [PPToken], PPLine> {
//         match directive {
//             PPDirective::Line => left(
//                 map(PPTokens::parse_into, |expr| {
//                     PPLine::SharpLine(sharp.clone(), Self::Unparsed(expr))
//                 }),
//                 match_next!(PPToken::LineEnd),
//             )
//             .parse(input),
//             _ => none("").parse(input),
//         }
//     }
// }

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
        let lines = Self::parse(true.into(), stream.tokens())
            .map(|(rem, group)| group)
            .map_err(|err| KrangError::ParseError(trace_parse_errors(mapper)(err)))?;

        Ok(Self { stream, lines })
    }

    pub fn parse(
        ctx: Rc<bool>,
        input: &'a [PPToken],
    ) -> ParseResult<'a, &'a [PPToken], Vec<PPLine>> {
        range(0.., PPLine::parse_into).parse(ctx, input)
    }
}

#[derive(Debug, Clone)]
pub enum PPLine {
    Sharp(PPDirective, PPTokens, PPTokens),
    SharpNull(PPTokens, Vec<PPToken>),
    Text(PPTokens),
    Empty,
}
impl PPLine {
    pub fn loc(&self) -> Option<&Loc> {
        use PPLine::*;
        match self {
            Sharp(_, leading, ..) | SharpNull(leading, ..) | Text(leading, ..) => {
                Some(leading.loc())
            }
            Empty => None,
        }
    }

    fn from_matched_directive(
        o_lead: Option<PPToken>,
        sharp: PPToken,
        o_sep: Option<PPToken>,
        directive: Option<PPToken>,
        remaining: Vec<PPToken>,
    ) -> Result<Self, String> {
        let v_lead = vec![
            o_lead.map(|lead| vec![lead]).unwrap_or_default(),
            vec![sharp],
            o_sep.map(|sep| vec![sep]).unwrap_or_default(),
        ]
        .concat();
        let lead = match v_lead.split_first() {
            Some((head, tail)) => Ok(PPTokens(head.clone(), tail.to_vec())),
            None => Err("from_matched_directive: empty leading tokens".to_owned()),
        }?;
        let tail = vec![
            directive.clone().map(|dir| vec![dir]).unwrap_or_default(),
            remaining,
        ]
        .concat();
        match &directive {
            None => Ok(Self::SharpNull(lead, tail)),
            Some(directive @ PPToken::Id(_, id)) => Ok(Self::Sharp(
                PPDirective::from_id(id.clone()),
                lead,
                PPTokens(directive.clone(), tail),
            )),
            Some(directive) => Err(format!(
                "from_matched_directive: invalid directive token {:?}",
                directive
            )),
        }
    }
}
impl<'a> Parses<'a> for PPLine {
    type Context = bool;
    type Input = &'a [PPToken];
    fn parse_into(ctx: Rc<Self::Context>, input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            // only truly empty lines should be treated as such
            map(match_next!(PPToken::LineEnd), |_| Self::Empty),
            // otherwise...
            or_else(
                // peek for # <id> to identify directive
                and_then(
                    pair(
                        ok(match_next!(PPToken::HSpace(..))),
                        pair(
                            match_next!(PPToken::Punct(_, Punctuator::Sharp)),
                            pair(
                                ok(match_next!(PPToken::HSpace(..))),
                                pair(
                                    ok(match_next!(PPToken::Id(..))),
                                    pair(
                                        range(
                                            0..,
                                            map(not_next!(PPToken::LineEnd), |token| token.clone()),
                                        ),
                                        match_next!(PPToken::LineEnd),
                                    ),
                                ),
                            ),
                        ),
                    ),
                    |(o_lead, (sharp, (o_sep, (o_dir, (remaining, end)))))| {
                        Self::from_matched_directive(
                            o_lead.cloned(),
                            sharp.clone(),
                            o_sep.cloned(),
                            o_dir.cloned(),
                            remaining,
                        )
                    },
                ),
                // otherwise, treat as text line
                map(
                    left(PPTokens::parse_into, match_next!(PPToken::LineEnd)),
                    |tokens| Self::Text(tokens),
                ),
            ),
        )
        .parse(ctx, input)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        error::KrangError,
        preproc::{file::PPFileCalculator, file::PathCalculator},
    };

    #[test]
    fn ifendif() -> Result<(), KrangError> {
        let calc = PPFileCalculator::new();
        let file = calc.calculate(Path::new("tests/ifendif.h"))?;

        print!("{:#?}", file);
        Ok(())
    }
}
