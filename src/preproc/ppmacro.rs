use crate::{parse::*, punct::Punctuator};

use super::{id::Id, pptoken::*};

#[derive(Debug, Clone)]
pub enum PPDefineMatch {
    /// object-like macro: id
    Object(Id),
    /// function-like macro: id ( Option<idlist> )
    Function(Id, Option<PPIdList>),
    /// function-like macro with var-args: id ( Option<idlist>, ... )
    FunctionVarArgs(Id, Option<PPIdList>),
}

impl<'a> Parses<'a> for PPDefineMatch {
    type Input = &'a [PPToken];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        max(
            map(
                parse_next!(PPToken::Id(_, id) => id.to_owned(), "id not parsed"),
                Self::Object,
            ),
            max(
                map(
                    pair(
                        left(
                            parse_next!(PPToken::Id(_, id) => id.to_owned(), "id not parsed"),
                            match_next!(PPToken::Punct(_, Punctuator::LRnd)),
                        ),
                        right(
                            ok(match_next!(PPToken::HSpace(_))),
                            left(
                                ok(PPIdList::parse_into),
                                right(
                                    ok(match_next!(PPToken::HSpace(_))),
                                    match_next!(PPToken::Punct(_, Punctuator::RRnd)),
                                ),
                            ),
                        ),
                    ),
                    |(head, tail)| Self::Function(head, tail),
                ),
                map(
                    pair(
                        left(
                            parse_next!(PPToken::Id(_, id) => id.to_owned(), "id not parsed"),
                            match_next!(PPToken::Punct(_, Punctuator::LRnd)),
                        ),
                        left(
                            right(
                                ok(match_next!(PPToken::HSpace(_))),
                                ok(PPIdList::parse_into),
                            ),
                            right(
                                ok(match_next!(PPToken::HSpace(_))),
                                right(
                                    ok(match_next!(PPToken::Punct(_, Punctuator::Elips3))),
                                    right(
                                        ok(match_next!(PPToken::HSpace(_))),
                                        match_next!(PPToken::Punct(_, Punctuator::RRnd)),
                                    ),
                                ),
                            ),
                        ),
                    ),
                    |(head, tail)| Self::FunctionVarArgs(head, tail),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Debug, Clone)]
pub struct PPDefineReplace(pub Option<PPTokens>);

#[derive(Debug, Clone)]
pub struct PPIdList(Id, Vec<Id>);
impl<'a> Parses<'a> for PPIdList {
    type Input = &'a [PPToken];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(
                parse_next!(PPToken::Id(_, id) => id.to_owned(), "id not parsed"),
                range(
                    0..,
                    right(
                        ok(match_next!(PPToken::HSpace(_))),
                        right(
                            match_next!(PPToken::Punct(_, Punctuator::Comma)),
                            right(
                                ok(match_next!(PPToken::HSpace(_))),
                                parse_next!(PPToken::Id(_, id) => id.to_owned(), "id not parsed"),
                            ),
                        ),
                    ),
                ),
            ),
            |(head, tail)| Self(head, tail),
        )
        .parse(input)
    }
}
