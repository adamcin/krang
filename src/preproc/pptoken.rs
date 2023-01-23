use std::num::ParseIntError;

use crate::{error::KrangError, parse::*, punct::Punctuator, scan::*, source::file::SourceFile};

use super::id::Id;

/// Each preprocessing token that is converted to a token shall have the lexical form of a
/// keyword, an identifier, a constant, a string literal, or a punctuator.
///
/// If the input stream has been parsed into preprocessing tokens up to a given character, the
/// next preprocessing token is the longest sequence of characters that could constitute a
/// preprocessing token.
///
/// Preprocessing tokens can be separated by white space; this consists of
/// comments (described later), or white-space characters (space, horizontal tab, new-line,
/// vertical tab, and form-feed), or both. As described in 6.10, in certain circumstances
/// during translation phase 4, white space (or the absence thereof) serves as more than
/// preprocessing token separation. White space may appear within a preprocessing token
/// only as part of a header name or between the quotation characters in a character constant
/// or string literal.
///
/// grammar:
///     preprocessing-token:
///         header-name
///         identifier
///         pp-number
///         character-constant
///         string-literal
///         punctuator
///         else: each non-white-space character that cannot be one of the above
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PPToken {
    /// header-name
    ///
    /// header name preprocessing tokens are recognized only within #include preprocessing directives and in
    /// implementation-defined locations within #pragma directives. In such contexts, a
    /// sequence of characters that could be either a header name or a string literal is recognized
    /// as the former.
    ///
    /// H-chars are delimited by < and >
    HChars(Loc, Vec<HChar>),
    /// Q-chars are delimited only by "
    QChars(Loc, Vec<QChar>),
    /// identifier
    ///
    /// An identifier is a sequence of nondigit characters (including the underscore _, the
    /// lowercase and uppercase Latin letters, and other characters) and digits, which designates
    /// one or more entities as described in 6.2.1. Lowercase and uppercase letters are distinct.
    /// There is no specific limit on the maximum length of an identifier.
    Id(Loc, Id),
    /// pp-number
    ///
    /// A preprocessing number begins with a digit optionally preceded by a period (.) and may
    /// be followed by valid identifier characters and the character sequences e+, e-, E+, E-,
    /// p+, p-, P+, or P-.
    Num(Loc, PPNum),
    /// character-constant
    CChars(Loc, CEncoding, Vec<CChar>),
    /// string-literal
    SChars(Loc, SEncoding, Vec<SChar>),
    /// punctuator
    Punct(Loc, Punctuator),
    /// each non-white-space character that cannot be one of the above
    /// If a ' or a " character matches the last category, the behavior is
    /// undefined.
    Other(Ch),
    /// A placeholder for at least one unquoted horizontal space (' ' or '\t', not '\n')
    HSpace(Loc),
    /// A placeholder for a newline character or EOF
    LineEnd,
    /// A macro replacement
    Replace(Loc, PPReplacement),
}

impl<'a> PPToken {
    pub fn loc(&'a self) -> Option<&'a Loc> {
        match self {
            Self::HChars(loc, ..)
            | Self::QChars(loc, ..)
            | Self::Id(loc, ..)
            | Self::Num(loc, ..)
            | Self::CChars(loc, ..)
            | Self::SChars(loc, ..)
            | Self::Punct(loc, ..) => Some(loc),
            Self::Other(ch) => Some(ch.loc()),
            _ => None,
        }
    }
    pub fn parse_id(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(Id::parse_into, |value, i| {
            Self::Id(i[0].loc().clone(), value)
        })
        .parse(input)
    }

    pub fn parse_num(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(PPNum::parse_into, |value, i| {
            Self::Num(i[0].loc().clone(), value)
        })
        .parse(input)
    }

    pub fn parse_hchars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(
            right(
                match_literal("<"),
                left(range(0.., HChar::parse_into), match_literal(">")),
            ),
            |value, i| Self::HChars(i[0].loc().clone(), value),
        )
        .parse(input)
    }

    pub fn parse_cchars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(
            pair(
                CEncoding::parse_into,
                left(range(0.., CChar::parse_into), match_literal("'")),
            ),
            |(enc, value), i| Self::CChars(i[0].loc().clone(), enc, value),
        )
        .parse(input)
    }

    pub fn parse_qchars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(
            right(
                match_literal("\""),
                left(range(0.., QChar::parse_into), match_literal("\"")),
            ),
            |value, i| Self::QChars(i[0].loc().clone(), value),
        )
        .parse(input)
    }

    pub fn parse_schars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(
            pair(
                SEncoding::parse_into,
                left(range(0.., SChar::parse_into), match_literal("\"")),
            ),
            |(enc, value), i| Self::SChars(i[0].loc().clone(), enc, value),
        )
        .parse(input)
    }

    pub fn parse_punct(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(Punctuator::parse_into, |punct, i| {
            Self::Punct(i[0].loc().clone(), punct)
        })
        .parse(input)
    }

    pub fn parse_other(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map(nonwsp(), |ch| Self::Other(ch.clone())).parse(input)
    }
}

impl<'a> Parses<'a> for PPToken {
    type Input = &'a [Ch];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            or_else(
                right(pad0(), left(map(newline(), |_| Self::LineEnd), pad0())),
                map(pad1(), |ch| Self::HSpace(ch.first().unwrap().loc().clone())),
            ),
            or_else(
                max(
                    Self::parse_punct, // matches dot and <
                    max(
                        Self::parse_num,    // possibly leading dot
                        Self::parse_hchars, // leading <
                    ),
                ),
                or_else(
                    Self::parse_qchars, // leading " is unambiguous
                    or_else(
                        max(
                            Self::parse_cchars,
                            max(
                                Self::parse_schars,
                                Self::parse_id, // matches u, U, and L as well as others
                            ),
                        ),
                        Self::parse_other,
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

pub fn trace_parse_errors<'a, 'f, F, Atom>(
    mapper: F,
) -> impl Fn(((String, &[Atom]), Vec<(String, &[Atom])>)) -> Vec<(Option<Loc>, String)>
where
    F: Fn(&Atom) -> Option<Loc> + 'f,
    'f: 'a,
{
    move |(head, tail)| {
        let combined = vec![vec![head], tail].concat();
        combined
            .into_iter()
            .rev()
            .map(|(err, input)| (input.get(0).and_then(&mapper), err))
            .collect()
    }
}

#[derive(Clone, Debug)]
pub struct PPTokenStream {
    src_file: SourceFile,
    tokens: Vec<PPToken>,
}

impl<'a> PPTokenStream {
    pub fn path(&self) -> &str {
        self.src_file.path()
    }

    pub fn new(src_file: SourceFile) -> Result<Self, KrangError> {
        let mapper = |ch: &Ch| Some(ch.loc().clone());
        let tokens = Self::parse(src_file.stream())
            .map(|(_, tokens)| tokens)
            .map_err(|err| KrangError::ParseError(trace_parse_errors(mapper)(err)))?;
        Ok(Self { src_file, tokens })
    }

    pub fn parse(input: &'a [Ch]) -> ParseResult<'a, &'a [Ch], Vec<PPToken>> {
        map(range(0.., PPToken::parse_into), |parsed| {
            if matches!(parsed.iter().last(), Some(PPToken::LineEnd)) {
                parsed
            } else {
                vec![parsed, vec![PPToken::LineEnd]].concat()
            }
        })
        .parse(input)
    }
}

impl PPTokenStream {
    pub fn tokens(&self) -> &[PPToken] {
        self.tokens.as_slice()
    }
}

#[derive(Clone, Debug)]
pub struct PPLine(pub Vec<PPToken>);
impl PPLine {
    pub fn loc(&self) -> Option<&Loc> {
        match self {
            Self(tokens) => tokens.first().and_then(|tok| tok.loc()),
        }
    }

    pub fn as_tokens(&self) -> Option<PPTokens> {
        match self.0.split_first() {
            Some((head, tail)) => Some(PPTokens(head.clone(), tail.to_vec())),
            None => None,
        }
    }
}

impl<'a> Parses<'a> for PPLine {
    type Input = &'a [PPToken];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            left(
                range(0.., map(not_next!(PPToken::LineEnd), |token| token.clone())),
                match_next!(PPToken::LineEnd),
            ),
            Self,
        )
        .parse(input)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SimpleEsc {
    Apos,
    Quot,
    Query,
    BSlash,
    Alert,
    Bsp,
    Feed,
    Newline,
    Return,
    Tab,
    Vtab,
}

impl SimpleEsc {
    pub fn as_char(&self) -> Option<char> {
        Some(match self {
            Self::Apos => '\'',
            Self::Quot => '"',
            Self::Query => '?',
            Self::BSlash => '\\',
            Self::Alert => '\x07',
            Self::Bsp => '\x08',
            Self::Feed => '\x0C',
            Self::Newline => '\n',
            Self::Return => '\r',
            Self::Tab => '\t',
            Self::Vtab => '\x0B',
        })
    }

    pub fn as_esc_char(&self) -> char {
        match self {
            Self::Apos => '\'',
            Self::Quot => '"',
            Self::Query => '?',
            Self::BSlash => '\\',
            Self::Alert => 'a',
            Self::Bsp => 'b',
            Self::Feed => 'f',
            Self::Newline => 'n',
            Self::Return => 'r',
            Self::Tab => 't',
            Self::Vtab => 'v',
        }
    }

    pub fn all() -> Vec<Self> {
        vec![
            Self::Apos,
            Self::Quot,
            Self::Query,
            Self::BSlash,
            Self::Alert,
            Self::Bsp,
            Self::Feed,
            Self::Newline,
            Self::Return,
            Self::Tab,
            Self::Vtab,
        ]
    }
}

impl<'a> Parser<'a, &'a [Ch], SimpleEsc> for SimpleEsc {
    fn parse(&self, input: &'a [Ch]) -> ParseResult<'a, &'a [Ch], SimpleEsc> {
        map(pred(any_char, |&ch| ch == self.as_esc_char()), |_| *self).parse(input)
    }
}

impl<'a> Parses<'a> for SimpleEsc {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        let init_parser: Box<dyn Parser<'a, &'a [Ch], Self>> =
            Box::new(none("unexpected error in SimpleEsc::parse_into"));

        Self::all()
            .iter()
            .fold(init_parser, |acc, esc| -> Box<dyn Parser<&[Ch], Self>> {
                Box::new(or_else(
                    move |input| esc.parse(input),
                    move |input| acc.parse(input),
                ))
            })
            .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CEsc {
    Sim(SimpleEsc),
    /// \ + 1-3 octal digits (eager)
    Oct(u32),
    /// \x + 1..n hex digits (eager)
    Hex(u32),
    /// \u hex-quad or \U hex-quad hex-quad
    Ucn(Ucn),
}

impl CEsc {
    pub fn as_char(&self) -> Option<char> {
        match self {
            Self::Sim(esc) => esc.as_char(),
            Self::Oct(value) | Self::Hex(value) => char::from_u32(*value),
            Self::Ucn(value) => Some(value.as_char()),
        }
    }
}

impl<'a> Parses<'a> for CEsc {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        max(
            map(Ucn::parse_into, Self::Ucn),
            right(
                match_literal("\\"),
                max(
                    and_then(
                        range(1..=3, pred(digit_char(), |&ch| ch != '8' && ch != '9')),
                        |octs| {
                            u32::from_str_radix(&String::from_iter(octs.into_iter()), 8)
                                .map(Self::Oct)
                                .map_err(|err| format!("invalid octal escape {:?}", err))
                        },
                    ),
                    max(
                        right(
                            match_literal("x"),
                            and_then(
                                range(1.., pred(any_char, |&ch| ch.is_ascii_hexdigit())),
                                |hexs| -> Result<CEsc, String> {
                                    u32::from_str_radix(&String::from_iter(hexs.into_iter()), 16)
                                        .map(Self::Hex)
                                        .map_err(|err| format!("invalid hex escape {:?}", err))
                                },
                            ),
                        ),
                        map(SimpleEsc::parse_into, Self::Sim),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

/// \u hex-quad or \U hex-quad hex-quad
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ucn {
    ch: char,
}

impl Ucn {
    fn new(ch: char) -> Self {
        Self { ch }
    }

    /// A universal character name shall not specify a character whose short identifier is less than 00A0
    /// other than 0024 ($), 0040 (@), or 0060 (‘), nor one in the range D800 through DFFF inclusive.
    ///
    /// The disallowed characters are the characters in the basic character set and the code positions reserved by ISO/IEC 10646
    /// for control characters, the character DELETE, and the S-zone (reserved for use by UTF–16).
    fn convert(value: u32) -> Option<Self> {
        if value >= 0x00A0 || value == 0x0024 || value == 0x0040 || value == 0x0060 {
            char::from_u32(value).map(Self::new)
        } else {
            None
        }
    }

    pub fn from_u16(value: u16) -> Result<Self, String> {
        Self::convert(value as u32)
            .map(Ok)
            .unwrap_or_else(|| Err(format!("invalid ucn value: \\u{:04x}", value)))
    }

    pub fn from_u32(value: u32) -> Result<Self, String> {
        Self::convert(value)
            .map(Ok)
            .unwrap_or_else(|| Err(format!("invalid ucn value: \\U{:08x}", value)))
    }

    pub fn as_char(&self) -> char {
        self.ch
    }
}

impl<'a> Parses<'a> for Ucn {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        right(
            pred(any_char, |&ch| ch == '\\'),
            or_else(
                right(
                    match_literal("u"),
                    and_then(
                        range(4..=4, pred(any_char, |&ch| ch.is_ascii_hexdigit())),
                        |value| {
                            let as_string = String::from_iter(value.into_iter());
                            let ucn = u16::from_str_radix(as_string.as_str(), 16).unwrap();
                            Ucn::from_u16(ucn)
                        },
                    ),
                ),
                right(
                    match_literal("U"),
                    and_then(
                        range(8..=8, pred(any_char, |&ch| ch.is_ascii_hexdigit())),
                        |value| {
                            let as_string = String::from_iter(value.into_iter());
                            let ucn = u32::from_str_radix(as_string.as_str(), 16).unwrap();
                            Ucn::from_u32(ucn)
                        },
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CChar {
    Esc(CEsc),
    Char(char),
}

impl<'a> Parses<'a> for CChar {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            map(CEsc::parse_into, Self::Esc),
            map(
                pred(any_char, |&ch| ch != '\'' && ch != '\\' && ch != '\n'),
                Self::Char,
            ),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HChar(Ch);
impl<'a> Parses<'a> for HChar {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pred(single::<Ch>(), |&ch| ch.chat() != '>' && ch.chat() != '\n'),
            |ch| Self(ch.clone()),
        )
        .parse(input)
    }
}
impl HChar {
    pub fn as_char(&self) -> char {
        match self {
            Self(ch) => ch.chat(),
        }
    }
    pub fn to_string(chars: &[Self]) -> String {
        String::from_iter(chars.iter().map(|ch| ch.as_char()))
    }
    pub fn reparse(loc: Loc, hchars: &[Ch]) -> Result<PPToken, String> {
        let input: Vec<Ch> = hchars.to_vec();
        let result = match range(0.., SChar::parse_into).parse(&input) {
            Ok((_, res)) => Ok(PPToken::SChars(loc, SEncoding::Char, res)),
            Err(((msg, _), _)) => Err(msg),
        };
        result
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QChar(Ch);
impl<'a> Parses<'a> for QChar {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pred(single::<Ch>(), |&ch| ch.chat() != '\"' && ch.chat() != '\n'),
            |ch| Self(ch.clone()),
        )
        .parse(input)
    }
}
impl QChar {
    pub fn as_char(&self) -> char {
        match self {
            Self(ch) => ch.chat(),
        }
    }
    pub fn to_string(chars: &[Self]) -> String {
        String::from_iter(chars.iter().map(|ch| ch.as_char()))
    }
    pub fn reparse(loc: Loc, qchars: &[Ch]) -> Result<PPToken, String> {
        let input: Vec<Ch> = qchars.to_vec();
        let result = match range(0.., SChar::parse_into).parse(&input) {
            Ok((_, res)) => Ok(PPToken::SChars(loc, SEncoding::Char, res)),
            Err(((msg, _), _)) => Err(msg),
        };
        result
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CEncoding {
    /// no prefix, or upgraded from QChars
    Char,
    /// prefix of L
    Wchar,
    /// prefix of u
    Char16,
    /// prefix of U
    Char32,
}
impl<'a> Parses<'a> for CEncoding {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            map(match_literal("'"), |_| Self::Char),
            or_else(
                map(match_literal("L'"), |_| Self::Wchar),
                or_else(
                    map(match_literal("u'"), |_| Self::Char16),
                    map(match_literal("U'"), |_| Self::Char32),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SEncoding {
    /// no prefix, or upgraded from QChars
    Char,
    /// prefix of u8
    Utf8,
    /// prefix of L
    Wchar,
    /// prefix of u
    Char16,
    /// prefix of U
    Char32,
}
impl<'a> Parses<'a> for SEncoding {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            map(match_literal("\""), |_| Self::Char),
            or_else(
                map(match_literal("u8\""), |_| Self::Utf8),
                or_else(
                    map(match_literal("L\""), |_| Self::Wchar),
                    or_else(
                        map(match_literal("u\""), |_| Self::Char16),
                        map(match_literal("U\""), |_| Self::Char32),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SChar {
    Esc(CEsc),
    Char(char),
}
impl SChar {
    pub fn as_char(&self) -> Option<char> {
        match self {
            Self::Char(ch) => Some(*ch),
            Self::Esc(esc) => esc.as_char(),
        }
    }
    pub fn to_string(chars: &[Self]) -> String {
        String::from_iter(chars.iter().map(|ch| ch.as_char().unwrap_or('?')))
    }
}

impl<'a> Parses<'a> for SChar {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            map(CEsc::parse_into, Self::Esc),
            map(
                pred(any_char, |&ch| ch != '\"' && ch != '\\' && ch != '\n'),
                Self::Char,
            ),
        )
        .parse(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PPNum(pub String);
impl<'a> Parses<'a> for PPNum {
    type Input = &'a [Ch];
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(
                map(pair(ok(dot()), digit_char()), |(dot, digit)| {
                    dot.map(|ch| vec![ch, digit]).unwrap_or_else(|| vec![digit])
                }),
                map(
                    range(
                        0..,
                        max(
                            map(or_else(dot(), pred(any_char, Id::is_ident)), |ch| vec![ch]),
                            max(
                                map(Ucn::parse_into, |ucn| vec![ucn.as_char()]),
                                map(
                                    pair(
                                        pred(any_char, |&ch| {
                                            ch == 'e' || ch == 'E' || ch == 'p' || ch == 'P'
                                        }),
                                        pred(any_char, |&ch| ch == '+' || ch == '-'),
                                    ),
                                    |(letter, sign)| vec![letter, sign],
                                ),
                            ),
                        ),
                    ),
                    |seqs| seqs.concat(),
                ),
            ),
            |(head, tail)| Self(String::from_iter(vec![head, tail].concat().iter())),
        )
        .parse(input)
    }
}

impl TryInto<usize> for PPNum {
    type Error = (Self, ParseIntError);
    fn try_into(self) -> Result<usize, Self::Error> {
        let Self(value) = &self;
        value.parse().map_err(|err| (self, err))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PPReplacement {
    depth: usize,
    after: Vec<PPToken>,
}

impl<'a> PPReplacement {}

#[derive(Debug, Clone)]
/// a sequence of pp-tokens separated by non-newline whitespace.
pub struct PPTokens(pub PPToken, pub Vec<PPToken>);
impl<'a> PPTokens {
    pub fn loc(&'a self) -> &'a Loc {
        match self {
            Self(head, _) => head.loc().unwrap(),
        }
    }
}

impl<'a> Parses<'a> for PPTokens {
    type Input = &'a [PPToken];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pair(
                not_next!(PPToken::LineEnd),
                range(0.., not_next!(PPToken::LineEnd)),
            ),
            |(head, tail)| {
                Self(
                    head.clone(),
                    tail.iter().map(|&token| token.clone()).collect(),
                )
            },
        )
        .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Error, path::Path};

    use crate::source::file::SourceFile;

    use super::PPTokenStream;

    #[test]
    fn give_a_spin() -> Result<(), Error> {
        let file = SourceFile::open(Path::new("tests/hello_world.c"))?;
        if let Ok(tokens) = PPTokenStream::new(file) {
            println!("{:#?}", tokens);
        } else {
            assert!(false);
        }

        Ok(())
    }

    #[test]
    fn expect_eof() -> Result<(), Error> {
        Ok(())
    }
}
