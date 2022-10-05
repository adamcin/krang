use std::{fmt::Debug, io::Error, ops::RangeBounds};

use crate::{
    parse::*,
    punct::Punctuator,
    scan::{Ch, Loc},
};

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
pub enum PPToken<'a> {
    /// header-name
    ///
    /// header name preprocessing tokens are recognized only within #include preprocessing directives and in
    /// implementation-defined locations within #pragma directives. In such contexts, a
    /// sequence of characters that could be either a header name or a string literal is recognized
    /// as the former.
    ///
    /// H-chars are delimited by < and >
    HChars(Loc<'a>, Vec<HChar>),
    /// Q-chars are delimited only by "
    QChars(Loc<'a>, Vec<QChar<'a>>),
    /// identifier
    ///
    /// An identifier is a sequence of nondigit characters (including the underscore _, the
    /// lowercase and uppercase Latin letters, and other characters) and digits, which designates
    /// one or more entities as described in 6.2.1. Lowercase and uppercase letters are distinct.
    /// There is no specific limit on the maximum length of an identifier.
    Id(Loc<'a>, Vec<IChar>),
    /// pp-number
    ///
    /// A preprocessing number begins with a digit optionally preceded by a period (.) and may
    /// be followed by valid identifier characters and the character sequences e+, e-, E+, E-,
    /// p+, p-, P+, or P-.
    Num(Loc<'a>, Vec<NChar>),
    /// character-constant
    CChars(Loc<'a>, CEncoding, Vec<CChar>),
    /// string-literal
    SChars(Loc<'a>, SEncoding, Vec<SChar>),
    /// punctuator
    Punct(Loc<'a>, Punctuator),
    /// each non-white-space character that cannot be one of the above
    /// If a ' or a " character matches the last category, the behavior is
    /// undefined.
    Other(Vec<&'a Ch<'a>>),
}

impl<'a> PPToken<'a> {
    pub fn parse_id(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        none("parse_id not implemented").parse(input)
    }

    pub fn parse_num(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        none("parse_num not implemented").parse(input)
    }

    pub fn parse_hchars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        none("parse_hchars not implemented").parse(input)
    }

    pub fn parse_cchars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(
            pair(
                CEncoding::parse_into,
                left(range(CChar::parse_into, 0..), match_literal("'")),
            ),
            |(enc, value), i| Self::CChars(*i[0].loc(), enc, value),
        )
        .parse(input)
    }

    pub fn parse_qchars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(
            right(
                match_literal("\""),
                left(range(QChar::parse_into, 0..), match_literal("\"")),
            ),
            |value, i| Self::QChars(*i[0].loc(), value),
        )
        .parse(input)
    }

    pub fn parse_schars(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(
            pair(
                SEncoding::parse_into,
                left(range(SChar::parse_into, 0..), match_literal("\"")),
            ),
            |(enc, value), i| Self::SChars(*i[0].loc(), enc, value),
        )
        .parse(input)
    }

    pub fn parse_punct(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map_input(Punctuator::parse_into, |punct, i| {
            Self::Punct(*i[0].loc(), punct)
        })
        .parse(input)
    }

    pub fn parse_other(
        input: <Self as Parses<'a>>::Input,
    ) -> ParseResult<'_, <Self as Parses>::Input, Self> {
        map(range(nonwsp(), 1..), Self::Other).parse(input)
    }
}

impl<'a> Parses<'a> for PPToken<'a> {
    type Input = &'a [Ch<'a>];

    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        or_else(
            Self::parse_id,
            or_else(
                Self::parse_num,
                or_else(
                    Self::parse_cchars,
                    or_else(
                        Self::parse_schars,
                        or_else(Self::parse_punct, Self::parse_other),
                    ),
                ),
            ),
        )
        .parse(input)
    }
}

pub enum CEsc {
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
    /// \ + 1-3 octal digits (eager)
    Oct(u64),
    /// \x + 1..n hex digits (eager)
    Hex(u64),
    /// \u hex-quad or \U hex-quad hex-quad
    Ucn(Ucn),
}

impl<'a> Parses<'a> for CEsc {
    type Input = &'a Atoms<'a>;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        none("CEsc::parses_into unimplemented").parse(input)
    }
}

/// \u hex-quad or \U hex-quad hex-quad
pub struct Ucn(u128);

pub enum IChar {
    Char(char),
    Ucn(Ucn),
}

pub enum CChar {
    Esc(CEsc),
    Char(char),
}

impl<'a> Parses<'a> for CChar {
    type Input = &'a Atoms<'a>;
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

pub struct HChar(char);
impl<'a> Parses<'a> for HChar {
    type Input = &'a Atoms<'a>;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(pred(any_char, |&ch| ch != '>' && ch != '\n'), Self).parse(input)
    }
}

pub struct QChar<'a>(&'a Ch<'a>);
impl<'a> QChar<'a> {
    pub fn reparse(loc: &Loc<'a>, qchars: &[&'a Ch]) -> Result<PPToken<'a>, String> {
        let input: Vec<Ch<'a>> = qchars.iter().map(|&ch| ch.to_owned()).collect();
        let result = match range(SChar::parse_into, 0..).parse(&input) {
            Ok((_, res)) => Ok(PPToken::SChars(*loc, SEncoding::Char, res)),
            Err((msg, _)) => Err(msg),
        };
        result
    }
}

impl<'a> Parses<'a> for QChar<'a> {
    type Input = &'a Atoms<'a>;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a,
    {
        map(
            pred(single::<Atom<'a>>(), |&ch| {
                ch.chat() != '\"' && ch.chat() != '\n'
            }),
            Self,
        )
        .parse(input)
    }
}

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
    type Input = &'a Atoms<'a>;
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
    type Input = &'a Atoms<'a>;
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

pub enum SChar {
    Esc(CEsc),
    Char(char),
}

impl<'a> Parses<'a> for SChar {
    type Input = &'a Atoms<'a>;
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

pub enum NChar {
    Char(char),
    Ucn(Ucn),
}

pub type Atom<'p> = Ch<'p>;
pub type Atoms<'p> = [Atom<'p>];

pub fn until<'a, 'u, P, A, B>(
    p: P,
    bounds: B,
    until: &'u str,
) -> impl Parser<'a, &'a Atoms<'a>, Vec<A>>
where
    B: RangeBounds<usize> + Debug,
    P: Parser<'a, &'a Atoms<'a>, A>,
    'u: 'a,
{
    let parser = right(not(match_literal(until)), p);
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        if bounds.contains(&result.len()) {
            Ok((input, result))
        } else {
            Err((
                format!("until '{until}' not matched in bounds {:?}", bounds),
                input,
            ))
        }
    }
}

pub fn peek<'a, A, F, P>(
    count: usize,
    pred: F,
    opt_p: P,
) -> impl Parser<'a, &'a Atoms<'a>, Option<A>>
where
    F: Parser<'a, &'a Atoms<'a>, bool>,
    P: Parser<'a, &'a Atoms<'a>, A>,
{
    move |input: &'a Atoms| match pred.parse(&input[0..count.min(input.len())]).ok() {
        Some((_, true)) => opt_p
            .parse(input)
            .map(|(remaining, output)| (remaining, Some(output))),
        _ => Ok((input, None)),
    }
}

pub fn match_literal<'a, 'b>(expected: &'b str) -> impl Parser<'a, &'a Atoms<'a>, ()>
where
    'b: 'a,
{
    move |input: &'a Atoms| match input.get(0..expected.len()) {
        Some(next) if Atom::as_string(next) == expected => Ok((&input[expected.len()..], ())),
        _ => Err((format!("match_literal not matched '{expected}'"), input)),
    }
}

pub fn any_char<'a>(input: &'a Atoms<'a>) -> ParseResult<&'a Atoms<'a>, char> {
    msg(
        map(single(), |atom: &Atom| atom.chat()),
        "any_char not matched",
    )
    .parse(input)
}

pub fn eof<'a>(input: &'a Atoms<'a>) -> ParseResult<&'a Atoms<'a>, ()> {
    if input.is_empty() {
        Ok((input, ()))
    } else {
        Err(("eof not matched".to_owned(), input))
    }
}

pub fn whitespace_atom<'a>() -> impl Parser<'a, &'a Atoms<'a>, &'a Atom<'a>> {
    pred(single::<Atom<'a>>(), |&ch| ch.chat().is_whitespace())
}

pub fn nonwsp<'a>() -> impl Parser<'a, &'a Atoms<'a>, &'a Atom<'a>> {
    pred(single::<Atom<'a>>(), |&ch| !ch.chat().is_whitespace())
}

pub fn whitespace_char<'a>() -> impl Parser<'a, &'a Atoms<'a>, char> {
    pred(any_char, |c| c.is_whitespace())
}

pub fn inlinespace_char<'a>() -> impl Parser<'a, &'a Atoms<'a>, char> {
    pred(any_char, |c| c.is_whitespace() && c != &'\n' && c != &'\r')
}

pub fn non_nl_char<'a>() -> impl Parser<'a, &'a Atoms<'a>, char> {
    msg(
        pred(any_char, |c| c != &'\n' && c != &'\r'),
        "non_nl_char not matched",
    )
}

pub fn space1<'a>() -> impl Parser<'a, &'a Atoms<'a>, Vec<&'a Atom<'a>>> {
    one_or_more(whitespace_atom())
}

pub fn space0<'a>() -> impl Parser<'a, &'a Atoms<'a>, Vec<&'a Atom<'a>>> {
    zero_or_more(whitespace_atom())
}

pub fn pad1<'a>() -> impl Parser<'a, &'a Atoms<'a>, Vec<char>> {
    one_or_more(inlinespace_char())
}

pub fn pad0<'a>() -> impl Parser<'a, &'a Atoms<'a>, Vec<char>> {
    zero_or_more(inlinespace_char())
}

pub fn digit_char<'a>() -> impl Parser<'a, &'a Atoms<'a>, char> {
    msg(
        pred(any_char, |c| c.is_ascii_digit()),
        "digit_char not matched",
    )
}

pub fn newline<'a>() -> impl Parser<'a, &'a Atoms<'a>, ()> {
    msg(
        or_else(eof, or_else(match_literal("\r\n"), match_literal("\n"))),
        "newline not matched",
    )
}

pub fn non_nl0<'a>() -> impl Parser<'a, &'a Atoms<'a>, Vec<char>> {
    zero_or_more(non_nl_char())
}

#[cfg(test)]
mod test {
    use std::io::Error;

    use crate::source::file::SourceFile;

    use super::*;

    #[test]
    fn literal_parser() -> Result<(), Error> {
        let hello_mike = SourceFile::inline("Hello Mike!")?;
        let hello_joe_and_robert = SourceFile::inline("Hello Joe! Hello Robert!")?;
        let hello_joe = SourceFile::inline("Hello Joe!")?;
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(
            Ok((&hello_joe[hello_joe.len()..], ())),
            parse_joe.parse(hello_joe.stream())
        );
        assert_eq!(
            Ok((&hello_joe_and_robert[10..], ())),
            parse_joe.parse(hello_joe_and_robert.stream())
        );
        assert_eq!(
            Err((
                "match_literal not matched 'Hello Joe!'".to_owned(),
                hello_mike.stream()
            )),
            parse_joe.parse(hello_mike.stream())
        );
        Ok(())
    }

    #[test]
    fn digit_parser() -> Result<(), Error> {
        let letters = SourceFile::inline("abcd")?;
        let numlet = SourceFile::inline("1abc")?;
        let parse_digit = digit_char();
        assert_eq!(Ok((&numlet[1..], '1')), parse_digit.parse(numlet.stream()));
        assert_eq!(
            Err(("digit_char not matched".to_owned(), letters.stream())),
            parse_digit.parse(letters.stream())
        );
        Ok(())
    }

    #[test]
    fn whitespace_parser() -> Result<(), Error> {
        let letters = SourceFile::inline("abcd")?;
        let wsplet = SourceFile::inline(" abc")?;
        let parse_wsp = whitespace_char();
        assert_eq!(Ok((&wsplet[1..], ' ')), parse_wsp.parse(wsplet.stream()));
        assert_eq!(
            Err(("whitespace_char not matched".to_owned(), letters.stream())),
            parse_wsp.parse(letters.stream())
        );
        Ok(())
    }

    #[test]
    fn newline_parser() -> Result<(), Error> {
        let letters = SourceFile::inline("abcd")?;
        let carret = SourceFile::inline("\r\nabc")?;
        let justnew = SourceFile::inline("\nHey")?;
        let parse_nl = newline();
        assert_eq!(Ok((&justnew[1..], ())), parse_nl.parse(justnew.stream()));
        assert_eq!(Ok((&carret[2..], ())), parse_nl.parse(carret.stream()));
        assert_eq!(
            Err(("newline not matched".to_owned(), letters.stream())),
            parse_nl.parse(letters.stream())
        );
        Ok(())
    }

    fn is_eq<'a>(p: &'a [Ch<'a>]) -> ParseResult<&'a [Ch<'a>], bool> {
        Ok((p, p.iter().any(|q| q.chat() == '=')))
    }

    #[test]
    fn peek_parser() -> Result<(), Error> {
        let abc_eq = SourceFile::inline("ABC=")?;
        let abc = SourceFile::inline("ABC")?;
        let abcd = SourceFile::inline("ABCD")?;
        let ad_eq_f = SourceFile::inline("AD=f")?;
        let amd_eq_f = SourceFile::inline("AMD=f")?;
        let parse_peek = msg(
            peek(
                4,
                is_eq,
                map(
                    left(
                        range(
                            pred(any_char, |c| c == &'A' || c == &'D' || c == &'M'),
                            1..=3,
                        ),
                        match_literal("="),
                    ),
                    |chars| -> String { chars.into_iter().collect() },
                ),
            ),
            "equal peeker not matched",
        );
        assert_eq!(
            Ok((&amd_eq_f[amd_eq_f.len() - 1..], Some("AMD".to_owned()))),
            parse_peek.parse(amd_eq_f.stream())
        );
        assert_eq!(
            Ok((&ad_eq_f[ad_eq_f.len() - 1..], Some("AD".to_owned()))),
            parse_peek.parse(ad_eq_f.stream())
        );
        assert_eq!(Ok((abcd.stream(), None)), parse_peek.parse(abcd.stream()));
        assert_eq!(Ok((abc.stream(), None)), parse_peek.parse(abc.stream()));
        assert_eq!(
            Err(("equal peeker not matched".to_owned(), abc_eq.stream())),
            parse_peek.parse(abc_eq.stream())
        );

        Ok(())
    }
}
