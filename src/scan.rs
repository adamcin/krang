use std::{fmt::Debug, io::Error, ops::RangeBounds, path::PathBuf, rc::Rc};

use crate::{
    parse::*,
    read::{CharReaderIter, TryingIterator},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Loc {
    path: Rc<String>,
    byte: usize,
    line: usize,
    col: usize,
}

impl Loc {
    pub fn init(path: Rc<String>) -> Self {
        Loc {
            path,
            byte: 0,
            line: 0,
            col: 0,
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn path(&self) -> PathBuf {
        let mut buf = PathBuf::new();
        buf.push(self.path.as_str());
        buf
    }

    pub fn next(&self, ch: char) -> Self {
        let (line, col) = if ch == '\n' {
            (self.line + 1, 0)
        } else {
            (self.line, self.col + 1)
        };
        Self {
            path: self.path.clone(),
            byte: self.byte + ch.len_utf8(),
            line,
            col,
        }
    }

    pub fn range_with(&self, to: &Loc) -> LocRange {
        LocRange::new(self.clone(), to.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LocRange {
    first: Loc,
    last: Loc,
}

impl LocRange {
    pub fn new(first: Loc, last: Loc) -> Self {
        if first.path != last.path {
            panic!("mismatched LocRange first:{:?} last:{:?}", first, last);
        }
        if first.byte > last.byte {
            Self { last, first }
        } else {
            Self { first, last }
        }
    }

    pub fn first(&self) -> &Loc {
        &self.first
    }

    pub fn last(&self) -> &Loc {
        &self.last
    }
}

/// represents a lexable character
#[derive(PartialEq, Eq, Clone)]
pub struct Ch {
    loc: Loc,
    chat: char,
}

impl Ch {
    pub fn new(loc: Loc, chat: char) -> Self {
        Self { loc, chat }
    }

    pub fn loc(&self) -> &Loc {
        &self.loc
    }

    pub fn chat(&self) -> char {
        self.chat
    }

    pub fn next(&self, chat: char) -> Self {
        let next_loc = self.loc.next(self.chat);
        Self::new(next_loc, chat)
    }

    pub fn as_string(poss: &[Self]) -> String {
        String::from_iter(poss.iter().map(|pos| pos.chat()))
    }

    pub fn as_space(&self) -> Self {
        self.as_char(' ')
    }

    pub fn as_char(&self, chat: char) -> Self {
        Self::new(self.loc.clone(), chat)
    }
}

impl Debug for Ch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.chat())
    }
}

pub struct ChIter<'a> {
    next: Loc,
    chars: CharReaderIter<'a>,
}

impl<'a> ChIter<'a> {
    pub fn new(path: Rc<String>, chars: CharReaderIter<'a>) -> Self {
        Self {
            next: Loc::init(path),
            chars,
        }
    }
}

impl<'a> TryingIterator for ChIter<'a> {
    type OkItem = Ch;

    fn try_next(&mut self) -> Result<Option<Self::OkItem>, Error> {
        match self.chars.try_next() {
            Ok(Some(ch)) => {
                let pos = Ch::new(self.next.clone(), ch);
                self.next = self.next.next(ch);
                Ok(Some(pos))
            }
            Err(err) => Err(err),
            _ => Ok(None),
        }
    }
}

pub type Atom = Ch;
pub type Atoms = [Atom];

pub fn until<'a, 'u, P, A, B, C: Clone>(
    p: P,
    bounds: B,
    until: &'u str,
) -> impl Parser<'a, &'a [Ch], Vec<A>, C>
where
    B: RangeBounds<usize> + Debug,
    P: Parser<'a, &'a [Ch], A, C>,
    'u: 'a,
{
    let parser = right(not(match_literal(until)), p);
    move |ctx: C, mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(ctx.clone(), input) {
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

pub fn peek<'a, A, F, P, C: Clone>(
    count: usize,
    pred: F,
    opt_p: P,
) -> impl Parser<'a, &'a [Ch], Option<A>, C>
where
    F: Parser<'a, &'a [Ch], bool, C>,
    P: Parser<'a, &'a [Ch], A, C>,
{
    move |ctx: C, input: &'a Atoms| match pred
        .parse(ctx.clone(), &input[0..count.min(input.len())])
        .ok()
    {
        Some((_, true)) => opt_p
            .parse(ctx, input)
            .map(|(remaining, output)| (remaining, Some(output))),
        _ => Ok((input, None)),
    }
}

pub fn match_literal<'a, 'b, C>(expected: &'b str) -> impl Parser<'a, &'a [Ch], (), C> + 'b
where
    'b: 'a,
{
    move |ctx, input: &'a Atoms| match input.get(0..expected.len()) {
        Some(next) if Atom::as_string(next) == expected => Ok((&input[expected.len()..], ())),
        _ => Err((format!("match_literal not matched"), input)),
    }
}

pub fn any_char<'a, C>(ctx: C, input: &'a [Ch]) -> ParseResult<&'a [Ch], char> {
    msg(
        map(single(), |atom: &Atom| atom.chat()),
        "any_char not matched",
    )
    .parse(ctx, input)
}

pub fn dot<'a, C>() -> impl Parser<'a, &'a [Ch], char, C> {
    msg(pred(any_char, |&ch| ch == '.'), "dot not matched")
}

pub fn whitespace_atom<'a, C>() -> impl Parser<'a, &'a [Ch], &'a Ch, C> {
    pred(single::<Ch, C>(), |&ch| ch.chat().is_ascii_whitespace())
}

pub fn nonwsp<'a, C>() -> impl Parser<'a, &'a [Ch], &'a Ch, C> {
    pred(single::<Ch, C>(), |&ch| !ch.chat().is_ascii_whitespace())
}

pub fn whitespace_char<'a, C>() -> impl Parser<'a, &'a [Ch], char, C> {
    msg(
        pred(any_char, |c| c.is_ascii_whitespace()),
        "whitespace_char not matched",
    )
}

pub fn inlinespace_atom<'a, C>() -> impl Parser<'a, &'a [Ch], &'a Ch, C> {
    pred(single::<Ch, C>(), |&ch| {
        ch.chat() == ' ' || ch.chat() == '\t'
    })
}

pub fn non_nl_char<'a, C>() -> impl Parser<'a, &'a [Ch], char, C> {
    msg(
        pred(any_char, |c| c != &'\n' && c != &'\r'),
        "non_nl_char not matched",
    )
}

pub fn space1<'a, C: Clone>() -> impl Parser<'a, &'a [Ch], Vec<&'a Ch>, C> {
    one_or_more(whitespace_atom())
}

pub fn space0<'a, C: Clone>() -> impl Parser<'a, &'a [Ch], Vec<&'a Ch>, C> {
    zero_or_more(whitespace_atom())
}

pub fn pad1<'a, C: Clone>() -> impl Parser<'a, &'a [Ch], Vec<&'a Ch>, C> {
    one_or_more(inlinespace_atom())
}

pub fn pad0<'a, C: Clone>() -> impl Parser<'a, &'a [Ch], Vec<&'a Ch>, C> {
    zero_or_more(inlinespace_atom())
}

pub fn digit_char<'a, C>() -> impl Parser<'a, &'a [Ch], char, C> {
    msg(
        pred(any_char, |c| c.is_ascii_digit()),
        "digit_char not matched",
    )
}

pub fn newline<'a, C: Clone>() -> impl Parser<'a, &'a [Ch], (), C> {
    msg(
        or_else(match_literal("\r\n"), match_literal("\n")),
        "newline not matched",
    )
}

pub fn non_nl0<'a, C: Clone>() -> impl Parser<'a, &'a [Ch], Vec<char>, C> {
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
            parse_joe.parse(Rc::new(true), hello_joe.stream())
        );
        assert_eq!(
            Ok((&hello_joe_and_robert[10..], ())),
            parse_joe.parse(Rc::new(true), hello_joe_and_robert.stream())
        );
        assert_eq!(
            Err(("match_literal not matched".to_owned(), hello_mike.stream())),
            parse_joe.parse(Rc::new(true), hello_mike.stream())
        );
        Ok(())
    }

    #[test]
    fn digit_parser() -> Result<(), Error> {
        let letters = SourceFile::inline("abcd")?;
        let numlet = SourceFile::inline("1abc")?;
        let parse_digit = digit_char();
        assert_eq!(
            Ok((&numlet[1..], '1')),
            parse_digit.parse(Rc::new(true), numlet.stream())
        );
        assert_eq!(
            Err(("digit_char not matched".to_owned(), letters.stream())),
            parse_digit.parse(Rc::new(true), letters.stream())
        );
        Ok(())
    }

    #[test]
    fn whitespace_parser() -> Result<(), Error> {
        let letters = SourceFile::inline("abcd")?;
        let wsplet = SourceFile::inline(" abc")?;
        let parse_wsp = whitespace_char();
        assert_eq!(
            Ok((&wsplet[1..], ' ')),
            parse_wsp.parse(Rc::new(true), wsplet.stream())
        );
        assert_eq!(
            Err(("whitespace_char not matched".to_owned(), letters.stream())),
            parse_wsp.parse(Rc::new(true), letters.stream())
        );
        Ok(())
    }

    #[test]
    fn newline_parser() -> Result<(), Error> {
        let letters = SourceFile::inline("abcd")?;
        let carret = SourceFile::inline("\r\nabc")?;
        let justnew = SourceFile::inline("\nHey")?;
        let parse_nl = newline();
        assert_eq!(
            Ok((&justnew[1..], ())),
            parse_nl.parse(Rc::new(true), justnew.stream())
        );
        assert_eq!(
            Ok((&carret[2..], ())),
            parse_nl.parse(Rc::new(true), carret.stream())
        );
        assert_eq!(
            Err(("newline not matched".to_owned(), letters.stream())),
            parse_nl.parse(Rc::new(true), letters.stream())
        );
        Ok(())
    }

    fn is_eq<'a>(ctx: Rc<bool>, p: &'a [Ch]) -> ParseResult<&'a [Ch], bool> {
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
                            1..=3,
                            pred(any_char, |c| c == &'A' || c == &'D' || c == &'M'),
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
            parse_peek.parse(Rc::new(true), amd_eq_f.stream())
        );
        assert_eq!(
            Ok((&ad_eq_f[ad_eq_f.len() - 1..], Some("AD".to_owned()))),
            parse_peek.parse(Rc::new(true), ad_eq_f.stream())
        );
        assert_eq!(
            Ok((abcd.stream(), None)),
            parse_peek.parse(Rc::new(true), abcd.stream())
        );
        assert_eq!(
            Ok((abc.stream(), None)),
            parse_peek.parse(Rc::new(true), abc.stream())
        );
        assert_eq!(
            Err(("equal peeker not matched".to_owned(), abc_eq.stream())),
            parse_peek.parse(Rc::new(true), abc_eq.stream())
        );

        Ok(())
    }
}
