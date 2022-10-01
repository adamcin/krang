use std::{fmt::Debug, ops::RangeBounds};

pub type ParseResult<'a, Input, Output> = Result<(Input, Output), Input>;

pub trait Parser<'a, Input, Output> {
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output>;
}

pub trait Parses<'a>: Sized {
    type Input;
    fn parse_into(input: Self::Input) -> ParseResult<'a, Self::Input, Self>
    where
        Self::Input: 'a;
}

impl<'a, F, Input, Output> Parser<'a, Input, Output> for F
where
    F: Fn(Input) -> ParseResult<'a, Input, Output>,
{
    fn parse(&self, input: Input) -> ParseResult<'a, Input, Output> {
        self(input)
    }
}

pub fn map<'a, P, F, I, A, B>(p: P, map_fn: F) -> impl Parser<'a, I, B>
where
    P: Parser<'a, I, A>,
    F: Fn(A) -> B,
{
    move |input| {
        p.parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

pub fn and_then<'a, P, F, I: 'a + ?Sized, A, B, E>(p: P, map_fn: F) -> impl Parser<'a, &'a I, B>
where
    P: Parser<'a, &'a I, A>,
    F: Fn(A) -> Result<B, E>,
{
    move |input: &'a I| {
        p.parse(input)
            .and_then(|(next_input, result)| match map_fn(result) {
                Ok(result) => Ok((next_input, result)),
                Err(_) => Err(input),
            })
    }
}

pub fn pair<'a, P1, P2, I: 'a + ?Sized, R1, R2>(
    left: P1,
    right: P2,
) -> impl Parser<'a, &'a I, (R1, R2)>
where
    P1: Parser<'a, &'a I, R1>,
    P2: Parser<'a, &'a I, R2>,
{
    move |input| {
        left.parse(input).and_then(|(next_input, result1)| {
            right
                .parse(next_input)
                .map_err(|_| input)
                .map(|(last_input, result2)| (last_input, (result1, result2)))
        })
    }
}

pub fn ok<'a, P, I: 'a + ?Sized + Debug, R>(p: P) -> impl Parser<'a, &'a I, Option<R>>
where
    P: Parser<'a, &'a I, R>,
{
    move |input| match p.parse(input).ok() {
        Some((remaining, output)) => Ok((remaining, Some(output))),
        None => Ok((input, None)),
    }
}

pub fn or_else<'a, P1, P2, I, R>(p: P1, elze: P2) -> impl Parser<'a, I, R>
where
    P1: Parser<'a, I, R>,
    P2: Parser<'a, I, R>,
{
    move |input| p.parse(input).or_else(|input| elze.parse(input))
}

pub fn none<'a, I, R>() -> impl Parser<'a, I, R> {
    move |input| Err(input)
}

pub fn left<'a, P1, P2, I: 'a + ?Sized, R1, R2>(left: P1, r: P2) -> impl Parser<'a, &'a I, R1>
where
    P1: Parser<'a, &'a I, R1>,
    P2: Parser<'a, &'a I, R2>,
{
    map(pair(left, r), |(left, _right)| left)
}

pub fn right<'a, P1, P2, I: 'a + ?Sized, R1, R2>(l: P1, right: P2) -> impl Parser<'a, &'a I, R2>
where
    P1: Parser<'a, &'a I, R1>,
    P2: Parser<'a, &'a I, R2>,
{
    map(pair(l, right), |(_left, right)| right)
}

pub fn pred<'a, P, I: 'a + ?Sized, A, F>(p: P, pred: F) -> impl Parser<'a, &'a I, A>
where
    P: Parser<'a, &'a I, A>,
    F: Fn(&A) -> bool,
{
    move |input| {
        if let Ok((next_input, value)) = p.parse(input) {
            if pred(&value) {
                return Ok((next_input, value));
            }
        }
        Err(input)
    }
}

pub fn range<'a, P, I: 'a + ?Sized, A, B>(p: P, bounds: B) -> impl Parser<'a, &'a I, Vec<A>>
where
    B: RangeBounds<usize>,
    P: Parser<'a, &'a I, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = p.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        if bounds.contains(&result.len()) {
            Ok((input, result))
        } else {
            Err(input)
        }
    }
}

pub fn not<'a, P, I: 'a + ?Sized, A>(p: P) -> impl Parser<'a, &'a I, ()>
where
    P: Parser<'a, &'a I, A>,
{
    move |input| match p.parse(input) {
        Ok(_) => Err(input),
        Err(_) => Ok((input, ())),
    }
}

pub fn until<'a, 'u, P, A, B>(p: P, bounds: B, until: &'u str) -> impl Parser<'a, &'a str, Vec<A>>
where
    B: RangeBounds<usize>,
    P: Parser<'a, &'a str, A>,
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
            Err(input)
        }
    }
}

pub fn peek<'a, A, F, P>(count: usize, pred: F, opt_p: P) -> impl Parser<'a, &'a str, Option<A>>
where
    F: Parser<'a, &'a str, bool>,
    P: Parser<'a, &'a str, A>,
{
    move |input: &'a str| match pred.parse(&input[0..count.min(input.len())]).ok() {
        Some((_, true)) => opt_p
            .parse(input)
            .map(|(remaining, output)| (remaining, Some(output))),
        _ => Ok((input, None)),
    }
}

pub fn zero_or_more<'a, P, I: 'a + ?Sized, A>(p: P) -> impl Parser<'a, &'a I, Vec<A>>
where
    P: Parser<'a, &'a I, A>,
{
    range(p, 0..)
}

pub fn one_or_more<'a, P, I: 'a + ?Sized, A>(p: P) -> impl Parser<'a, &'a I, Vec<A>>
where
    P: Parser<'a, &'a I, A>,
{
    range(p, 1..)
}

pub fn match_literal<'a, 'b>(expected: &'b str) -> impl Parser<'a, &'a str, ()>
where
    'b: 'a,
{
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

pub fn any_char(input: &str) -> ParseResult<&str, char> {
    match input.chars().next() {
        Some(next) => Ok((&input[next.len_utf8()..], next)),
        _ => Err(input),
    }
}

pub fn eof(input: &str) -> ParseResult<&str, ()> {
    if input.is_empty() {
        Ok((input, ()))
    } else {
        Err(input)
    }
}

pub fn whitespace_char<'a>() -> impl Parser<'a, &'a str, char> {
    pred(any_char, |c| c.is_whitespace())
}

pub fn inlinespace_char<'a>() -> impl Parser<'a, &'a str, char> {
    pred(any_char, |c| c.is_whitespace() && c != &'\n' && c != &'\r')
}

pub fn non_nl_char<'a>() -> impl Parser<'a, &'a str, char> {
    pred(any_char, |c| c != &'\n' && c != &'\r')
}

pub fn space1<'a>() -> impl Parser<'a, &'a str, Vec<char>> {
    one_or_more(whitespace_char())
}

pub fn space0<'a>() -> impl Parser<'a, &'a str, Vec<char>> {
    zero_or_more(whitespace_char())
}

pub fn pad1<'a>() -> impl Parser<'a, &'a str, Vec<char>> {
    one_or_more(inlinespace_char())
}

pub fn pad0<'a>() -> impl Parser<'a, &'a str, Vec<char>> {
    zero_or_more(inlinespace_char())
}

pub fn digit_char<'a>() -> impl Parser<'a, &'a str, char> {
    pred(any_char, |c| c.is_ascii_digit())
}

pub fn i16_literal<'a>() -> impl Parser<'a, &'a str, i16> {
    and_then(
        pair(ok(pred(any_char, |c| c == &'-')), range(digit_char(), 0..)),
        |(neg, digits)| {
            let value: String = vec![neg.map(|c| vec![c]).unwrap_or_default(), digits]
                .concat()
                .into_iter()
                .collect();
            value.as_str().parse::<i16>()
        },
    )
}

pub fn newline<'a>() -> impl Parser<'a, &'a str, ()> {
    or_else(eof, or_else(match_literal("\r\n"), match_literal("\n")))
}

pub fn non_nl0<'a>() -> impl Parser<'a, &'a str, Vec<char>> {
    zero_or_more(non_nl_char())
}

pub fn inline_comment<'a>() -> impl Parser<'a, &'a str, String> {
    right(
        match_literal("//"),
        left(
            map(until(any_char, 0.., "\n"), |chars| {
                chars.into_iter().collect()
            }),
            newline(),
        ),
    )
}

pub fn block_comment<'a>() -> impl Parser<'a, &'a str, String> {
    right(
        match_literal("/*"),
        left(
            map(until(any_char, 0.., "*/"), |chars| {
                chars.into_iter().collect()
            }),
            match_literal("*/"),
        ),
    )
}

pub fn drop<'a, A, I, P>(p: P) -> impl Parser<'a, I, ()>
where
    P: Parser<'a, I, A>,
{
    map(p, |_| ())
}

pub fn comspace<'a>() -> impl Parser<'a, &'a str, ()> {
    drop(range(
        or_else(
            drop(space1()),
            or_else(drop(block_comment()), drop(inline_comment())),
        ),
        1..,
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(Ok(("", ())), parse_joe.parse("Hello Joe!"));
        assert_eq!(
            Ok((" Hello Robert!", ())),
            parse_joe.parse("Hello Joe! Hello Robert!")
        );
        assert_eq!(Err("Hello Mike!"), parse_joe.parse("Hello Mike!"));
    }

    #[test]
    fn digit_parser() {
        let parse_digit = digit_char();
        assert_eq!(Ok(("abc", '1')), parse_digit.parse("1abc"));
        assert_eq!(Err("abcd"), parse_digit.parse("abcd"));
    }

    #[test]
    fn whitespace_parser() {
        let parse_wsp = whitespace_char();
        assert_eq!(Ok(("abc", ' ')), parse_wsp.parse(" abc"));
        assert_eq!(Err("abcd"), parse_wsp.parse("abcd"));
    }

    #[test]
    fn newline_parser() {
        let parse_nl = newline();
        assert_eq!(Ok(("Hey", ())), parse_nl.parse("\nHey"));
        assert_eq!(Ok(("abc", ())), parse_nl.parse("\r\nabc"));
        assert_eq!(Err("abcd"), parse_nl.parse("abcd"));
    }

    #[test]
    fn peek_parser() {
        let parse_peek = peek(
            4,
            |p: &'static str| Ok((p, p.contains('='))),
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
        );
        assert_eq!(Ok(("f", Some("AMD".to_owned()))), parse_peek.parse("AMD=f"));
        assert_eq!(Ok(("f", Some("AD".to_owned()))), parse_peek.parse("AD=f"));
        assert_eq!(Ok(("ABCD", None)), parse_peek.parse("ABCD"));
        assert_eq!(Ok(("ABC", None)), parse_peek.parse("ABC"));
        assert_eq!(Err("ABC="), parse_peek.parse("ABC="));
    }

    #[test]
    fn simple_test() {}
}
