use std::{fmt::Debug, ops::RangeBounds};

macro_rules! parse_next {
    ($(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? => $replace:expr, $($arg:tt)*) => {
        and_then(single(), |_atom| { if false { println!("parse_next! atom={:?}", _atom); } match _atom {
            $( $pattern )|+ $( if $guard )? => Ok($replace),
            _ => Err((format!($($arg)*))),
        }})
    };
}

macro_rules! match_next {
    ($(|)? $( $pattern:pat_param )|+ $( if $guard: expr )?) => {
        pred(single(), |_atom| match _atom {
            $( $pattern )|+ $( if $guard )? => true,
            _ => false,
        })
    };
}

macro_rules! not_next {
    ($(|)? $( $pattern:pat_param )|+ $( if $guard: expr )?) => {
        pred(single(), |_atom| match _atom {
            $( $pattern )|+ $( if $guard )? => false,
            _ => true,
        })
    };
}
pub type ParseError<Input> = (String, Input);
pub type ParseResult<'a, Input, Output> =
    Result<(Input, Output), (ParseError<Input>, Vec<ParseError<Input>>)>;

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

pub fn map_input<'a, P, F, I: 'a + ?Sized, A, B>(p: P, map_fn: F) -> impl Parser<'a, &'a I, B>
where
    P: Parser<'a, &'a I, A>,
    F: Fn(A, &'a I) -> B,
{
    move |input| {
        p.parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result, input)))
    }
}

pub fn and_then<'a, P, F, I: 'a + ?Sized, A, B>(p: P, map_fn: F) -> impl Parser<'a, &'a I, B>
where
    P: Parser<'a, &'a I, A>,
    F: Fn(A) -> Result<B, String>,
{
    move |input: &'a I| {
        p.parse(input)
            .and_then(|(next_input, result)| match map_fn(result) {
                Ok(result) => Ok((next_input, result)),
                Err(msg) => Err(((msg, input), Vec::new())),
            })
    }
}

pub fn and_then_input<'a, P, F, I: 'a + ?Sized, A, B>(p: P, map_fn: F) -> impl Parser<'a, &'a I, B>
where
    P: Parser<'a, &'a I, A>,
    F: Fn(A, &'a I) -> Result<B, String>,
{
    move |input: &'a I| {
        p.parse(input)
            .and_then(|(next_input, result)| match map_fn(result, input) {
                Ok(result) => Ok((next_input, result)),
                Err(msg) => Err(((msg, input), Vec::new())),
            })
    }
}

pub fn msg<'a, P, I, A>(p: P, msg: &'a str) -> impl Parser<'a, I, A>
where
    P: Parser<'a, I, A>,
{
    move |input| {
        p.parse(input)
            .map_err(|((orig, err_in), prev)| ((msg.to_owned(), err_in), prev))
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
                .map_err(|((msg, nested), prev)| {
                    let msg2 = msg.as_str().to_owned();
                    ((msg, input), vec![vec![(msg2, nested)], prev].concat())
                })
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

/// wrap a parser with a gate that always fails. rename to 'free' to quickly switch to allow success
pub fn fail<'a, P, I: 'a + ?Sized + Debug, R>(p: P, msg: &'a str) -> impl Parser<'a, &'a I, R>
where
    P: Parser<'a, &'a I, R>,
{
    move |input| {
        println!("*** fail! msg: {}, input: {:?}", msg, input);
        Err(((format!("fail: {}", msg), input), Vec::new()))
    }
}

/// wrap a parser with a gate that logs a message before parsing. rename to 'fail' to quickly switch to a failure mode
pub fn free<'a, P, I: 'a + Debug, R: Debug>(p: P, msg: &'a str) -> impl Parser<'a, &'a [I], R>
where
    P: Parser<'a, &'a [I], R>,
{
    move |input: &'a [I]| {
        println!("*** free! msg: {}, input: {:?}", msg, input.get(0));
        let output = p.parse(input);
        println!(
            "*** free! msg: {}, output: {}",
            msg,
            max_len(120, format!("{:?}", output))
        );
        output
    }
}

fn max_len(max: usize, s: String) -> String {
    if s.len() <= max {
        s
    } else {
        (&s[0..max]).to_owned()
    }
}

pub fn max<'a, P1, P2, I: 'a + Debug, R: Debug>(p1: P1, p2: P2) -> impl Parser<'a, &'a [I], R>
where
    P1: Parser<'a, &'a [I], R>,
    P2: Parser<'a, &'a [I], R>,
{
    move |input| match (p1.parse(input), p2.parse(input)) {
        (Ok((rem1, res1)), Ok((rem2, res2))) => {
            if rem2.len() < rem1.len() {
                Ok((rem2, res2))
            } else {
                Ok((rem1, res1))
            }
        }
        (Err(_), r2 @ Ok(_)) => r2,
        (r1, _) => r1,
    }
}

pub fn single<'a, I: 'a>() -> impl Parser<'a, &'a [I], &'a I> {
    move |input: &'a [I]| match input.iter().next() {
        Some(elem) => Ok((&input[1..], elem)),
        None => Err((("single not matched".to_owned(), input), Vec::new())),
    }
}

pub fn or_else<'a, P1, P2, I: Debug, R: Debug>(p: P1, elze: P2) -> impl Parser<'a, I, R>
where
    P1: Parser<'a, I, R>,
    P2: Parser<'a, I, R>,
{
    move |input| p.parse(input).or_else(|((_, input), _)| elze.parse(input))
}

pub fn none<'a, I, R>(msg: &'a str) -> impl Parser<'a, I, R> {
    move |input| Err(((msg.to_owned(), input), Vec::new()))
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
        Err((("pred not matched".to_owned(), input), Vec::new()))
    }
}

pub fn range<'a, P, I: 'a + ?Sized, A: Debug, B>(bounds: B, p: P) -> impl Parser<'a, &'a I, Vec<A>>
where
    B: RangeBounds<usize> + Debug,
    P: Parser<'a, &'a I, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = p.parse(input) {
            input = next_input;
            if result.len() > 1000 {
                println!("len: {}, next: {:?}", result.len(), next_item);
            }
            result.push(next_item);
        }

        if bounds.contains(&result.len()) {
            Ok((input, result))
        } else {
            Err((
                (format!("range not matched. bounds {:?}", bounds), input),
                Vec::new(),
            ))
        }
    }
}

pub fn not<'a, P, I: 'a + ?Sized, A>(p: P) -> impl Parser<'a, &'a I, ()>
where
    P: Parser<'a, &'a I, A>,
{
    move |input| match p.parse(input) {
        Ok(_) => Err((("matched. not!".to_owned(), input), Vec::new())),
        Err(..) => Ok((input, ())),
    }
}

pub fn zero_or_more<'a, P, I: 'a + ?Sized, A: Debug>(p: P) -> impl Parser<'a, &'a I, Vec<A>>
where
    P: Parser<'a, &'a I, A>,
{
    range(0.., p)
}

pub fn one_or_more<'a, P, I: 'a + ?Sized, A: Debug>(p: P) -> impl Parser<'a, &'a I, Vec<A>>
where
    P: Parser<'a, &'a I, A>,
{
    range(1.., p)
}

pub fn drop<'a, A, I, P>(p: P) -> impl Parser<'a, I, ()>
where
    P: Parser<'a, I, A>,
{
    map(p, |_| ())
}
