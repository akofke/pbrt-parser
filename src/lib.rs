use std::convert::{TryFrom, TryInto, Infallible};
use std::fmt::{Debug, Display};

use nom::branch::alt;
use nom::bytes::complete::{tag, take_till};
use nom::character::complete::{multispace1, none_of};
use nom::combinator::{map, map_res, opt};
use nom::{IResult, InputTakeAtPosition, AsBytes};
use nom::multi::{many0, separated_list1};
use nom::number::complete::float;
use nom::sequence::{delimited, terminated};

pub use params::{Param, ParamVal};
pub use parser::{ParserError, PbrtParser, PbrtScene};
pub use statements::{HeaderStmt, WorldStmt};
pub use transform::TransformStmt;
use once_cell::sync::Lazy;
use crate::interner::StringInterner;
use std::sync::Arc;
use nom::Err::Error;
use nom::error::ErrorKind;

#[macro_use]
pub mod macros;
mod params;
pub mod parser;
pub mod statements;
mod transform;
mod interner;

pub type Float2 = [f32; 2];
pub type Float3 = [f32; 3];

pub static STR_POOL: Lazy<StringInterner> = Lazy::new(|| Default::default());

fn quoted_string(s: &str) -> IResult<&str, Arc<str>> {
    let (s, _) = tag("\"")(s)?;
    let (s, contents) = s.split_at_position_complete(|c| c == '\"')?;
    let (s, _) = tag("\"")(s)?;
    let interned = STR_POOL.get_or_intern(contents);
    Ok((s, interned))
}

fn quoted_string_owned(s: &str) -> IResult<&str, String> {
    map(
        delimited(tag("\""), many0(none_of("\"")), tag("\"")),
        |chars: Vec<char>| chars.into_iter().collect(),
    )(s) 
}

#[inline]
fn ws_term<'a, T>(
    f: impl Fn(&'a str) -> IResult<&'a str, T>,
) -> impl FnMut(&'a str) -> IResult<&'a str, T> {
    terminated(f, ws_or_comment)
}

#[inline]
pub fn opt_ws(s: &str) -> IResult<&str, ()> {
    let s = unsafe { std::str::from_utf8_unchecked(eat_opt_ws(s.as_bytes())) };
    Ok((s, ()))
}

#[inline]
fn opt_ws_term<'a, T>(
    f: impl FnMut(&'a str) -> IResult<&'a str, T>,
) -> impl FnMut(&'a str) -> IResult<&'a str, T> {
    terminated(f, opt_ws)
}

fn fixed_float_list<A>(s: &str) -> IResult<&str, Box<A>>
where
    Box<A>: TryFrom<Box<[f32]>>,
{
    map_res(float_list, |v| {
        v.into_boxed_slice().try_into() // TODO: can't get it to work with copied slice
    })(s)
}

fn float_list(s: &str) -> IResult<&str, Vec<f32>> {
    separated_list1(ws_or_comment, float)(s)
}

#[inline]
pub fn ws_or_comment(s: &str) -> IResult<&str, ()> {
    let trimmed = unsafe { std::str::from_utf8_unchecked(eat_opt_ws(s.as_bytes())) };
    if std::ptr::eq(s, trimmed) || s.is_empty() && trimmed.is_empty() {
        Err(Error((s, ErrorKind::MultiSpace)))
    } else {
        Ok((trimmed, ()))
    }
}

#[inline]
pub fn eat_opt_ws(s: &[u8]) -> &[u8] {
    if s.len() >= 2 && s[0].is_ascii_whitespace() && !s[1].is_ascii_whitespace() && !s[1] == b'#' {
        return &s[1..]
    }
    let mut s = trim_ws_start(s);
    while !s.is_empty() && s[0] == b'#' {
        s = trim_comment(s);
        s = trim_ws_start(s);
    }
    s
}

#[inline]
fn trim_ws_start(s: &[u8]) -> &[u8] {
    let i = s.iter().position(|&b| !b.is_ascii_whitespace());
    match i {
        Some(i) => &s[i..],
        None => b""
    }
}

#[inline]
fn trim_comment(s: &[u8]) -> &[u8] {
    let i = s.iter().position(|&b| b == b'\n');
    match i {
        Some(i) => &s[i..],
        None => b""
    }
}

pub fn dbg_dmp<'a, I: Display + Clone, F, O, E: Debug>(
    f: F,
    context: I,
) -> impl Fn(I) -> IResult<I, O, E>
where
    F: Fn(I) -> IResult<I, O, E>,
{
    move |i: I| {
        let input = i.clone();
        match f(i) {
            Err(e) => {
                println!("{}: Error({:?}) at:\n{}", context, e, input);
                Err(e)
            }
            a => a,
        }
    }
}

#[cfg(test)]
pub(crate) mod test_helpers {
    use std::fmt::Debug;
    use nom::error::ErrorKind;
    use nom::IResult;

    pub fn ok_consuming<T: Debug + PartialEq>(val: T) -> IResult<&'static str, T> {
        Ok(("", val))
    }

    pub fn assert_errs_immediate<T: Debug + PartialEq>(
        f: impl Fn(&str) -> IResult<&str, T>,
        err: ErrorKind,
        s: &str,
    ) {
        assert_eq!(f(s), Err(nom::Err::Error((s, err))));
    }
}

#[cfg(test)]
mod tests {
    use nom::error::ErrorKind;

    use crate::{fixed_float_list, float_list, ws_or_comment, opt_ws};

    #[test]
    fn test_ws() {
        assert_eq!(ws_or_comment("#comment\n\t\n#another\n"), Ok(("", ())));

        assert_eq!(opt_ws(" #hi\n foo"), Ok(("foo", ())));
        assert_eq!(opt_ws(" #hi"), Ok(("", ())));
        assert_eq!(opt_ws("#hi"), Ok(("", ())));
        assert_eq!(opt_ws(""), Ok(("", ())));
    }

    #[test]
    fn test_float_list() {
        assert_eq!(
            float_list("1 3.4 5.123e4"),
            Ok(("", vec![1.0, 3.4, 5.123e4]))
        )
    }

    #[test]
    fn test_fixed_float_list() {
        assert_eq!(
            fixed_float_list::<[f32; 3]>("1.0 2 3.2"),
            Ok(("", Box::new([1.0, 2.0, 3.2])))
        );
        assert_eq!(
            fixed_float_list::<[f32; 2]>("1.0 2 3.2"),
            Err(nom::Err::Error(("1.0 2 3.2", ErrorKind::MapRes)))
        )
    }
}
