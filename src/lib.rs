use nom::IResult;
use nom::bytes::complete::{tag, take_till};
use nom::character::complete::{line_ending, multispace0, multispace1, none_of};
use nom::branch::alt;
use nom::combinator::{map, map_res, opt};
use nom::multi::{separated_nonempty_list, many1, many0, separated_list};
use nom::number::complete::float;
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;
use nom::sequence::{separated_pair, delimited, terminated, preceded};
use std::array::TryFromSliceError;
use crate::statements::world_stmt;

#[macro_use]
pub mod macros;
pub mod parser;
mod transform;
mod params;
pub mod statements;

pub use params::{Param, ParamVal, SpectrumVal};
pub use transform::TransformStmt;
pub use statements::{HeaderStmt, WorldStmt};
pub use parser::{PbrtParser, PbrtScene, ParserError};

pub type Float2 = [f32; 2];
pub type Float3 = [f32; 3];


fn quoted_string(s: &str) -> IResult<&str, String> {
    map(
        delimited(tag("\""), many0(none_of("\"")), tag("\"")),
        |chars: Vec<char>| chars.into_iter().collect()
    )(s)
}

fn ws_term<'a, T>(f: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, T> {
    terminated(f, ws_or_comment)
}

fn opt_ws_term<'a, T>(f: impl Fn(&'a str) -> IResult<&'a str, T>) -> impl Fn(&'a str) -> IResult<&'a str, T> {
    terminated(f, opt(ws_or_comment))
}

// TODO: fix duplication
fn try_from_vec_2(v: Vec<f32>) -> Result<Float2, TryFromSliceError>
{
    v.as_slice().try_into()
}

fn try_from_vec_3(v: Vec<f32>) -> Result<Float3, TryFromSliceError>
{
    v.as_slice().try_into()
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
    separated_nonempty_list(ws_or_comment, float)(s)
}


fn ws_or_comment(s: &str) -> IResult<&str, ()> {
    many1(single_ws_or_comment)(s).map(|(s, _)| (s, ()))
}

fn single_ws_or_comment(s: &str) -> IResult<&str, ()> {
    alt((multispace1, comment))(s).map(|(s, _)| (s, ()))
}

/// A comment starts with a '#' and continues until a line ending
/// Produces the contents of the comment, not including the '#' or the newline.
fn comment(s: &str) -> IResult<&str, &str> {
    let (s, _) = tag("#")(s)?;
    take_till(|c| {
        c == '\n' || c == '\r'
    })(s)
}

pub fn make_vals<T: Clone>(f: impl Fn(T) -> ParamVal, vals: &[T]) -> Vec<ParamVal> {
    vals.iter().cloned().map(f).collect()
}

#[cfg(test)]
pub(crate) mod test_helpers {
    use nom::IResult;
    use nom::error::ErrorKind;
    use std::fmt::Debug;
    use crate::params::ParamVal;

    pub fn ok_consuming<T: Debug + PartialEq>(val: T) -> IResult<&'static str, T> {
        Ok(("", val))
    }

    pub fn assert_errs_immediate<T: Debug + PartialEq>(f: impl Fn(&str) -> IResult<&str, T>, err: ErrorKind, s: &str) {
        assert_eq!(f(s), Err(nom::Err::Error((s, err))));
    }

    pub fn make_vals<T: Clone>(f: impl Fn(T) -> ParamVal, vals: &[T]) -> Vec<ParamVal> {
        vals.iter().cloned().map(f).collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::{comment, float_list, fixed_float_list, ws_or_comment};
    use nom::error::ErrorKind;

    #[test]
    fn test_comment() {
        assert_eq!(comment("# comment\n"), Ok(("\n", " comment")));
        assert_eq!(comment("not # comment"), Err(nom::Err::Error(("not # comment", ErrorKind::Tag))));
        assert_eq!(comment("#no newline"), Ok(("", "no newline")));
    }

    #[test]
    fn test_multiple_ws() {
        assert_eq!(ws_or_comment("#comment\n\t\n#another\n"), Ok(("", ())));
    }

    #[test]
    fn test_float_list() {
        assert_eq!(float_list("1 3.4 5.123e4"), Ok(("", vec![1.0, 3.4, 5.123e4])))
    }

    #[test]
    fn test_fixed_float_list() {
        assert_eq!(fixed_float_list::<[f32; 3]>("1.0 2 3.2"), Ok(("", Box::new([1.0, 2.0, 3.2]))));
        assert_eq!(fixed_float_list::<[f32; 2]>("1.0 2 3.2"), Err(nom::Err::Error(("1.0 2 3.2", ErrorKind::MapRes))))
    }

}
