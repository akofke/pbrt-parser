use nom::IResult;
use nom::bytes::complete::{tag, take_till};
use nom::character::complete::{line_ending, multispace0, multispace1};
use nom::branch::alt;
use nom::combinator::{map, map_res};
use nom::multi::separated_nonempty_list;
use nom::number::complete::float;
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;
use nom::sequence::separated_pair;

mod transform;

pub enum HeaderStmt {

}

pub enum WorldStmt {

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

#[cfg(test)]
mod tests {
    use crate::{comment, float_list, fixed_float_list};
    use nom::error::ErrorKind;

    #[test]
    fn test_comment() {
        assert_eq!(comment("# comment\n"), Ok(("\n", " comment")));
        assert_eq!(comment("not # comment"), Err(nom::Err::Error(("not # comment", ErrorKind::Tag))));
        assert_eq!(comment("#no newline"), Ok(("", "no newline")));
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
