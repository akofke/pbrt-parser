use nom::IResult;
use nom::combinator::{map, opt};
use nom::bytes::complete::tag;
use nom::sequence::{separated_pair, delimited, preceded};
use crate::{ws_or_comment, fixed_float_list, opt_ws_term, opt_ws};
use nom::branch::alt;
use std::convert::TryFrom;

#[derive(Debug, PartialEq, Clone)]
pub enum TransformStmt {
    Identity,
    Translate(Box<[f32; 3]>),
    Scale(Box<[f32; 3]>),
    Rotate(Box<[f32; 4]>),
    LookAt(Box<[f32; 9]>), // TODO: separate vectors?
    CoordinateSystem(String),
    CoordSysTransform(String),
    Transform(Box<[f32; 16]>),
    ConcatTransform(Box<[f32; 16]>)
}

fn tagged_float_list<'s, A, M>(
    tag_name: &'static str,
    stmt_mapper: M
) -> impl FnMut(&'s str) -> IResult<&'s str, TransformStmt>
    where
        Box<A>: TryFrom<Box<[f32]>>,
        M: Fn(Box<A>) -> TransformStmt

{
    map(
        separated_pair(
            tag(tag_name),
            opt_ws,
            alt((
                fixed_float_list,
                delimited(
                    opt_ws_term(tag("[")),
                    fixed_float_list,
                    preceded(opt_ws, tag("]"))
                )
            ))
        ),
        move |(_, arr)| stmt_mapper(arr)
    )
}

fn identity(s: &str) -> IResult<&str, TransformStmt> {
    map(tag("Identity"), |_| TransformStmt::Identity)(s)
}

//fn translate(s: &str) -> IResult<&str, TransformStmt> {
//    map(separated_pair(tag("Translate"), ws_or_comment, fixed_float_list::<[f32; 3]>),
//        |(_, arr)| TransformStmt::Translate(arr))(s)
//}



pub fn transform_stmt(s: &str) -> IResult<&str, TransformStmt> {
    alt((
        identity,
        tagged_float_list("Translate", TransformStmt::Translate),
        tagged_float_list("Scale", TransformStmt::Scale),
        tagged_float_list("Rotate", TransformStmt::Rotate),
        tagged_float_list("LookAt", TransformStmt::LookAt),
        tagged_float_list("Transform", TransformStmt::Transform),
        tagged_float_list("ConcatTransform", TransformStmt::Transform),
    ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transform_stmt() {
        assert_eq!(
            transform_stmt("Translate 1 2 3"),
            Ok(("", TransformStmt::Translate(Box::new([1.0, 2.0, 3.0]))))
        );

        assert_eq!(
            transform_stmt("Translate\n \t1 #comment\n2 3"),
            Ok(("", TransformStmt::Translate(Box::new([1.0, 2.0, 3.0]))))
        )
    }
}
