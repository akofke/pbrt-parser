use crate::{Float2, Float3, ws_or_comment};
use nom::IResult;
use nom::character::complete::{digit1, anychar, none_of, space1, alphanumeric1};
use nom::combinator::{map_res, opt, map, value};
use nom::number::complete::float;
use nom::sequence::{tuple, terminated, delimited, preceded, separated_pair};
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::multi::{many0, separated_nonempty_list};

#[derive(PartialEq, Debug)]
pub struct Param {
    pub name: String,
    pub value: ParamVal
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum ParamType {
    Int, Float, Point2, Point3, Vector2, Vector3, Normal3, Spectrum(SpectrumType), Bool, String
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum SpectrumType {
    Rgb, Xyz, Sampled, Blackbody
}

#[derive(PartialEq, Debug)]
pub enum ParamVal {
    Int(i32),
    Float(f32),
    Point2(Float2),
    Point3(Float3),
    Vector2(Float2),
    Vector3(Float3),
    Normal3(Float3),
    Spectrum(SpectrumVal),
    Bool(bool),
    String(String),
}

#[derive(PartialEq, Debug)]
pub enum SpectrumVal {
    Rgb(Float3),
    Xyz(Float3),
    Sampled(Vec<f32>),
    Blackbody((f32, f32))
}

/// Parses a parameter declaration `"type name"` and returns a tuple of the parameter
/// type and name.
fn param_declaration(s: &str) -> IResult<&str, (ParamType, String)> {
    delimited(
        tag("\""),
        // assumes names can only be alphanumeric, change this if needed
        separated_pair(
            param_type,
            space1,
            map(alphanumeric1, |s: &str| s.to_string())),
        tag("\"")
    )(s)
}

fn param_type(s: &str) -> IResult<&str, ParamType> {
    alt((
        value(ParamType::Int, tag("integer")),
        value(ParamType::Float, tag("float")),
        value(ParamType::Point2, tag("point2")),
        value(ParamType::Vector2, tag("vector2")),
        value(ParamType::Vector3, alt((tag("vector3"), tag("vector")))),
        value(ParamType::Point3, alt((tag("point3"), tag("point")))),
        value(ParamType::Normal3, alt((tag("normal3"), tag("normal")))),
        map(spectrum_type, ParamType::Spectrum),
        value(ParamType::Bool, tag("bool")),
        value(ParamType::String, tag("string")),
    ))(s)
}

fn spectrum_type(s: &str) -> IResult<&str, SpectrumType> {
    alt((
        value(SpectrumType::Rgb, alt((tag("rgb"), tag("color")))),
        value(SpectrumType::Xyz, tag("xyz")),
        value(SpectrumType::Sampled, tag("spectrum")),
        value(SpectrumType::Blackbody, tag("blackbody")),
    ))(s)
}

fn parameter_value<P>(param_parser: P, s: &str) -> IResult<&str, Vec<ParamVal>>
    where P: Fn(&str) -> IResult<&str, ParamVal> + Copy // is this right?
{
    // Can be one or more values enclosed in brackets, or single value without brackets
    alt((
        map(param_parser, |val| vec![val]),
        delimited(
            terminated(tag("["), opt(ws_or_comment)),
            separated_nonempty_list(ws_or_comment, param_parser),
            preceded(opt(ws_or_comment), tag("]"))
        )
    ))(s)
}

fn int_val(s: &str) -> IResult<&str, ParamVal> {
    map_res(digit1, |s: &str| s.parse::<i32>())(s).map(|(s, i)| (s, ParamVal::Int(i)))
}

fn float_val(s: &str) -> IResult<&str, ParamVal> {
    float(s).map(|(s, x)| (s, ParamVal::Float(x)))
}

fn float_then_ws(s: &str) -> IResult<&str, f32> {
    terminated(float, ws_or_comment)(s)
}

fn float2(s: &str) -> IResult<&str, Float2> {
    tuple((float_then_ws, float))(s).map(|(s, t)| (s, [t.0, t.1]))
}

fn float3(s: &str) -> IResult<&str, Float3> {
    tuple((float_then_ws, float_then_ws, float))(s).map(|(s, t)| (s, [t.0, t.1, t.2]))
}

fn bool_val(s: &str) -> IResult<&str, ParamVal> {
    map_res(
        delimited(tag("\""), alt((tag("true"), tag("false"))), tag("\"")),
        |s: &str| s.parse::<bool>()
    )(s).map(|(s, b)| (s, ParamVal::Bool(b)))
}

fn string_val(s: &str) -> IResult<&str, ParamVal> {
    map(
        delimited(tag("\""), many0(none_of("\"")), tag("\"")),
        |chars: Vec<char>| ParamVal::String(chars.into_iter().collect())
    )(s)
}

fn vector3_val(s: &str) -> IResult<&str, ParamVal> {
    float3(s).map(|(s, v)| (s, ParamVal::Vector3(v)))
}

fn point3_val(s: &str) -> IResult<&str, ParamVal> {
    float3(s).map(|(s, v)| (s, ParamVal::Point3(v)))
}

fn normal3_val(s: &str) -> IResult<&str, ParamVal> {
    float3(s).map(|(s, v)| (s, ParamVal::Normal3(v)))
}

fn point2_val(s: &str) -> IResult<&str, ParamVal> {
    float2(s).map(|(s, v)| (s, ParamVal::Point2(v)))
}

fn vector2_val(s: &str) -> IResult<&str, ParamVal> {
    float2(s).map(|(s, v)| (s, ParamVal::Vector2(v)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::{assert_errs_immediate, ok_consuming};
    use nom::error::{ErrorKind, make_error};
    use nom::Err::Error;

    fn make_vals<T: Clone>(f: impl Fn(T) -> ParamVal, vals: &[T]) -> Vec<ParamVal> {
        vals.iter().cloned().map(f).collect()
    }

    #[test]
    fn test_param_value() {
        assert_eq!(parameter_value(int_val, "[1 2 3]"), Ok(("", make_vals(ParamVal::Int, &[1, 2, 3]))));

        assert_eq!(parameter_value(int_val, "1"), Ok(("", make_vals(ParamVal::Int, &[1]))));

        assert_eq!(parameter_value(int_val, "[ 1 2\n3#comment\n4 ]"), Ok(("", make_vals(ParamVal::Int, &[1, 2, 3, 4]))));

        assert_eq!(
            parameter_value(vector3_val, "[ 1 1 1.0 2.0\n2#comment\n2 ]"),
            Ok(("", make_vals(ParamVal::Vector3, &[[1.0, 1.0, 1.0], [2.0, 2.0, 2.0]]))));
    }

    #[test]
    fn test_param_decl() {
        assert_eq!(
            param_declaration(r#""float name""#),
            ok_consuming((ParamType::Float, "name".to_string()))
        );

        assert_eq!(
            param_declaration(r#""nope name""#),
            Err((Error((r#"nope name""#, ErrorKind::Tag))))
        );
    }
}
