use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, digit1};
use nom::combinator::{cut, map, map_res, opt};
use nom::Err::Error;
use nom::error::ErrorKind;
use nom::IResult;
use nom::multi::separated_list;
use nom::number::complete::float;
use nom::sequence::{delimited, preceded, terminated, tuple};
use once_cell::sync::Lazy;

use crate::{Float2, Float3, opt_ws, opt_ws_term, quoted_string, ws_or_comment, STR_POOL};
use std::sync::Arc;

#[derive(PartialEq, Debug, Clone)]
pub struct Param<S: AsRef<str>=Arc<str>> {
    pub name: S,
    pub value: ParamVal<S>,
}

impl<S: AsRef<str>> Param<S> {
    pub fn new(name: S, value: ParamVal<S>) -> Self {
        Self { name, value }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ParamVal<S: AsRef<str>=Arc<str>> {
    Int(Vec<i32>),
    Float(Vec<f32>),
    Point2(Vec<Float2>),
    Point3(Vec<Float3>),
    Vector2(Vec<Float2>),
    Vector3(Vec<Float3>),
    Normal3(Vec<Float3>),
    Bool(Vec<bool>),
    String(Vec<S>),
    Texture(Vec<S>),

    SpectrumRgb(Vec<Float3>), // TODO are these always only one value?
    SpectrumXyz(Vec<Float3>),
    SpectrumSampled(Vec<Float2>), // TODO filenam
    SpectrumBlackbody(Vec<Float2>),
}

pub(crate) fn parameter_list(s: &str) -> IResult<&str, Vec<Param>> {
    separated_list(ws_or_comment, parameter)(s)
}

const PARAM_KW: [&'static str; 18] = [
    "integer",
    "float",
    "point2",
    "vector2",
    "point3",
    "point",
    "vector3",
    "vector",
    "normal3",
    "normal",
    "bool",
    "string",
    "texture",
    "rgb",
    "color",
    "xyz",
    "spectrum",
    "blackbody",
];

static PARAM_KW_MATCHER: Lazy<AhoCorasick> = Lazy::new(|| {
    AhoCorasickBuilder::new()
        .auto_configure(&PARAM_KW)
        .anchored(true)
        .dfa(true)
        .build(&PARAM_KW)
});

fn parameter(s: &str) -> IResult<&str, Param<Arc<str>>> {
    use ParamVal::*;

    let (s, _) = opt_ws_term(tag("\""))(s)?;

    let param_type_match = PARAM_KW_MATCHER.find(s).ok_or(Error((s, ErrorKind::Tag)))?;
    let s = &s[param_type_match.end()..];
    let (s, _) = opt_ws(s)?;
    let (s, name) = alphanumeric1(s)?;

    let (s, _) = opt_ws_term(tag("\""))(s)?;

    let (s, val) = match param_type_match.pattern() {
        0 => int_vals(s),
        1 => float_vals(s),
        2 => point2_vals(s),
        3 => vector2_vals(s),
        4 | 5 => point3_vals(s),
        6 | 7 => vector3_vals(s),
        8 | 9 => normal3_vals(s),
        10 => bool_vals(s),
        11 => string_vals(s),
        12 => texture_vals(s),
        13 | 14 => spectrum_rgb_vals(s),
        15 => spectrum_xyz_vals(s),
        16 => spectrum_sampled_vals(s),
        17 => spectrum_blackbody_vals(s),
        n @ _ => panic!("{}", n),
    }?;

    let name = STR_POOL.get_or_intern(name);
    let param = Param::new(name, val);
    Ok((s, param))
}

macro_rules! val_list {
    ($fn_name:ident, $val_parser:expr, $constructor:ident) => {
        fn $fn_name(s: &str) -> IResult<&str, ParamVal> {
            let (s, found_bracket) = opt(opt_ws_term(tag("[")))(s).map(|(s, o)| (s, o.is_some()))?;
            let (s, val) = if found_bracket {
                let (s, val) = cut(opt_ws_term(separated_list(ws_or_comment, $val_parser)))(s)?;
                let (s, _) = cut(tag("]"))(s)?;
                (s, val)
            } else {
                map($val_parser, |v| vec![v])(s)?
            };
            let pval = ParamVal::$constructor(val);
            Ok((s, pval))
        }
    };
}

val_list!(int_vals, integer, Int);
val_list!(float_vals, float, Float);
val_list!(point2_vals, float2, Point2);
val_list!(vector2_vals, float2, Vector2);
val_list!(point3_vals, float3, Point3);
val_list!(vector3_vals, float3, Vector3);
val_list!(normal3_vals, float3, Normal3);
val_list!(bool_vals, quoted_bool, Bool);
val_list!(string_vals, quoted_string, String);
val_list!(texture_vals, quoted_string, Texture);
val_list!(spectrum_rgb_vals, float3, SpectrumRgb);
val_list!(spectrum_xyz_vals, float3, SpectrumXyz);
val_list!(spectrum_sampled_vals, float2, SpectrumSampled);
val_list!(spectrum_blackbody_vals, float2, SpectrumBlackbody);

fn integer(s: &str) -> IResult<&str, i32> {
    map_res(digit1, |s: &str| s.parse::<i32>())(s)
}

fn quoted_bool(s: &str) -> IResult<&str, bool> {
    map_res(
        delimited(tag("\""), alt((tag("true"), tag("false"))), tag("\"")),
        |s: &str| s.parse::<bool>(),
    )(s)
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

#[cfg(test)]
mod tests {
    use nom::Err::{Failure};
    use nom::error::{ErrorKind};

    use crate::test_helpers::{ok_consuming};

    use super::*;

    #[test]
    fn test_val_list() {
        assert_eq!(
            vector3_vals("[1 2 3 1 2 3]"),
            Ok((
                "",
                ParamVal::Vector3(vec![[1.0, 2.0, 3.0], [1.0, 2.0, 3.0]])
            ))
        );
    }

    #[test]
    fn test_parameter() {
        use ParamVal::*;
        assert_eq!(
            parameter(r#""float foo" [1.2]"#),
            ok_consuming(Param::new("foo".into(), Float(vec![1.2])))
        );

        assert_eq!(
            parameter(r#""integer foo" 5"#),
            ok_consuming(Param::new("foo".into(), Int(vec![5])))
        );

        assert_eq!(
            parameter(r#""vector foo" [1 1 1 2 2 2 3 3 3]"#),
            ok_consuming(Param::new(
                "foo".into(),
                Vector3(vec![[1.0, 1.0, 1.0], [2.0, 2.0, 2.0], [3.0, 3.0, 3.0]])
            ))
        );

        assert_eq!(
            parameter(r#""vector foo" [1 1 1 2 2 2 3 3 ]"#),
            Err(Failure(("3 3 ]", ErrorKind::Tag)))
        );

        assert_eq!(
            parameter(r#""string ty" [ "matte" ]"#),
            ok_consuming(param!(ty, String("matte")))
        );

        assert_eq!(
            parameter(r#""texture Kd" "mydiffuse""#),
            ok_consuming(param!(Kd, Texture("mydiffuse")))
        );

        assert_eq!(
            parameter(r#""string foo" [1 ]"#),
            Err(Failure(("1 ]", ErrorKind::Tag)))
        );
    }

    #[test]
    fn test_spectrum_params() {
        assert_eq!(
            parameter(r#""rgb Kd" [ 1 2 3 ]"#),
            ok_consuming(param!(Kd, SpectrumRgb([1.0, 2.0, 3.0])))
        );

        assert_eq!(
            parameter(r#""xyz Kd" [ 1 2 3 ]"#),
            ok_consuming(param!(Kd, SpectrumXyz([1.0, 2.0, 3.0])))
        );

        assert_eq!(
            parameter(r#""spectrum Kd" [ 1 2 3 4]"#),
            ok_consuming(param!(Kd, SpectrumSampled([1.0, 2.0], [3.0, 4.0])))
        );
    }

    #[test]
    fn test_parameter_list() {
        let input = r#""string filename" ["out.exr"]
             "float cropwindow" [ .2 .5 .3 .8 ] "#;
        let expected = vec![
            param!(filename, String("out.exr")),
            param!(cropwindow, Float(0.2, 0.5, 0.3, 0.8)),
        ];
        assert_eq!(parameter_list(input), Ok((" ", expected)))
    }

    #[test]
    fn test_ambiguous_strings() {
        let input = r#""string foo" "foo" "float notastring" 1.0"#;
        let expected = vec![
            param!(foo, String("foo")),
            param!(notastring, Float(1.0)),
        ];

        assert_eq!(parameter_list(input), ok_consuming(expected));
    }
}
