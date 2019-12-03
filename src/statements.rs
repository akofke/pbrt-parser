use crate::transform::TransformStmt;
use crate::params::{Param, parameter_list};
use nom::IResult;
use nom::sequence::tuple;
use nom::combinator::map;
use crate::{ws_term, quoted_string};
use nom::bytes::complete::tag;
use nom::branch::alt;

#[derive(PartialEq, Debug)]
pub enum HeaderStmt {
    Transform(TransformStmt),
    Camera(String, Vec<Param>),
    Sampler(String, Vec<Param>),
    Film(String, Vec<Param>),
    Filter(String, Vec<Param>),
    Integrator(String, Vec<Param>),
    Accelerator(String, Vec<Param>),
    // Mediums
    // Tf times
}


#[derive(PartialEq, Debug)]
pub enum WorldStmt {
    AttributeBlock(Vec<WorldStmt>),
    TransformBlock(Vec<WorldStmt>),
    InstanceBlock(String, Vec<WorldStmt>), // Should this only be shape statements?
    ReverseOrientation,

    Shape(String, Vec<Param>),
    ObjectInstance(String),
    LightSource(String, Vec<Param>),
    AreaLightSource(String),
    Material(String, Vec<Param>),
    MakeNamedMaterial(String, Vec<Param>),
    NamedMaterial(String),
    Texture {name: String, ty: String, class: String, params: Vec<Param>},
    MakeNamedMedium(String, Vec<Param>),
    MediumInterface(String, String),
}

fn name_params_stmt<'a, T, F>(tag_str: &'static str, f: F) -> impl Fn(&'a str) -> IResult<&'a str, T>
    where F: Fn(String, Vec<Param>) -> T
{
    map(
        tuple((
            ws_term(tag(tag_str)),
            ws_term(quoted_string),
            parameter_list
        )),
        move |(_, name, params)| f(name, params)
    )
}

pub(crate) fn header_stmt(s: &str) -> IResult<&str, HeaderStmt> {
    alt((
        name_params_stmt("Camera", HeaderStmt::Camera),
        name_params_stmt("Sampler", HeaderStmt::Sampler),
    ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::{make_vals, ok_consuming};
    use crate::params::ParamVal;

    #[test]
    fn test_header_stmt() {
        let input = r#"Camera "perspective" "float fov" [90]"#;
        let expected = HeaderStmt::Camera(
            "perspective".to_string(),
            vec![Param::new("fov".into(), make_vals(ParamVal::Float, &[90.0]))]
        );
        assert_eq!(header_stmt(input), ok_consuming(expected))
    }
}

