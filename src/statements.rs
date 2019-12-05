use crate::transform::{TransformStmt, transform_stmt};
use crate::params::{Param, parameter_list};
use nom::IResult;
use nom::sequence::{tuple, delimited, preceded, separated_pair};
use nom::combinator::{map, value, opt, map_res};
use crate::{ws_term, quoted_string, ws_or_comment};
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::multi::separated_list;
use crate::statements::WorldStmt::{ObjectInstance, NamedMaterial, ReverseOrientation, Transform};

/// Create a parser for the common case of statements in the form of
///
/// `TagName "type" param_list...`.
///
/// E.g. `Shape "sphere" "float radius" [ 2.0 ]`
fn tagged_named_params<'a, T, F>(tag_str: &'static str, f: F) -> impl Fn(&'a str) -> IResult<&'a str, T>
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

#[derive(PartialEq, Debug, Clone)]
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

pub fn header_stmt(s: &str) -> IResult<&str, HeaderStmt> {
    alt((
        tagged_named_params("Camera", HeaderStmt::Camera),
        tagged_named_params("Sampler", HeaderStmt::Sampler),
        tagged_named_params("Film", HeaderStmt::Film),
        tagged_named_params("Filter", HeaderStmt::Filter),
        tagged_named_params("Integrator", HeaderStmt::Integrator),
        tagged_named_params("Accelerator", HeaderStmt::Accelerator),
        map(transform_stmt, HeaderStmt::Transform),
    ))(s)
}

#[derive(PartialEq, Debug, Clone)]
pub enum WorldStmt {
    AttributeBlock(Vec<WorldStmt>),
    TransformBlock(Vec<WorldStmt>),
    InstanceBlock(String, Vec<WorldStmt>), // Should this only be shape statements?

    ReverseOrientation,
    Transform(TransformStmt),

    Shape(String, Vec<Param>),
    ObjectInstance(String),
    LightSource(String, Vec<Param>),
    AreaLightSource(String, Vec<Param>),
    Material(String, Vec<Param>),
    MakeNamedMaterial(String, Vec<Param>),
    NamedMaterial(String),
    Texture {name: String, ty: String, class: String, params: Vec<Param>},
    MakeNamedMedium(String, Vec<Param>),
    MediumInterface(String, String),

    Include(String),
    ResolvedInclude(Vec<WorldStmt>)
}

pub(crate) fn world_stmt(s: &str) -> IResult<&str, WorldStmt> {
    alt((
        attribute_block,
        transform_block,
        instance_block,

        value(WorldStmt::ReverseOrientation, tag("ReverseOrientation")),
        map(transform_stmt, WorldStmt::Transform),

        tagged_named_params("Shape", WorldStmt::Shape),
        object_instance_stmt,
        tagged_named_params("LightSource", WorldStmt::LightSource),
        tagged_named_params("AreaLightSource", WorldStmt::AreaLightSource),
        tagged_named_params("Material", WorldStmt::Material),
        tagged_named_params("MakeNamedMaterial", WorldStmt::MakeNamedMaterial),
        named_material_stmt,
        texture_stmt,
        tagged_named_params("MakeNamedMedium", WorldStmt::MakeNamedMedium),

        include_stmt
    ))(s)
}

fn attribute_block(s: &str) -> IResult<&str, WorldStmt> {
    map(
        delimited(
        ws_term(tag("AttributeBegin")),
        separated_list(ws_or_comment, world_stmt),
        preceded(ws_or_comment, tag("AttributeEnd"))
        ),
    WorldStmt::AttributeBlock
    )(s)
}

fn transform_block(s: &str) -> IResult<&str, WorldStmt> {
    map(
        delimited(
            ws_term(tag("TransformBegin")),
            separated_list(ws_or_comment, world_stmt),
            preceded(ws_or_comment, tag("TransformEnd"))
        ),
        WorldStmt::TransformBlock
    )(s)
}

fn instance_block(s: &str) -> IResult<&str, WorldStmt> {
    let (s, name) = preceded(ws_term(tag("ObjectBegin")), quoted_string)(s)?;
    let (s, statements) = separated_list(ws_or_comment, world_stmt)(s)?;
    preceded(ws_or_comment, tag("ObjectEnd"))(s).map(|(s, _)| (s, WorldStmt::InstanceBlock(name, statements)))
}

// `Texture "name" "type" "class" params...`
fn texture_stmt(s: &str) -> IResult<&str, WorldStmt> {
    let parser = tuple((
        ws_term(tag("Texture")),
        ws_term(quoted_string),
        ws_term(quoted_string),
        ws_term(quoted_string),
        parameter_list
    ));
    map(parser, |(_, name, ty, class, params)| WorldStmt::Texture {name, ty, class, params})(s)
}

pub fn object_instance_stmt(s: &str) -> IResult<&str, WorldStmt> {
    // TODO: lots of ways to do this, investigate perf
//    map(separated_pair(tag("ObjectInstance"), ws_or_comment, quoted_string), |(_, name)| ObjectInstance(name))(s)
    map(preceded(ws_term(tag("ObjectInstance")), quoted_string), ObjectInstance)(s)
}

pub fn named_material_stmt(s: &str) -> IResult<&str, WorldStmt> {
    map(separated_pair(tag("NamedMaterial"), ws_or_comment, quoted_string), |(_, name)| NamedMaterial(name))(s)
}

pub fn medium_interface_stmt(s: &str) -> IResult<&str, WorldStmt> {
    map(
        tuple((
            ws_term(tag("MediumInterface")),
            ws_term(quoted_string),
            quoted_string,
        )),
        |(_, med1, med2)| WorldStmt::MediumInterface(med1, med2)
    )(s)
}

fn include_stmt(s: &str) -> IResult<&str, WorldStmt> {
    map(preceded(ws_term(tag("Include")), quoted_string), WorldStmt::Include)(s)
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

    #[test]
    fn test_texture_stmt() {
        let input = r#"Texture "mydiffuse" "spectrum" "imagemap" "string filename" "image.tga""#;
        let expected = WorldStmt::Texture {
            name: "mydiffuse".into(),

            ty: "spectrum".into(),
            class: "imagemap".into(),
            params: vec![Param::new("filename".into(), make_vals(ParamVal::String, &["image.tga".into()]))]
        };
        assert_eq!(texture_stmt(input), ok_consuming(expected));
    }
}

