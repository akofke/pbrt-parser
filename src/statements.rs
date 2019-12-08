use crate::transform::{TransformStmt, transform_stmt};
use crate::params::{Param, parameter_list};
use nom::IResult;
use nom::sequence::{tuple, delimited, preceded, separated_pair};
use nom::combinator::{map, value, opt, map_res};
use crate::{ws_term, quoted_string, ws_or_comment, opt_ws_term, dbg_dmp};
use nom::bytes::complete::tag;
use nom::branch::alt;
use nom::multi::{separated_list, many0};
use crate::statements::WorldStmt::{ObjectInstance, NamedMaterial, ReverseOrientation, Transform};
use nom::error::context;

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
            opt_ws_term(tag(tag_str)),
            opt_ws_term(quoted_string),
            opt(parameter_list)
        )),
        move |(_, name, params)| f(name, params.unwrap_or_else(|| Vec::with_capacity(0)))
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
        tagged_named_params("PixelFilter", HeaderStmt::Filter),
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
        opt_ws_term(tag("AttributeBegin")),
        many0(opt_ws_term(world_stmt)),
//        separated_list(ws_or_comment, world_stmt),
        tag("AttributeEnd")
        ),
    WorldStmt::AttributeBlock
    )(s)
}

fn transform_block(s: &str) -> IResult<&str, WorldStmt> {
    map(
        delimited(
            opt_ws_term(tag("TransformBegin")),
            many0(opt_ws_term(world_stmt)),
            tag("TransformEnd")
        ),
        WorldStmt::TransformBlock
    )(s)
}

fn instance_block(s: &str) -> IResult<&str, WorldStmt> {
    let (s, name) = preceded(opt_ws_term(tag("ObjectBegin")), opt_ws_term(quoted_string))(s)?;
    let (s, statements) = many0(opt_ws_term(world_stmt))(s)?;
    tag("ObjectEnd")(s).map(|(s, _)| (s, WorldStmt::InstanceBlock(name, statements)))
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
    use crate::WorldStmt::*;

    #[test]
    fn test_header_stmt() {
        let input = r#"Camera "perspective" "float fov" [90]"#;
        let expected = HeaderStmt::Camera(
            "perspective".to_string(),
            vec![param!(fov, Float(90.0))]
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
            params: vec![param!(filename, String("image.tga".to_string()))]
        };
        assert_eq!(texture_stmt(input), ok_consuming(expected));
    }

    #[test]
    fn test_transform_stmt() {
        let input = r#"Transform [ -0.999887 0.00390257 0.0145262 -0 -0 0.965755 -0.259457 -0 0.0150413 0.259428 0.965645 -0 0.146624 -9.36998 28.765 1]"#;
        let expected = WorldStmt::Transform(TransformStmt::Transform(Box::new(
            [
                -0.999887, 0.00390257, 0.0145262, -0.0,
                -0.0, 0.965755, -0.259457, -0.0,
                0.0150413, 0.259428, 0.965645, -0.0,
                0.146624, -9.36998, 28.765, 1.0
            ])));

        assert_eq!(world_stmt(input), ok_consuming(expected));
    }

    #[test]
    fn test_no_params() {
        let input = r#"Shape "sphere""#;
        let expected = WorldStmt::Shape("sphere".to_string(), vec![]);

        assert_eq!(world_stmt(input), ok_consuming(expected));
    }

    #[test]
    fn test_no_spaces() {
        let input = r#"AttributeBeginShape"sphere"AttributeEnd"#;
        let expected = AttributeBlock(vec![Shape("sphere".to_string(), vec![])]);
        assert_eq!(world_stmt(input), Ok(("", expected)));
    }

    #[test]
    fn test_attribute_block() {
        let input = r#"AttributeBegin
        Shape "sphere"
        AttributeEnd"#;

        let expected = AttributeBlock(vec![Shape("sphere".to_string(), vec![])]);
        assert_eq!(world_stmt(input), Ok(("", expected)));
    }

    #[test]
    fn test_transform_block() {
        let input = r#"TransformBegin
        Shape "sphere"
        TransformEnd"#;

        let expected = TransformBlock(vec![Shape("sphere".to_string(), vec![])]);
        assert_eq!(world_stmt(input), Ok(("", expected)));
    }

    #[test]
    fn test_instance_block() {
        let input = r#"ObjectBegin "foo"
        Shape "sphere"
        ObjectEnd"#;

        let expected = InstanceBlock("foo".to_string(), vec![Shape("sphere".to_string(), vec![])]);
        assert_eq!(world_stmt(input), Ok(("", expected)));
    }

    #[test]
    fn test_nested_block() {
        let input = r#"AttributeBegin
            ObjectBegin "Buddha_Mesh25251"

            AttributeBegin
                NamedMaterial "Buda"
                Shape "plymesh" "string filename" "geometry/buddha.ply"
            AttributeEnd
            ObjectEnd
        AttributeEnd "#;

        let expected = AttributeBlock(vec![
            InstanceBlock("Buddha_Mesh25251".to_string(), vec![
                AttributeBlock(vec![
                    NamedMaterial("Buda".to_string()),
                    Shape("plymesh".to_string(), vec![param!(filename, String("geometry/buddha.ply".to_string()))])
                ])
            ])
        ]);

        assert_eq!(world_stmt(input), Ok((" ", expected)));

    }
}

