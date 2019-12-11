use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::{map, opt};
use nom::IResult;
use nom::sequence::{separated_pair, tuple};
use once_cell::sync::Lazy;

use crate::{opt_ws, opt_ws_term, quoted_string, ws_term};
use crate::params::{Param, parameter_list};
use crate::transform::{transform_stmt, TransformStmt};

/// Create a parser for the common case of statements in the form of
///
/// `TagName "type" param_list...`.
///
/// E.g. `Shape "sphere" "float radius" [ 2.0 ]`
fn tagged_named_params<'a, T, F>(
    tag_str: &'static str,
    f: F,
) -> impl Fn(&'a str) -> IResult<&'a str, T>
where
    F: Fn(String, Vec<Param>) -> T,
{
    map(
        tuple((
            opt_ws_term(tag(tag_str)),
            opt_ws_term(quoted_string),
            opt(parameter_list),
        )),
        move |(_, name, params)| f(name, params.unwrap_or_else(|| Vec::with_capacity(0))),
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
    AttributeBegin,
    AttributeEnd,
    TransformBegin,
    TransformEnd,
    ObjectBegin(String),
    ObjectEnd,

    ReverseOrientation,
    Transform(TransformStmt),

    Shape(String, Vec<Param>),
    ObjectInstance(String),
    LightSource(String, Vec<Param>),
    AreaLightSource(String, Vec<Param>),
    Material(String, Vec<Param>),
    MakeNamedMaterial(String, Vec<Param>),
    NamedMaterial(String),
    Texture(Box<TextureStmt>),
    MakeNamedMedium(String, Vec<Param>),
    MediumInterface(String, String),

    Include(String),
    ResolvedInclude(Vec<WorldStmt>),
}

impl WorldStmt {
    pub fn texture(name: String, ty: String, class: String, params: Vec<Param>) -> Self {
        WorldStmt::Texture(Box::new(TextureStmt {
            name,
            ty,
            class,
            params,
        }))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TextureStmt {
    pub name: String,
    pub ty: String,
    pub class: String,
    pub params: Vec<Param>,
}

macro_rules! type_and_params {
    ($fn_name:ident, $constructor:expr) => {
        fn $fn_name(s: &str) -> IResult<&str, WorldStmt> {
            let (s, (ty, params)) = separated_pair(quoted_string, opt_ws, opt(parameter_list))(s)?;
            let params = params.unwrap_or(Vec::new());
            let stmt = $constructor(ty, params);
            Ok((s, stmt))
        }
    };
}

macro_rules! quoted_string {
    ($fn_name:ident, $constructor:expr) => {
        fn $fn_name(s: &str) -> IResult<&str, WorldStmt> {
            let (s, name) = quoted_string(s)?;
            let stmt = $constructor(name);
            Ok((s, stmt))
        }
    };
}

// TODO: find some way to statically check that these are kept in sync with parser fns etc
const WORLD_STMT_KW: [&'static str; 18] = [
    "AttributeBegin",
    "AttributeEnd",
    "TransformBegin",
    "TransformEnd",
    "ObjectBegin",
    "ObjectEnd",
    "ReverseOrientation",
    "Shape",
    "ObjectInstance",
    "LightSource",
    "AreaLightSource",
    "Material",
    "MakeNamedMaterial",
    "NamedMaterial",
    "Texture",
    "MakeNamedMedium",
    "MediumInterface",
    "Include",
];

static WORLD_KW_MATCHER: Lazy<AhoCorasick> = Lazy::new(|| {
    AhoCorasickBuilder::new()
        .auto_configure(&WORLD_STMT_KW)
        .anchored(true)
        .dfa(true)
        .build(&WORLD_STMT_KW)
});

pub(crate) fn world_stmt(s: &str) -> IResult<&str, WorldStmt> {
    use crate::WorldStmt::*;

    let kw_match = WORLD_KW_MATCHER.find(s);
    let kw_match = match kw_match {
        Some(m) => m,
        None => return map(transform_stmt, Transform)(s),
    };

    let s = &s[kw_match.end()..];
    let (s, _) = opt_ws(s)?;

    match kw_match.pattern() {
        0 => Ok((s, AttributeBegin)),
        1 => Ok((s, AttributeEnd)),
        2 => Ok((s, TransformBegin)),
        3 => Ok((s, TransformEnd)),
        4 => parse_object_begin_params(s),
        5 => Ok((s, ObjectEnd)),
        6 => Ok((s, ReverseOrientation)),
        7 => parse_shape_params(s),
        8 => parse_object_instance_params(s),
        9 => parse_lightsource_params(s),
        10 => parse_arealightsource_params(s),
        11 => parse_material_params(s),
        12 => parse_mk_named_material_params(s),
        13 => parse_named_material_params(s),
        14 => texture_params(s),
        15 => parse_make_named_medium_params(s),
        16 => medium_interface_params(s),
        17 => parse_include_params(s),
        n @ _ => panic!("{}", n),
    }
}

type_and_params!(parse_shape_params, WorldStmt::Shape);
type_and_params!(parse_lightsource_params, WorldStmt::LightSource);
type_and_params!(parse_arealightsource_params, WorldStmt::AreaLightSource);
type_and_params!(parse_material_params, WorldStmt::Material);
type_and_params!(parse_mk_named_material_params, WorldStmt::MakeNamedMaterial);
type_and_params!(parse_make_named_medium_params, WorldStmt::MakeNamedMedium);
quoted_string!(parse_object_begin_params, WorldStmt::ObjectBegin);
quoted_string!(parse_object_instance_params, WorldStmt::ObjectInstance);
quoted_string!(parse_named_material_params, WorldStmt::NamedMaterial);
quoted_string!(parse_include_params, WorldStmt::Include);

// `Texture "name" "type" "class" params...`
// TODO opt ws
fn texture_params(s: &str) -> IResult<&str, WorldStmt> {
    let parser = tuple((
        opt_ws_term(quoted_string),
        opt_ws_term(quoted_string),
        opt_ws_term(quoted_string),
        parameter_list,
    ));
    map(parser, |(name, ty, class, params)| {
        WorldStmt::texture(name, ty, class, params)
    })(s)
}

pub fn medium_interface_params(s: &str) -> IResult<&str, WorldStmt> {
    map(
        tuple((ws_term(quoted_string), quoted_string)),
        |(med1, med2)| WorldStmt::MediumInterface(med1, med2),
    )(s)
}

#[cfg(test)]
mod tests {

    use crate::test_helpers::{ok_consuming};


    use super::*;

    #[test]
    fn test_header_stmt() {
        let input = r#"Camera "perspective" "float fov" [90]"#;
        let expected =
            HeaderStmt::Camera("perspective".to_string(), vec![param!(fov, Float(90.0))]);
        assert_eq!(header_stmt(input), ok_consuming(expected))
    }

    #[test]
    fn test_texture_stmt() {
        let input = r#"Texture "mydiffuse" "spectrum" "imagemap" "string filename" "image.tga""#;
        let expected = WorldStmt::Texture(Box::new(TextureStmt {
            name: "mydiffuse".into(),

            ty: "spectrum".into(),
            class: "imagemap".into(),
            params: vec![param!(filename, String("image.tga".to_string()))],
        }));
        assert_eq!(world_stmt(input), ok_consuming(expected));
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
}
