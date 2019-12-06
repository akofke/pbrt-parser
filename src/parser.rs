use crate::statements::{HeaderStmt, WorldStmt, world_stmt, header_stmt};
use nom::IResult;
use nom::sequence::{delimited, preceded, terminated};
use crate::{ws_term, ws_or_comment, single_ws_or_comment};
use nom::multi::{separated_list, many0, fold_many0};
use nom::bytes::complete::tag;
use std::path::{PathBuf, Path};
use nom::combinator::{opt, all_consuming};
use nom::error::{ErrorKind, ParseError};
use std::fmt::Display;
use nom::lib::std::fmt::{Formatter};
use nom::Err;

#[derive(Debug)]
pub enum ParserError {
    Io(std::io::Error),
    Parse(nom::Err<(String, ErrorKind)>)
}

impl From<std::io::Error> for ParserError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<nom::Err<(&str, ErrorKind)>> for ParserError {
    fn from(e: nom::Err<(&str, ErrorKind)>) -> Self {
        // TODO: how to prevent storing whole remaining string
//        Self::Parse(nom::Err::convert(e))
        let f = match e {
            Err::Incomplete(n) => Err::Incomplete(n),
            Err::Error((s, k)) => Err::Error((s.lines().next().unwrap().to_string(), k)),
            Err::Failure((s, k)) => Err::Failure((s.lines().next().unwrap().to_string(), k)),
        };
        Self::Parse(f)
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for ParserError {

}

#[derive(PartialEq, Debug)]
pub struct PbrtScene {
    pub header: Vec<HeaderStmt>,
    pub world: Vec<WorldStmt>,
}

/// Resolves include paths relative to a base path, i.e. the path of the
/// main pbrt file.
pub struct BasePathIncludeHandler {
    pub base_path: PathBuf,
}


impl BasePathIncludeHandler {
    fn parse_include(&self, path: impl AsRef<Path>) -> Result<WorldStmt, ParserError> {
        use std::io;

        let mut incl_file_path = self.base_path.clone();
        incl_file_path.push(path);

        let contents = std::fs::read_to_string(incl_file_path)?;
        let (s, _) = opt(ws_or_comment)(&contents)?;
        let (s, statements) = all_consuming(
            many0(terminated(world_stmt, opt(ws_or_comment)))
        )(s)?;
        Ok(WorldStmt::ResolvedInclude(statements))
    }
}

pub struct PbrtParser;

impl PbrtParser {

    pub fn parse_with_includes(path: impl AsRef<Path>) -> Result<PbrtScene, ParserError> {
        let base_path = path.as_ref().parent()
            .ok_or(std::io::Error::new(std::io::ErrorKind::Other, "Invalid path"))?
            .to_path_buf();
        let include_handler = BasePathIncludeHandler { base_path };
        let contents = std::fs::read_to_string(path)?;
        let (_, mut scene) = Self::parse_string(&contents)?;
        Self::resolve_includes(&include_handler, &mut scene.world)?;
        Ok(scene)
    }

    pub fn parse_string(contents: &str) -> IResult<&str, PbrtScene> {
        let (s, header) = header_block(contents)?;
        let (s, world) = all_consuming(world_block)(s)?;

        let scene = PbrtScene { header, world };
        Ok((s, scene))
    }

    fn resolve_includes(handler: &BasePathIncludeHandler, statements: &mut [WorldStmt]) -> Result<(), ParserError> {
        use WorldStmt::*;
        for stmt in statements {
            match stmt {
                WorldStmt::Include(path) => {
                    let mut resolved = handler.parse_include(path)?;
                    Self::resolve_includes(handler, std::slice::from_mut(&mut resolved))?;
                    std::mem::replace(stmt, resolved);
                },

                AttributeBlock(s)
                | TransformBlock(s)
                | InstanceBlock(_, s)
                | ResolvedInclude(s) => {
                    Self::resolve_includes(handler, s)?;
                },

                _ => {}
            }
        }
        Ok(())
    }
}

fn world_block(s: &str) -> IResult<&str, Vec<WorldStmt>> {
    delimited(
        ws_term(tag("WorldBegin")),
        many0(terminated(world_stmt, opt(ws_or_comment))),
        terminated(tag("WorldEnd"), opt(ws_or_comment))
    )(s)

}

fn header_block(s: &str) -> IResult<&str, Vec<HeaderStmt>> {
    let (s, _) = opt(ws_or_comment)(s)?;
    many0(terminated(header_stmt, opt(ws_or_comment)))(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transform::TransformStmt;
    use crate::macros::*;
    use crate::test_helpers::ok_consuming;

    #[test]
    fn test_parser() {
        let file = r#"
        # test scene
        Rotate -5 0 0 1
        Camera "perspective" "float fov" [39]
        Integrator "path"
        WorldBegin
        AttributeBegin
            Material "matte" "color Kd" [0 0 0]
            Shape "sphere" "float radius" [3]
            Include "foo"
        AttributeEnd
        Shape "sphere"
        Include "foo"
        WorldEnd
        "#;

        let header = vec![
            HeaderStmt::Transform(tf!(Rotate(-5, 0, 0, 1))),
            HeaderStmt::Camera("perspective".into(), vec![param!(fov, Float(39.0))]),
            HeaderStmt::Integrator("path".into(), vec![]),
        ];

        use WorldStmt::*;
        let world = vec![
            AttributeBlock(vec![
                Material("matte".into(), vec![param!(Kd, Spectrum(rgb!(0, 0, 0)))]),
                Shape("sphere".into(), vec![param!(radius, Float(3.0))]),
                Include("foo".into())
            ]),
            Shape("sphere".into(), vec![]),
            Include("foo".into())
        ];

        let scene = PbrtScene {header, world};

        assert_eq!(PbrtParser::parse_string(file), ok_consuming(scene));
    }
}
