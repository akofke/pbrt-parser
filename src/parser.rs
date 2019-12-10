use crate::statements::{HeaderStmt, WorldStmt, world_stmt, header_stmt};
use nom::IResult;
use nom::sequence::{delimited, preceded, terminated};
use crate::{ws_term, ws_or_comment, single_ws_or_comment, opt_ws_term, opt_ws};
use nom::multi::{separated_list, many0, fold_many0};
use nom::bytes::complete::tag;
use std::path::{PathBuf, Path};
use nom::combinator::{opt, all_consuming, iterator};
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

struct ParserState {
    world: Vec<WorldStmt>,
}

pub struct PbrtParser<> {
    pub base_path: PathBuf,
    pub resolve_includes: bool,
    state: ParserState
}

impl PbrtParser {

    pub fn parse_with_includes(path: impl AsRef<Path>) -> Result<PbrtScene, ParserError> {
        let base_path = path.as_ref().parent()
            .ok_or(std::io::Error::new(std::io::ErrorKind::Other, "Invalid path"))?
            .to_path_buf();
        let parser = PbrtParser {
            base_path,
            resolve_includes: true,
            state: ParserState {
                world: Vec::new(),
            }
        };
        let contents = std::fs::read_to_string(&path)?;
        // TODO
        eprintln!("Main file {:?}, contents size {} MiB", path.as_ref().as_os_str(), contents.len() as f64 / 1024.0 / 1024.0);
        let scene = parser.parse_string(&contents)?;
        Ok(scene)
    }

    pub fn parse_string_no_includes(contents: &str) -> Result<PbrtScene, ParserError> {
        let parser = PbrtParser {
            base_path: PathBuf::new(),
            resolve_includes: false,
            state: ParserState {
                world: Vec::new(),
            }
        };
        let scene = parser.parse_string(contents)?;
        Ok(scene)
    }

    pub fn parse_string(mut self, contents: &str) -> Result<PbrtScene, ParserError> {
        let (s, header) = header_block(contents)?;
        let (s, _) = opt_ws_term(tag("WorldBegin"))(s)?;
        let remain = self.parse_world_stmts(s)?;
        let (_, _) = all_consuming(opt_ws_term(tag("WorldEnd")))(remain)?;

        let scene = PbrtScene { header, world: self.state.world };
        Ok(scene)
    }

    fn parse_world_stmts<'a>(&mut self, contents: &'a str) -> Result<&'a str, ParserError> {
        let state = &mut self.state;
        let mut it = iterator(contents, opt_ws_term(world_stmt));
        let res: Result<(), ParserError> = (&mut it).try_for_each(|stmt| {
                match stmt {
                    WorldStmt::Include(path) if self.resolve_includes => {
                        let contents = self.read_include_path(path)?;
                        let (s, _) = opt_ws(&contents)?;
                        let remain = self.parse_world_stmts(&s)?;
                        all_consuming(opt_ws)(remain)?;
                        Ok(())
                    },
                    stmt @ _ => {
                        self.state.world.push(stmt);
                        Ok(())
                    }
                }
            });
        let _ = res?;
        let (remain, _) = it.finish()?;
        Ok(remain)
    }

    fn read_include_path(&self, path: impl AsRef<Path>) -> Result<String, std::io::Error> {
        let mut incl_file_path = self.base_path.clone();
        incl_file_path.push(path);

        let start = std::time::Instant::now();
        let contents = std::fs::read_to_string(&incl_file_path)?;
        let time = start.elapsed();
        // TODO
        eprintln!("Included {:?} in {} ms, contents size {} MiB", incl_file_path.as_os_str(), time.as_millis(), contents.len() as f64 / 1024.0 / 1024.0);
        Ok(contents)
    }
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
            AttributeBegin,
            Material("matte".into(), vec![param!(Kd, SpectrumRgb([0.0, 0.0, 0.0]))]),
            Shape("sphere".into(), vec![param!(radius, Float(3.0))]),
            Include("foo".into()),
            AttributeEnd,
            Shape("sphere".into(), vec![]),
            Include("foo".into())
        ];

        let scene = PbrtScene {header, world};

        assert_eq!(PbrtParser::parse_string_no_includes(file).unwrap(), scene);
    }
}
