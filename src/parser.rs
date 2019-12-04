use crate::statements::{HeaderStmt, WorldStmt, world_stmt, header_stmt};
use nom::IResult;
use nom::sequence::{delimited, preceded, terminated};
use crate::{ws_term, ws_or_comment};
use nom::multi::{separated_list, many0, fold_many0};
use nom::bytes::complete::tag;
use std::path::PathBuf;
use nom::combinator::{opt, all_consuming};

#[derive(PartialEq, Debug)]
pub struct PbrtScene {
    pub header: Vec<HeaderStmt>,
    pub world: Vec<WorldStmt>,
}

pub trait IncludeHandler {

    fn parse_include(&self, path: String) -> Vec<WorldStmt>;
}

pub struct NoOpIncludeHandler;

impl IncludeHandler for NoOpIncludeHandler {

    fn parse_include(&self, path: String) -> Vec<WorldStmt> {
        vec![WorldStmt::Include(path)]
    }
}

pub struct PbrtParser<I: IncludeHandler> {
    include_handler: I,
}

impl PbrtParser<NoOpIncludeHandler> {
    pub fn new() -> Self {
        Self { include_handler: NoOpIncludeHandler }
    }
}

impl<I: IncludeHandler> PbrtParser<I> {
    pub fn parse_string<'s>(&self, contents: &'s str) -> IResult<&'s str, PbrtScene> {
        let (s, header) = header_block(contents)?;
        let (s, world) = all_consuming(world_block(&self.include_handler))(s)?;

        let scene = PbrtScene { header, world };
        Ok((s, scene))
    }
}

fn world_block(include_handler: &impl IncludeHandler) -> impl Fn(&str) -> IResult<&str, Vec<WorldStmt>> + '_ {
    move |s| {
        delimited(
            ws_term(tag("WorldBegin")),
            fold_many0(
                terminated(world_stmt, opt(ws_or_comment)),
                Vec::new(),
                |mut statements, stmt| {
                    match stmt {
                        WorldStmt::Include(path) => {
                            statements.append(&mut include_handler.parse_include(path));
                        },
                        stmt @ _ => {
                            statements.push(stmt)
                        }
                    }
                    statements
                }
            ),
            terminated(tag("WorldEnd"), opt(ws_or_comment))
        )(s)
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
        AttributeEnd
        Shape "sphere"
        WorldEnd
        "#;

        let header = vec![
            HeaderStmt::Transform(tf!(Rotate(-5, 0, 0, 1))),
            HeaderStmt::Camera("perspective".into(), vec![param!(fov, Float(39.0))])
        ];

        let world = vec![
            WorldStmt::AttributeBlock(vec![])
        ];

        let scene = PbrtScene {header, world};

        let parser = PbrtParser::new();

        assert_eq!(parser.parse_string(file), ok_consuming(scene));
    }
}
