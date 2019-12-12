use crate::statements::{HeaderStmt, WorldStmt, world_stmt, header_stmt, TextureStmt};
use nom::IResult;
use nom::sequence::{delimited, preceded, terminated};
use crate::{ws_term, ws_or_comment, single_ws_or_comment, opt_ws_term, opt_ws, Param};
use nom::multi::{separated_list, many0, fold_many0};
use nom::bytes::complete::tag;
use std::path::{PathBuf, Path};
use nom::combinator::{opt, all_consuming, iterator};
use nom::error::{ErrorKind, ParseError};
use std::fmt::Display;
use nom::lib::std::fmt::{Formatter};
use nom::Err;
use memmap::Mmap;
use crate::interner::StringInterner;
use std::rc::Rc;

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

#[derive(Debug)]
pub struct PbrtScene {
    pub header: Vec<HeaderStmt>,
    pub world: Vec<WorldStmt<Rc<str>>>,
//    strings: StringInterner,
}

impl PartialEq for PbrtScene {
    fn eq(&self, other: &Self) -> bool {
        (&self.header, &self.world) == (&other.header, &other.world)
    }
}

pub struct PbrtParser<> {
    pub file_path: PathBuf,
    pub resolve_includes: bool,
    world: Vec<WorldStmt<Rc<str>>>,
    interner: StringInterner,
}

impl PbrtParser {

    pub fn new(path: impl AsRef<Path>, resolve_includes: bool) -> Self {
        let file_path = path.as_ref().to_path_buf();
        Self {
            file_path,
            resolve_includes,
            world: Vec::new(),
            interner: Default::default()
        }
    }

    pub fn parse_with_includes(path: impl AsRef<Path>) -> Result<PbrtScene, ParserError> {
        let parser = Self::new(path, true);
        parser.parse()
    }

    pub fn parse(self) -> Result<PbrtScene, ParserError> {
        let contents = std::fs::read_to_string(&self.file_path)?;
        // TODO
        eprintln!("Main file {:?}, contents size {} MiB", &self.file_path.as_os_str(), contents.len() as f64 / 1024.0 / 1024.0);
        let scene = self.parse_string(&contents)?;
        Ok(scene)
    }

    pub fn parse_string_no_includes(contents: &str) -> Result<PbrtScene, ParserError> {
        let parser = Self::new("", false);
        let scene = parser.parse_string(contents)?;
        Ok(scene)
    }

    pub fn parse_string(mut self, contents: &str) -> Result<PbrtScene, ParserError> {
        let (s, header) = header_block(contents)?;
        let (s, _) = opt_ws_term(tag("WorldBegin"))(s)?;
        let remain = self.parse_world_stmts(s)?;
        let (_, _) = all_consuming(opt_ws_term(tag("WorldEnd")))(remain)?;

        let scene = PbrtScene { header, world: self.world };
        dbg!(self.interner);
        Ok(scene)
    }

    fn parse_world_stmts<'a>(&mut self, contents: &'a str) -> Result<&'a str, ParserError> {
        let mut it = iterator(contents, opt_ws_term(world_stmt));
        let res: Result<(), ParserError> = (&mut it).try_for_each(|stmt| {
                match stmt {
                    WorldStmt::Include(path) if self.resolve_includes => {
                        // TODO!!!!
                        let mmap = self.read_include_path(path)?;
                        let contents_str = unsafe { std::str::from_utf8_unchecked(mmap.as_ref()) };
                        let (s, _) = opt_ws(contents_str)?;
                        let remain = self.parse_world_stmts(&s)?;
                        all_consuming(opt_ws)(remain)?;
                        Ok(())
                    },
                    stmt @ _ => {
                        let interned_stmt = self.intern_stmt_strings(stmt);
                        self.world.push(interned_stmt);
                        Ok(())
                    }
                }
            });
        let _ = res?;
        let (remain, _) = it.finish()?;
        Ok(remain)
    }

    fn read_include_path(&self, path: impl AsRef<Path>) -> Result<Mmap, std::io::Error> {
        let mut incl_file_path = self.file_path.parent().ok_or(std::io::Error::new(std::io::ErrorKind::Other, "Invalid path"))?.to_path_buf();
        incl_file_path.push(path);

        let start = std::time::Instant::now();
        let file = std::fs::File::open(&incl_file_path)?;
        let mmap = unsafe { memmap::Mmap::map(&file)? };
        let len = mmap.len();
        unsafe {
            libc::madvise(mmap.as_ptr() as *mut _, mmap.len(), libc::MADV_SEQUENTIAL);
        }
        let time = start.elapsed();
        // TODO
        eprintln!("Included {:?} in {} ms, contents size {} MiB", incl_file_path.as_os_str(), time.as_millis(), len as f64 / 1024.0 / 1024.0);
        Ok(mmap)
    }

    fn intern_stmt_strings(&self, stmt: WorldStmt<&str>) -> WorldStmt<Rc<str>> {
        use WorldStmt::*;
        let i = &self.interner;
        match stmt {
            AttributeBegin => AttributeBegin,
            AttributeEnd => AttributeEnd,
            TransformBegin => TransformBegin,
            TransformEnd => TransformEnd,
            ObjectEnd => ObjectEnd,
            ReverseOrientation => ReverseOrientation,
            ObjectBegin(s) => ObjectBegin(i.get_or_intern(s)),
            Transform(tf) => Transform(tf),
            Shape(s, p) => Shape(i.get_or_intern(s), self.intern_param_strings(p)),
            ObjectInstance(s) => ObjectInstance(i.get_or_intern(s)),
            LightSource(s, p) => LightSource(i.get_or_intern(s), self.intern_param_strings(p)),
            AreaLightSource(s, p) => AreaLightSource(i.get_or_intern(s), self.intern_param_strings(p)),
            Material(s, p) => Material(i.get_or_intern(s), self.intern_param_strings(p)),
            MakeNamedMaterial(s, p) => MakeNamedMaterial(i.get_or_intern(s), self.intern_param_strings(p)),
            NamedMaterial(s) => NamedMaterial(i.get_or_intern(s)),
            Texture(tex) => {
                let TextureStmt { name, ty, class, params } = *tex;
                WorldStmt::texture(i.get_or_intern(name), i.get_or_intern(ty), i.get_or_intern(class), self.intern_param_strings(params))
            },
            MakeNamedMedium(s, p) => MakeNamedMedium(i.get_or_intern(s), self.intern_param_strings(p)),
            MediumInterface(s1, s2) => MediumInterface(i.get_or_intern(s1), i.get_or_intern(s2)),
            Include(s) => Include(i.get_or_intern(s)),
        }
    }

    fn intern_param_strings(&self, params: Vec<Param<&str>>) -> Vec<Param<Rc<str>>> {
        params.into_iter().map(|p| p.map_strings(|s| self.interner.get_or_intern(s))).collect()
    }

    pub fn interner(&self) -> &StringInterner {
        &self.interner
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
