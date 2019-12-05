use std::error::Error;
use pbrt_parser::parser::{PbrtParser, PbrtScene};
use pbrt_parser::statements::{WorldStmt, HeaderStmt};
use std::mem::size_of;
use pbrt_parser::{Param, ParamVal, SpectrumVal};

fn main() -> Result<(), Box<dyn Error>> {
    match PbrtParser::parse_with_includes("data/killeroo-simple.pbrt") {
        Ok(scene) => {
            println!("{:#?}", scene);
            print_stats(&scene);
        },
        Err(e) => println!("{:#?}", e)
    }
    Ok(())
}

fn print_stats(scene: &PbrtScene) {
    dbg!(size_of::<WorldStmt>());
    dbg!(size_of::<HeaderStmt>());
    dbg!(size_of::<Param>());
    dbg!(size_of::<ParamVal>());
    dbg!(size_of::<SpectrumVal>());
}

trait MemSize {
    fn total_size(&self) -> usize;
}

impl MemSize for WorldStmt {
    fn total_size(&self) -> usize {
        unimplemented!()
    }
}