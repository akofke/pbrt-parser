use std::error::Error;
use pbrt_parser::parser::{PbrtParser, PbrtScene};
use pbrt_parser::statements::{WorldStmt, HeaderStmt};
use std::mem::{size_of, size_of_val};
use pbrt_parser::{Param, ParamVal, Float3, TransformStmt, Float2};
use std::time::Instant;

fn main() -> Result<(), Box<dyn Error>> {
    dbg!(size_of::<WorldStmt>());
    dbg!(size_of::<HeaderStmt>());
    dbg!(size_of::<Param>());
    dbg!(size_of::<ParamVal>());
    dbg!(size_of::<Float3>());
    dbg!(size_of::<String>());
    dbg!(size_of::<Vec<f32>>());
    let path = std::env::args().nth(1).unwrap();
    let start = Instant::now();
    let res = PbrtParser::parse_with_includes(path);
    let elapsed = start.elapsed();
    eprintln!("Parsed in {} ms", elapsed.as_millis());
    match res {
        Ok(scene) => {
//            println!("{:#?}", scene);
            print_stats(&scene);
        },
        Err(e) => println!("{:#?}", e)
    }
    Ok(())
}

fn print_stats(scene: &PbrtScene) {
    let size = world_size(&scene.world);
    let mb = size as f64 / (1024.0 * 1024.0);
    eprintln!("Scene memory usage: {} MiB", mb);
}

fn world_size(w: &[WorldStmt]) -> usize {
    w.iter().map(|s| {
        size_of::<WorldStmt>() + match s {
            WorldStmt::AttributeBlock(v) => {
                world_size(v)
            },
            WorldStmt::TransformBlock(v) => {
                world_size(v)
            },
            WorldStmt::InstanceBlock(s, v) => {
                s.len() + world_size(v)
            },
            WorldStmt::ReverseOrientation => 0,
            WorldStmt::Transform(tf) => {
                tf_size(tf)
            },
            WorldStmt::Shape(s, p) => {
                s.len() + params_size(p)
            },
            WorldStmt::ObjectInstance(s) => s.len(),
            WorldStmt::LightSource(s, p) => s.len() + params_size(p),
            WorldStmt::AreaLightSource(s, p) => s.len() + params_size(p),
            WorldStmt::Material(s, p) => s.len() + params_size(p),
            WorldStmt::MakeNamedMaterial(s, p) => s.len() + params_size(p),
            WorldStmt::NamedMaterial(s) => s.len(),
            WorldStmt::Texture { name, ty, class, params } => {
                name.len() + ty.len() + class.len() + params_size(params)
            },
            WorldStmt::MakeNamedMedium(s, p) => s.len() + params_size(p),
            WorldStmt::MediumInterface(s, s2) => s.len() + s2.len(),
            WorldStmt::Include(s) => s.len(),
            WorldStmt::ResolvedInclude(v) => world_size(v),
        }

    }).sum()
}

fn tf_size(tf: &TransformStmt) -> usize {
    size_of::<TransformStmt>() + match tf {
        TransformStmt::Identity => 0,
        TransformStmt::Translate(_) => size_of::<Float3>(),
        TransformStmt::Scale(_) => size_of::<Float3>(),
        TransformStmt::Rotate(_) => size_of::<[f32; 4]>(),
        TransformStmt::LookAt(_) => size_of::<[f32; 9]>(),
        TransformStmt::CoordinateSystem(s) => s.len(),
        TransformStmt::CoordSysTransform(s) => s.len(),
        TransformStmt::Transform(_) => size_of::<[f32; 16]>(),
        TransformStmt::ConcatTransform(_) => size_of::<[f32; 16]>(),
    }
}

fn vec_size<T: Sized>(v: &Vec<T>) -> usize {
    v.capacity() * size_of::<T>()
}

fn params_size(params: &[Param]) -> usize {
    params.iter().map(|p| {
        size_of::<Param>() + p.name.len() + match &p.value {
            ParamVal::Int(v) => vec_size(v),
            ParamVal::Float(v) => vec_size(v),
            ParamVal::Point2(v) => vec_size(v),
            ParamVal::Point3(v) => vec_size(v),
            ParamVal::Vector2(v) => vec_size(v),
            ParamVal::Vector3(v) => vec_size(v),
            ParamVal::Normal3(v) => vec_size(v),
            ParamVal::Bool(v) => vec_size(v),
            ParamVal::String(s) => s.len(),
            ParamVal::Texture(s) => s.len(),
            ParamVal::SpectrumRgb(v) => vec_size(v),
            ParamVal::SpectrumXyz(v) => vec_size(v),
            ParamVal::SpectrumSampled(v) => vec_size(v),
            ParamVal::SpectrumBlackbody(v) => vec_size(v),
        }
    }).sum()
}

