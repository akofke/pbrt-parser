use std::error::Error;
use pbrt_parser::parser::{PbrtParser, PbrtScene};
use pbrt_parser::statements::{WorldStmt, HeaderStmt, TextureStmt};
use std::mem::{size_of, size_of_val, align_of};
use pbrt_parser::{Param, ParamVal, Float3, TransformStmt, Float2, STR_POOL};
use std::time::Instant;
use std::collections::HashMap;
use nom::lib::std::fmt::{Debug, Formatter};
use humansize::{FileSize, file_size_opts};

fn main() -> Result<(), Box<dyn Error>> {
    dbg!(size_of::<WorldStmt>());
    dbg!(size_of::<HeaderStmt>());
    dbg!(size_of::<Param>());
    dbg!(size_of::<ParamVal>());
    dbg!(align_of::<Param>());
    dbg!(align_of::<ParamVal>());
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
            eprintln!("String pool: {:#?}", *STR_POOL);
            print_stats(&scene);
        },
        Err(e) => println!("{:#?}", e)
    }
    Ok(())
}

fn print_stats(scene: &PbrtScene) {
    let stats = SceneStats::analyze_scene(scene);
//    let mb = size as f64 / (1024.0 * 1024.0);
//    eprintln!("Total world statements: {}", scene.world.len());
//    eprintln!("Scene memory usage: {} MiB", mb);
    eprintln!("{:#?}", stats);
}

fn format_bytes(n: usize) -> String {
    n.file_size(file_size_opts::DECIMAL).unwrap()
}

#[derive(Default)]
struct SceneStats {
    pub mem_bytes: usize,
    pub world_statements: usize,
    pub total_params: usize,
    pub param_type_counts: HashMap<&'static str, usize>,
    pub mem_usage_breakdown: HashMap<&'static str, usize>,
}

impl Debug for SceneStats {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let map: HashMap<_, _> = self.mem_usage_breakdown.iter()
            .map(|(&k, &v)| (k, format_bytes(v)))
            .collect();
        f.debug_struct("SceneStats")
            .field("mem_size", &format_bytes(self.mem_bytes))
            .field("world_statements", &self.world_statements)
            .field("total_params", &self.total_params)
            .field("param_type_counts", &self.param_type_counts)
            .field("mem_usage_breakdown", &map)
            .finish()
    }
}

impl SceneStats {
    pub fn analyze_scene(scene: &PbrtScene) -> Self {
        let mut stats = Self::default();
        stats.world_stats(&scene.world);
        stats
    }

    #[allow(unused_variables)]
    fn world_stats(&mut self, w: &[WorldStmt]) {
        let world_stmt_size = size_of::<WorldStmt>() * w.len();
        self.mem_bytes += world_stmt_size;
        self.world_statements += w.len();
        *self.mem_usage_breakdown.entry("world_statements").or_insert(0) += world_stmt_size;
        w.iter().for_each(|s| {
            match s {
                WorldStmt::Transform(tf) => {
                    let tf_sz = tf_size(tf);
                    self.mem_bytes += tf_sz;
                    *self.mem_usage_breakdown.entry("transforms").or_insert(0) += tf_sz;
                },
                WorldStmt::Shape(s, p) => {
                    self.params_stats(p)
                },
                WorldStmt::LightSource(s, p) => self.params_stats(p),
                WorldStmt::AreaLightSource(s, p) => self.params_stats(p),
                WorldStmt::Material(s, p) => self.params_stats(p),
                WorldStmt::MakeNamedMaterial(s, p) => self.params_stats(p),
                WorldStmt::Texture(b) => {
                    self.params_stats(&b.params)
                },
                WorldStmt::MakeNamedMedium(s, p) => self.params_stats(p),
                _ => {},
            };

        });
    }

    fn params_stats(&mut self, params: &[Param]) {
        let params_size = size_of::<Param>() * params.len();
        self.mem_bytes += params_size;
        self.total_params += params.len();
        *self.mem_usage_breakdown.entry("params").or_insert(0) += params_size;

        params.iter().for_each(|p| {
            match &p.value {
                ParamVal::Int(v) => self.param_val_stats(v, "int"),
                ParamVal::Float(v) => self.param_val_stats(v, "float"),
                ParamVal::Point2(v) => self.param_val_stats(v, "point2"),
                ParamVal::Point3(v) => self.param_val_stats(v, "point3"),
                ParamVal::Vector2(v) => self.param_val_stats(v, "vector2"),
                ParamVal::Vector3(v) => self.param_val_stats(v, "vector3"),
                ParamVal::Normal3(v) => self.param_val_stats(v, "normal3"),
                ParamVal::Bool(v) => self.param_val_stats(v, "bool"),
                ParamVal::SpectrumRgb(v) => self.param_val_stats(v, "spectrum_rgb"),
                ParamVal::SpectrumXyz(v) => self.param_val_stats(v, "spectrum_xyz"),
                ParamVal::SpectrumSampled(v) => self.param_val_stats(v, "spectrum_sampled"),
                ParamVal::SpectrumBlackbody(v) => self.param_val_stats(v, "spectrum_blackbody"),
                _ => {}
            };
        });
    }

    fn param_val_stats<T>(&mut self, v: &[T], name: &'static str) {
        let vals_size = size_of::<T>() * v.len();
        self.mem_bytes += vals_size;
        *self.mem_usage_breakdown.entry("param_vals").or_insert(0) += vals_size;
        *self.param_type_counts.entry(name).or_insert(0) += v.len();
    }
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
        size_of::<Param>() + match &p.value {
            ParamVal::Int(v) => vec_size(v),
            ParamVal::Float(v) => vec_size(v),
            ParamVal::Point2(v) => vec_size(v),
            ParamVal::Point3(v) => vec_size(v),
            ParamVal::Vector2(v) => vec_size(v),
            ParamVal::Vector3(v) => vec_size(v),
            ParamVal::Normal3(v) => vec_size(v),
            ParamVal::Bool(v) => vec_size(v),
            ParamVal::String(s) => 0,
            ParamVal::Texture(s) => 0,
            ParamVal::SpectrumRgb(v) => vec_size(v),
            ParamVal::SpectrumXyz(v) => vec_size(v),
            ParamVal::SpectrumSampled(v) => vec_size(v),
            ParamVal::SpectrumBlackbody(v) => vec_size(v),
        }
    }).sum()
}

