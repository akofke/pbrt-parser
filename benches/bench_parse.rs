use rand::{thread_rng, Rng};
use rand::distributions::WeightedIndex;
use pbrt_parser::{Float3, ws_or_comment, opt_ws};
use criterion::{Criterion, Throughput, BenchmarkId, criterion_group, criterion_main};
use pbrt_parser::parser::PbrtParser;
use std::fmt::Write;
use std::time::Duration;
use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::value;
use nom::IResult;
use once_cell::sync::{Lazy, OnceCell};
use nom::multi::many1;

fn gen_random_input(n_statements: usize) -> String {
    let mut s = r#"WorldBegin"#.to_string();
    let mut rng = thread_rng();
    let choices = [""];

    for _ in 0..n_statements {
    }
    unimplemented!()
}

fn gen_mesh(n_points: usize) -> String {
    let mut rng = thread_rng();
    let mut s = String::with_capacity(10 * n_points);
    s.push_str(r#"Shape "trianglemesh" "point P" [ "#);
    for _ in 0..n_points {
        let x: f32 = rng.gen_range(-2.0, 2.0);
        let y: f32 = rng.gen_range(-2.0, 2.0);
        let z: f32 = rng.gen_range(-2.0, 2.0);
        writeln!(s, "{} {} {}", x, y, z).unwrap();
    }
    s.push_str("]\n");
    s.push_str(r#""integer indices" [ "#);
    for _ in 0..n_points {
        write!(s, "{} ", rng.gen_range(0, 100)).unwrap();
    }
    s.push_str("]");
    s
}

pub fn bench_single_large_mesh(c: &mut Criterion) {
    let size = 1000;
    let mesh = gen_mesh(size);
    let contents = format!("WorldBegin\n{}\nWorldEnd", mesh);

    let mut group = c.benchmark_group("large mesh");
    group.throughput(Throughput::Bytes(contents.as_bytes().len() as u64));
    group.bench_with_input(BenchmarkId::from_parameter(size), &contents, |b, contents| {
        b.iter_with_large_drop(|| PbrtParser::parse_string_no_includes(contents).unwrap());
    });
}

pub fn bench_curves(c: &mut Criterion) {
    let curve = concat!(
        r#"Shape "curve" "string type" [ "cylinder" ]
        "point P" [ -0.417543 15.3533 -1.06839 -0.422991 15.4336 -1.06742 -0.420546 15.5172 -1.06813 -0.416833 15.5945 -1.0825 ] "float width0" [ 0.005593 ] "float width1" [ 0.005333 ] "#,
        "\n"
    );
    let mut group = c.benchmark_group("curves");
    for &size in &[1, 10, 100] {

        let contents = format!("WorldBegin\n{}\nWorldEnd", curve.repeat(size));
        group.throughput(Throughput::Bytes(contents.as_bytes().len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &contents, |b, contents| {
            b.iter_with_large_drop(|| PbrtParser::parse_string_no_includes(contents).unwrap());
        });
    }
}

pub fn bench_instances(c: &mut Criterion) {
    let instance = concat!(
    r#"AttributeBegin
        Transform [ 1 0 0 0 0 1 0 0 0 0 1 0 11.3663998 2.08030009 28.5746002 1  ]
        ObjectInstance "Buddha_Mesh25251"
    AttributeEnd"#,
    "\n");

    let mut group = c.benchmark_group("instances");
    for &size in &[1, 10, 100] {

        let contents = format!("WorldBegin\n{}\nWorldEnd", instance.repeat(size));
        group.throughput(Throughput::Bytes(contents.as_bytes().len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &contents, |b, contents| {
            b.iter_with_large_drop(|| PbrtParser::parse_string_no_includes(contents).unwrap());
        });
    }
}

pub fn bench_parse_ws(c: &mut Criterion) {
    let input: &str = "                   #hi  \n\n \t\t    v";
    let short_sp = " 1";
    assert_eq!(ws_or_comment(input).unwrap().0, "v");
    assert_eq!(eat_opt_ws(input.as_bytes()), b"v");

    let mut group = c.benchmark_group("eat ws");
    group.throughput(Throughput::Bytes(input.len() as u64));

    group.bench_with_input(BenchmarkId::new("nom", "long"), &input, |b, &input| {
        b.iter(|| opt_ws(input));
    });

    group.bench_with_input(BenchmarkId::new("nom", "short"), &short_sp, |b, &input| {
        b.iter(|| opt_ws(input));
    });

    group.bench_with_input(BenchmarkId::new("custom", "long"), &input, |b, &input| {
        b.iter(|| eat_opt_ws(input.as_bytes()));
    });

    group.bench_with_input(BenchmarkId::new("custom", "short"), &short_sp, |b, &input| {
        b.iter(|| eat_opt_ws(input.as_bytes()));
    });

}

#[inline]
pub fn eat_opt_ws(s: &[u8]) -> &[u8] {
    if s.len() >= 2 && s[0].is_ascii_whitespace() && !s[1].is_ascii_whitespace() && !s[1] == b'#' {
        return &s[1..]
    }
    let mut s = trim_ws_start(s);
    while !s.is_empty() && s[0] == b'#' {
        s = trim_comment(s);
        s = trim_ws_start(s);
    }
    s
}

#[inline]
fn trim_ws_start(s: &[u8]) -> &[u8] {
    let i = s.iter().position(|&b| !b.is_ascii_whitespace());
    match i {
        Some(i) => &s[i..],
        None => b""
    }
}

#[inline]
fn trim_comment(s: &[u8]) -> &[u8] {
    let i = s.iter().position(|&b| b == b'\n');
    match i {
        Some(i) => &s[i..],
        None => b""
    }
}

criterion_group!(benches, bench_single_large_mesh, bench_curves, bench_instances, bench_parse_ws);
criterion_main!(benches);