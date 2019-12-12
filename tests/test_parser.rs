use pbrt_parser::{WorldStmt as Ws, PbrtScene, PbrtParser};

#[test]
fn parse_file() {
    let header = vec![];

    let world = vec![
        Ws::AttributeBegin,
        Ws::ObjectBegin("foo".into()),
        Ws::Shape("sphere".into(), vec![]),
        Ws::ObjectEnd,
        Ws::AttributeEnd,
        Ws::ObjectInstance("foo".into()),
    ];
    let expected = PbrtScene {header, world};

    let parsed = PbrtParser::parse_with_includes("data/testfile.pbrt").unwrap();

    assert_eq!(parsed, expected);
}