use pbrt_parser::{WorldStmt as Ws, PbrtScene, PbrtParser};

#[test]
fn parse_file() {
    let header = vec![];

    let world = vec![
        Ws::AttributeBegin,
        Ws::ObjectBegin("foo".to_string()),
        Ws::Shape("sphere".to_string(), vec![]),
        Ws::ObjectEnd,
        Ws::AttributeEnd,
        Ws::ObjectInstance("foo".to_string()),
    ];
    let expected = PbrtScene {header, world};

    let parsed = PbrtParser::parse_with_includes("data/testfile.pbrt").unwrap();

    assert_eq!(parsed, expected);
}