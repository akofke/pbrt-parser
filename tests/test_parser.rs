use pbrt_parser::{WorldStmt as Ws, PbrtScene, PbrtParser, param};

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
        Ws::AttributeBegin,
        Ws::Shape("trianglemesh".into(), vec![
            param!(P, Point3([1.0, 1.0, 1.0])),
            param!(st, Point2([0.0, 1.0], [1.0, 0.0])),
        ]),
        Ws::AttributeEnd,
    ];
    let expected = PbrtScene {header, world};

    let parsed = PbrtParser::parse_with_includes("data/testfile.pbrt").unwrap();

    assert_eq!(parsed, expected);
}