use pbrt_parser::{WorldStmt as Ws, PbrtScene, PbrtParser};

#[test]
fn parse_file() {
    let header = vec![];

    let world = vec![
        Ws::AttributeBlock(vec![
            Ws::InstanceBlock("foo".to_string(), vec![
                Ws::Shape("sphere".to_string(), vec![])
            ])
        ])
    ];
    let expected = PbrtScene {header, world};

    let parsed = PbrtParser::parse_with_includes("data/testfile.pbrt").unwrap();

    assert_eq!(parsed, expected);
}