# pbrt-parser

This crate is a work-in-progress attempt to create a fast, lazy parser for [PBRT scene input files](https://www.pbrt.org/fileformat-v3.html).  It is built with the [nom](https://github.com/Geal/nom) parser-combinator library for fast text parsing and to avoid
haivng to create an in-memory syntax tree with a separate lexing step. It intends to provide data in a renderer-agnostic form without assigning any semantic meaning to statements to remain flexible for any use-case.

## Goals

* PBRT files can potentially contain large amounts of data such as mesh vertices, curve shapes, and instance declarations. In particular the eventual goal for this library is to be able
to parse the [Disney Moana island scene](https://www.technology.disneyanimation.com/islandscene) provided in PBRT format (41 GB of scene files) as fast as possible. Using memory mapped IO and an iterator-based API, this should be possible while only using "real" memory for one statement at a time.
