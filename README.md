# A Universal Chess Interface (UCI) Parser

This crate contains types and functions for communicating with chess engines and chess GUIs through the [Universal Chess Interface (UCI)](https://backscattering.de/chess/uci/) protocol.

## Overview

The primary function of this crate is to provide well-typed representations for every message/command described in the UCI protocol, along with an easy-to-use API for converting between these well-typed representations and strings.

## Examples

The simplest use case is parsing commands like `uci`, which is as easy as it sounds:

```rust
use uci_parser::UciCommand;
let cmd = UciCommand::new("uci").unwrap();
assert_eq!(cmd, UciCommand::Uci);
```

Commands implement `FromStr`, so you can call `.parse()`:

```rust
use uci_parser::UciCommand;

// Arbitrary whitespace is handled appropriately
let cmd = "setoption    name \n    Threads value \t 16".parse::<UciCommand>();
assert!(cmd.is_ok());

assert_eq!(
    cmd.unwrap(),
    UciCommand::SetOption {
        name: "Threads".to_string(),
        value: Some("16".to_string())
    }
);
```

Commands that have many optional arguments, like `go`, implement `Default` so they can be parsed cleanly:

```rust
use std::time::Duration;
use uci_parser::{UciCommand, UciSearchOptions};

let cmd = "go movetime 42".parse::<UciCommand>();
assert!(cmd.is_ok());

assert_eq!(
    cmd.unwrap(),
    UciCommand::Go(UciSearchOptions {
        // Times are provided as Durations
        movetime: Some(Duration::from_millis(42)),
        ..Default::default()
    })
);
```

Engine-to-GUI responses can also be created and printed easily:

```rust
use uci_parser::UciResponse;
let resp = UciResponse::BestMove {
    bestmove: Some("e2e4"),
    ponder: Some("c7c5")
};

assert_eq!(resp.to_string(), "bestmove e2e4 ponder c7c5");
```

More complex responses, such as `info`, have helper functions:

```rust
use uci_parser::{UciResponse, UciInfo, UciScore};
let score = UciScore::cp(42);
let info = UciInfo::new().nodes(440).depth(2).score(score);
let resp = UciResponse::info(info);

// `info` params are displayed in the same order every time.
assert_eq!(resp.to_string(), "info depth 2 nodes 440 score cp 42");
```

## Crate Features

How edge cases should be handled is a delicate subject and the correct answer depends on the needs of your engine.
Rather than enforce my own opinion on handling those edge cases, I've marked them as crate features.

-   `parse-go-perft`: Adds support for parsing `perft <depth>` as an argument to the `go` command.

    -   This is not part of the UCI protocol, but is [common among engines](https://github.com/official-stockfish/Stockfish/blob/d6043970bd156b1d2ab6cb51e8d5cb0c6a40797c/tests/perft.sh#L17).

-   `parse-bench`: Adds support for parsing the string `bench` into `UciCommand::Bench`.

    -   This is not part of the UCI protocol, but is [common among engines](https://official-stockfish.github.io/docs/stockfish-wiki/UCI-&-Commands.html#bench) and very useful for engine development.
    -   The arguments to `bench` are the same as the arguments to `go`, since both commands involve running searches.

-   `parse-position-kiwipete`: Adds support to parse [`kiwipete`](https://www.stmintz.com/ccc/index.php?id=274926) as a special argument to `position` (similar to `startpos`).

    -   The ["kiwipete" position](https://www.chessprogramming.org/Perft_Results#Position_2) is useful for debugging engines, as it is a messy position with many possible moves available.
    -   If enabled, `position kiwipete` will be equivalent to parsing `position fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1`, so the `fen` field of `Position` will be `Some(<kiwipete fen>)`.

-   `validate-promotion-moves`: Restricts the grammar when parsing moves in UCI notation to ensure that promotions are valid:

    -   By default, moves are parsed with the grammar `[a-h][1-8][a-h][1-8][pnbrqk]`, which will parse `e2e4p` successfully, even though it doesn't "make sense" in-game. With this feature enabled, the grammar restricts to: `[a-h][1-8][a-h][1-8] | [a-h]7[a-h]8[qnrb] | [a-h]2[a-h]1[qnrb]`. This means that only moves from the penultimate ranks to the final ranks will be parsed as promotions, and the only valid pieces for a promotion are a Queen, Knight, Rook, or Bishop.

-   `clamp-negatives`: Clamps negative numbers to `0` when parsing.

    -   By default, all numbers are assumed to be positive, and the parser will fail on strings like `go wtime -80`. This is normally not a problem,
    -   All numeric values within the UCI protocol _should_ be positive. That said, there _have_ been instances where some GUIs send negative move times, which could mean any variety of things. If this feature is enabled, _all_ numeric values are clamped to _at least_ 0. That is, if a GUI sent `go movetime -42`, this crate will parse that as a `Duration` of `0` milliseconds. It is up to your engine to determine how to respond to these situations.

-   `err-on-unused-input`: Causes the parser to fail if the input text was not fully consumed during parsing.

    -   As per the [protocol](https://backscattering.de/chess/uci/#unknown), unknown tokens encountered _before_ a command are ignored (`joho debug on` parses to `debug on`). Unknown tokens encountered while parsing a specific command will generate errors (`debug joho on` fails). Unknown tokens _after_ a command are, by default, ignored (`debug on joho` parses to `debug on`). If this feature is enabled, the parser will fail if _all_ tokens were not consumed during parsing (`debug on joho` will fail).

-   `types`: Exposes several well-typed representations of UCI components, such as moves.
    -   By default, commands like `position startpos moves e2e4` will yield a list of `String`s for all parsed `moves`, leaving you to have to re-parse them in your engine later. If this feature is enabled, any `String` that is parsed as a move will be converted to [`UciMove`], which contains types representing the files, ranks, squares, and pieces involved in each move.
    -   See the [`types`] module for more information.

```

```

```

```
