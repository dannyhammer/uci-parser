/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{str::FromStr, time::Duration};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{anychar, digit1, multispace0, multispace1},
    combinator::{cut, eof, map, map_res, opt, recognize, value, verify},
    multi::{many0_count, many1_count, many_till},
    sequence::{delimited, pair, preceded},
    Err, IResult,
};

use crate::{UciCommand, UciParseError, UciSearchOptions};

#[cfg(feature = "types")]
use crate::{uci_move, UciMove};

#[cfg(feature = "validate-promotion-moves")]
use nom::character::complete::char;
#[cfg(feature = "validate-promotion-moves")]
use nom::combinator::not;

#[cfg(not(feature = "types"))]
use nom::{character::complete::one_of, sequence::tuple};

/// Top-level parser to convert a string into a [`UciCommand`].
///
/// See also [`UciCommand::new`]
pub(crate) fn parse_uci_command(input: &str) -> Result<UciCommand, UciParseError> {
    #[cfg(feature = "err-on-unused-input")]
    let mut parser =
        nom::combinator::all_consuming(map(many_till(anychar, parse_command), |(_, cmd)| cmd));

    // The `many_till(anychar)` parser consumes any unknown characters until a command is parsed.
    #[cfg(not(feature = "err-on-unused-input"))]
    let mut parser = map(many_till(anychar, parse_command), |(_, cmd)| cmd);

    parser(input).map(|(_rest, cmd)| cmd).map_err(|e| match e {
        // A recoverable error means that the command was not recognized
        Err::Error(_) | Err::Incomplete(_) => UciParseError::UnrecognizedCommand {
            cmd: input.to_string(),
        },

        // A failure means that the command was recognized, but the argument(s) provided to it were invalid.
        Err::Failure(e) => {
            // Find the location of where this error originated
            let place = input.rfind(e.input).unwrap_or_default();

            // If the location of this error is NOT at the end of the input string, subtract 1 from it (to get rid of trailing whitespace)
            let place = if place < input.len() {
                place.checked_sub(1).unwrap_or_default()
            } else {
                place
            };

            // Get everything that was successfully parsed.
            let cmd = input.get(..place).unwrap().to_string();

            // Get everything that *wasn't* parsed.
            let arg = e.input.to_string();

            if arg.is_empty() {
                UciParseError::InsufficientArguments { cmd }
            } else {
                UciParseError::InvalidArgument { cmd, arg }
            }
        }
    })
}

/// Parses a single-word [`UciCommand`] like `uci` or `isready`.
fn single_command<'a>(
    ident: &'static str,
    cmd: UciCommand,
) -> impl FnMut(&'a str) -> IResult<&'a str, UciCommand> {
    value(cmd, term(ident))
}

/// Parses a multi-word [`UciCommand`] like `go` or `setoption`.
fn multi_command<'a, F>(
    ident: &'static str,
    parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, UciCommand>
where
    F: FnMut(&'a str) -> IResult<&'a str, UciCommand>,
{
    preceded(term(ident), cut(parser))
}

/// Parses a single UCI command
fn parse_command(input: &str) -> IResult<&str, UciCommand> {
    alt((
        single_command("uci", UciCommand::Uci),
        multi_command("debug", parse_debug_args),
        single_command("isready", UciCommand::IsReady),
        multi_command("setoption", parse_setoption_args),
        multi_command("register", parse_register_args),
        single_command("ucinewgame", UciCommand::UciNewGame),
        multi_command("position", parse_position_args),
        multi_command("go", parse_go_args),
        single_command("stop", UciCommand::Stop),
        single_command("ponderhit", UciCommand::PonderHit),
        single_command("quit", UciCommand::Quit),
        #[cfg(feature = "parse-bench")]
        multi_command("bench", parse_bench_args),
    ))(input)
}

/// Parses arguments to the `debug` command.
fn parse_debug_args(input: &str) -> IResult<&str, UciCommand> {
    map(
        alt((
            value(true, term("on")),   // "on" to `true`
            value(false, term("off")), // "off" to `false`
        )),
        UciCommand::Debug,
    )(input)
}

/// Parses arguments to the `setoption` command.
fn parse_setoption_args(input: &str) -> IResult<&str, UciCommand> {
    let name = between("name", "value");
    let value = rest_after("value");

    map(pair(name, opt(value)), |(name, value)| {
        UciCommand::SetOption {
            name: name.to_string(),
            value: value.map(str::to_string),
        }
    })(input)
}

/// Parses arguments to the `register` command.
fn parse_register_args(input: &str) -> IResult<&str, UciCommand> {
    // Parses `later` as `(None, None)`
    let later = value((None, None), term("later"));

    let name = between("name", "code");
    let code = rest_after("code");

    map(
        alt((
            later, // Either `later` which maps to no name or code
            // Or an optional name and optional code, but at least one of the two!
            verify(pair(opt(name), opt(code)), |(n, c)| {
                c.is_some() || n.is_some() // Ensure a name OR code was provided
            }),
        )),
        |(name, code)| UciCommand::Register {
            name: name.map(str::to_string),
            code: code.map(str::to_string),
        },
    )(input)
}

/// Parses arguments to the `position` command.
fn parse_position_args(input: &str) -> IResult<&str, UciCommand> {
    // Either `startpos` or `fen <FEN>`
    let fen = alt((
        value(None, term("startpos")),
        map(parse_fen, |s| Some(s.to_string())),
        #[cfg(feature = "parse-position-kiwipete")]
        value(
            Some(String::from(
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            )),
            term("kiwipete"),
        ),
    ));

    let moves = map(opt(moves_after("moves")), Option::unwrap_or_default);

    map(pair(fen, moves), |(fen, moves)| UciCommand::Position {
        fen,
        moves: moves.into_iter().map(Into::into).collect(),
    })(input)
}

/// Parses all arguments to the `go` command.
fn parse_go_args(input: &str) -> IResult<&str, UciCommand> {
    map(parse_search_options, UciCommand::Go)(input)
}

/// Parses individual arguments to the `go` command into a [`UciSearchOptions`] struct.
///
/// This is in it's own function because [`UciSearchOptions`] is used by both `go` and `bench`.
fn parse_search_options(input: &str) -> IResult<&str, UciSearchOptions> {
    let mut opt = UciSearchOptions::default();

    // Arguments to `go` can be in any order
    let (input, count) = many0_count(alt((
        map(moves_after("searchmoves"), |x| {
            opt.searchmoves = x.into_iter().map(Into::into).collect()
        }),
        map(term("ponder"), |_| opt.ponder = true),
        map(time_after("wtime"), |x| opt.wtime = Some(x)),
        map(time_after("btime"), |x| opt.btime = Some(x)),
        map(time_after("winc"), |x| opt.winc = Some(x)),
        map(time_after("binc"), |x| opt.binc = Some(x)),
        map(num_after("movestogo"), |x| opt.movestogo = Some(x)),
        map(num_after("depth"), |x| opt.depth = Some(x)),
        map(num_after("nodes"), |x| opt.nodes = Some(x)),
        map(num_after("mate"), |x| opt.mate = Some(x)),
        map(time_after("movetime"), |x| opt.movetime = Some(x)),
        map(term("infinite"), |_| opt.infinite = true),
        #[cfg(feature = "parse-go-perft")]
        map(num_after("perft"), |x| opt.perft = Some(x)),
    )))(input)?;

    // If no arguments were supplied, this should be treated as `go infinite`
    if count == 0 {
        opt.infinite = true;
    }

    Ok((input, opt))
}

/// Parses arguments to the `bench` command.
#[cfg(feature = "parse-bench")]
fn parse_bench_args(input: &str) -> IResult<&str, UciCommand> {
    map(parse_search_options, UciCommand::Bench)(input)
}

/// A combinator that takes an identifier `ident` and produces a parser that
/// parses ONLY `ident`, consuming any leading/trailing whitespace and failing if
/// anything other than EOF or whitespace follows `ident`.
fn term<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(ident), alt((eof, multispace1)))
}

/// A parser to consume the remainder of `input`, erroring with a [`nom::Err::Failure`] if there is no remaining input.
fn rest_nonempty(input: &str) -> IResult<&str, &str> {
    recognize(cut(many1_count(anychar)))(input)
}

/// A combinator that parses everything after `ident` until `end` is found, returning everything in between.
///
/// This fails if there is no input between `ident` and `end` to consume.
fn between<'a>(ident: &'a str, end: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    preceded(
        term(ident),
        verify(
            map(alt((take_until(end), rest_nonempty)), str::trim),
            |s: &str| !s.is_empty(),
        ),
    )
}

/// A combinator that takes an identifier `ident` and produces a parser that discards `ident`
/// and consumes the rest of the input, failing if there is no more input to consume.
fn rest_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    preceded(term(ident), map(rest_nonempty, str::trim))
}

/// Parses a non-empty list of moves after `ident`
#[cfg(not(feature = "types"))]
fn moves_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<&'a str>> {
    preceded(
        term(ident),
        cut(nom::multi::many0(delimited(
            multispace0,
            recognize(uci_move),
            multispace0,
        ))),
    )
}

#[cfg(feature = "types")]
fn moves_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<UciMove>> {
    preceded(
        term(ident),
        cut(nom::multi::many1(delimited(
            multispace0,
            uci_move,
            multispace0,
        ))),
    )
}

/// Parses everything after `fen` as a FEN until either the keyword `moves` or the end of the line.
///
/// Fails if there is no non-whitespace input following `fen`.
fn parse_fen(input: &str) -> IResult<&str, &str> {
    between("fen", "moves")(input)
}

/// Parses and maps a base-10 number
///
/// Negative numbers are handled by the `clamp-negatives` crate feature.
fn parse_num<T>(input: &str) -> IResult<&str, T>
where
    T: FromStr + Default + Clone,
{
    #[cfg(feature = "clamp-negatives")]
    {
        cut(alt((
            map_res(digit1, str::parse), // First try a normal, positive number
            preceded(tag("-"), value(T::default(), digit1)), // Then try negative, and clamp it
        )))(input)
    }

    #[cfg(not(feature = "clamp-negatives"))]
    cut(map_res(digit1, str::parse))(input)
}

/// Parses and maps a positive base-10 number following `ident`
///
/// Negative numbers are handled by the `clamp-negatives` crate feature.
fn num_after<'a, T>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    T: FromStr + Default + Clone,
{
    preceded(term(ident), parse_num)
}

/// Parses and maps a [`Duration`]
fn parse_time(input: &str) -> IResult<&str, Duration> {
    map(parse_num, Duration::from_millis)(input)
}

/// Parses and maps a [`Duration`] following `ident`
fn time_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, Duration> {
    preceded(term(ident), parse_time)
}

#[cfg(not(feature = "types"))]
/// Parses a UCI move, which is in the format `<start square><end square>[promotion]`.
///
/// This can also parse a nullmove, which is the string `0000`.
///
/// If the feature `validate-promotion-moves` is enabled, this function will fail to
/// parse "invalid" moves such as `e2e4q` or `b7b8p`.
fn uci_move(input: &str) -> IResult<&str, &str> {
    // [a-h]
    #[inline(always)]
    fn file(input: &str) -> IResult<&str, char> {
        one_of("abcdefgh")(input)
    }

    // [1-8]
    #[inline(always)]
    fn rank(input: &str) -> IResult<&str, char> {
        one_of("12345678")(input)
    }

    // [a-h][1-8]
    #[inline(always)]
    fn square(input: &str) -> IResult<&str, (char, char)> {
        pair(file, rank)(input)
    }

    #[cfg(not(feature = "validate-promotion-moves"))]
    {
        // any piece
        let piece = one_of("PpNnBbRrQqKk");

        recognize(alt((
            recognize(tuple((square, square, opt(piece)))),
            term("0000"),
        )))(input)
    }

    #[cfg(feature = "validate-promotion-moves")]
    {
        // [qnrb]
        #[inline(always)]
        fn piece(input: &str) -> IResult<&str, char> {
            one_of("QqNnRrBb")(input)
        }

        // [a-h][1-8][a-h][1-8]
        let non_promotion = recognize(tuple((square, square, not(piece))));

        // [a-h]2[a-h]1[qnrb]
        let promotion_rank1 =
            recognize(tuple((pair(file, char('2')), pair(file, char('1')), piece)));

        // [a-h]7[a-h]8[qnrb]
        let promotion_rank8 =
            recognize(tuple((pair(file, char('7')), pair(file, char('8')), piece)));

        alt((
            non_promotion,
            promotion_rank1,
            promotion_rank8,
            term("0000"),
        ))(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Creates a new command and asserts that it is `Ok`.
    fn new_cmd(input: &str) -> UciCommand {
        let cmd = UciCommand::new(input);
        assert!(
            cmd.is_ok(),
            "Failed to parse {input:?}\nGot {:?}",
            cmd.unwrap_err()
        );
        cmd.unwrap()
    }

    /// Creates a new command and asserts that it is `Err`.
    fn new_err(input: &str) {
        let cmd = UciCommand::new(input);
        assert!(cmd.is_err(), "Should error from {input:?}\nGot {cmd:?}");
    }

    /// Converts a list of UCI-notation move strings to a vector of [`UciMove`] structs.
    ///
    /// # Panics
    ///
    /// Will panic if any strings in `moves` are not valid UCI notation.
    #[cfg(feature = "types")]
    fn moves(moves: &[&str]) -> Vec<UciMove> {
        moves.iter().map(|s| s.parse().unwrap()).collect()
    }

    #[cfg(not(feature = "types"))]
    fn moves(moves: &[&str]) -> Vec<String> {
        moves.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn test_parse_unknown_word() {
        new_cmd("joho debug on");

        new_err("joho");

        new_err("debug joho on");

        #[cfg(feature = "err-on-unused-input")]
        new_err("debug on joho");

        #[cfg(not(feature = "err-on-unused-input"))]
        {
            new_cmd("debug on joho");
        }
    }
    #[test]
    fn test_parse_single_commands() {
        let cmd = new_cmd("    uci ");
        assert_eq!(cmd, UciCommand::Uci);

        let cmd = new_cmd("    \t\nuci \t   \n");
        assert_eq!(cmd, UciCommand::Uci);

        let cmd = new_cmd("    isready ");
        assert_eq!(cmd, UciCommand::IsReady);
    }

    #[test]
    fn test_parse_debug() {
        let cmd = new_cmd("    debug    on");
        assert_eq!(cmd, UciCommand::Debug(true));

        let cmd = new_cmd("    debug    off    ");
        assert_eq!(cmd, UciCommand::Debug(false));

        new_err("    debug    ");

        new_err("    debug    no");
    }

    #[test]
    fn test_parse_setoption() {
        let cmd = new_cmd("setoption name Nullmove value true\t");
        assert_eq!(
            cmd,
            UciCommand::SetOption {
                name: "Nullmove".into(),
                value: Some("true".into())
            }
        );

        let cmd = new_cmd("setoption     name   Selectivity    value   3  ");
        assert_eq!(
            cmd,
            UciCommand::SetOption {
                name: "Selectivity".into(),
                value: Some("3".into())
            }
        );

        let cmd = new_cmd("setoption     name   Style    value   Risky  ");
        assert_eq!(
            cmd,
            UciCommand::SetOption {
                name: "Style".into(),
                value: Some("Risky".into())
            }
        );

        let cmd = new_cmd("setoption     name   Clear Hash  ");
        assert_eq!(
            cmd,
            UciCommand::SetOption {
                name: "Clear Hash".into(),
                value: None,
            }
        );

        let cmd = new_cmd(
            "setoption     name   NalimovPath    value   c:\\chess\\tb\\4;c:\\chess\\tb\\5".into(),
        );
        assert_eq!(
            cmd,
            UciCommand::SetOption {
                name: "NalimovPath".into(),
                value: Some("c:\\chess\\tb\\4;c:\\chess\\tb\\5".into())
            }
        );

        new_err("setoption");

        new_err("setoption  name");

        new_err("setoption   name  value");

        new_err("setoption   name  value    Risky");

        new_err("setoption   name  Clear Hash  value");
    }

    #[test]
    fn test_parse_register() {
        let cmd = new_cmd("register  later");
        assert_eq!(
            cmd,
            UciCommand::Register {
                name: None,
                code: None
            }
        );

        let cmd = new_cmd("register    name Stefan MK   code\t\t4359874324   \t");
        assert_eq!(
            cmd,
            UciCommand::Register {
                name: Some("Stefan MK".into()),
                code: Some("4359874324".into())
            }
        );

        let cmd = new_cmd("register    name Stefan MK \t");
        assert_eq!(
            cmd,
            UciCommand::Register {
                name: Some("Stefan MK".into()),
                code: None
            }
        );

        let cmd = new_cmd("register    code\t\t4359874324   \t");
        assert_eq!(
            cmd,
            UciCommand::Register {
                name: None,
                code: Some("4359874324".into())
            }
        );

        new_err("register");

        new_err("  register   name  ");

        new_err("register   name  code  ");

        new_err("register   name   Stefan MK  code");
    }

    #[test]
    fn test_parse_ucinewgame() {
        let cmd = new_cmd("  ucinewgame  ");
        assert_eq!(cmd, UciCommand::UciNewGame);
    }

    #[test]
    fn test_parse_position() {
        let res =
            parse_fen("fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1");
        assert_eq!(
            res.unwrap().1,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        );

        let res = parse_fen(
            "fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 moves e2a6",
        );
        assert_eq!(
            res.unwrap().1,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        );

        let res = moves_after("moves")("moves   e2a6 \th3g2   h1g1 e6d5");
        assert_eq!(res.unwrap().1, moves(&["e2a6", "h3g2", "h1g1", "e6d5"]));

        let cmd = new_cmd("position startpos");
        assert_eq!(
            cmd,
            UciCommand::Position {
                fen: None,
                moves: vec![]
            }
        );

        let cmd = new_cmd("position   fen  r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1   moves  e2a6 \t h3g2  h1g1 e6d5");
        assert_eq!(
            cmd,
            UciCommand::Position {
                fen: Some(
                    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1".into()
                ),
                moves: moves(&["e2a6", "h3g2", "h1g1", "e6d5"])
            }
        );

        new_err("  position ");
        new_err("position   fen   ");
        new_err("  position \tfen\t moves\t");
        new_err("\t position  fen   moves   e2e4\t");
        new_err("position   moves\t");
        new_err("\tposition\t moves    e2e4 ");
    }

    #[test]
    fn test_parse_num() {
        let res = parse_num::<u64>("3141");
        assert_eq!(res.unwrap().1, 3141);

        let res = parse_num::<u8>("0");
        assert_eq!(res.unwrap().1, 0);

        let res = parse_num::<u32>("-");
        assert!(res.is_err());

        #[cfg(feature = "clamp-negatives")]
        {
            let res = parse_num::<u32>("-42");
            assert_eq!(res.unwrap().1, 0);
        }

        #[cfg(not(feature = "clamp-negatives"))]
        {
            let res = parse_num::<u32>("-42");
            assert!(res.is_err());
        }
    }

    #[test]
    fn test_parse_time() {
        let res = parse_time("0");
        assert_eq!(res.unwrap().1, Duration::from_millis(0));

        #[cfg(feature = "clamp-negatives")]
        {
            let res = parse_time("-42");
            assert_eq!(res.unwrap().1, Duration::from_millis(0));
        }

        #[cfg(not(feature = "clamp-negatives"))]
        {
            let res = parse_time("-42");
            assert!(res.is_err());
        }
    }

    #[test]
    fn test_parse_go() {
        let cmd = new_cmd("go");
        assert_eq!(
            cmd,
            UciCommand::Go(UciSearchOptions {
                infinite: true,
                ..Default::default()
            })
        );

        let cmd = new_cmd("go   infinite");
        assert_eq!(
            cmd,
            UciCommand::Go(UciSearchOptions {
                infinite: true,
                ..Default::default()
            })
        );

        let cmd = new_cmd("go  wtime   4000 btime   500 winc   60 binc 7");
        assert_eq!(
            cmd,
            UciCommand::Go(UciSearchOptions {
                wtime: Some(Duration::from_millis(4000)),
                btime: Some(Duration::from_millis(500)),
                winc: Some(Duration::from_millis(60)),
                binc: Some(Duration::from_millis(7)),
                ..Default::default()
            })
        );

        let cmd = new_cmd("go  ponder   searchmoves   e2e4 \t  b6b7  nodes\t 42");
        assert_eq!(
            cmd,
            UciCommand::Go(UciSearchOptions {
                ponder: true,
                searchmoves: moves(&["e2e4", "b6b7"]),
                nodes: Some(42),
                ..Default::default()
            })
        );

        let cmd = new_cmd("go  searchmoves   e2e4  b6b7 \tponder\t\t wtime  10   btime 20\t winc 30     binc 40   movestogo   5 depth 6 nodes 7 mate  8 movetime 90  infinite  ");
        assert_eq!(
            cmd,
            UciCommand::Go(UciSearchOptions {
                searchmoves: moves(&["e2e4", "b6b7"]),
                ponder: true,
                wtime: Some(Duration::from_millis(10)),
                btime: Some(Duration::from_millis(20)),
                winc: Some(Duration::from_millis(30)),
                binc: Some(Duration::from_millis(40)),
                movestogo: Some(5),
                depth: Some(6),
                nodes: Some(7),
                mate: Some(8),
                movetime: Some(Duration::from_millis(90)),
                infinite: true,
                #[cfg(feature = "parse-go-perft")]
                perft: None,
            })
        );

        #[cfg(feature = "err-on-unused-input")]
        new_err("  go   joho  ");

        new_err("  go   movetime    ");

        new_err("  go   movestogo    ");

        new_err("  go   movestogo   mate ");
    }

    #[test]
    fn test_parse_errors() {
        let unknown = "shutdown".parse::<UciCommand>();
        assert!(matches!(
            unknown.unwrap_err(),
            UciParseError::UnrecognizedCommand { cmd: _ }
        ));

        let invalid = "position default".parse::<UciCommand>();
        assert!(matches!(
            invalid.unwrap_err(),
            UciParseError::InvalidArgument { cmd: _, arg: _ }
        ));

        let insufficient = "setoption".parse::<UciCommand>();
        assert!(matches!(
            insufficient.unwrap_err(),
            UciParseError::InsufficientArguments { cmd: _ }
        ));
    }

    #[test]
    #[cfg(feature = "types")]
    fn test_parse_uci_move() {
        use crate::*;
        // Nullmove
        let input = "0000";
        let res = uci_move(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap().1,
            UciMove {
                src: Square(File::A, Rank::One),
                dst: Square(File::A, Rank::One),
                promote: None
            }
        );

        // Standard, easy-to-parse move
        let input = "e2e4";
        let res = uci_move(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap().1,
            UciMove {
                src: Square(File::E, Rank::Two),
                dst: Square(File::E, Rank::Four),
                promote: None
            }
        );

        // Promotion
        let input = "b7b8q";
        let res = uci_move(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap().1,
            UciMove {
                src: Square(File::B, Rank::Seven),
                dst: Square(File::B, Rank::Eight),
                promote: Some(Piece::Queen)
            }
        );

        // Invalid promotion, but still proper UCI notation/grammar
        #[cfg(not(feature = "validate-promotion-moves"))]
        {
            let input = "e2e4k";
            let res = uci_move(input);
            assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
            assert_eq!(
                res.unwrap().1,
                UciMove {
                    src: Square(File::E, Rank::Two),
                    dst: Square(File::E, Rank::Four),
                    promote: Some(Piece::King)
                }
            );
        }

        // Invalid promotion
        #[cfg(feature = "validate-promotion-moves")]
        {
            let input = "e2e4k";
            let res = uci_move(input);
            assert!(res.is_err());
        }
    }
}
