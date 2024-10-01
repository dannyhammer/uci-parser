/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{str::FromStr, time::Duration};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{anychar, digit1, multispace0, multispace1, one_of},
    combinator::{cut, eof, map, map_res, opt, recognize, rest, value, verify},
    multi::{many0_count, many1, many1_count, many_till},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

use crate::{UciCommand, UciSearchOptions};

/// Top-level parser to convert a string into a [`UciCommand`].
///
/// This will return an error if the `input` is not completely consumed during parsing.
///
/// See also [`UciCommand::new`]
pub fn parse_uci_command(input: &str) -> IResult<&str, UciCommand> {
    #[cfg(feature = "err-on-unused-input")]
    {
        nom::combinator::all_consuming(map(many_till(anychar, parse_command), |(_, cmd)| cmd))(
            input,
        )
    }

    #[cfg(not(feature = "err-on-unused-input"))]
    map(many_till(anychar, parse_command), |(_, cmd)| cmd)(input)
}

/// Parses a single UCI command
fn parse_command(input: &str) -> IResult<&str, UciCommand> {
    alt((
        value(UciCommand::Uci, term("uci")),
        preceded(term("debug"), parse_debug_args),
        value(UciCommand::IsReady, term("isready")),
        preceded(term("setoption"), parse_setoption_args),
        preceded(term("register"), parse_register_args),
        value(UciCommand::UciNewGame, term("ucinewgame")),
        preceded(term("position"), parse_position_args),
        preceded(term("go"), parse_go_args),
        value(UciCommand::Stop, term("stop")),
        value(UciCommand::PonderHit, term("ponderhit")),
        value(UciCommand::Quit, term("quit")),
        #[cfg(feature = "parse-bench")]
        preceded(term("bench"), parse_bench_args),
        // consumed(term),
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
    let name = rest_after_until("name", "value");
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

    let name = rest_after_until("name", "code");
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
        map(parse_fen, |s| Some(s.to_string())),
        value(None, term("startpos")),
    ));

    // Optional list of moves
    let moves = map(opt(moves_after("moves")), Option::unwrap_or_default);

    map(pair(fen, moves), |(fen, moves)| UciCommand::Position {
        fen,
        moves: moves.into_iter().map(str::to_string).collect(),
    })(input)
}

/// Parses all arguments to the `go` command.
fn parse_go_args(input: &str) -> IResult<&str, UciCommand> {
    map(parse_search_options, UciCommand::Go)(input)
}

/// Parses individual arguments to the `go` command into a [`UsiSearchOptions`] struct.
///
/// This is in it's own function because [`UciSearchOptions`] is used by both `go` and `bench`.
fn parse_search_options<'a>(input: &'a str) -> IResult<&'a str, UciSearchOptions> {
    let mut opt = UciSearchOptions::default();

    // Arguments to `go` can be in any order
    let (input, count) = many0_count(alt((
        map(moves_after("searchmoves"), |x| {
            opt.searchmoves = x.into_iter().map(str::to_string).collect()
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

/// A combinator that takes an identifier `ident` and produces a parser that:
///     - Consumes all leading whitespace
///     - Consumes either:
///         - All but 1 trailing whitespace
///         - EOF
///     - Returns everything after `ident` in the input
fn term<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    delimited(multispace0, tag(ident), alt((eof, multispace1)))
}

/// A parser to consume the remainder of `input`, erroring with a [`nom::Err::Failure`] if there is no remaining input.
fn rest_nonempty<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    recognize(cut(many1_count(anychar)))(input)
}

/// A combinator that parses everything after `ident` until `end` is found, returning everything in between.
///
/// This fails if there is no input between `ident` and `end` to consume.
fn rest_after_until<'a>(
    ident: &'a str,
    end: &'a str,
) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    preceded(
        term(ident),
        map(alt((take_until(end), rest_nonempty)), str::trim),
    )
}

/// A combinator that takes an identifier `ident` and produces a parser that discards `ident`
/// and consumes the rest of the input, failing if there is no more input to consume.
fn rest_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str> {
    preceded(term(ident), map(rest_nonempty, str::trim))
}

/// Parses a non-empty list of moves after `ident`
fn moves_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<&'a str>> {
    preceded(
        term(ident),
        many1(delimited(multispace0, recognize(uci_move), multispace0)),
    )
}

/// Parses everything after `fen` as a FEN until either the keyword `moves` or the end of the line
fn parse_fen(input: &str) -> IResult<&str, &str> {
    map(
        preceded(term("fen"), alt((take_until("moves"), rest))),
        str::trim,
    )(input)
}
/// Parses and maps a base-10 number
///
/// Negative numbers are handled by the `clamp-negatives` crate feature.
pub fn parse_num<T>(input: &str) -> IResult<&str, T>
where
    T: FromStr + Default + Clone,
{
    #[cfg(feature = "clamp-negatives")]
    {
        alt((
            map_res(digit1, str::parse), // First try a normal, positive number
            preceded(tag("-"), value(T::default(), digit1)), // Then try negative, and clamp it
        ))(input)
    }

    #[cfg(not(feature = "clamp-negatives"))]
    map_res(digit1, str::parse)(input)
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

/// The six pieces of a chessboard.
///
/// Used as a result when parsing characters in UCI notation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

/// Parses a case-insensitive `P` to a [`Piece::Pawn`].
pub fn pawn(input: &str) -> IResult<&str, Piece> {
    value(Piece::Pawn, one_of("Pp"))(input)
}

/// Parses a case-insensitive `K` to a [`Piece::Knight`].
pub fn knight(input: &str) -> IResult<&str, Piece> {
    value(Piece::Knight, one_of("Nn"))(input)
}

/// Parses a case-insensitive `B` to a [`Piece::Bishop`].
pub fn bishop(input: &str) -> IResult<&str, Piece> {
    value(Piece::Bishop, one_of("Bb"))(input)
}

/// Parses a case-insensitive `R` to a [`Piece::Rook`].
pub fn rook(input: &str) -> IResult<&str, Piece> {
    value(Piece::Rook, one_of("Rr"))(input)
}

/// Parses a case-insensitive `Q` to a [`Piece::Queen`].
pub fn queen(input: &str) -> IResult<&str, Piece> {
    value(Piece::Queen, one_of("Qq"))(input)
}

/// Parses a case-insensitive `K` to a [`Piece::King`].
pub fn king(input: &str) -> IResult<&str, Piece> {
    value(Piece::King, one_of("Kk"))(input)
}

/// Parses a case-insensitive character to a [`Piece`] through UCI notation.
pub fn piece(input: &str) -> IResult<&str, Piece> {
    alt((pawn, knight, bishop, rook, queen, king))(input)
}

/// The eight files/columns on a chessboard.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum File {
    #[default]
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

/// Parses a file on a chessboard, which is a case-insensitive character between `a` and `h` (inclusive).
pub fn file(input: &str) -> IResult<&str, File> {
    map(one_of("abcdefghABCDEFGH"), |c| {
        match c.to_ascii_lowercase() {
            'a' => File::A,
            'b' => File::B,
            'c' => File::C,
            'd' => File::D,
            'e' => File::E,
            'f' => File::F,
            'g' => File::G,
            'h' => File::H,
            _ => unreachable!("The char {c:?} is not a valid chess File."),
        }
    })(input)
}

/// The eight ranks/rows on a chessboard.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Rank {
    #[default]
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

/// Parses a rank on a chessboard, which is a numerical character between `1` and `8` (inclusive).
pub fn rank(input: &str) -> IResult<&str, Rank> {
    map(one_of("12345678"), |c| match c {
        '1' => Rank::One,
        '2' => Rank::Two,
        '3' => Rank::Three,
        '4' => Rank::Four,
        '5' => Rank::Five,
        '6' => Rank::Six,
        '7' => Rank::Seven,
        '8' => Rank::Eight,
        _ => unreachable!("The char {c:?} is not a valid chess Rank."),
    })(input)
}

/// One of the 64 squares on a chessboard.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Square(pub File, pub Rank);

/// Parses a square on a chessboard in UCI file-rank notation.
pub fn square(input: &str) -> IResult<&str, Square> {
    map(pair(file, rank), |(f, r)| Square(f, r))(input)
}

/// Parses a UCI nullmove, represented by `0000`.
pub fn nullmove(input: &str) -> IResult<&str, (Square, Square, Option<Piece>)> {
    value((Square::default(), Square::default(), None), term("0000"))(input)
}

/// Parses a UCI move, which is in the format `<start square><end square>[promotion]`.
///
/// Returns the indices of the start square, indices of the end square, and promotion
/// (if applicable).
///
/// This can also parse a nullmove, which is the string `0000`.
///
/// If the feature `validate-promotion-moves` is enabled, this function will fail to
/// parse "invalid" moves such as `e2e4q` or `b7b8p`.
pub fn uci_move(input: &str) -> IResult<&str, (Square, Square, Option<Piece>)> {
    #[cfg(feature = "validate-promotion-moves")]
    {
        // Promotion on White's home rank (1)
        let promotion_rank_1 = tuple((
            verify(square, |s| s.1 == Rank::Two),
            verify(square, |s| s.1 == Rank::One),
            opt(alt((queen, knight, rook, bishop))),
        ));

        // Promotion on Black's home rank (8)
        let promotion_rank_8 = tuple((
            verify(square, |s| s.1 == Rank::Seven),
            verify(square, |s| s.1 == Rank::Eight),
            opt(alt((queen, knight, rook, bishop))),
        ));

        // Explicitly not a promotion move
        let non_promotion = map(tuple((square, square, nom::combinator::not(piece))), |mv| {
            (mv.0, mv.1, None)
        });

        // Either a non-promotion, promotion on rank 1 or 8, or a nullmove
        alt((non_promotion, promotion_rank_1, promotion_rank_8, nullmove))(input)
    }

    #[cfg(not(feature = "validate-promotion-moves"))]
    alt((tuple((square, square, opt(piece))), nullmove))(input)
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

    #[test]
    fn test_parse_unknown_word() {
        let res = new_cmd("joho debug on");
        assert_eq!(res, UciCommand::Debug(true));

        new_err("joho");

        new_err("debug joho on");

        #[cfg(feature = "err-on-unused-input")]
        new_err("debug on joho");

        #[cfg(not(feature = "err-on-unused-input"))]
        {
            let res = new_cmd("debug on jobo");
            assert_eq!(res, UciCommand::Debug(true));
        }
    }
    #[test]
    fn test_parse_uci() {
        let cmd = new_cmd("uci");
        assert_eq!(cmd, UciCommand::Uci);

        let cmd = new_cmd("\nuci\t");
        assert_eq!(cmd, UciCommand::Uci);
    }

    #[test]
    fn test_parse_debug() {
        let res = UciCommand::new("    debug     on  ");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), UciCommand::Debug(true));

        let res = UciCommand::new("    debug     off  ");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), UciCommand::Debug(false));

        let res = UciCommand::new("    debug     ");
        assert!(res.is_err());

        let res = UciCommand::new("    debug     no");
        assert!(res.is_err());
    }

    #[test]
    fn test_parse_isready() {
        let res = UciCommand::new("  isready  ");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), UciCommand::IsReady);
    }

    #[test]
    fn test_parse_setoption() {
        let res = UciCommand::new("setoption name Nullmove value true\t");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::SetOption {
                name: "Nullmove".into(),
                value: Some("true".into())
            }
        );

        let res = UciCommand::new("setoption     name   Selectivity    value   3  ");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::SetOption {
                name: "Selectivity".into(),
                value: Some("3".into())
            }
        );

        let res = UciCommand::new("setoption     name   Style    value   Risky  ");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::SetOption {
                name: "Style".into(),
                value: Some("Risky".into())
            }
        );

        let input = "setoption     name   Clear Hash  ";
        let res = UciCommand::new(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap(),
            UciCommand::SetOption {
                name: "Clear Hash".into(),
                value: None,
            }
        );

        let res = UciCommand::new(
            "setoption     name   NalimovPath    value   c:\\chess\\tb\\4;c:\\chess\\tb\\5".into(),
        );
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::SetOption {
                name: "NalimovPath".into(),
                value: Some("c:\\chess\\tb\\4;c:\\chess\\tb\\5".into())
            }
        );

        let input = "setoption";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");

        let input = "setoption   name";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");

        let input = "setoption   name  value";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");

        let input = "setoption   name  Clear Hash value";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");
    }

    #[test]
    fn test_parse_register() {
        let input = "register    later  ";
        let res = UciCommand::new(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap(),
            UciCommand::Register {
                name: None,
                code: None
            }
        );

        let input = "register    name Stefan MK   code\t\t4359874324   \t";
        let res = UciCommand::new(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap(),
            UciCommand::Register {
                name: Some("Stefan MK".into()),
                code: Some("4359874324".into())
            }
        );

        let input = "register    name Stefan MK \t";
        let res = UciCommand::new(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap(),
            UciCommand::Register {
                name: Some("Stefan MK".into()),
                code: None
            }
        );

        let input = "register    code\t\t4359874324   \t";
        let res = UciCommand::new(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap(),
            UciCommand::Register {
                name: None,
                code: Some("4359874324".into())
            }
        );

        let input = "register";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");

        let input = "register   name";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");

        let input = "register   name code";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");

        let input = "register   name  Stefan MK  code";
        let res = UciCommand::new(input);
        assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");
    }

    #[test]
    fn test_parse_ucinewgame() {
        let res = UciCommand::new("  ucinewgame  ");
        assert!(res.is_ok());
        assert_eq!(res.unwrap(), UciCommand::UciNewGame);
    }

    #[test]
    fn test_parse_position() {
        let res =
            parse_fen("fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap().1,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        );

        let res = parse_fen(
            "fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 moves e2a6",
        );
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap().1,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        );

        let res = moves_after("moves")("moves   e2a6 \th3g2   h1g1 e6d5");
        assert!(res.is_ok());
        assert_eq!(res.unwrap().1, vec!["e2a6", "h3g2", "h1g1", "e6d5"]);

        let res = UciCommand::new("position startpos");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::Position {
                fen: None,
                moves: vec![]
            }
        );

        let res = UciCommand::new("position   fen  r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1   moves  e2a6 \t h3g2  h1g1 e6d5");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::Position {
                fen: Some(
                    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1".into()
                ),
                moves: vec!["e2a6".into(), "h3g2".into(), "h1g1".into(), "e6d5".into()]
            }
        );
    }

    #[test]
    fn test_parse_num() {
        let res = parse_num::<u64>("3141");
        assert!(res.is_ok());
        assert_eq!(res.unwrap().1, 3141);

        let res = parse_num::<u8>("0");
        assert!(res.is_ok());
        assert_eq!(res.unwrap().1, 0);

        #[cfg(feature = "clamp-negatives")]
        {
            let res = parse_num::<u32>("-42");
            assert!(res.is_ok());
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
        assert!(res.is_ok());
        assert_eq!(res.unwrap().1, Duration::from_millis(0));

        #[cfg(feature = "clamp-negatives")]
        {
            let res = parse_time("-42");
            assert!(res.is_ok());
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
        let res = UciCommand::new("go");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::Go(UciSearchOptions {
                infinite: true,
                ..Default::default()
            })
        );

        let res = UciCommand::new("go   infinite");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::Go(UciSearchOptions {
                infinite: true,
                ..Default::default()
            })
        );

        let res = UciCommand::new("go  wtime   4000 btime   500 winc   60 binc 7");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::Go(UciSearchOptions {
                wtime: Some(Duration::from_millis(4000)),
                btime: Some(Duration::from_millis(500)),
                winc: Some(Duration::from_millis(60)),
                binc: Some(Duration::from_millis(7)),
                ..Default::default()
            })
        );

        let res = UciCommand::new("go  ponder   searchmoves   e2e4 \t  b6b7  nodes\t 42");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::Go(UciSearchOptions {
                ponder: true,
                searchmoves: vec!["e2e4".into(), "b6b7".into()],
                nodes: Some(42),
                ..Default::default()
            })
        );

        let res = UciCommand::new("go  searchmoves   e2e4  b6b7 \tponder\t\t wtime  10   btime 20\t winc 30     binc 40   movestogo   5 depth 6 nodes 7 mate  8 movetime 90  infinite  ");
        assert!(res.is_ok());
        assert_eq!(
            res.unwrap(),
            UciCommand::Go(UciSearchOptions {
                searchmoves: vec!["e2e4".into(), "b6b7".into()],
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
    }

    #[test]
    fn test_parse_uci_move() {
        // Nullmove
        let input = "0000";
        let res = uci_move(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap().1,
            (Square(File::A, Rank::One), Square(File::A, Rank::One), None)
        );

        // Standard, easy-to-parse move
        let input = "e2e4";
        let res = uci_move(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap().1,
            (
                Square(File::E, Rank::Two),
                Square(File::E, Rank::Four),
                None
            )
        );

        // Promotion
        let input = "b7b8q";
        let res = uci_move(input);
        assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
        assert_eq!(
            res.unwrap().1,
            (
                Square(File::B, Rank::Seven),
                Square(File::B, Rank::Eight),
                Some(Piece::Queen)
            )
        );

        // Invalid promotion, but still proper UCI notation/grammar
        #[cfg(not(feature = "validate-promotion-moves"))]
        {
            let input = "e2e4k";
            let res = uci_move(input);
            assert!(res.is_ok(), "Failed to parse {input:?}\nGot {res:?}");
            assert_eq!(
                res.unwrap().1,
                (
                    Square(File::E, Rank::Two),
                    Square(File::E, Rank::Four),
                    Some(Piece::King)
                )
            );
        }

        // Invalid promotion
        #[cfg(feature = "validate-promotion-moves")]
        {
            let input = "e2e4k";
            let res = uci_move(input);
            assert!(res.is_err(), "Should error from {input:?}\nGot {res:?}");
        }
    }
}
