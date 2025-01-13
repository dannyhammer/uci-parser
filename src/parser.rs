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
    combinator::{cut, eof, map, map_res, opt, recognize, value, verify},
    error::Error,
    multi::{many0, many0_count, many1_count, many_till},
    sequence::{delimited, pair, preceded, tuple},
    Err, IResult,
};

use crate::{
    UciBound, UciCheckingStatus, UciCommand, UciInfo, UciOption, UciOptionType, UciParseError,
    UciResponse, UciScore, UciScoreType, UciSearchOptions,
};

#[cfg(feature = "types")]
use crate::{uci_move, UciMove};

/// Top-level parser to convert a string into a [`UciCommand`].
///
/// See also [`UciCommand::new`]
pub(crate) fn parse_uci_command(input: &str) -> Result<UciCommand, UciParseError> {
    parse_uci_line(input, parse_command)
}

/// Top-level parser to convert a string into a [`UciResponse`].
///
/// See also [`UciResponse::new`]
pub(crate) fn parse_uci_response(input: &str) -> Result<UciResponse, UciParseError> {
    parse_uci_line(input, parse_response)
}

fn parse_uci_line<T, F>(input: &str, f: F) -> Result<T, UciParseError>
where
    F: FnMut(&str) -> IResult<&str, T>,
{
    // The `many_till(anychar)` parser consumes any unknown characters until a command is parsed.
    #[allow(unused_mut)]
    let mut parser = map(many_till(anychar, f), |(_, cmd)| cmd);

    #[cfg(feature = "err-on-unused-input")]
    let mut parser = nom::combinator::all_consuming(parser);

    parser(input)
        .map(|(_rest, cmd)| cmd)
        .map_err(|e| create_error(e, input))
}

fn create_error(err: Err<Error<&str>>, input: &str) -> UciParseError {
    match err {
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
    }
}

/// Parses a single-word command like `uci` or `isready`.
fn single_command<'a, T>(ident: &'static str, cmd: T) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    T: Clone,
{
    value(cmd, term(ident))
}

/// Parses a multi-word command like `go` or `setoption`.
fn multi_command<'a, T, F>(
    ident: &'static str,
    parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    T: Clone,
    F: FnMut(&'a str) -> IResult<&'a str, T>,
{
    preceded(term(ident), cut(parser))
}

/// Parses a single [`UciCommand`]
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

/// Parses a single [`UciResponse`]
fn parse_response(input: &str) -> IResult<&str, UciResponse> {
    alt((
        multi_command("id", parse_id_args),
        single_command("uciok", UciResponse::UciOk),
        single_command("readyok", UciResponse::ReadyOk),
        multi_command("bestmove", parse_bestmove_args),
        multi_command("copyprotection", parse_copyprotection_args),
        multi_command("registration", parse_registration_args),
        multi_command("info", parse_info_args),
        multi_command("option", parse_option_args),
    ))(input)
}

/// Parses arguments to the `id` response.
fn parse_id_args(input: &str) -> IResult<&str, UciResponse> {
    let name = rest_after("name");
    let author = rest_after("author");

    alt((
        map(name, |name| UciResponse::Name(name.to_string())),
        map(author, |author| UciResponse::Author(author.to_string())),
    ))(input)
}

/// Parses arguments to the `bestmove` response.
fn parse_bestmove_args(input: &str) -> IResult<&str, UciResponse> {
    let bestmove = alt((value(None, term("(none)")), map(recognize_uci_move, Some)));
    let ponder = opt(multi_command("ponder", recognize_uci_move));

    map(pair(bestmove, ponder), |(bestmove, ponder)| {
        UciResponse::BestMove {
            bestmove: bestmove.map(str::to_string),
            ponder: ponder.map(str::to_string),
        }
    })(input)
}

/// Parses arguments to the `copyprotection` response.
fn parse_copyprotection_args(input: &str) -> IResult<&str, UciResponse> {
    map(parse_uci_checking_status, UciResponse::CopyProtection)(input)
}

/// Parses arguments to the `registration` response.
fn parse_registration_args(input: &str) -> IResult<&str, UciResponse> {
    map(parse_uci_checking_status, UciResponse::Registration)(input)
}

/// Parses [`UciCheckingStatus`], which is used in both the `copyprotection` and `registration` responses
fn parse_uci_checking_status(input: &str) -> IResult<&str, UciCheckingStatus> {
    alt((
        single_command("checking", UciCheckingStatus::Checking),
        single_command("ok", UciCheckingStatus::Ok),
        single_command("error", UciCheckingStatus::Error),
    ))(input)
}

/// Parses arguments to the `info` response.
fn parse_info_args(input: &str) -> IResult<&str, UciResponse> {
    let mut info = UciInfo::default();

    let (input, _) = many0_count(alt((
        map(num_after("depth"), |x| info.depth = Some(x)),
        map(num_after("seldepth"), |x| info.seldepth = Some(x)),
        map(time_after("time"), |x| {
            info.time = Some(x.as_millis().to_string())
        }),
        map(num_after("nodes"), |x| info.nodes = Some(x)),
        map(recognize_moves_after("pv"), |x| {
            info.pv = x.into_iter().map(str::to_string).collect()
        }),
        map(num_after("multipv"), |x| info.multipv = Some(x)),
        map(multi_command("score", parse_score_args), |x| {
            info.score = Some(x)
        }),
        map(multi_command("currmove", recognize_uci_move), |x| {
            info.currmove = Some(x.to_string())
        }),
        map(num_after("currmovenumber"), |x| {
            info.currmovenumber = Some(x)
        }),
        map(num_after("hashfull"), |x| info.hashfull = Some(x)),
        map(num_after("nps"), |x| info.nps = Some(x)),
        map(num_after("tbhits"), |x| info.tbhits = Some(x)),
        map(num_after("sbhits"), |x| info.sbhits = Some(x)),
        map(num_after("cpuload"), |x| info.cpuload = Some(x)),
        map(rest_after("string"), |x| info.string = Some(x.to_string())),
        map(recognize_moves_after("refutation"), |x| {
            info.refutation = x.into_iter().map(str::to_string).collect()
        }),
        map(
            multi_command("currline", pair(opt(parse_num::<String>), recognize_moves)),
            |(cpunr, moves)| {
                info.currline = cpunr
                    .into_iter()
                    .chain(moves.into_iter().map(str::to_string))
                    .collect();
            },
        ),
    )))(input)?;

    Ok((input, UciResponse::Info(Box::new(info))))
}

/// Parses arguments to `score` inside an `info` response
fn parse_score_args(input: &str) -> IResult<&str, UciScore> {
    let score_type = alt((
        value(UciScoreType::Centipawns, term("cp")),
        value(UciScoreType::Mate, term("mate")),
    ));
    let bound = opt(alt((
        value(UciBound::Lowerbound, term("lowerbound")),
        value(UciBound::Upperbound, term("upperbound")),
    )));

    map(
        tuple((score_type, parse_signed_num, bound)),
        |(score_type, score, bound)| UciScore {
            score,
            score_type,
            bound,
        },
    )(input)
}

/// Parses arguments to the `option` response.
fn parse_option_args(input: &str) -> IResult<&str, UciResponse> {
    let name = between("name", "type");
    let opt_type = preceded(
        term("type"),
        alt((
            multi_command("check", parse_option_check_args),
            multi_command("spin", parse_option_spin_args),
            multi_command("combo", parse_option_combo_args),
            single_command("button", UciOptionType::Button),
            multi_command("string", parse_option_string_args),
        )),
    );

    map(pair(name, opt_type), |(name, opt_type)| {
        UciResponse::Option(UciOption {
            name: name.to_string(),
            opt_type,
        })
    })(input)
}

fn default<'a, T, F>(parser: F) -> impl FnMut(&'a str) -> IResult<&'a str, T>
where
    F: FnMut(&'a str) -> IResult<&'a str, T>,
{
    preceded(term("default"), parser)
}

/// Parses arguments to an `option` response with type `check`.
fn parse_option_check_args(input: &str) -> IResult<&str, UciOptionType> {
    map(default(parse_bool), |default| UciOptionType::Check {
        default,
    })(input)
}

/// Parses arguments to an `option` response with type `spin`.
fn parse_option_spin_args(input: &str) -> IResult<&str, UciOptionType> {
    map(
        tuple((
            default(parse_signed_num),
            preceded(term("min"), parse_signed_num),
            preceded(term("max"), parse_signed_num),
        )),
        |(default, min, max)| UciOptionType::Spin { default, min, max },
    )(input)
}

/// Parses arguments to an `option` response with type `combo`.
fn parse_option_combo_args(input: &str) -> IResult<&str, UciOptionType> {
    map(
        pair(
            between("default", "var"),
            many0(map(between("var", "var"), str::to_string)),
        ),
        |(default, vars)| UciOptionType::Combo {
            default: default.to_string(),
            vars,
        },
    )(input)
}

/// Parses arguments to an `option` response with type `string`.
fn parse_option_string_args(input: &str) -> IResult<&str, UciOptionType> {
    map(default(rest_nonempty), |default| UciOptionType::String {
        default: default.to_string(),
    })(input)
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

/// Parses a non-empty list of moves after `ident` into `&str`s
fn recognize_moves_after<'a>(
    ident: &'a str,
) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<&'a str>> {
    preceded(term(ident), cut(recognize_moves))
}

/// Parses a non-empty list of moves after `ident`
#[cfg(not(feature = "types"))]
fn moves_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<&'a str>> {
    recognize_moves_after(ident)
}

/// Parses a non-empty list of moves after `ident`
#[cfg(feature = "types")]
fn moves_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, Vec<UciMove>> {
    preceded(term(ident), cut(moves))
}

/// Parses a non-empty list of moves into `&str`s
fn recognize_moves<'a>(input: &str) -> IResult<&str, Vec<&str>> {
    nom::multi::many0(delimited(multispace0, recognize_uci_move, multispace0))(input)
}

/// Parses a non-empty list of moves
#[cfg(feature = "types")]
fn moves<'a>(input: &str) -> IResult<&str, Vec<UciMove>> {
    nom::multi::many1(delimited(multispace0, uci_move, multispace0))(input)
}

/// Parses everything after `fen` as a FEN until either the keyword `moves` or the end of the line.
///
/// Fails if there is no non-whitespace input following `fen`.
fn parse_fen(input: &str) -> IResult<&str, &str> {
    between("fen", "moves")(input)
}

/// Parses and maps a positive base-10 number
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

/// Parses and maps a signed base-10 number
fn parse_signed_num<T>(input: &str) -> IResult<&str, T>
where
    T: FromStr + Default + Clone,
{
    map_res(recognize(pair(opt(one_of("+-")), digit1)), str::parse)(input)
}

/// Parses a [`bool`]
fn parse_bool(input: &str) -> IResult<&str, bool> {
    alt((value(false, term("false")), value(true, term("true"))))(input)
}

/// Parses and maps a [`Duration`]
fn parse_time(input: &str) -> IResult<&str, Duration> {
    map(parse_num, Duration::from_millis)(input)
}

/// Parses and maps a [`Duration`] following `ident`
fn time_after<'a>(ident: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, Duration> {
    preceded(term(ident), parse_time)
}

/// Recognizes a UCI move.
///
/// This is the default parser with the `types` feature disabled, but it's also used
/// in [`UciResponse`]'s [`FromStr`] implementation, see [#1](https://github.com/dannyhammer/uci-parser/issues/1#issuecomment-2585831960)
fn recognize_uci_move(input: &str) -> IResult<&str, &str> {
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
    use std::fmt::Debug;

    use crate::UciOption;

    use super::*;

    /// Parses using [`FromStr`] and asserts that the result is `Ok`.
    fn new_from_str<T>(input: &str) -> T
    where
        T: FromStr + Debug,
        T::Err: Debug,
    {
        let cmd = T::from_str(input);
        assert!(
            cmd.is_ok(),
            "Failed to parse {input:?}\nGot {:?}",
            cmd.unwrap_err()
        );
        cmd.unwrap()
    }

    /// Creates a new command and asserts that it is `Ok`.
    fn new_cmd(input: &str) -> UciCommand {
        new_from_str(input)
    }

    /// Creates a new response and asserts that it is `Ok`.
    fn new_response(input: &str) -> UciResponse {
        new_from_str(input)
    }

    /// Parses using [`FromStr`] and asserts that the result is `Err`.
    fn new_err_from_str<T>(input: &str)
    where
        T: FromStr + Debug,
        T::Err: Debug,
    {
        let cmd = T::from_str(input);
        assert!(cmd.is_err(), "Should error from {input:?}\nGot {cmd:?}");
    }

    /// Creates a new command and asserts that it is `Err`.
    fn new_err(input: &str) {
        new_err_from_str::<UciCommand>(input)
    }

    /// Creates a new response and asserts that it is `Err`.
    fn new_response_err(input: &str) {
        new_err_from_str::<UciResponse>(input)
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
    fn test_parse_single_responses() {
        let cmd = new_response(" \t\n uciok ");
        assert_eq!(cmd, UciResponse::UciOk);

        let cmd = new_response("    readyok ");
        assert_eq!(cmd, UciResponse::ReadyOk);
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
    fn test_parse_id() {
        let res = new_response("id name toad 3.0.0");
        assert_eq!(res, UciResponse::Name("toad 3.0.0".to_string()));

        let res = new_response("id author Danny Hammer <hammerapi@gmail.com>");
        assert_eq!(
            res,
            UciResponse::Author("Danny Hammer <hammerapi@gmail.com>".to_string())
        );

        new_response_err("idname nope");
        new_response_err("id name");
    }

    #[test]
    fn test_parse_bestmove() {
        fn test_bestmove(input: &str, bestmove: Option<String>, ponder: Option<String>) {
            let res = new_response(input);
            assert_eq!(res, UciResponse::BestMove { bestmove, ponder })
        }

        test_bestmove("bestmove e2e4", Some("e2e4".to_string()), None);
        test_bestmove("bestmove (none)", None, None);
        test_bestmove(
            "bestmove e2e4 ponder d2d4",
            Some("e2e4".to_string()),
            Some("d2d4".to_string()),
        );
        test_bestmove(
            "bestmove (none) ponder d2d4",
            None,
            Some("d2d4".to_string()),
        );

        new_response_err("bestmove e2e4 ponder");
        new_response_err("bestmove ponder d2d4");
    }

    #[test]
    fn test_parse_copyprotection_registration() {
        fn test_parse_uci_checking_status<F>(command: &str, mut constructor: F)
        where
            F: FnMut(UciCheckingStatus) -> UciResponse,
        {
            let res = new_response(&format!("{command} checking"));
            assert_eq!(res, constructor(UciCheckingStatus::Checking));
            let res = new_response(&format!("{command} ok"));
            assert_eq!(res, constructor(UciCheckingStatus::Ok));
            let res = new_response(&format!("{command} error"));
            assert_eq!(res, constructor(UciCheckingStatus::Error));
        }

        test_parse_uci_checking_status("copyprotection", UciResponse::CopyProtection);
        test_parse_uci_checking_status("registration", UciResponse::Registration);
    }

    #[test]
    fn test_parse_info() {
        let res = new_response("info currmove e2e4 currmovenumber 1");
        assert_eq!(
            res,
            UciResponse::info(UciInfo::new().currmove("e2e4").currmovenumber(1))
        );

        let res = new_response("info depth 12 nodes 123456 nps 100000");
        assert_eq!(
            res,
            UciResponse::info(UciInfo::new().depth(12).nodes(123456).nps(100000)),
        );

        let res = new_response(
            "info depth 2 score cp 214 time 1242 nodes 2124 nps 34928 pv e2e4 e7e5 g1f3",
        );
        assert_eq!(
            res,
            UciResponse::info(
                UciInfo::new()
                    .depth(2)
                    .score(UciScore::cp(214))
                    .time(1242)
                    .nodes(2124)
                    .nps(34928)
                    .pv(["e2e4", "e7e5", "g1f3"])
            )
        );

        let res = new_response("info refutation d1h5 g6h5");
        assert_eq!(
            res,
            UciResponse::info(UciInfo::new().refutation(["d1h5", "g6h5"])),
        );

        let res = new_response("info refutation d1h5");
        assert_eq!(res, UciResponse::info(UciInfo::new().refutation(["d1h5"])));

        let res = new_response("info depth 1 seldepth 0");
        assert_eq!(res, UciResponse::info(UciInfo::new().depth(1).seldepth(0)));

        let res = new_response("info score cp 13  depth 1 nodes 13 time 15 pv f1b5");
        assert_eq!(
            res,
            UciResponse::info(
                UciInfo::new()
                    .score(UciScore::cp(13))
                    .depth(1)
                    .nodes(13)
                    .time(15)
                    .pv(["f1b5"])
            )
        );

        let res = new_response("info string I see you");
        assert_eq!(res, UciResponse::info_string("I see you".to_string()));
    }

    #[test]
    fn test_parse_option() {
        let res = new_response("option name Nullmove type check default true");
        assert_eq!(res, UciResponse::Option(UciOption::check("Nullmove", true)));

        let res = new_response("option name Selectivity type spin default 2 min 0 max 4");
        assert_eq!(
            res,
            UciResponse::Option(UciOption::spin("Selectivity", 2, 0, 4))
        );

        let res = new_response(
            "option name Style type combo default Normal var Solid var Normal var Risky",
        );
        assert_eq!(
            res,
            UciResponse::Option(UciOption::combo(
                "Style",
                "Normal",
                ["Solid", "Normal", "Risky"]
            ))
        );

        let res = new_response(r"option name NalimovPath type string default c:\");
        assert_eq!(
            res,
            UciResponse::Option(UciOption::string("NalimovPath", r"c:\"))
        );

        let res = new_response("option name Clear Hash type button");
        assert_eq!(res, UciResponse::Option(UciOption::button("Clear Hash")));

        let res = new_response(
            "option name Style type combo default Normal var Not Normal var Normal var Very Normal",
        );
        assert_eq!(
            res,
            UciResponse::Option(UciOption::combo(
                "Style",
                "Normal",
                ["Not Normal", "Normal", "Very Normal"]
            ))
        );

        new_response_err("option name type button");
        new_response_err("option name Incomplete");
        new_response_err("option name Missing Values type spin");
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
