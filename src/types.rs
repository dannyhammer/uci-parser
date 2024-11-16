/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{fmt, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::one_of,
    combinator::{map, opt, value},
    sequence::{pair, tuple},
    IResult,
};

#[cfg(feature = "validate-promotion-moves")]
use nom::combinator::verify;

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

impl FromStr for Piece {
    type Err = String;
    /// Attempts to construct a [`Piece`] from a case-insensitive string the UCI notation of `[pnbrqk]`.
    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        piece(s).map(|x| x.1).map_err(|e| e.to_string())
    }
}

impl fmt::Display for Piece {
    /// Formats this piece in UCI notation of `[pnbrqk]` (lowercase).
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Piece::*;
        let p = match self {
            Pawn => 'p',
            Knight => 'n',
            Bishop => 'b',
            Rook => 'r',
            Queen => 'q',
            King => 'k',
        };
        write!(f, "{p}")
    }
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

impl FromStr for File {
    type Err = String;
    /// Attempts to construct a [`Rank`] from a string in UCI notation.
    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        file(s).map(|x| x.1).map_err(|e| e.to_string())
    }
}

impl fmt::Display for File {
    /// Formats this file in UCI notation of `[a-h]`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", (*self as u8 + b'a') as char)
    }
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

impl FromStr for Rank {
    type Err = String;
    /// Attempts to construct a [`Rank`] from a string in UCI notation.
    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        rank(s).map(|x| x.1).map_err(|e| e.to_string())
    }
}

impl fmt::Display for Rank {
    /// Formats this rank in UCI notation of `[1-8]`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", (*self as u8 + b'1') as char)
    }
}

/// One of the 64 squares on a chessboard.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Square(pub File, pub Rank);

impl FromStr for Square {
    type Err = String;
    /// Attempts to construct a [`Square`] from a string in UCI notation.
    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        square(s).map(|x| x.1).map_err(|e| e.to_string())
    }
}

impl fmt::Display for Square {
    /// Formats this square in UCI notation of `<file><rank>`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

/// A move of a piece on a chessboard.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UciMove {
    /// Start (origin) square of this move.
    pub src: Square,

    /// End (target) square of this move.
    pub dst: Square,

    /// [`Piece`] to promote to, if supplied.
    pub promote: Option<Piece>,
}

impl FromStr for UciMove {
    type Err = String;
    /// Attempts to construct a [`UciMove`] from a string in UCI notation.
    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        uci_move(s).map(|x| x.1).map_err(|e| e.to_string())
    }
}

impl fmt::Display for UciMove {
    /// Formats this move in UCI notation of `<src><dst>[promote]`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(p) = self.promote {
            write!(f, "{}{}{p}", self.src, self.dst)
        } else {
            write!(f, "{}{}", self.src, self.dst)
        }
    }
}

/// Parses a case-insensitive `P` to a [`Piece::Pawn`].
#[inline(always)]
fn pawn(input: &str) -> IResult<&str, Piece> {
    value(Piece::Pawn, one_of("Pp"))(input)
}

/// Parses a case-insensitive `K` to a [`Piece::Knight`].
#[inline(always)]
fn knight(input: &str) -> IResult<&str, Piece> {
    value(Piece::Knight, one_of("Nn"))(input)
}

/// Parses a case-insensitive `B` to a [`Piece::Bishop`].
#[inline(always)]
fn bishop(input: &str) -> IResult<&str, Piece> {
    value(Piece::Bishop, one_of("Bb"))(input)
}

/// Parses a case-insensitive `R` to a [`Piece::Rook`].
#[inline(always)]
fn rook(input: &str) -> IResult<&str, Piece> {
    value(Piece::Rook, one_of("Rr"))(input)
}

/// Parses a case-insensitive `Q` to a [`Piece::Queen`].
#[inline(always)]
fn queen(input: &str) -> IResult<&str, Piece> {
    value(Piece::Queen, one_of("Qq"))(input)
}

/// Parses a case-insensitive `K` to a [`Piece::King`].
#[inline(always)]
fn king(input: &str) -> IResult<&str, Piece> {
    value(Piece::King, one_of("Kk"))(input)
}

/// Parses a case-insensitive character to a [`Piece`] through UCI notation.
fn piece(input: &str) -> IResult<&str, Piece> {
    alt((pawn, knight, bishop, rook, queen, king))(input)
}

/// Parses a file on a chessboard, which is a case-insensitive character between `a` and `h` (inclusive).
fn file(input: &str) -> IResult<&str, File> {
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

/// Parses a rank on a chessboard, which is a numerical character between `1` and `8` (inclusive).
fn rank(input: &str) -> IResult<&str, Rank> {
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

/// Parses a square on a chessboard in UCI file-rank notation.
#[inline(always)]
fn square(input: &str) -> IResult<&str, Square> {
    map(pair(file, rank), |(f, r)| Square(f, r))(input)
}

/// Parses a UCI nullmove, represented by `0000`.
#[inline(always)]
fn nullmove(input: &str) -> IResult<&str, UciMove> {
    value(UciMove::default(), tag("0000"))(input)
}

/// Parses a UCI move, which is in the format `<start square><end square>[promotion]`.
///
/// This can also parse a nullmove, which is the string `0000`.
///
/// If the feature `validate-promotion-moves` is enabled, this function will fail to
/// parse "invalid" moves such as `e2e4q` or `b7b8p`.
pub(crate) fn uci_move(input: &str) -> IResult<&str, UciMove> {
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
        alt((
            map(
                alt((non_promotion, promotion_rank_1, promotion_rank_8)),
                |(src, dst, promote)| UciMove { src, dst, promote },
            ),
            nullmove,
        ))(input)
    }

    #[cfg(not(feature = "validate-promotion-moves"))]
    alt((
        map(
            tuple((square, square, opt(piece))),
            |(src, dst, promote)| UciMove { src, dst, promote },
        ),
        nullmove,
    ))(input)
}
