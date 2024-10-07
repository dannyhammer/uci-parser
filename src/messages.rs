/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{fmt, str::FromStr, time::Duration};

use crate::{parse_uci_command, UciParseError};

#[cfg(feature = "types")]
use crate::UciMove;

/// A command sent from a GUI to an engine.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UciCommand {
    /// Check if the engine supports the UCI protocol.
    ///
    /// # Command structure:
    /// ```text
    /// uci
    /// ```
    ///
    /// # Protocol Description
    ///
    /// Tell engine to use the uci (universal chess interface).
    /// This will be sent once as a first command after program boot to tell the
    /// engine to switch to uci mode.
    ///
    /// After receiving the uci command the engine must identify itself with the `id`
    /// command and send the `option` commands to tell the GUI which engine settings
    /// the engine supports if any.
    ///
    /// After that the engine should send `uciok` to acknowledge the uci mode.
    /// If no uciok is sent within a certain time period, the engine task will be
    /// killed by the GUI.
    Uci,

    /// Enable/disable printing debug information.
    ///
    /// # Command structure:
    /// ```text
    /// debug [on | off]
    /// ```
    /// where `on = true`
    ///
    /// # Protocol Description
    ///
    /// Switch the debug mode of the engine on and off.
    ///
    /// In debug mode the engine should send additional infos to the GUI.
    ///   - e.g. with the `info string` command, to help debugging.
    ///   - e.g. the commands that the engine has received etc.
    ///
    /// This mode should be switched off by default and this command can be sent any
    /// time, also when the engine is thinking.
    Debug(bool),

    /// Synchronize with the engine.
    ///
    /// # Command structure:
    /// ```text
    /// isready
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This is used to synchronize the engine with the GUI.
    /// When the GUI has sent a command or multiple commands that can take some time
    /// to complete, this command can be used to wait for the engine to be ready
    /// again or to ping the engine to find out if it is still alive. E.g. this
    /// should be sent after setting the path to the table bases as this can take
    /// some time.
    ///
    /// This command is also required once before the engine is asked to do any
    /// search to wait for the engine to finish initializing.
    ///
    /// This command must always be answered with `readyok` and can be sent also
    /// when the engine is calculating in which case the engine should also
    /// immediately answer with `readyok` without stopping the search.
    IsReady,

    /// Modify an option in the engine.
    ///
    /// # Command structure:
    /// ```text
    /// setoption name <id> [value <x>]
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This is sent to the engine when the user wants to change the internal
    /// parameters of the engine. For the `button` type no value is needed.
    ///
    /// One string will be sent for each parameter and this will only be sent when
    /// the engine is waiting. The name and value of the option in `<id>` should not
    /// be case sensitive and can include spaces.
    ///
    /// The substrings `value` and `name` should be avoided in `<id>` and `<x>` to
    /// allow unambiguous parsing, for example do not use `<name> = draw value`.
    ///
    /// ## Examples
    ///
    /// Here are some strings for the example below:
    /// * `setoption name Nullmove value true\n`
    /// * `setoption name Selectivity value 3\n`
    /// * `setoption name Style value Risky\n`
    /// * `setoption name Clear Hash\n`
    /// * `setoption name NalimovPath value c:\chess\tb\4;c:\chess\tb\5\n`
    SetOption {
        /// The name of the engine parameter to modify.
        name: String,

        /// If provided, the new value for the specified parameter option.
        value: Option<String>,
    },

    /// Register with this engine.
    ///
    /// # Command structure:
    /// ```text
    /// registration [name <name> code <code> | later]
    /// ```
    ///
    /// If called with `later`, then the `name` and `code` parameters will be `None`.
    /// Otherwise, they are likely to both be `Some`, though they are parsed
    /// independently, and it is up to you(r engine) to determine whether your
    /// registration process requires both values.
    ///
    /// # Protocol Description
    ///
    /// This is the command to try to register an engine or to tell the engine that
    /// registration will be done later. This command should always be sent if the
    /// engine has sent `registration error` at program startup.
    ///
    /// The following tokens are allowed:
    /// * `later` - The user doesn't want to register the engine now.
    /// * `name <x>` - The engine should be registered with the name `<x>`
    /// * `code <y>` - The engine should be registered with the code `<y>`
    ///
    /// ## Example:
    ///    `register later`
    ///    `register name Stefan MK code 4359874324`
    Register {
        /// If provided, the name to use during registration.
        name: Option<String>,

        /// If provided, the code to use during registration.
        code: Option<String>,
    },

    /// Tell the engine that the following commands are to take place on a new game.
    ///
    /// # Command structure:
    /// ```text
    /// ucinewgame
    /// ```
    ///
    /// # Protocol Description
    ///
    /// This is sent to the engine when the next search (started with `position`
    /// and `go`) will be from a different game. This can be a new game the engine
    /// should play or a new game it should analyze but also the next position from
    /// a test suite with positions only.
    ///
    /// If the GUI hasn't sent a `ucinewgame` before the first `position` command,
    /// the engine shouldn't expect any further ucinewgame commands as the GUI is
    /// probably not supporting the ucinewgame command. So the engine should not rely
    /// on this command even though all new GUIs should support it.
    ///
    /// As the engine's reaction to `ucinewgame` can take some time the GUI should
    /// always send `isready` after `ucinewgame` to wait for the engine to finish
    /// its operation.
    UciNewGame,

    /// Set up the internal position for the engine.
    ///
    /// # Command structure:
    /// ```text
    /// position [fen <string> | startpos] [moves <move_1> [<move_2> ...]]
    /// ```
    ///
    /// If called with `startpos`, then the `fen` field will be `None`.
    ///
    /// # Protocol Description
    ///
    /// Set up the position described in FEN string on the internal board and
    /// play the moves on the internal chess board.
    ///
    /// If the game was played from the start position, the string `startpos` will
    /// be sent.
    ///
    /// Note: no `new` command is needed. However, if this position is from a
    /// different game than the last position sent to the engine, the GUI should have
    /// sent a `ucinewgame` in between.
    Position {
        /// If provided, the FEN string of the position to start with.
        ///
        /// If `startpos` was provided, this will be `None`.
        fen: Option<String>,

        /// A list of moves to apply to the provided/default position.
        #[cfg(feature = "types")]
        moves: Vec<UciMove>,
        #[cfg(not(feature = "types"))]
        moves: Vec<String>,
    },

    /// Start a search on the engine.
    ///
    /// # Command structure:
    /// ```text
    /// go [searchmoves <move_1> [<move_2> ...]] [ponder] [wtime <x>] [btime <x>] [winc <x>] [binc <x>] [movestogo <x>] [depth <x>] [nodes <x>] [mate <x>] [movetime <x>] [infinite]
    /// ```
    /// If no parameters are received, this should will be treated as `go infinite`
    /// and the `infinite` field will be set to `true`.
    ///
    /// # Protocol Description
    ///
    /// Start calculating on the current position set up with the `position` command.
    ///
    /// There are a number of commands that can follow this command, all will be sent
    /// in the same string. If one command is not sent its value should be
    /// interpreted as it would not influence the search.
    ///
    /// ## Arguments
    ///
    /// ```text
    /// searchmoves <move_1> [<move_2> ... <move_i>]
    /// ```
    /// Restrict search to this moves only
    ///
    /// Example: After `position startpos` and `go infinite searchmoves e2e4 d2d4`
    /// the engine should only search the two moves `e2e4` and `d2d4` in the initial
    /// position.
    ///
    /// ```text
    /// ponder
    /// ```
    /// Start searching in pondering mode.
    ///
    /// Do not exit the search in ponder mode, even if it's mate!
    ///
    /// This means that the last move sent in in the position string is the ponder
    /// move. The engine can do what it wants to do, but after a `ponderhit` command
    /// it should execute the suggested move to ponder on. This means that the ponder
    /// move sent by the GUI can be interpreted as a recommendation about which move
    /// to ponder. However, if the engine decides to ponder on a different move, it
    /// should not display any mainlines as they are likely to be misinterpreted by
    /// the GUI because the GUI expects the engine to ponder on the suggested move.
    ///
    /// ```text
    /// wtime <x>
    /// ```
    /// White has `x` milliseconds left on the clock.
    ///
    /// ```text
    /// btime <x>
    /// ```
    /// Black has `x` milliseconds left on the clock.
    ///
    /// ```text
    /// winc <x>
    /// ```
    /// White increment per move in milliseconds if `x > 0`.
    ///
    /// ```text
    /// binc <x>
    /// ```
    /// Black increment per move in milliseconds if `x > 0`.
    ///
    /// ```text
    /// movestogo <x>
    /// ```
    /// There are `x` moves to the next time control.
    ///
    /// This will only be sent if `x > 0`.
    ///
    /// If you don't get this and get the `wtime` and `btime`, it's sudden death.
    ///
    /// ```text
    /// depth <x>
    /// ```
    /// Search `x` plies only.
    ///
    /// ```text
    /// nodes <x>
    /// ```
    /// Search `x` nodes only.
    ///
    /// ```text
    /// mate <x>
    /// ```
    /// Search for a mate in `x` moves.
    ///
    /// ```text
    /// movetime <x>
    /// ```
    /// Search exactly `x` milliseconds.
    ///
    /// ```text
    /// infinite
    /// ```
    /// Search until the `stop` command. Do not exit the search without being told
    /// so in this mode!
    Go(UciSearchOptions),

    /// Tell the engine to stop searching.
    ///
    /// # Command structure:
    /// ```text
    /// stop
    /// ```
    ///
    /// # Protocol Description
    ///
    /// Stop calculating as soon as possible.
    ///
    /// Don't forget the `bestmove` and possibly the `ponder` token when finishing
    /// the search.
    Stop,

    /// Tell the engine that the opponent played the pondered move.
    ///
    /// # Command structure:
    /// ```text
    /// ponderhit
    /// ```
    ///
    /// # Protocol Description
    ///
    /// The user has played the expected move. This will be sent if the engine was
    /// told to ponder on the same move the user has played. The engine should
    /// continue searching but switch from pondering to normal search.
    PonderHit,

    /// Tell the engine to quit as soon as possible.
    ///
    /// # Command structure:
    /// ```text
    /// quit
    /// ```
    ///
    /// This does not necessarily need to immediately exit, but should make sure it
    /// performs any necessary clean-up before exiting.
    ///
    /// It is likely that you want this command to be a special case in your
    /// engine's event handler.
    ///
    /// # Protocol Description
    ///
    /// Quit the program as soon as possible.
    Quit,

    /// Run a benchmark suite.
    ///
    /// ```text
    /// bench <x>
    /// ```
    /// Not part of the UCI protocol, but [very common among engines](https://official-stockfish.github.io/docs/stockfish-wiki/UCI-&-Commands.html#bench).
    ///
    /// Used to make the engine execute a benchmark on a pre-set suite of positions.
    #[cfg(feature = "parse-bench")]
    Bench(UciSearchOptions),
}

impl UciCommand {
    /// Attempt to parse `input` into a valid [`UciCommand`].
    #[inline(always)]
    pub fn new(input: &str) -> Result<Self, UciParseError> {
        parse_uci_command(input)
    }
}

impl FromStr for UciCommand {
    type Err = UciParseError;
    /// Alias for [`UciCommand::new`].
    #[inline(always)]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
    }
}

impl fmt::Display for UciCommand {
    /// Formats this [`UciCommand`] using the given formatter.
    ///
    /// The formatted string will almost always be identical to the source it was
    /// parsed from, with the following exceptions:
    /// 1. Parameters to commands like [`UciCommand::Go`] will have a fixed order,
    ///     regardless of how they were originally supplied.
    /// 2. Leading/trailing/excessive whitespace is parsed out.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UciCommand::*;
        match self {
            Uci => write!(f, "uci"),

            Debug(status) => {
                write!(f, "debug {}", if *status { "on" } else { "off" })
            }

            IsReady => write!(f, "isready"),

            SetOption { name, value } => {
                if let Some(value) = value {
                    write!(f, "setoption name {name} value {value}")
                } else {
                    write!(f, "setoption name {name}")
                }
            }

            Register { name, code } => match (name, code) {
                (Some(name), Some(code)) => write!(f, "register name {name} code {code}"),
                (Some(name), None) => write!(f, "register name {name}"),
                (None, Some(code)) => write!(f, "register code {code}"),
                (None, None) => write!(f, "register later"),
            },

            UciNewGame => write!(f, "ucinewgame"),

            Position { fen, moves } => {
                if let Some(fen) = fen {
                    write!(f, "position fen {fen}")?;
                } else {
                    write!(f, "position startpos")?;
                }

                if !moves.is_empty() {
                    write!(f, " moves")?;

                    for mv in moves {
                        write!(f, " {mv}")?;
                    }
                }
                Ok(())
            }

            Go(options) => write!(f, "go{options}"), // The lack of space here is intentional, as the options may be empty

            Stop => write!(f, "stop"),

            PonderHit => write!(f, "ponderhit"),

            Quit => write!(f, "quit"),

            #[cfg(feature = "parse-bench")]
            Bench(args) => write!(f, "bench{args}"), // As with `go`, the lack of space here is intentional
        }
    }
}

/// Represents the arguments that can be sent to your engine via the `go` command.
#[derive(Default, Clone, PartialEq, Eq, Debug, Hash)]
pub struct UciSearchOptions {
    /// ```text
    /// searchmoves <move_1> [<move_2> ... <move_i>]
    /// ```
    ///
    /// Restrict search to this moves only
    ///
    /// Example: After `position startpos` and `go infinite searchmoves e2e4 d2d4`
    /// the engine should only search the two moves `e2e4` and `d2d4` in the initial
    /// position.
    #[cfg(feature = "types")]
    pub searchmoves: Vec<UciMove>,

    #[cfg(not(feature = "types"))]
    pub searchmoves: Vec<String>,

    /// ```text
    /// ponder
    /// ```
    /// Start searching in pondering mode.
    ///
    /// Do not exit the search in ponder mode, even if it's mate!
    ///
    /// This means that the last move sent in in the position string is the ponder
    /// move. The engine can do what it wants to do, but after a `ponderhit` command
    /// it should execute the suggested move to ponder on. This means that the ponder
    /// move sent by the GUI can be interpreted as a recommendation about which move
    /// to ponder. However, if the engine decides to ponder on a different move, it
    /// should not display any mainlines as they are likely to be misinterpreted by
    /// the GUI because the GUI expects the engine to ponder on the suggested move.
    pub ponder: bool,

    /// ```text
    /// wtime <x>
    /// ```
    /// White has `x` milliseconds left on the clock.
    pub wtime: Option<Duration>,

    /// ```text
    /// btime <x>
    /// ```
    /// Black has `x` milliseconds left on the clock.
    pub btime: Option<Duration>,

    /// ```text
    /// winc <x>
    /// ```
    /// White increment per move in milliseconds if `x > 0`.
    pub winc: Option<Duration>,

    /// ```text
    /// binc <x>
    /// ```
    /// Black increment per move in milliseconds if `x > 0`.
    pub binc: Option<Duration>,

    /// ```text
    /// movestogo <x>
    /// ```
    /// There are `x` moves to the next time control.
    ///
    /// This will only be sent if `x > 0`.
    ///
    /// If you don't get this and get the `wtime` and `btime`, it's sudden death.
    pub movestogo: Option<u32>,

    /// ```text
    /// depth <x>
    /// ```
    /// Search `x` plies only.
    pub depth: Option<u32>,

    /// ```text
    /// nodes <x>
    /// ```
    /// Search `x` nodes only.
    pub nodes: Option<u32>,

    /// ```text
    /// mate <x>
    /// ```
    /// Search for a mate in `x` moves.
    pub mate: Option<u32>,

    /// ```text
    /// movetime <x>
    /// ```
    /// Search exactly `x` milliseconds.
    pub movetime: Option<Duration>,

    /// ```text
    /// infinite
    /// ```
    /// Search until the `stop` command. Do not exit the search without being told
    /// so in this mode!
    pub infinite: bool,

    /// ```text
    /// perft <x>
    /// ```
    /// Not part of the UCI protocol, but [very common among engines](https://github.com/official-stockfish/Stockfish/blob/d6043970bd156b1d2ab6cb51e8d5cb0c6a40797c/tests/perft.sh#L17).
    ///
    /// Execute a [performance test](https://www.chessprogramming.org/Perft) (perft)
    /// on the current position at a depth of `x` plies.
    #[cfg(feature = "parse-go-perft")]
    pub perft: Option<u32>,
}

impl fmt::Display for UciSearchOptions {
    /// Formats the [`UciSearchOptions`] using the given formatter.
    ///
    /// This will always format the fields in the order in which they are listed
    /// [in the protocol definition](https://backscattering.de/chess/uci/#gui-go-tokens).
    ///
    /// `go perft` will appear last, if the `parse-go-perft` feature is enabled.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.searchmoves.is_empty() {
            write!(f, " searchmoves")?;
            for mv in &self.searchmoves {
                write!(f, " {mv}")?;
            }
        }

        if self.ponder {
            write!(f, " ponder")?;
        }

        if let Some(wtime) = self.wtime {
            write!(f, " wtime {}", wtime.subsec_millis())?;
        }

        if let Some(btime) = self.btime {
            write!(f, " btime {}", btime.subsec_millis())?;
        }

        if let Some(winc) = self.winc {
            write!(f, " winc {}", winc.subsec_millis())?;
        }

        if let Some(binc) = self.binc {
            write!(f, " binc {}", binc.subsec_millis())?;
        }

        if let Some(movestogo) = self.movestogo {
            write!(f, " movestogo {movestogo}")?;
        }

        if let Some(nodes) = self.nodes {
            write!(f, " nodes {nodes}")?;
        }

        if let Some(mate) = self.mate {
            write!(f, " mate {mate}")?;
        }

        if let Some(movetime) = self.movetime {
            write!(f, " movetime {}", movetime.subsec_millis())?;
        }

        if self.infinite {
            write!(f, " infinite")?;
        }

        #[cfg(feature = "parse-go-perft")]
        if let Some(perft) = self.perft {
            write!(f, " perft {perft}")?;
        }

        Ok(())
    }
}

/*************************************************************************************************/
/*                                 ENGINE TO GUI COMMUNICATION                                   */
/*************************************************************************************************/

/// # Responses sent from the Engine to the GUI via `stdout`.
///
/// These are all the commands the interface gets from the engine.
#[derive(Debug, Clone)]
pub enum UciResponse<T = String> {
    /// ```text
    /// id name <x>
    /// id author <x>
    /// ```
    Id { name: T, author: T },

    /// ```text
    /// uciok
    /// ```
    UciOk,

    /// ```text
    /// readyok
    /// ```
    ReadyOk,

    /// ```text
    /// bestmove <move_1> [ponder <move_2>]
    /// ```
    ///
    /// If the `bestmove` field is `None`, this will be printed as
    /// `bestmove (none) [ponder <ponder>]`.
    BestMove {
        bestmove: Option<T>,
        ponder: Option<T>,
    },

    /// ```text
    /// copyprotection [checking | ok | error]
    /// ```
    CopyProtection(UciCheckingStatus),

    /// ```text
    /// registration [checking | ok | error]
    /// ```
    Registration(UciCheckingStatus),

    /// ```text
    /// info [depth <x>] [seldepth <x>] [time <x>] [nodes <x>] [pv <move_1> [<move_2> ... <move_i>]] [score [cp <x> | mate <y>] [lowerbound | upperbound]] [currmove <move>] [currmovenumber <x>] [hashfull <x>] [nps <x>] [tbhits <x>] [sbhits <x>] [cpuload <x>] [string <str>] [refutation <move_1> <move_2> [... <move_i>]] [currline [cpunr] <move_1> [... <move_i>]]
    /// ```
    Info(Box<UciInfo>),

    /// ```text
    /// option name <id> type <t> [default <x>] [min <x>] [max <x>] [var <x>]
    /// ```
    Option(UciOption<T>),
}

impl<T: fmt::Display> fmt::Display for UciResponse<T> {
    /// Responses are formatted to display appropriately according to the UCI specifications.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Id { name, author } => write!(f, "id name {name}\nid author {author}"),
            Self::UciOk => write!(f, "uciok"),
            Self::ReadyOk => write!(f, "readyok"),
            Self::BestMove { bestmove, ponder } => match (bestmove, ponder) {
                (Some(b), Some(p)) => write!(f, "bestmove {b} ponder {p}"),
                (Some(b), None) => write!(f, "bestmove {b}"),
                (None, Some(p)) => write!(f, "bestmove (none) ponder {p}"),
                (None, None) => write!(f, "bestmove (none)"),
            },
            Self::CopyProtection(status) => write!(f, "copyprotection {status}"),
            Self::Registration(status) => write!(f, "registration {status}"),
            Self::Info(info) => write!(f, "info {info}"),
            Self::Option(opt) => write!(f, "option {opt}"),
        }
    }
}

/// Represents the status of the `copyprotection` and `registration` commands.
#[derive(Debug, Clone)]
pub enum UciCheckingStatus {
    /// The engine is checking the status of `copyprotection` or `registration`.
    Checking,

    /// All is well. Check was successful. No further action needed.
    Ok,

    /// An error occurred when checking `copyprotection` or `registration`
    Error,
}

impl fmt::Display for UciCheckingStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Checking => write!(f, "checking"),
            Self::Ok => write!(f, "ok"),
            Self::Error => write!(f, "error"),
        }
    }
}

/// Bounds for the `score` argument of the `info` response.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UciBound {
    /// The score is just a lowerbound.
    Lowerbound,

    /// The score is just an upperbound.
    Upperbound,
}

impl fmt::Display for UciBound {
    /// Formats as either `upperbound` or `lowerbound`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lowerbound => write!(f, "lowerbound"),
            Self::Upperbound => write!(f, "upperbound"),
        }
    }
}

/// Represents the type of score for the `score` argument of the `info` response.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UciScoreType {
    /// The score from the engine's point of view in centipawns.
    Centipawns,

    /// Mate in `<y>` moves (not plies).
    Mate,
}

impl fmt::Display for UciScoreType {
    /// Formats as either `cp` or `mate`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Centipawns => write!(f, "cp"),
            Self::Mate => write!(f, "mate"),
        }
    }
}

/// Represents
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UciScore {
    /// The score value, which is either a centipawn value or moves-to-mate,
    /// depending on the value of `score_type`.
    pub score: i32,

    /// Either `cp` or `mate`.
    pub score_type: UciScoreType,

    /// Either `lowerbound` or `upperbound`.
    pub bound: Option<UciBound>,
}

impl UciScore {
    /// Construct a new [`UciScore`] with the provided `score`, `score_type`, and
    /// `bound`.
    pub const fn new(score: i32, score_type: UciScoreType, bound: Option<UciBound>) -> Self {
        Self {
            score,
            score_type,
            bound,
        }
    }

    /// Construct a new [`UciScore`] with the provided `score` and `score_type`
    pub const fn new_unbounded(score: i32, score_type: UciScoreType) -> Self {
        Self::new(score, score_type, None)
    }

    /// Construct a new [`UciScore`] with `score_type` [`UciScoreType::Centipawns`].
    pub const fn cp(score: i32) -> Self {
        Self::new_unbounded(score, UciScoreType::Centipawns)
    }

    /// Construct a new [`UciScore`] with `score_type` [`UciScoreType::Mate`].
    pub const fn mate(moves_to_mate: i32) -> Self {
        Self::new_unbounded(moves_to_mate, UciScoreType::Mate)
    }

    /// Consumes `self` and appends the provided [`UciBound`] onto `self`.
    pub const fn with_bound(mut self, bound: UciBound) -> Self {
        self.bound = Some(bound);
        self
    }
}

impl fmt::Display for UciScore {
    /// Formats as `<cp <x> | mate <y>> [lowerbound | upperbound]`
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(bound) = &self.bound {
            write!(f, "{} {} {bound}", self.score_type, self.score)
        } else {
            write!(f, "{} {}", self.score_type, self.score)
        }
    }
}

/// Represents all information that can be sent with the `info` command.
#[derive(Debug, Clone, Default)]
pub struct UciInfo {
    /// ```text
    /// depth <x>
    /// ```
    /// Search depth (in plies).
    pub depth: Option<String>,

    /// ```text
    /// seldepth <x>
    /// ```
    /// Selective search depth (in plies),
    ///
    /// If the engine sends `seldepth` there must also be a `depth` present in the
    /// same string.
    pub seldepth: Option<String>,

    /// ```text
    /// time <x>
    /// ```
    /// The time searched (in ms).
    /// This should be sent together with the `pv`.
    pub time: Option<String>,

    /// ```text
    /// nodes <x>
    /// ```
    /// `<x>` nodes searched.
    /// The engine should send this info regularly.
    pub nodes: Option<String>,

    /// ```text
    /// pv <move_1> [<move_2> ... <move_i>]
    /// ```
    /// The best line found.
    pub pv: Vec<String>,

    /// ```text
    /// multipv <num>
    /// ```
    /// This for the multi pv mode.
    ///
    /// For the best move/pv add `multipv 1` in the string when you send the pv.
    ///
    /// In *k*-best mode always send all k variants in k strings together.
    pub multipv: Option<String>,

    /// ```text
    /// score [cp <x> | mate <y> | lowerbound | upperbound]
    /// ```
    ///
    ///   - `cp <x>` - The score from the engine's point of view in centipawns.
    ///   - `mate <y>` - Mate in `y` moves, not plies.
    ///
    /// If the engine is getting mated, use negative values for `y`.
    ///
    ///   - `lowerbound` - The score is just a lower bound.
    ///   - `upperbound` - The score is just an upper bound.
    pub score: Option<UciScore>,

    /// ```text
    /// currmove <move>
    /// ```
    /// Currently searching this move
    pub currmove: Option<String>,

    /// ```text
    /// Currmovenumber <x>
    /// ```
    /// Currently searching move number `x`, for the first move `x` should be `1` not
    /// `0`.
    pub currmovenumber: Option<String>,

    /// ```text
    /// hashfull <x>
    /// ```
    /// The hash is `x` permill full.
    ///
    /// The engine should send this info regularly.
    pub hashfull: Option<String>,

    /// ```text
    /// nps <x>
    /// ```
    /// `x` nodes per second searched.
    ///
    /// The engine should send this info regularly.
    pub nps: Option<String>,

    /// ```text
    /// tbhits <x>
    /// ```
    /// `x` positions where found in the endgame table bases.
    pub tbhits: Option<String>,

    /// ```text
    /// sbhits <x>
    /// ```
    /// `x` positions where found in the shredder endgame databases.
    pub sbhits: Option<String>,

    /// ```text
    /// cpuload x
    /// ```
    /// The cpu usage of the engine is `x` permill.
    pub cpuload: Option<String>,

    /// ```text
    /// string <str>
    /// ```
    /// Any string `str` which will be displayed be the engine.
    ///
    /// If there is a string command the rest of the line will be interpreted as
    /// `str`.
    pub string: Option<String>,

    /// ```text
    /// refutation <move_1> <move_2> ... <move_i>
    /// ```
    /// Move `<move_1>` is refuted by the line `<move_2> ... <move_1>`.
    /// `i` can be any number `>= 1`.
    ///
    /// Example: after move `d1h5` is searched, the engine can send
    /// `info refutation d1h5 g6h5` if `g6h5` is the best answer after
    /// `d1h5` or if `g6h5` refutes the move `d1h5`.
    ///
    /// If there is no refutation for `d1h5` found, the engine should just send
    /// `info refutation d1h5`.
    ///
    /// The engine should only send this if the option `UCI_ShowRefutations` is set
    /// to `true`.
    pub refutation: Vec<String>,

    /// ```text
    /// currline <cpnunr> <move_1> [<move_2> ... <move_i>]
    /// ```
    /// This is the current line the engine is calculating. `cpunr` is the number of
    /// the cpu if the engine is running on more than one cpu. `cpunr = 1,2,3...`.
    ///
    /// if the engine is just using one cpu, `cpunr` can be omitted.
    ///
    /// If `cpunr` is greater than `1`, always send all *k* lines in *k* strings
    /// together.
    ///
    /// The engine should only send this if the option `UCI_ShowCurrLine` is set to
    /// `true`.
    pub currline: Vec<String>,
}

impl UciInfo {
    /// Creates a new, empty, [`UciInfo`] struct.
    pub fn new() -> Self {
        Self::default()
    }

    /// Consumes `self` and adds the provided `depth` value.
    pub fn depth(mut self, depth: impl fmt::Display) -> Self {
        self.depth = Some(depth.to_string());
        self
    }

    /// Consumes `self` and adds the provided `seldepth` value.
    pub fn seldepth(mut self, seldepth: impl fmt::Display) -> Self {
        self.seldepth = Some(seldepth.to_string());
        self
    }

    /// Consumes `self` and adds the provided `time` value.
    pub fn time(mut self, time: impl fmt::Display) -> Self {
        self.time = Some(time.to_string());
        self
    }

    /// Consumes `self` and adds the provided `nodes` value.
    pub fn nodes(mut self, nodes: impl fmt::Display) -> Self {
        self.nodes = Some(nodes.to_string());
        self
    }

    /// Consumes `self` and adds the provided `multipv` value.
    pub fn multipv(mut self, multipv: impl fmt::Display) -> Self {
        self.multipv = Some(multipv.to_string());
        self
    }

    /// Consumes `self` and adds the provided `score` value.
    pub fn score(mut self, score: impl Into<UciScore>) -> Self {
        self.score = Some(score.into());
        self
    }

    /// Consumes `self` and adds the provided `currmove` value.
    pub fn currmove(mut self, currmove: impl fmt::Display) -> Self {
        self.currmove = Some(currmove.to_string());
        self
    }

    /// Consumes `self` and adds the provided `currmovenumber` value.
    pub fn currmovenumber(mut self, currmovenumber: impl fmt::Display) -> Self {
        self.currmovenumber = Some(currmovenumber.to_string());
        self
    }

    /// Consumes `self` and adds the provided `hashfull` value.
    pub fn hashfull(mut self, hashfull: impl fmt::Display) -> Self {
        self.hashfull = Some(hashfull.to_string());
        self
    }

    /// Consumes `self` and adds the provided `nps` value.
    pub fn nps(mut self, nps: impl fmt::Display) -> Self {
        self.nps = Some(nps.to_string());
        self
    }

    /// Consumes `self` and adds the provided `tbhits` value.
    pub fn tbhits(mut self, tbhits: impl fmt::Display) -> Self {
        self.tbhits = Some(tbhits.to_string());
        self
    }

    /// Consumes `self` and adds the provided `sbhits` value.
    pub fn sbhits(mut self, sbhits: impl fmt::Display) -> Self {
        self.sbhits = Some(sbhits.to_string());
        self
    }

    /// Consumes `self` and adds the provided `cpuload` value.
    pub fn cpuload(mut self, cpuload: impl fmt::Display) -> Self {
        self.cpuload = Some(cpuload.to_string());
        self
    }

    /// Consumes `self` and adds the provided `string` value.
    pub fn string(mut self, string: impl fmt::Display) -> Self {
        self.string = Some(string.to_string());
        self
    }

    /// Consumes `self` and adds the provided `pv` value.
    pub fn pv<T: fmt::Display>(mut self, pv: impl IntoIterator<Item = T>) -> Self {
        self.pv = pv.into_iter().map(|x| x.to_string()).collect();
        self
    }

    /// Consumes `self` and adds the provided `refutation` value.
    pub fn refutation<T: fmt::Display>(mut self, refutation: impl IntoIterator<Item = T>) -> Self {
        self.refutation = refutation.into_iter().map(|x| x.to_string()).collect();
        self
    }

    /// Consumes `self` and adds the provided `currline` value.
    pub fn currline<T: fmt::Display>(mut self, currline: impl IntoIterator<Item = T>) -> Self {
        self.currline = currline.into_iter().map(|x| x.to_string()).collect();
        self
    }
}

impl fmt::Display for UciInfo {
    /// An info command will only display data that it has.
    ///
    /// Any `None` fields are not displayed.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(x) = &self.depth {
            write!(f, "depth {x} ")?;
        }
        if let Some(x) = &self.seldepth {
            write!(f, "seldepth {x} ")?
        }
        if let Some(x) = &self.time {
            write!(f, "time {x} ")?;
        }
        if let Some(x) = &self.nodes {
            write!(f, "nodes {x} ")?;
        }
        if let Some(x) = &self.multipv {
            write!(f, "multipv {x} ")?;
        }
        if let Some(x) = &self.score {
            write!(f, "score {x} ")?;
        }
        if let Some(x) = &self.currmove {
            write!(f, "currmove {x} ")?;
        }
        if let Some(x) = &self.currmovenumber {
            write!(f, "currmovenumber {x} ")?;
        }
        if let Some(x) = &self.hashfull {
            write!(f, "hashfull {x} ")?;
        }
        if let Some(x) = &self.nps {
            write!(f, "nps {x} ")?;
        }
        if let Some(x) = &self.tbhits {
            write!(f, "tbhits {x} ")?;
        }
        if let Some(x) = &self.sbhits {
            write!(f, "sbhits {x} ")?;
        }
        if let Some(x) = &self.cpuload {
            write!(f, "cpuload {x} ")?;
        }
        if let Some(x) = &self.string {
            write!(f, "string {x} ")?;
        }
        if !self.refutation.is_empty() {
            write!(f, "refutation {}", self.refutation.join(" "))?;
        }
        if !self.currline.is_empty() {
            write!(f, "currline {}", self.currline.join(" "))?;
        }
        if !self.pv.is_empty() {
            write!(f, "pv {}", self.pv.join(" "))?;
        }
        Ok(())
    }
}

/// Represents a UCI-compatible option that can be modified for your Engine.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct UciOption<T = String> {
    /// Name of the option.
    pub name: T,

    /// What type of option it is.
    pub opt_type: UciOptionType<T>,
}

impl<T> UciOption<T> {
    /// Create a new [`UciOption`] with the provided name and type.
    pub const fn new(name: T, opt_type: UciOptionType<T>) -> Self {
        Self { name, opt_type }
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Check`].
    pub const fn check(name: T, default: bool) -> Self {
        Self::new(name, UciOptionType::Check { default })
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Spin`].
    pub const fn spin(name: T, default: i32, min: i32, max: i32) -> Self {
        Self::new(name, UciOptionType::Spin { default, min, max })
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Combo`].
    pub fn combo(name: T, default: T, vars: impl IntoIterator<Item = T>) -> Self {
        Self::new(
            name,
            UciOptionType::Combo {
                default,
                vars: vars.into_iter().collect(),
            },
        )
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::Button`].
    pub const fn button(name: T) -> Self {
        Self::new(name, UciOptionType::Button)
    }

    /// Create a new [`UciOption`] of type [`UciOptionType::String`].
    pub const fn string(name: T, default: T) -> Self {
        Self::new(name, UciOptionType::String { default })
    }
}

impl<T: fmt::Display> fmt::Display for UciOption<T> {
    /// An option is displayed as `name <name> type <type>`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "name {} type {}", self.name, self.opt_type)
    }
}

/// Represents the type of UCI-compatible options your engine can expose to the GUI.
#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum UciOptionType<T = String> {
    ///```text
    /// check
    /// ```
    /// A checkbox that can either be `true` or `false`.
    Check { default: bool },

    ///```text
    /// spin
    /// ```
    /// A spin wheel that can be an integer in a certain range.
    Spin { default: i32, min: i32, max: i32 },

    ///```text
    /// combo
    /// ```
    /// A combo box that can have different predefined strings as a value.
    Combo { default: T, vars: Vec<T> },

    ///```text
    /// button
    /// ```
    /// A button that can be pressed to send a command to the engine.
    Button,

    ///```text
    /// string
    /// ```
    /// A text field that has a string as a value
    ///
    /// An empty string has the value `<empty>`
    String { default: T },
}

impl<T: fmt::Display> fmt::Display for UciOptionType<T> {
    /// Option types are displayed like [these examples](https://backscattering.de/chess/uci/#engine-option-examples).
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UciOptionType::Check { default } => write!(f, "check default {default}"),
            UciOptionType::Spin { default, min, max } => {
                write!(f, "spin default {default} min {min} max {max}")
            }
            UciOptionType::Combo { default, vars } => {
                write!(f, "combo default {default}")?;
                for var in vars {
                    write!(f, " var {var}")?;
                }
                Ok(())
            }
            UciOptionType::Button => write!(f, "button"),
            UciOptionType::String { default } => write!(f, "string default {default}"),
        }
    }
}
