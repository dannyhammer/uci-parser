/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{fmt, str::FromStr, time::Duration};

use crate::parse_uci_command;

/// A command sent from a GUI to an engine.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum UciCommand {
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
        moves: Vec<String>,
    },

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
    ///
    /// If this function fails, `Err` will contain an error message as to why.
    pub fn new(input: &str) -> Result<Self, String> {
        parse_uci_command(input)
            .map(|(_rest, cmd)| cmd)
            .map_err(|e| e.to_string())
    }
}

impl FromStr for UciCommand {
    type Err = String;
    /// Alias for [`UciCommand::new`].
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
    /// regardless of how they were originally supplied.
    /// 2. Leading/trailing/excessive whitespace is parsed out.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UciCommand::*;
        match self {
            Uci => write!(f, "uci"),

            Debug(status) => {
                if *status {
                    write!(f, "debug on")
                } else {
                    write!(f, "debug off")
                }
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
            Bench(args) => write!(f, "bench{args}"), // As above, the lack of space here is intentional
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
