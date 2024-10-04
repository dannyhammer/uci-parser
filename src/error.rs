use thiserror::Error;

/// Errors that can occur when parsing a UCI command.
#[derive(Error, Debug, PartialEq, Eq)]
pub enum UciParseError {
    /// The provided command could not be parsed and is likely not a UCI command.
    #[error("unrecognized UCI command: {cmd:?}")]
    UnrecognizedCommand { cmd: String },

    /// The provided command is UCI, but was given invalid arguments.
    #[error("invalid argument {arg:?} to {cmd:?}")]
    InvalidArgument { cmd: String, arg: String },

    /// The provided command is UCI, but was not given enough arguments
    #[error("insufficient arguments for {cmd:?}")]
    InsufficientArguments { cmd: String },
}
