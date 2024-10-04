/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#![doc = include_str!("../README.md")]

/// Error handling when parsing/building messages.
pub mod error;

/// Types representing the various GUI-to-Engine and Engine-to-GUI messages that can be sent.
pub mod messages;

/// Utilities related to parsing UCI messages.
mod parser;

/// Well-typed representations of pieces and moves.
///
/// Only available by enabling the `types` feature.
///
/// Each type in this module implements both [Display] and [FromStr] following the UCI specification,
/// making it easy to convert to/from them from strings.
/// All enums are unit-only, making [`as` casts safe](https://doc.rust-lang.org/reference/items/enumerations.html#casting).
///
/// [Display]: std::fmt::Display
/// [FromStr]: std::str::FromStr
#[cfg(feature = "types")]
pub mod types;

pub use error::*;
pub use messages::*;
use parser::*;
#[cfg(feature = "types")]
pub use types::*;
