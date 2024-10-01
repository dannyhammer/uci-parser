/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

#![doc = include_str!("../README.md")]

/// Utilities related to parsing UCI messages.
mod parser;

/// Types representing the various GUI-to-Engine and Engine-to-GUI messages that can be sent.
mod messages;

pub use messages::*;
pub use parser::*;
