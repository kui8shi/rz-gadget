//! Add example usage here.

#![cfg_attr(feature = "clippy", feature(plugin))]
#![cfg_attr(feature = "clippy", plugin(clippy))]

extern crate libc;
extern crate rzpipe;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

pub mod structs;
#[macro_use]
pub mod api;
