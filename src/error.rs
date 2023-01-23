use std::io::Error;

use crate::{
    preproc::{
        id::Id,
        ppinclude::PPInclude,
        pptoken::{PPNum, PPTokens},
    },
    scan::Loc,
};

#[derive(Debug)]
pub enum KrangError {
    IOError(Error),
    ParseError(Vec<(Option<Loc>, String)>),
    /// error produced by processing an #error pp directive
    ErrorDirective(Loc, Option<PPTokens>),
    PPUnrecognizedId(Loc, Id),
    PPNumFormatError(Loc, PPNum, String),
    MaxIncludeLevelsReached(Loc, PPInclude),
    Pair(Box<(KrangError, KrangError)>),
}
