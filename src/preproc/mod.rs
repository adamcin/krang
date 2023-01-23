use std::{cell::RefCell, io::Error, path::Path};

use crate::{common::err_invalid_input, compile::unit::Unit};

use self::file::FileManager;

pub(crate) mod directive;
pub(crate) mod expression;
pub(crate) mod file;
pub(crate) mod id;
pub(crate) mod keyword;
pub(crate) mod ppif;
pub(crate) mod ppinclude;
pub(crate) mod ppmacro;
pub(crate) mod pptoken;
pub(crate) mod preprocessor;
pub(crate) mod stdc;
pub(crate) mod token;
pub(crate) mod translate;

// 4. Preprocessing directives are executed, macro invocations are expanded, and
// _Pragma unary operator expressions are executed. If a character sequence that
// matches the syntax of a universal character name is produced by token
// concatenation (6.10.3.3), the behavior is undefined. A #include preprocessing
// directive causes the named header or source file to be processed from phase 1
// through phase 4, recursively. All preprocessing directives are then deleted.
