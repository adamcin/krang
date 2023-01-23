use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::Error;

use std::io::Write;
use std::path::{Path, PathBuf};

pub fn err_invalid_input<E>(msg: E) -> Error
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Error::new(std::io::ErrorKind::InvalidInput, msg)
}
