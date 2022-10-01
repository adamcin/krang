use std::fmt::{Debug, Display};
use std::fs::File;
use std::io::Error;

pub trait UnitFactory {
    type Unit: crate::common::Unit;

    fn unit_from(src_path: &str) -> Result<Self::Unit, Error>;
}

pub trait Unit {
    type Syntax: Display + Debug;

    fn src_path<'a>(&'a self) -> &'a str;
}

pub trait FileUnit: Unit {
    fn save(&self, syntax: &Self::Syntax) -> Result<(), Error>
    where
        Self::Syntax: Display + Debug,
    {
        let mut out = File::create(self.src_path())?;
        write!(out, "{}", syntax)
    }

    fn parse(&self) -> Result<Self::Syntax, Error>;
}

pub trait DirUnit: Unit {
    fn filename_for(elem: &Self::Syntax) -> String;
    fn save(&self, syntax: &Vec<Self::Syntax>) -> Result<(), Error> {
        for elem in syntax {
            let mut vm_path = PathBuf::new();
            let my_path = Path::new(self.src_path());
            let filename = Self::filename_for(&elem);
            vm_path.push(my_path);
            vm_path.push(Path::new(&filename));
            let mut out = File::create(vm_path)?;
            write!(out, "{}", elem)?;
        }
        Ok(())
    }

    fn parse(&self) -> Result<Vec<Self::Syntax>, Error>;
}

use std::io::Write;
use std::path::{Path, PathBuf};

pub fn err_invalid_input<E>(msg: E) -> Error
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Error::new(std::io::ErrorKind::InvalidInput, msg)
}
