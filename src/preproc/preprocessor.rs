use std::{cell::Cell, fmt::Debug, path::Path, rc::Rc};

use crate::{error::KrangError, scan::Loc};

use super::{
    file::FileManager,
    ppcontext::PPContext,
    ppgroup::{PPGroup, PPGroupPart},
    ppinclude::PPInclude,
    stdc::Stdcs,
};

#[derive(Debug)]
pub struct PPUnit(pub PPGroup);

pub struct Preprocessor {
    files: Rc<FileManager>,
}
impl Preprocessor {
    pub fn new() -> Self {
        Self {
            files: Rc::new(FileManager::new(vec!["/usr/include".to_owned()])),
        }
    }
    /// entry point for the preprocess phase
    pub fn preprocess<P: AsRef<Path>>(&self, path: P) -> Result<PPUnit, KrangError> {
        let ctx = PPContext::new(self.files.clone());
        let ppfile = self.files.get(path)?;
        let group = ppfile.group().pass(&ctx)?;
        Ok(PPUnit(group))
    }
}

pub trait PPAble: Clone + Debug {
    fn pass(&self, ctx: &PPContext) -> Result<Self, KrangError> {
        println!("** DEBUG: PPAble::pass self={:?}", self);
        Ok(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{error::KrangError, preproc::preprocessor::Preprocessor};

    #[test]
    fn spin() -> Result<(), KrangError> {
        let pp = Preprocessor::new();
        let unit = pp.preprocess(Path::new("tests/simple.c"))?;
        print!("{:#?}", unit);
        Ok(())
    }

    #[test]
    fn ifendif() -> Result<(), KrangError> {
        let pp = Preprocessor::new();
        let unit = pp.preprocess(Path::new("tests/ifendif.h"))?;
        print!("{:#?}", unit);
        Ok(())
    }

    #[test]
    fn ifelse() -> Result<(), KrangError> {
        let pp = Preprocessor::new();
        let unit = pp.preprocess(Path::new("tests/ifelse.h"))?;
        print!("{:#?}", unit);
        Ok(())
    }

    #[test]
    fn hello_world() -> Result<(), KrangError> {
        let pp = Preprocessor::new();
        let unit = pp.preprocess(Path::new("tests/hello_world.c"))?;
        print!("{:#?}", unit);
        Ok(())
    }
}
