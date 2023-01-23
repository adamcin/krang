use std::{cell::Cell, fmt::Debug, path::Path, rc::Rc};

use crate::{error::KrangError, scan::Loc};

use super::{
    directive::{PPGroup, PPGroupPart},
    file::FileManager,
    ppinclude::PPInclude,
    stdc::Stdcs,
};

#[derive(Debug)]
pub struct PPUnit(pub PPGroup);

const MAX_INCLUDES: usize = 2;
pub struct PPContext {
    times_included: usize,
    files: Rc<FileManager>,
    stdcs: Rc<Stdcs>,
}
impl PPContext {
    fn new(files: Rc<FileManager>) -> Self {
        Self {
            times_included: 0,
            files,
            stdcs: Rc::new(Stdcs::new()),
        }
    }

    fn sub_inc(&self) -> Self {
        Self {
            times_included: self.times_included + 1,
            files: self.files.clone(),
            stdcs: self.stdcs.clone(),
        }
    }

    pub fn stdc(&self) -> &Stdcs {
        self.stdcs.as_ref()
    }

    pub fn include(&self, loc: &Loc, inc: &PPInclude) -> Result<PPGroup, KrangError> {
        if self.times_included >= MAX_INCLUDES {
            Err(KrangError::MaxIncludeLevelsReached(
                loc.clone(),
                inc.clone(),
            ))
        } else {
            let file_ref = self.files.get_include(loc, inc)?;
            let subctx = self.sub_inc();
            file_ref.group().pass(&subctx)
        }
    }
}

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
    fn hello_world() -> Result<(), KrangError> {
        let pp = Preprocessor::new();
        let unit = pp.preprocess(Path::new("tests/hello_world.c"))?;
        print!("{:#?}", unit);
        Ok(())
    }
}
