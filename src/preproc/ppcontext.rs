use std::rc::Rc;

use crate::{error::KrangError, scan::Loc};

use super::{
    file::FileManager, ppgroup::PPGroup, ppinclude::PPInclude, preprocessor::PPAble, stdc::Stdcs,
};

const MAX_INCLUDES: usize = 2;
pub struct PPContext {
    times_included: usize,
    files: Rc<FileManager>,
    stdcs: Rc<Stdcs>,
}
impl PPContext {
    pub fn new(files: Rc<FileManager>) -> Self {
        Self {
            times_included: 0,
            files,
            stdcs: Rc::new(Stdcs::new()),
        }
    }

    pub fn sub_inc(&self) -> Self {
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
