use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{
    common::err_invalid_input, error::KrangError, parse::*, scan::Loc, source::file::SourceFile,
};

use super::{ppgroup::*, ppinclude::PPInclude, ppline::*, pptoken::*};

/// Represents a file that been preprocessed to level 3.
#[derive(Debug)]
pub struct PPFile {
    stream: PPLineStream,
    group: PPGroup,
}
impl<'a> PPFile {
    pub fn path(&self) -> &str {
        self.stream.path()
    }

    pub fn new(stream: PPLineStream) -> Result<Self, KrangError> {
        let mapper = |line: &PPLine| line.loc().cloned();
        let group = Self::parse(stream.lines())
            .map(|(rem, group)| group)
            .map_err(|err| KrangError::ParseError(trace_parse_errors(mapper)(err)))?;

        Ok(Self { stream, group })
    }

    pub fn parse(input: &'a [PPLine]) -> ParseResult<'a, &'a [PPLine], PPGroup> {
        PPGroup::parse_into(Rc::new(true), input)
    }
}

#[derive(Debug, Clone)]
pub struct PPFileRef {
    store: Rc<FileStore>,
    ppfile: Rc<PPFile>,
}
impl PPFileRef {
    pub fn path(&self) -> &str {
        self.ppfile.path()
    }

    pub fn group(&self) -> &PPGroup {
        &self.ppfile.group
    }
}

#[derive(Debug)]
pub struct PathCache<T, W> {
    calculator: T,
    values: HashMap<String, W>,
}

impl<'a, T, W> PathCache<T, W>
where
    Self: 'a,
    T: PathCalculator<'a, Output = W>,
{
    pub fn new(calculator: T) -> Self {
        PathCache {
            calculator,
            values: HashMap::new(),
        }
    }

    // We take a new type parameter, Y.
    // The calculation function takes &Y as a parameter
    // It does not need to be sized because we only use references to it.
    pub fn get<P: AsRef<Path>>(&mut self, input: P) -> Result<&W, KrangError> {
        use std::collections::hash_map::Entry;
        match self.values.entry(input.as_ref().display().to_string()) {
            Entry::Occupied(entry) => Ok(entry.into_mut()),
            Entry::Vacant(entry) => {
                let value = self.calculator.calculate(input.as_ref())?;
                Ok(entry.insert(value))
            }
        }
    }
}

pub trait PathCalculator<'a> {
    type Output: 'a;
    fn calculate(&self, path: &Path) -> Result<Self::Output, KrangError>;
}

#[derive(Debug)]
pub struct PPFileCalculator {}
impl PPFileCalculator {
    pub fn new() -> Self {
        Self {}
    }
}
impl<'a> PathCalculator<'a> for PPFileCalculator {
    type Output = Rc<PPFile>;
    fn calculate(&self, path: &Path) -> Result<Self::Output, KrangError> {
        let source = SourceFile::open(path).map_err(KrangError::IOError)?;
        let stream = PPTokenStream::new(source)?;
        let lines = PPLineStream::new(stream)?;
        println!("{:?}", lines);
        PPFile::new(lines).map(Rc::new)
    }
}

#[derive(Debug)]
pub struct FileStore {
    ppfiles: RefCell<PathCache<PPFileCalculator, Rc<PPFile>>>,
}
impl<'a> FileStore {
    pub fn new() -> Self {
        Self {
            ppfiles: RefCell::new(PathCache::new(PPFileCalculator {})),
        }
    }

    pub fn get<P: AsRef<Path>>(&'a self, path: P) -> Result<Rc<PPFile>, KrangError> {
        self.ppfiles.borrow_mut().get(path).map(|rc| rc.clone())
    }
}

#[derive(Debug)]
pub struct FileManager {
    ipaths: Vec<String>,
    store: Rc<FileStore>,
}
impl<'a> FileManager {
    pub fn new(ipaths: Vec<String>) -> Self {
        Self {
            ipaths,
            store: Rc::new(FileStore::new()),
        }
    }

    pub fn get_include(&self, loc: &Loc, directive: &PPInclude) -> Result<PPFileRef, KrangError> {
        match directive {
            PPInclude::HName(chars) => self.search_hname(loc, &HChar::to_string(chars)),
            PPInclude::QName(chars) => {
                let qname = QChar::to_string(chars);
                self.search_qname(loc, &qname).or_else(|qerr| {
                    self.search_hname(loc, &qname)
                        .map_err(|herr| KrangError::Pair(Box::new((qerr, herr))))
                })
            }
            PPInclude::Included(..) => Err(KrangError::ParseError((
                Some(loc.clone()),
                format!("attempt to re-load an #include directive: {:?}", directive),
            ))),
            PPInclude::Unparsed(..) => Err(KrangError::ParseError((
                Some(loc.clone()),
                format!("attempt to include unparsed directive: {:?}", directive),
            ))),
        }
    }

    fn search_qname(&self, loc: &Loc, qname: &String) -> Result<PPFileRef, KrangError> {
        let q_as_path = Path::new(qname);
        if q_as_path.has_root() {
            self.get(q_as_path)
        } else if let Some(abs_parent) = loc.path().as_path().parent() {
            let mut buf = PathBuf::new();
            buf.push(abs_parent);
            buf.push(q_as_path);
            self.get(&buf)
        } else {
            Err(KrangError::IOError(err_invalid_input(format!(
                "failed to resolve \"{}\" from loc {:?}",
                qname, loc
            ))))
        }
    }

    fn search_hname(&self, loc: &Loc, hname: &String) -> Result<PPFileRef, KrangError> {
        let h_as_path = Path::new(hname);
        for ipath in self.ipaths.iter() {
            let i_as_path = Path::new(ipath);
            let mut buf = PathBuf::new();
            if !i_as_path.has_root() {
                buf.push(i_as_path.canonicalize().map_err(KrangError::IOError)?)
            } else {
                buf.push(i_as_path);
            }
            buf.push(h_as_path);
            if let Ok(file_ref) = self.get(&buf) {
                return Ok(file_ref);
            } else {
                println!("{}", buf.display());
            }
        }
        Err(KrangError::IOError(err_invalid_input(format!(
            "failed to resolve <{}> from any include path {:?}",
            hname, self.ipaths
        ))))
    }

    pub fn get<P: AsRef<Path>>(&'a self, path: P) -> Result<PPFileRef, KrangError> {
        self.store.get(path).map(|ppfile| PPFileRef {
            store: self.store.clone(),
            ppfile,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::{path::Path, rc::Rc};

    use crate::{error::KrangError, preproc::ppinclude::PPInclude, scan::Loc};

    use super::FileManager;

    #[test]
    fn spin() -> Result<(), KrangError> {
        let manager = FileManager::new(vec!["/usr/include".to_owned()]);
        let ppfile = manager.get(Path::new("tests/hello_world.c"))?;
        print!("{:#?}", ppfile);
        Ok(())
    }

    #[test]
    fn stdio() -> Result<(), KrangError> {
        let manager = FileManager::new(vec!["tests".to_owned()]);
        let path = Path::new("tests/hello_world.c")
            .canonicalize()
            .map_err(KrangError::IOError)?;
        let loc = Loc::init(Rc::new(path.display().to_string()));
        let hname = "simple1.h".to_owned();
        let simple1_h = manager.search_hname(&loc, &hname)?;
        print!("{:#?}", &simple1_h);
        Ok(())
    }
}
