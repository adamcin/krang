use std::{
    cell::Cell,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlagStart {
    Dash,
    DashNo,
    DashNoDash,
    Slash,
    Long,
    LongNo,
    DashF,
    DashFNo,
    DashG,
    DashGNo,
    DashM,
    DashMNo,
    DashW,
    DashWNo,
}

impl FlagStart {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Dash => "-",
            Self::DashNo => "-no",
            Self::DashNoDash => "-no-",
            Self::Slash => "/",
            Self::Long => "--",
            Self::LongNo => "--no-",
            Self::DashF => "-F",
            Self::DashFNo => "-Fno-",
            Self::DashG => "-g",
            Self::DashGNo => "-gno-",
            Self::DashM => "-m",
            Self::DashMNo => "-mno-",
            Self::DashW => "-W",
            Self::DashWNo => "-Wno-",
        }
    }

    pub fn is_no(&self) -> bool {
        matches!(
            self,
            Self::DashNo
                | Self::DashNoDash
                | Self::LongNo
                | Self::DashFNo
                | Self::DashGNo
                | Self::DashMNo
                | Self::DashWNo
        )
    }

    pub fn all() -> Vec<Self> {
        vec![
            Self::Dash,
            Self::DashNo,
            Self::DashNoDash,
            Self::Slash,
            Self::Long,
            Self::LongNo,
            Self::DashF,
            Self::DashFNo,
            Self::DashG,
            Self::DashGNo,
            Self::DashM,
            Self::DashMNo,
            Self::DashW,
            Self::DashWNo,
        ]
    }

    pub fn parse(arg: &str) -> Vec<Self> {
        Self::all()
            .into_iter()
            .filter(|fs| arg.starts_with(fs.as_str()))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Flag {
    start: FlagStart,
    name: String,
}

impl Flag {
    pub fn new(name: &str, start: FlagStart) -> Self {
        Self {
            start,
            name: name.to_owned(),
        }
    }
    pub fn parse(arg: &str) -> Vec<Self> {
        FlagStart::parse(arg)
            .into_iter()
            .map(|start| Flag::new(&arg[0..start.as_str().len()], start))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputArg {
    Flag(Vec<Flag>),
    Arg(String),
}

impl InputArg {
    fn parse(full_arg: &str) -> Vec<Self> {
        let (flag_base, flag_args) = match full_arg.split_once('=') {
            None => (full_arg, None),
            Some((name, value)) => (
                name,
                Some(
                    value
                        .split(',')
                        .map(|arg| Self::Arg(arg.to_owned()))
                        .collect(),
                ),
            ),
        };
        let flags = Flag::parse(flag_base);
        if flags.is_empty() {
            vec![Self::Arg(full_arg.to_owned())]
        } else {
            vec![vec![Self::Flag(flags)], flag_args.unwrap_or_default()].concat()
        }
    }

    pub fn argv() -> Vec<Self> {
        std::env::args()
            .skip(1)
            .flat_map(|arg| Self::parse(&arg).into_iter())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum KrangAction {
    Preprocess,
    Assemble,
    Compile,
}

pub struct KrangFlags {
    prefix: Cell<PathBuf>,
    output: Cell<PathBuf>,
    action: Cell<KrangAction>,
}
