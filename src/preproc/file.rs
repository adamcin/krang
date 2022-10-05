use crate::{scan::Loc, source::token::PPToken};

use super::id::Id;

pub struct PPFile<'a> {
    group: Option<PPGroup<'a>>,
}

pub struct PPGroup<'a> {
    loc: Loc<'a>,
    prev: Box<PPGroup<'a>>,
    part: PPGroupPart<'a>,
}

pub enum PPGroupPart<'a> {
    IfSection(Loc<'a>),
    ControlLine(Loc<'a>),
    TextLine(Loc<'a>),
    NonDirective(Loc<'a>),
}

pub enum PPIfGroup<'a> {
    If(Loc<'a>),
    IfDef(Loc<'a>),
    IfNotDef(Loc<'a>),
}

pub struct PPElIfs<'a> {
    loc: Loc<'a>,
    prev: Box<PPElIfs<'a>>,
    group: PPElIf<'a>,
}

pub struct PPElIf<'a> {
    loc: Loc<'a>,
    group: Box<Option<PPGroup<'a>>>,
}

pub struct PPElse<'a> {
    loc: Loc<'a>,
    group: Box<Option<PPGroup<'a>>>,
}

pub struct PPEndIf<'a> {
    loc: Loc<'a>,
}

pub enum PPControlLine<'a> {
    Include(Loc<'a>, PPToken<'a>),
    Define(Loc<'a>, Id),
}
