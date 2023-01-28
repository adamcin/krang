use std::{cell::Cell, collections::HashMap};

use super::{id::Id, ppline::PPOnOffSwitch};

pub struct Stdcs {
    defaults: HashMap<&'static str, bool>,
    switches: HashMap<&'static str, Cell<bool>>,
}

const STDC_CX_LIMITED_RANGE: &str = "CX_LIMITED_RANGE";
const STDC_FP_CONTRACT: &str = "FP_CONTRACT";
const STDC_FENV_ACCESS: &str = "FENV_ACCESS";

impl Stdcs {
    pub fn new() -> Self {
        let mut defaults = HashMap::new();
        defaults.insert(STDC_CX_LIMITED_RANGE, false);
        defaults.insert(STDC_FP_CONTRACT, false);
        defaults.insert(STDC_FENV_ACCESS, false);

        let mut switches = HashMap::new();
        switches.insert(STDC_CX_LIMITED_RANGE, Cell::new(false));
        switches.insert(STDC_FP_CONTRACT, Cell::new(false));
        switches.insert(STDC_FENV_ACCESS, Cell::new(false));

        Self { switches, defaults }
    }

    pub fn flip<'a>(&self, id: &'a Id, state: &PPOnOffSwitch) -> Result<(), &'a Id> {
        if let Some((key, value)) = self.switches.get_key_value(id.to_string().as_str()) {
            match state {
                PPOnOffSwitch::Default => value.set(self.defaults[key]),
                PPOnOffSwitch::Off => value.set(false),
                PPOnOffSwitch::On => value.set(true),
            };
            Ok(())
        } else {
            Err(id)
        }
    }

    pub fn is(&self, id: &Id) -> bool {
        match self.switches.get(id.to_string().as_str()) {
            Some(value) => value.get(),
            _ => false,
        }
    }
}
