use std::collections::HashSet;

use crate::preproc::token::TokenStream;

pub struct Unit {
    paths: HashSet<String>,
    stream: TokenStream,
}
