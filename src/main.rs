#![feature(path_file_prefix)]
#![feature(trait_alias)]
#![feature(absolute_path)]

mod common;
#[macro_use]
mod parse;
mod cli;
mod compile;
mod error;
mod preproc;
mod punct;
mod read;
mod scan;
mod source;

fn main() {
    let mut opts = std::env::args();
    while let Some(opt) = opts.next() {
        let negated = opt.starts_with("--no-");
    }
    println!("Hello, world!");
}
