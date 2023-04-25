mod ast;
mod compile;
mod lex;
mod node;
mod parse;
mod ui;

use std::{
    fs,
    ops::{Add, Rem},
};

use hodaun::{Mix, OutputDeviceMixer, Stereo};

use crate::compile::compile;

fn main() {
    let input = match fs::read_to_string("test.cube") {
        Ok(input) => input,
        Err(e) => {
            println!("{e}");
            return;
        }
    };
    let source = match compile(&input) {
        Ok(source) => source,
        Err(e) => {
            println!("{e}");
            return;
        }
    };
    let mut output = OutputDeviceMixer::<Stereo>::with_default_device().unwrap();
    if let Some(source) = source {
        output.add(source);
    }
    output.play_blocking().unwrap();
}

pub fn modulus<T, M>(a: T, m: M) -> <<<T as Rem<M>>::Output as Add<M>>::Output as Rem<M>>::Output
where
    M: Copy,
    T: Rem<M>,
    <T as Rem<M>>::Output: Add<M>,
    <<T as Rem<M>>::Output as Add<M>>::Output: Rem<M>,
{
    (a % m + m) % m
}
