mod builtin;
mod lex;
mod node;
mod parse;
mod vector;

use std::fs;

use hodaun::{Mix, Mono, OutputDeviceMixer};
use node::NodeSource;
use parse::parse;
use vector::Vector;

fn main() {
    let input = fs::read_to_string("test.cube").unwrap();
    match parse(&input) {
        Ok(Some(root)) => {
            println!("{root:?}");

            let mut output = OutputDeviceMixer::<Mono>::with_default_device().unwrap();

            let source = NodeSource::new(root, Vector::ZERO, Vector::new(1.0, 0.0, 0.0));

            output.add(source);

            output.play_blocking().unwrap();
        }
        Ok(None) => println!("No root node"),
        Err(err) => println!("Parse error: {:?}", err),
    }
}
