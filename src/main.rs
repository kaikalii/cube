mod builtin;
mod compile;
mod lex;
mod node;
mod value;
mod vector;

use std::fs;

use compile::compile;
use hodaun::{Mix, Mono, OutputDeviceMixer};
use node::NodeSource;

fn main() {
    let input = fs::read_to_string("test.cube").unwrap();
    match compile(&input) {
        Ok(cube) => {
            println!("{:?}", cube.root);

            let mut output = OutputDeviceMixer::<Mono>::with_default_device().unwrap();

            let source = NodeSource {
                root: cube.root,
                pos: cube.initial_pos,
                dir: cube.initial_dir.into(),
                tempo: cube.tempo,
            };

            output.add(source);

            output.play_blocking().unwrap();
        }
        Err(err) => println!("Error at {err}"),
    }
}
