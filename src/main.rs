mod lex;
mod node;
mod parse;
mod vector;

use hodaun::{Mix, Mono, OutputDeviceMixer};
use node::{kick_wave, Node, NodeSource};
use vector::Vector;

fn main() {
    let mut output = OutputDeviceMixer::<Mono>::with_default_device().unwrap();

    let root = Node::synth(|pos| kick_wave(pos.x, 40.0, 0.5, 0.5));

    let source = NodeSource::new(root, Vector::ZERO, Vector::new(1.0, 0.0, 0.0));

    output.add(source);

    output.play_blocking().unwrap();
}
