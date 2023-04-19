mod node;
mod vector;

use std::f64::consts::TAU;

use hodaun::{Mix, Mono, OutputDeviceMixer};
use node::{Node, NodeSource, Wave3};
use vector::Vector;

fn main() {
    let mut output = OutputDeviceMixer::<Mono>::with_default_device().unwrap();

    let root = NodeSource::new(
        Node::Wave(Wave3::new(220.0, |pos| {
            (pos.x * TAU).sin() + (pos.y * TAU).sin() + (pos.z * TAU).sin()
        })),
        Vector::ZERO,
        Vector::new(1.0, 2f64.powf(4.0 / 12.0), 2f64.powf(7.0 / 12.0)),
    );

    output.add(root);

    output.play_blocking().unwrap();
}
