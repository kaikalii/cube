mod node;
mod vector;

use std::f64::consts::TAU;

use hodaun::{Mix, Mono, OutputDeviceMixer};
use node::{BiEnvelope, Node, NodeSource, Wave3};
use vector::{RectPrism, Vector};

fn main() {
    let mut output = OutputDeviceMixer::<Mono>::with_default_device().unwrap();

    let root = Node::Wave(Wave3::new(220.0, |pos| {
        (pos.x * TAU).sin() + (pos.y * TAU).sin() + (pos.z * TAU).sin()
    }))
    .envolope(
        RectPrism::from_min_max(Vector::ZERO, Vector::splat(4.0)),
        Vector::splat(BiEnvelope::symmetric(0.1, 0.1, 0.5)),
    );

    let source = NodeSource::new(
        root,
        Vector::ZERO,
        Vector::new(1.0, 2f64.powf(4.0 / 12.0), 2f64.powf(7.0 / 12.0)),
    );

    output.add(source);

    output.play_blocking().unwrap();
}
