mod node;
mod vector;

use hodaun::{Mix, Mono, OutputDeviceMixer};
use node::{switch2, true_saw_wave, true_square_wave, Node, NodeSource, Wave3};
use vector::Vector;

fn main() {
    let mut output = OutputDeviceMixer::<Mono>::with_default_device().unwrap();

    let root = Node::Wave(Wave3::new(55.0, |pos| {
        pos.map(|x| true_square_wave(x, 100)).reduce(|a, b| a + b)
    }))
    .amplify(0.3)
    .envelope(|pos| {
        (pos.map(|x| true_saw_wave(x * switch2(x, 2.0, |_| 2.0, |_| 8.0), 10))
            .reduce(|a, b| a + b)
            * 0.7
            + 0.3)
            .max(0.0)
    });

    let source = NodeSource::new(root, Vector::ZERO, Vector::new(1.0, 0.0, 0.0));

    output.add(source);

    output.play_blocking().unwrap();
}
