use std::sync::Arc;

use hodaun::{Mono, Shared, Source};

use crate::vector::{RectPrism, Vector};

pub enum Node {
    RectGate(RectPrism, Box<Node>),
    Wave(Wave3),
}

impl Node {
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        match self {
            Node::RectGate(rect, node) => {
                node.sample(sample_rate, pos, dir) * rect.contains(pos) as u8 as f64
            }
            Node::Wave(wave) => {
                let sample = (wave.one_hz)(wave.pos);
                wave.pos += dir * (wave.freq / sample_rate);
                Vector::X * sample
            }
        }
    }
}

pub struct Wave3 {
    pub one_hz: Arc<dyn Fn(Vector) -> f64 + Send + Sync>,
    pub freq: f64,
    pub pos: Vector,
}

impl Wave3 {
    pub fn new(freq: f64, one_hz: impl Fn(Vector) -> f64 + Send + Sync + 'static) -> Self {
        Self {
            one_hz: Arc::new(one_hz),
            freq,
            pos: Vector::ZERO,
        }
    }
}

pub struct NodeSource {
    pub pos: Vector,
    pub dir: Shared<Vector>,
    pub root: Node,
}

impl NodeSource {
    pub fn new(root: Node, pos: Vector, dir: impl Into<Shared<Vector>>) -> Self {
        Self {
            pos,
            dir: dir.into(),
            root,
        }
    }
}

impl Source for NodeSource {
    type Frame = Mono;
    fn next(&mut self, sample_rate: f64) -> Option<Self::Frame> {
        let dir = self.dir.get();
        let sample = self.root.sample(sample_rate, self.pos, dir);
        self.pos += dir * (1.0 / sample_rate);
        Some(sample.x)
    }
}
