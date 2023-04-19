use std::{
    f64::consts::{PI, TAU},
    sync::Arc,
};

use hodaun::{Mono, Shared, Source};

use crate::vector::{modulus, Vector};

#[derive(Clone)]
pub enum Node {
    Wave(Wave3),
    Synth(Arc<dyn Fn(Vector) -> f64 + Send + Sync>),
    Envelope(Enveloped),
}

impl Node {
    pub fn synth(synth: impl Fn(Vector) -> f64 + Send + Sync + 'static) -> Self {
        Self::Synth(Arc::new(synth))
    }
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        match self {
            Node::Wave(wave) => {
                let sample = (wave.one_hz)(wave.pos);
                wave.pos += dir * (wave.freq / sample_rate);
                Vector::X * sample
            }
            Node::Synth(synth) => Vector::X * (synth)(pos),
            Node::Envelope(adsr) => {
                let sample = adsr.node.sample(sample_rate, pos, dir);
                let amp = (adsr.envelope)(pos);
                sample * amp
            }
        }
    }
    pub fn envelope(self, envelope: impl Fn(Vector) -> f64 + Send + Sync + 'static) -> Self {
        Self::Envelope(Enveloped {
            envelope: Arc::new(envelope),
            node: Box::new(self),
        })
    }
    pub fn amplify(self, amp: f64) -> Self {
        self.envelope(move |_| amp)
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct Enveloped {
    pub envelope: Arc<dyn Fn(Vector) -> f64 + Send + Sync>,
    pub node: Box<Node>,
}

pub fn true_square_wave(time: f64, n: usize) -> f64 {
    let mut sum = 0.0;
    let k = time * TAU;
    for i in 1..=n {
        let n = (i as f64).mul_add(2.0, -1.0);
        sum += (n * k).sin() / n;
    }
    sum
}

pub fn true_saw_wave(time: f64, n: usize) -> f64 {
    let mut sum = 0.0;
    let k = time * TAU;
    for i in 1..=n {
        let n = i as f64;
        sum += (n * k).sin() / n;
    }
    sum * 2.0 / PI
}

pub fn true_triangle_wave(time: f64, n: usize) -> f64 {
    let mut sum = 0.0;
    let k = time * TAU;
    for i in 1..=n {
        let sign = if i % 2 == 0 { -1.0 } else { 1.0 };
        let n = (i as f64).mul_add(2.0, -1.0);
        sum += (n * k).sin() / n.powi(2) * sign;
    }
    sum * 8.0 / PI.powi(2)
}

pub fn kick_wave(time: f64, freq: f64, falloff: f64, period: f64) -> f64 {
    ((time % period).powf(falloff) * freq * TAU).sin()
}

pub fn switch2(time: f64, period: f64, a: impl Fn(f64) -> f64, b: impl Fn(f64) -> f64) -> f64 {
    let part_period = period / 2.0;
    if modulus(time, period) < part_period {
        a(modulus(time, part_period))
    } else {
        b(modulus(time, part_period))
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
