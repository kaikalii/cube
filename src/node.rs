use hodaun::{Mono, Shared, Source};

use crate::vector::{RectPrism, Vector};

pub enum Node {
    Wave(Wave3),
    Envelope(Enveloped),
}

impl Node {
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        match self {
            Node::Wave(wave) => {
                let sample = (wave.one_hz)(wave.pos);
                wave.pos += dir * (wave.freq / sample_rate);
                Vector::X * sample
            }
            Node::Envelope(adsr) => {
                let sample = adsr.node.sample(sample_rate, pos, dir);
                let amp = adsr
                    .rect
                    .tlf
                    .zip(adsr.rect.size)
                    .zip(pos)
                    .zip(adsr.envelope)
                    .map(|(((start, length), pos), env)| env.amplitude(start, length, pos));
                sample * amp
            }
        }
    }
    pub fn envolope(self, rect: RectPrism, envelope: Vector<BiEnvelope>) -> Self {
        Self::Envelope(Enveloped {
            envelope,
            rect,
            node: Box::new(self),
        })
    }
}

pub struct Wave3 {
    pub one_hz: Box<dyn Fn(Vector) -> f64 + Send + Sync>,
    pub freq: f64,
    pub pos: Vector,
}

impl Wave3 {
    pub fn new(freq: f64, one_hz: impl Fn(Vector) -> f64 + Send + Sync + 'static) -> Self {
        Self {
            one_hz: Box::new(one_hz),
            freq,
            pos: Vector::ZERO,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BiEnvelope {
    pub front_attack: f64,
    pub front_decay: f64,
    pub sustain: f64,
    pub back_attack: f64,
    pub back_decay: f64,
}

impl BiEnvelope {
    pub fn new(
        front_attack: f64,
        front_decay: f64,
        sustain: f64,
        back_attack: f64,
        back_decay: f64,
    ) -> Self {
        Self {
            front_attack,
            front_decay,
            sustain,
            back_attack,
            back_decay,
        }
    }
    pub fn symmetric(attack: f64, decay: f64, sustain: f64) -> Self {
        Self::new(attack, decay, sustain, attack, decay)
    }
    pub fn amplitude(&self, start: f64, length: f64, pos: f64) -> f64 {
        if pos < start {
            0.0
        } else {
            let attack_start = start + self.front_attack;
            if pos < attack_start {
                (pos - start) / self.front_attack
            } else if pos < attack_start + self.front_decay {
                (1.0 - (pos - start - self.front_attack) / self.front_decay) * (1.0 - self.sustain)
                    + self.sustain
            } else {
                let end = start + length;
                if pos < end - self.back_attack - self.back_decay {
                    self.sustain
                } else if pos < end - self.back_attack {
                    (1.0 - (end - self.back_attack - pos) / self.back_decay) * (1.0 - self.sustain)
                        + self.sustain
                } else if pos < end {
                    1.0 - (pos - end + self.back_attack) / self.back_attack
                } else {
                    0.0
                }
            }
        }
    }
}

pub struct Enveloped {
    pub envelope: Vector<BiEnvelope>,
    pub rect: RectPrism,
    pub node: Box<Node>,
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
