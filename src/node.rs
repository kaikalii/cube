use std::{
    f64::consts::{PI, TAU},
    fmt::{self, Debug},
    sync::Arc,
};

use hodaun::{Mono, Shared, Source};

use crate::vector::{modulus, Vector};

pub trait Node: Debug + Send + Sync {
    fn boxed(&self) -> NodeBox;
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector;
}

#[derive(Debug)]
pub struct NodeBox(Box<dyn Node>);

impl NodeBox {
    pub fn new(node: impl Node + 'static) -> Self {
        Self(Box::new(node))
    }
}

impl Clone for NodeBox {
    fn clone(&self) -> Self {
        self.0.boxed()
    }
}

impl Node for NodeBox {
    fn boxed(&self) -> NodeBox {
        self.clone()
    }
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        self.0.sample(sample_rate, pos, dir)
    }
}

#[derive(Clone)]
pub struct Wave3 {
    pub name: &'static str,
    pub one_hz: Arc<dyn Fn(Vector) -> f64 + Send + Sync>,
    pub freq: f64,
    pub pos: Vector,
}

impl fmt::Debug for Wave3 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} hz {} wave", self.freq, self.name)
    }
}

impl Wave3 {
    pub fn new(
        name: &'static str,
        freq: f64,
        one_hz: impl Fn(Vector) -> f64 + Send + Sync + 'static,
    ) -> Self {
        Self {
            name,
            one_hz: Arc::new(one_hz),
            freq,
            pos: Vector::ZERO,
        }
    }
}

impl Node for Wave3 {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, sample_rate: f64, _pos: Vector, dir: Vector) -> Vector {
        let sample = (self.one_hz)(self.pos);
        self.pos += dir * (self.freq / sample_rate);
        Vector::X * sample
    }
}

#[derive(Clone)]
pub struct Enveloped {
    pub name: &'static str,
    pub envelope: Arc<dyn Fn(Vector) -> f64 + Send + Sync>,
    pub node: NodeBox,
}

impl fmt::Debug for Enveloped {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} envelope {:?}", self.name, self.node)
    }
}

impl Node for Enveloped {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        let sample = self.node.sample(sample_rate, pos, dir);
        let amp = (self.envelope)(pos);
        sample * amp
    }
}

#[derive(Clone)]
pub struct GenericNode<F, S = ()> {
    pub name: String,
    pub state: S,
    pub f: F,
}

impl<F> GenericNode<F> {
    pub fn new(name: impl Into<String>, f: F) -> Self {
        Self {
            name: name.into(),
            state: (),
            f,
        }
    }
}

impl<F> GenericNode<F> {
    pub fn simple(
        name: impl Into<String>,
        f: F,
    ) -> GenericNode<impl Fn(&mut (), f64, Vector, Vector) -> Vector + Clone + Send + Sync + 'static>
    where
        F: Fn(Vector) -> f64 + Clone + Send + Sync + 'static,
    {
        GenericNode {
            name: name.into(),
            state: (),
            f: move |_: &mut (), _, pos, _| Vector::X * f(pos),
        }
    }
}

pub fn state_node<S>(
    name: impl Into<String>,
    state: S,
    f: impl Fn(&mut S, f64, Vector, Vector) -> Vector + Clone + Send + Sync + 'static,
) -> GenericNode<impl Fn(&mut S, f64, Vector, Vector) -> Vector + Clone + Send + Sync + 'static, S>
{
    GenericNode {
        name: name.into(),
        state,
        f,
    }
}

impl<F, S> fmt::Debug for GenericNode<F, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} node", self.name)
    }
}

impl<F, S> Node for GenericNode<F, S>
where
    F: Fn(&mut S, f64, Vector, Vector) -> Vector + Clone + Send + Sync + 'static,
    S: Clone + Send + Sync + 'static,
{
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, sample_rate: f64, pos: Vector, dir: Vector) -> Vector {
        (self.f)(&mut self.state, sample_rate, pos, dir)
    }
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
    pub root: NodeBox,
}

impl NodeSource {
    pub fn new(root: impl Node + 'static, pos: Vector, dir: impl Into<Shared<Vector>>) -> Self {
        Self {
            pos,
            dir: dir.into(),
            root: NodeBox::new(root),
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
