use std::{
    f64::consts::{PI, TAU},
    fmt,
    sync::Arc,
};

use hodaun::{Mono, Shared, Source};
use rand::prelude::*;

use crate::vector::{modulus, Vector};

pub struct Env {
    pub sample_rate: f64,
    pub pos: Vector,
    pub dir: Vector,
    pub tempo: f64,
}

impl Env {
    pub fn beat_freq(&self) -> f64 {
        self.tempo / 60.0
    }
}

pub trait Node: fmt::Debug + Send + Sync + 'static {
    fn boxed(&self) -> NodeBox;
    fn sample(&mut self, env: &Env) -> Vector;
}

impl Node for f64 {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(constant_scalar_node(*self))
    }
    fn sample(&mut self, _: &Env) -> Vector {
        Vector::new(*self, *self, *self)
    }
}

pub struct NodeBox(Box<dyn Node>);

impl fmt::Debug for NodeBox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

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
    fn sample(&mut self, env: &Env) -> Vector {
        self.0.sample(env)
    }
}

#[derive(Clone)]
pub struct Wave3 {
    pub name: &'static str,
    pub one_hz: Arc<dyn Fn(Vector) -> Vector + Send + Sync>,
    pub freq: NodeBox,
    pub pos: Vector,
}

impl fmt::Debug for Wave3 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} hz {} wave", self.freq, self.name)
    }
}

impl Wave3 {
    pub fn new(
        name: &'static str,
        freq: impl Node + 'static,
        one_hz: impl Fn(Vector) -> Vector + Send + Sync + 'static,
    ) -> Self {
        Self {
            name,
            one_hz: Arc::new(one_hz),
            freq: freq.boxed(),
            pos: Vector::ZERO,
        }
    }
}

impl Node for Wave3 {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, env: &Env) -> Vector {
        let sample = (self.one_hz)(self.pos);
        self.pos += env.dir * (self.freq.sample(env) / env.sample_rate);
        sample
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
    fn sample(&mut self, env: &Env) -> Vector {
        let sample = self.node.sample(env);
        let amp = (self.envelope)(env.pos);
        sample * amp
    }
}

#[derive(Clone)]
pub struct GenericNode<F, S = ()> {
    pub name: String,
    pub state: S,
    pub f: F,
}

pub trait NodeFn<S>: Fn(&mut S, &Env) -> Vector + Clone + Send + Sync + 'static {}

impl<F, S> NodeFn<S> for F where F: Fn(&mut S, &Env) -> Vector + Clone + Send + Sync + 'static {}

pub fn constant_scalar_node(n: f64) -> GenericNode<impl NodeFn<()>> {
    GenericNode {
        name: n.to_string(),
        state: (),
        f: move |_: &mut (), _: &Env| Vector::splat(n),
    }
}

pub fn constant_vector_node(v: Vector) -> GenericNode<impl NodeFn<()>> {
    GenericNode {
        name: v.to_string(),
        state: (),
        f: move |_: &mut (), _: &Env| v,
    }
}

pub fn scalar_node<F>(name: impl Into<String>, f: F) -> GenericNode<impl NodeFn<()>>
where
    F: Fn(Vector) -> f64 + Clone + Send + Sync + 'static,
{
    GenericNode {
        name: name.into(),
        state: (),
        f: move |_: &mut (), env: &Env| Vector::splat(f(env.pos)),
    }
}

pub fn pure_node(
    name: impl Into<String>,
    f: impl Fn(&Env) -> Vector + Clone + Send + Sync + 'static,
) -> GenericNode<impl NodeFn<()>> {
    GenericNode {
        name: name.into(),
        state: (),
        f: move |_: &mut (), env: &Env| f(env),
    }
}

pub fn state_node<S>(
    name: impl Into<String>,
    state: S,
    f: impl NodeFn<S>,
) -> GenericNode<impl NodeFn<S>, S> {
    GenericNode {
        name: name.into(),
        state,
        f,
    }
}

impl<F, S> fmt::Debug for GenericNode<F, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl<F, S> Node for GenericNode<F, S>
where
    F: NodeFn<S>,
    S: Clone + Send + Sync + 'static,
{
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, env: &Env) -> Vector {
        (self.f)(&mut self.state, env)
    }
}

pub fn square_wave(time: f64) -> f64 {
    if modulus(time, 1.0) < 0.5 {
        1.0
    } else {
        -1.0
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

pub fn saw_wave(time: f64) -> f64 {
    1.0 - modulus(time, 1.0) * 2.0
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

pub fn triangle_wave(time: f64) -> f64 {
    4.0 * (time - (time + 0.5).floor()).abs() - 1.0
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

pub fn kick_wave(time: f64, period: f64, freq: f64, falloff: f64) -> f64 {
    ((time % period).powf(falloff) * freq * TAU).sin()
}

pub fn noise_node() -> GenericNode<impl NodeFn<SmallRng>, SmallRng> {
    state_node(
        "noise",
        SmallRng::from_entropy(),
        |rng: &mut SmallRng, _: &Env| Vector::new(rng.gen(), rng.gen(), rng.gen()),
    )
}

pub struct NodeSource {
    pub root: NodeBox,
    pub pos: Vector,
    pub dir: Shared<Vector>,
    pub tempo: f64,
}

impl Source for NodeSource {
    type Frame = Mono;
    fn next(&mut self, sample_rate: f64) -> Option<Self::Frame> {
        let dir = self.dir.get();
        let env = Env {
            sample_rate,
            pos: self.pos,
            dir,
            tempo: self.tempo,
        };
        let sample = self.root.sample(&env);
        self.pos += dir * (1.0 / sample_rate);
        Some(sample.reduce(|a, b| a + b))
    }
}
