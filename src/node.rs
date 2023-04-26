use std::{
    collections::VecDeque,
    f64::consts::{PI, TAU},
    fmt,
    sync::Arc,
};

use hodaun::{Shared, Source, Stereo};
use rand::prelude::*;

use crate::modulus;

pub struct Env<'a> {
    pub sample_rate: f64,
    pub time: f64,
    pub dir: f64,
    pub tempo: &'a mut f64,
}

impl<'a> Env<'a> {
    pub fn beat_freq(&self) -> f64 {
        *self.tempo / 60.0
    }
}

pub trait Node: fmt::Debug + Send + Sync + 'static {
    fn boxed(&self) -> NodeBox;
    fn sample(&mut self, env: &mut Env) -> Stereo;
}

impl Node for f64 {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(*self)
    }
    fn sample(&mut self, _: &mut Env) -> Stereo {
        Stereo::both(*self)
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
    fn sample(&mut self, env: &mut Env) -> Stereo {
        self.0.sample(env)
    }
}

#[derive(Clone)]
pub struct Wave3 {
    pub name: &'static str,
    pub one_hz: Arc<dyn Fn(f64) -> f64 + Send + Sync>,
    pub freq: NodeBox,
    pub time: f64,
}

impl fmt::Debug for Wave3 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} hz {} wave", self.freq, self.name)
    }
}

impl Wave3 {
    pub fn new(
        name: &'static str,
        freq: impl Node,
        one_hz: impl Fn(f64) -> f64 + Send + Sync + 'static,
    ) -> Self {
        Self {
            name,
            one_hz: Arc::new(one_hz),
            freq: freq.boxed(),
            time: 0.0,
        }
    }
}

impl Node for Wave3 {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, env: &mut Env) -> Stereo {
        let mut sample = Stereo::ZERO;
        let freq = self.freq.sample(env).average();
        sample += (self.one_hz)(self.time);
        self.time += env.dir * (freq / env.sample_rate);
        sample
    }
}

#[derive(Debug, Clone)]
pub struct LowPass {
    pub cutoff: NodeBox,
    pub source: NodeBox,
    pub acc: Option<Stereo>,
}

impl LowPass {
    pub fn new(cutoff: NodeBox, source: NodeBox) -> Self {
        Self {
            cutoff,
            source,
            acc: None,
        }
    }
}

impl Node for LowPass {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, env: &mut Env) -> Stereo {
        let cutoff = self.cutoff.sample(env).average();
        let sample = self.source.sample(env);
        if let Some(acc) = &mut self.acc {
            let t = (cutoff / env.sample_rate).min(1.0);
            *acc = acc.with(sample, |a, b| (b - a) * t + a);
            *acc
        } else {
            self.acc = Some(sample);
            sample
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reverb {
    pub period: NodeBox,
    pub n: NodeBox,
    pub source: NodeBox,
    pub reflections: Vec<VecDeque<Stereo>>,
}

impl Reverb {
    pub fn new(period: NodeBox, n: NodeBox, source: NodeBox) -> Self {
        Self {
            period,
            n,
            source,
            reflections: vec![],
        }
    }
}

impl Node for Reverb {
    fn boxed(&self) -> NodeBox {
        NodeBox::new(self.clone())
    }
    fn sample(&mut self, env: &mut Env) -> Stereo {
        let period = self.period.sample(env).average().abs();
        let n = self.n.sample(env).average().max(0.0).round() as usize;
        if self.reflections.len() < n {
            self.reflections.push(VecDeque::new());
        }
        if self.reflections.len() > n {
            self.reflections.pop();
        }
        let mut sample = self.source.sample(env);
        let len = (period * env.sample_rate).round() as usize;
        let mut prev = sample;
        for (i, refl) in self.reflections.iter_mut().enumerate() {
            refl.push_back(prev);
            let mut front = Stereo::ZERO;
            while refl.len() > len {
                front = refl.pop_front().unwrap();
            }
            sample += front / 2u64.saturating_pow(i as u32 + 1) as f64;
            prev = refl.get(len / 10).copied().unwrap_or_default();
        }
        sample
    }
}

#[derive(Clone)]
pub struct GenericNode<F, S = ()> {
    pub name: String,
    pub state: S,
    pub f: F,
}

pub trait NodeFn<S = ()>: Fn(&mut S, &mut Env) -> Stereo + Clone + Send + Sync + 'static {}

impl<F, S> NodeFn<S> for F where F: Fn(&mut S, &mut Env) -> Stereo + Clone + Send + Sync + 'static {}

pub fn pure_node<F>(name: impl Into<String>, f: F) -> GenericNode<impl NodeFn>
where
    F: Fn(&mut Env) -> Stereo + Clone + Send + Sync + 'static,
{
    GenericNode {
        name: name.into(),
        state: (),
        f: move |_: &mut (), env: &mut Env| f(env),
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
    fn sample(&mut self, env: &mut Env) -> Stereo {
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

pub fn noise_node() -> GenericNode<impl NodeFn> {
    pure_node("noise", |env: &mut Env| {
        Stereo::both(SmallRng::seed_from_u64(env.time.to_bits()).gen())
    })
}

pub struct NodeSource {
    pub root: NodeBox,
    pub time: Shared<f64>,
    pub dir: Shared<f64>,
    pub tempo: f64,
}

impl Source for NodeSource {
    type Frame = Stereo;
    fn next(&mut self, sample_rate: f64) -> Option<Self::Frame> {
        let dir = self.dir.get();
        let mut env = Env {
            sample_rate,
            time: self.time.get(),
            dir,
            tempo: &mut self.tempo,
        };
        let sample = self.root.sample(&mut env);
        self.time.with(|time| *time += dir * (1.0 / sample_rate));
        Some(sample)
    }
}
