use std::{
    collections::VecDeque,
    f64::consts::{PI, TAU},
    fmt,
};

use hodaun::{lerp, Shared, Source, Stereo};
use rand::prelude::*;

use crate::modulus;

pub struct Env<'a> {
    pub sample_rate: f64,
    pub time: f64,
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

pub trait IntoNode {
    type Node: Node;
    fn into_node(self) -> Self::Node;
}

impl<N: Node> IntoNode for N {
    type Node = N;
    fn into_node(self) -> Self::Node {
        self
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

pub fn wave_node<N, F>(
    name: impl Into<String>,
    freq: N,
    one_hz: F,
) -> GenericNode<impl NodeFn<(f64, N::Node)>, (f64, N::Node)>
where
    F: Fn(f64) -> f64 + Clone + Send + Sync + 'static,
    N: IntoNode,
{
    state_node(name, (0.0, freq.into_node()), move |(time, freq), env| {
        let freq = freq.sample(env).average();
        if *time == 0.0 && freq != 0.0 {
            *time = env.time * freq;
        }
        let mut sample = Stereo::ZERO;
        sample += one_hz(*time);
        *time += freq / env.sample_rate;
        sample
    })
}

pub fn harmonic_wave_node<H, N, F>(
    name: impl Into<String>,
    harmonics: H,
    freq: N,
    one_hz: F,
) -> GenericNode<impl NodeFn<(f64, H::Node, N::Node)>, (f64, H::Node, N::Node)>
where
    H: IntoNode,
    N: IntoNode,
    F: Fn(f64, f64) -> f64 + Clone + Send + Sync + 'static,
{
    state_node(
        name,
        (0.0, harmonics.into_node(), freq.into_node()),
        move |(time, harmonics, freq), env| {
            let freq = freq.sample(env).average();
            if *time == 0.0 && freq != 0.0 {
                *time = env.time * freq;
            }
            let harmonics = harmonics.sample(env).average().abs();
            let mut sample = Stereo::ZERO;
            sample += one_hz(*time, harmonics);
            *time += freq / env.sample_rate;
            sample
        },
    )
}

pub fn timed_node(
    pairs: impl IntoIterator<Item = (NodeBox, NodeBox)>,
) -> GenericNode<impl NodeFn<Vec<(NodeBox, NodeBox)>>, Vec<(NodeBox, NodeBox)>> {
    let pairs: Vec<_> = pairs.into_iter().collect();
    state_node("timed", pairs, move |pairs, env| {
        let durations: Vec<f64> = pairs
            .iter_mut()
            .map(|(_, dur)| dur.sample(env).average().abs())
            .collect();
        let duration: f64 = durations.iter().sum();
        let mut time = env.time % duration;
        for ((node, _), dur) in pairs.iter_mut().zip(durations) {
            if time < dur {
                return node.sample(env);
            }
            time -= dur;
        }
        unreachable!()
    })
}

pub fn automated_node(
    pairs: impl IntoIterator<Item = (NodeBox, NodeBox)>,
) -> GenericNode<impl NodeFn<Vec<(NodeBox, NodeBox)>>, Vec<(NodeBox, NodeBox)>> {
    let pairs: Vec<_> = pairs.into_iter().collect();
    state_node("automated", pairs, move |pairs, env| {
        let durations: Vec<f64> = pairs
            .iter_mut()
            .map(|(_, dur)| dur.sample(env).average().abs())
            .collect();
        let duration: f64 = durations.iter().sum();
        let mut time = env.time % duration;
        let mut index = 0;
        let mut t = 0.0;
        for (i, dur) in durations.into_iter().enumerate() {
            if time < dur {
                index = i;
                t = time / dur;
                break;
            }
            time -= dur;
        }
        let j = (index + 1) % pairs.len();
        let a = pairs[index].0.sample(env);
        let b = pairs[j].0.sample(env);
        a.with(b, |a, b| lerp(a, b, t))
    })
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
            prev = refl.get(len).copied().unwrap_or_default();
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

pub fn harmonic_square_wave(time: f64, mut harmonics: f64) -> f64 {
    let mut sum = 0.0;
    let k = time * TAU;
    for i in 1..=harmonics.ceil() as usize {
        let n = (i as f64).mul_add(2.0, -1.0);
        sum += harmonics.min(1.0) * (n * k).sin() / n;
        harmonics -= 1.0;
    }
    sum
}

pub fn saw_wave(time: f64) -> f64 {
    1.0 - modulus(time, 1.0) * 2.0
}

pub fn curve_wave(time: f64, falloff: f64) -> f64 {
    1.0 - modulus(time, 1.0).powf(1.0 / falloff) * 2.0
}

pub fn harmonic_saw_wave(time: f64, mut harmonics: f64) -> f64 {
    let mut sum = 0.0;
    let k = time * TAU;
    for i in 1..=harmonics.ceil() as usize {
        let n = i as f64;
        sum += harmonics.min(1.0) * (n * k).sin() / n;
        harmonics -= 1.0;
    }
    sum * 2.0 / PI
}

pub fn triangle_wave(time: f64) -> f64 {
    4.0 * (time - (time + 0.5).floor()).abs() - 1.0
}

pub fn harmonic_triangle_wave(time: f64, mut harmonics: f64) -> f64 {
    let mut sum = 0.0;
    let k = time * TAU;
    for i in 1..=harmonics.ceil() as usize {
        let sign = if i % 2 == 0 { -1.0 } else { 1.0 };
        let n = (i as f64).mul_add(2.0, -1.0);
        sum += harmonics.min(1.0) * (n * k).sin() / n.powi(2) * sign;
        harmonics -= 1.0;
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
            tempo: &mut self.tempo,
        };
        let sample = self.root.sample(&mut env);
        self.time.with(|time| *time += dir * (1.0 / sample_rate));
        Some(sample)
    }
}
