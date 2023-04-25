use indexmap::IndexMap;

use crate::lex::Sp;

#[derive(Debug)]
pub struct File {
    pub items: Vec<Item>,
    pub sheet: Option<Sheet>,
}

#[derive(Debug)]
pub enum Item {
    Track(Track),
    Sequence(Sp<String>, SequenceValue),
}

#[derive(Debug)]
pub enum SequenceValue {
    Ident(Sp<String>),
    List(Vec<Sp<NumberValue>>),
}

#[derive(Debug)]
pub enum NumberValue {
    Ident(String),
    Number(f64),
}

#[derive(Debug)]
pub struct Sequence {
    pub value: SequenceValue,
    pub indices: Option<SequenceValue>,
}

#[derive(Debug)]
pub struct Track {
    pub sound: Sp<String>,
    pub perbeat: Sp<f64>,
    pub volume: Sp<f64>,
    pub selectors: Selectors,
}

pub type Selectors<T = Sequence> = IndexMap<String, SelectorValue<T>>;

#[derive(Debug)]
pub enum SelectorValue<T = Sequence> {
    Selectors(Selectors<T>),
    Sequence(T),
}

#[derive(Debug)]
pub struct Sheet {
    pub name: String,
    pub body: Option<SheetBody>,
}

#[derive(Debug)]
pub struct SheetBody {
    pub children: Vec<Sheet>,
}
