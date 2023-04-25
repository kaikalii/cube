use std::{collections::HashMap, f64::consts::TAU, fmt};

use hodaun::{Letter, Mixer, Octave, Source, Stereo};

use crate::{
    ast::{self, Sheet, SheetBody},
    lex::*,
    node::*,
    parse::*,
};

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    InvalidIndex,
    IndexOutOfBounds { index: usize, len: usize },
    UnknownSequence(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::Parse(e) => write!(f, "{e}"),
            CompileError::InvalidIndex => write!(f, "Indices must all be natural numbers"),
            CompileError::IndexOutOfBounds { index, len } => {
                write!(f, "Index out of bounds: {index} >= {len}")
            }
            CompileError::UnknownSequence(s) => write!(f, "Unknown sequence: {s}"),
        }
    }
}

pub type CompileResult<T = ()> = Result<T, Sp<CompileError>>;

pub struct Song {
    pub root: Mixer<Stereo>,
}

pub fn compile(input: &str) -> CompileResult<Option<impl Source<Frame = Stereo>>> {
    let file = parse(input).map_err(|e| e.map(CompileError::Parse))?;
    let mut compiler = Compiler {
        sequences: HashMap::new(),
        tracks: Vec::new(),
    };
    for item in file.items {
        compiler.item(item)?;
    }
    file.sheet
        .map(|sheet| {
            compiler
                .sheet(sheet)
                .map(|sheet| SongSource::new(sheet, compiler.tracks))
        })
        .transpose()
}

struct Compiler {
    sequences: HashMap<String, Vec<Sp<f64>>>,
    tracks: Vec<Track>,
}

impl Compiler {
    fn item(&mut self, item: ast::Item) -> CompileResult {
        match item {
            ast::Item::Track(track) => self.track(track)?,
            ast::Item::Sequence(name, val) => {
                let seq = self.sequence_value(val)?;
                self.sequences.insert(name.value, seq);
            }
        }
        Ok(())
    }
    fn sequence(&self, sequence: ast::Sequence) -> CompileResult<Vec<f64>> {
        let values: Vec<f64> = self
            .sequence_value(sequence.value)?
            .into_iter()
            .map(|v| v.value)
            .collect();
        Ok(if let Some(indices) = sequence.indices {
            let indices = self.sequence_value(indices)?;
            let mut selected = Vec::with_capacity(indices.len());
            for index in indices {
                let span = index.span;
                let index = index.value;
                if index < 0.0 || index.fract() != 0.0 {
                    return Err(span.sp(CompileError::InvalidIndex));
                }
                let index = index as usize;
                if index >= values.len() {
                    return Err(span.sp(CompileError::IndexOutOfBounds {
                        index,
                        len: values.len(),
                    }));
                }
                selected.push(values[index]);
            }
            selected
        } else {
            values
        })
    }
    fn sequence_value(&self, val: ast::SequenceValue) -> CompileResult<Vec<Sp<f64>>> {
        Ok(match val {
            ast::SequenceValue::Ident(ident) => {
                if let Some(seq) = self.sequences.get(&ident.value) {
                    seq.clone()
                } else {
                    return Err(ident.span.sp(CompileError::UnknownSequence(ident.value)));
                }
            }
            ast::SequenceValue::List(list) => list
                .into_iter()
                .map(|val| self.number_value(val))
                .collect::<CompileResult<_>>()?,
        })
    }
    fn number_value(&self, val: Sp<ast::NumberValue>) -> CompileResult<Sp<f64>> {
        Ok(val.span.sp(match val.value {
            ast::NumberValue::Ident(ident) => {
                if let Some((letter, octave)) = parse_note(&ident) {
                    letter.frequency(octave)
                } else {
                    todo!("{ident}")
                }
            }
            ast::NumberValue::Number(num) => num,
        }))
    }
    fn track(&mut self, track: ast::Track) -> CompileResult {
        let sound = track.sound.value;
        let perbeat = track.perbeat.value;
        let selectors = self.selectors(track.selectors)?;
        let track = Track {
            sound,
            perbeat,
            selectors,
        };
        self.tracks.push(track);
        Ok(())
    }
    fn selectors(&self, selectors: ast::Selectors) -> CompileResult<Selectors> {
        let mut resolved = Selectors::new();
        for (key, value) in selectors {
            let value = self.selector_value(value)?;
            resolved.insert(key, value);
        }
        Ok(resolved)
    }
    fn selector_value(&self, selector_value: ast::SelectorValue) -> CompileResult<SelectorValue> {
        Ok(match selector_value {
            ast::SelectorValue::Selectors(selectors) => {
                SelectorValue::Selectors(self.selectors(selectors)?)
            }
            ast::SelectorValue::Sequence(seq) => SelectorValue::Sequence(self.sequence(seq)?),
        })
    }
    fn sheet(&self, sheet: ast::Sheet) -> CompileResult<Sheet> {
        Ok(Sheet {
            name: sheet.name,
            body: sheet.body.map(|body| self.sheet_body(body)).transpose()?,
        })
    }
    fn sheet_body(&self, body: ast::SheetBody) -> CompileResult<SheetBody> {
        let children = body
            .children
            .into_iter()
            .map(|child| self.sheet(child))
            .collect::<CompileResult<Vec<Sheet>>>()?;
        Ok(SheetBody { children })
    }
}

type Selectors = ast::Selectors<Vec<f64>>;
type SelectorValue = ast::SelectorValue<Vec<f64>>;

#[derive(Debug)]
struct Track {
    sound: String,
    perbeat: f64,
    selectors: Selectors,
}

fn parse_note(name: &str) -> Option<(Letter, Octave)> {
    if !name.ends_with(|c: char| c.is_ascii_digit()) {
        return None;
    };
    let octave: Octave = name[name.len() - 1..].parse().unwrap();
    let letter = &name[..name.len() - 1];
    let letter = match letter {
        "A" => Letter::A,
        "Ab" => Letter::Ab,
        "A#" => Letter::Ash,
        "B" => Letter::B,
        "Bb" => Letter::Bb,
        "C" => Letter::C,
        "C#" => Letter::Csh,
        "D" => Letter::D,
        "Db" => Letter::Db,
        "D#" => Letter::Dsh,
        "E" => Letter::E,
        "Eb" => Letter::Eb,
        "F" => Letter::F,
        "F#" => Letter::Fsh,
        "G" => Letter::G,
        "Gb" => Letter::Gb,
        "G#" => Letter::Gsh,
        _ => return None,
    };
    Some((letter, octave))
}

struct ResolvedTrack {
    one_hz: Box<dyn FnMut(f64) -> f64 + Send + Sync + 'static>,
    notes: Vec<f64>,
    perbeat: f64,
    time: f64,
    wave_time: f64,
}

pub struct SongSource {
    tracks: Vec<ResolvedTrack>,
}

impl SongSource {
    fn new(sheet: Sheet, tracks: Vec<Track>) -> Self {
        Self {
            tracks: resolve_tracks(&sheet, tracks),
        }
    }
}

impl Source for SongSource {
    type Frame = Stereo;
    fn next(&mut self, sample_rate: f64) -> Option<Self::Frame> {
        let mut frame = Stereo::ZERO;
        let tempo = 120.0;
        for track in &mut self.tracks {
            if track.notes.is_empty() {
                continue;
            }
            let note_length = 60.0 / tempo / track.perbeat;
            let index = (track.time / note_length) as usize;
            let freq = track.notes[index % track.notes.len()];
            let sample = (track.one_hz)(track.wave_time);
            frame += Stereo::both(sample);
            track.wave_time += freq / sample_rate;
            track.time += 1.0 / sample_rate;
        }
        Some(frame)
    }
}

fn resolve_tracks(sheet: &Sheet, tracks: Vec<Track>) -> Vec<ResolvedTrack> {
    let mut resolved = Vec::with_capacity(tracks.len());
    for track in &tracks {
        let one_hz: Box<dyn FnMut(f64) -> f64 + Send + Sync + 'static> = match track.sound.as_str()
        {
            "square" => Box::new(square_wave),
            "saw" => Box::new(saw_wave),
            "tri" => Box::new(triangle_wave),
            _ => Box::new(|t| (t * TAU).sin()),
        };
        resolved.push(ResolvedTrack {
            one_hz,
            notes: Vec::new(),
            time: 0.0,
            wave_time: 0.0,
            perbeat: track.perbeat,
        });
    }
    resolve_tracks_impl(Vec::new(), sheet, &tracks, &mut resolved);
    resolved
}

fn resolve_tracks_impl(
    mut path: Vec<String>,
    sheet: &Sheet,
    tracks: &[Track],
    resolved: &mut [ResolvedTrack],
) {
    path.push(sheet.name.clone());
    for (track, resolved) in tracks.iter().zip(&mut *resolved) {
        if let Some(seq) = get_sequence(&path, &track.selectors) {
            resolved.notes.extend_from_slice(seq);
        }
    }
    if let Some(body) = &sheet.body {
        for child in &body.children {
            resolve_tracks_impl(path.clone(), child, tracks, resolved);
        }
    }
}

fn get_sequence<'a>(path: &[String], selectors: &'a Selectors) -> Option<&'a [f64]> {
    let (head, tail) = path.split_first()?;
    match selectors.get(head)? {
        ast::SelectorValue::Selectors(selectors) => get_sequence(tail, selectors),
        ast::SelectorValue::Sequence(seq) => Some(seq),
    }
}
