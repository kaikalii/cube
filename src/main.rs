mod builtin;
mod compile;
mod lex;
mod node;
mod value;

use std::{
    fs,
    ops::{Add, Rem},
    path::Path,
    sync::mpsc::channel,
};

use hodaun::{Maintainer, Mix, OutputDeviceMixer, Shared, Source, Stereo};
use node::NodeSource;
use notify::{Event, EventKind, RecursiveMode, Watcher};

fn main() {
    let mut output =
        OutputDeviceMixer::<Stereo>::with_default_device().unwrap_or_else(|e| panic!("{e}"));

    let (send, recv) = channel();

    let compile = move |path: &Path| {
        let input = fs::read_to_string(path).unwrap_or_default();
        match compile::compile(&input) {
            Ok(compiled) => _ = send.send(compiled),
            Err(e) => println!("{e}"),
        }
    };

    let mut watcher = notify::recommended_watcher(move |res: Result<Event, _>| match res {
        Ok(event) => {
            if let EventKind::Modify(_) = event.kind {
                if event.paths[0]
                    .extension()
                    .map_or(false, |ext| ext == "cube")
                {
                    compile(&event.paths[0])
                }
            }
        }
        Err(e) => println!("watch error: {e}"),
    })
    .unwrap();

    watcher
        .watch(".".as_ref(), RecursiveMode::Recursive)
        .unwrap();

    let mut maintainer;
    let mut time: Option<Shared<f64>> = None;

    for compiled in recv {
        maintainer = Maintainer::<f64>::new();
        let time = time
            .get_or_insert_with(|| compiled.initial_time.into())
            .clone();
        let source = NodeSource {
            root: compiled.root,
            time,
            dir: 1.0.into(),
            tempo: compiled.tempo,
        }
        .maintained(&maintainer);
        output.add(source);
        output.play().unwrap();
    }
}

pub fn modulus<T, M>(a: T, m: M) -> <<<T as Rem<M>>::Output as Add<M>>::Output as Rem<M>>::Output
where
    M: Copy,
    T: Rem<M>,
    <T as Rem<M>>::Output: Add<M>,
    <<T as Rem<M>>::Output as Add<M>>::Output: Rem<M>,
{
    (a % m + m) % m
}
