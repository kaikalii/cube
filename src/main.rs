mod builtin;
mod compile;
mod lex;
mod node;
mod ui;
mod value;

use std::ops::{Add, Rem};

use ui::App;

fn main() {
    let mut app = App::default();
    app.load("test.cube");
    eframe::run_native(
        "Cube",
        eframe::NativeOptions {
            ..Default::default()
        },
        Box::new(|cc| {
            cc.egui_ctx.set_pixels_per_point(3.0);
            Box::new(app)
        }),
    )
    .unwrap();
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
