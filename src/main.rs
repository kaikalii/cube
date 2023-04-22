mod builtin;
mod compile;
mod lex;
mod node;
mod ui;
mod value;
mod vector;

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
