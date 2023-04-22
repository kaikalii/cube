use std::fs;

use eframe::egui::*;
use hodaun::*;

use crate::{compile::compile, node::NodeSource, vector::Vector};

pub struct App {
    output: OutputDeviceMixer<Mono>,
    dir: Shared<Vector>,
    maintainer: Maintainer,
    error: Option<String>,
}

impl Default for App {
    fn default() -> Self {
        let mut output =
            OutputDeviceMixer::<Mono>::with_default_device().unwrap_or_else(|e| panic!("{e}"));
        output.play().unwrap();
        App {
            output,
            dir: Shared::new(Vector::X),
            maintainer: Maintainer::new(),
            error: None,
        }
    }
}

impl App {
    pub fn load(&mut self, path: &str) {
        let input = match fs::read_to_string(path) {
            Ok(input) => input,
            Err(e) => {
                self.error = Some(e.to_string());
                return;
            }
        };
        let cube = match compile(&input) {
            Ok(cube) => cube,
            Err(e) => {
                self.error = Some(e.to_string());
                return;
            }
        };
        self.error = None;
        self.maintainer = Maintainer::new();
        self.dir = Shared::new(cube.initial_dir);
        let source = NodeSource {
            root: cube.root,
            pos: cube.initial_pos,
            dir: self.dir.clone(),
            tempo: cube.tempo,
        }
        .maintained(&self.maintainer);
        self.output.add(source);
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        CentralPanel::default().show(ctx, |ui| {
            if let Some(err) = &self.error {
                ui.label(RichText::new(err).color(Color32::RED));
            }
            Grid::new("arrows").show(ui, |ui| {
                let mut arrow = |ui: &mut Ui, icon: &str, vector: Vector| {
                    let selected = self.dir.get() == vector;
                    if ui
                        .selectable_label(selected, RichText::new(icon).monospace())
                        .clicked()
                    {
                        self.dir.set(vector);
                    }
                };
                arrow(ui, "[-X  Y]", Vector::new(-1.0, 1.0, 0.0));
                arrow(ui, "[    Y]", Vector::new(0.0, 1.0, 0.0));
                arrow(ui, "[ X  Y]", Vector::new(1.0, 1.0, 0.0));
                ui.end_row();

                arrow(ui, "[-X   ]", Vector::new(-1.0, 0.0, 0.0));
                ui.label("");
                arrow(ui, "[ X   ]", Vector::new(1.0, 0.0, 0.0));
                ui.end_row();

                arrow(ui, "[-X -Y]", Vector::new(-1.0, -1.0, 0.0));
                arrow(ui, "[   -Y]", Vector::new(0.0, -1.0, 0.0));
                arrow(ui, "[ X -Y]", Vector::new(1.0, -1.0, 0.0));
            });
        });
    }
}
