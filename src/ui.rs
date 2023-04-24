use std::fs;

use eframe::egui::*;
use hodaun::*;

use crate::compile::compile;

pub struct App {
    output: OutputDeviceMixer<Stereo>,
    maintainer: Maintainer,
    error: Option<String>,
}

impl Default for App {
    fn default() -> Self {
        let mut output =
            OutputDeviceMixer::<Stereo>::with_default_device().unwrap_or_else(|e| panic!("{e}"));
        output.play().unwrap();
        App {
            output,
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
        let source = match compile(&input) {
            Ok(source) => source,
            Err(e) => {
                self.error = Some(e.to_string());
                return;
            }
        };
        self.error = None;
        self.maintainer = Maintainer::new();
        if let Some(source) = source {
            let source = source.maintained(&self.maintainer);
            self.output.add(source);
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &Context, _frame: &mut eframe::Frame) {
        CentralPanel::default().show(ctx, |ui| {
            if let Some(err) = &self.error {
                ui.label(RichText::new(err).color(Color32::RED));
            }
        });
    }
}
