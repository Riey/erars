mod console;

#[allow(unused)]
mod fbs {
    include!(concat!(env!("OUT_DIR"), "/ui_generated.rs"));

    impl Eq for Color {}
}

pub use fbs::Color;

pub use console::VirtualConsole;
