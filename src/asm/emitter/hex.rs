use std::fs::File;
use crate::emitter::Emitter;

pub struct Hex {
    origin: u32,
    file: File,
}

impl Hex {
    pub fn new(file: File) -> Hex {
        Hex {
            origin: 0,
            file: file,
        }
    }
}

impl Emitter for Hex {
    fn emit_instruction(&mut self, instruction: &Instruction) {

    }
}