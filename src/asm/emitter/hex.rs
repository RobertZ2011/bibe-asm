use crate::asm::emitter::{
	Emitter,
	EmitterTarget,
	Result
};

use bibe_instr as isa;
use isa::Encode;

pub struct Hex(Box<dyn EmitterTarget>);

impl Emitter for Hex {
	fn emit_isa_instruction(&mut self, _: &crate::asm::object::Object, _: u64, instr: &isa::Instruction) -> Result<()> {
		let encoded = instr.encode();

		write!(self.0, "{encoded:08x} ").unwrap();
		Ok(())
	}

	fn emit_asm_directive(&mut self, _: &crate::asm::object::Object, _: u64, _: &crate::asm::Directive) -> Result<()> {
		// TODO
		Ok(())
	}
}

pub fn create(target: Box<dyn EmitterTarget>) -> Option<Box<dyn Emitter>> {
	Some(Box::new(Hex(target)))
}