use crate::asm::{
	Directive,
	object::Object,
	emitter::{
		Emitter,
		EmitterTarget,
		Result
	}
};

use bibe_instr as isa;
use isa::Encode;

pub struct Hex(Box<dyn EmitterTarget>);

impl Emitter for Hex {
	fn emit_isa_instruction(&mut self, _obj: &Object, _addr: u64, instr: &isa::Instruction) -> Result<()> {
		let encoded = instr.encode();

		write!(self.0, "{encoded:08x} ").unwrap();
		Ok(())
	}

	fn emit_asm_directive(&mut self, _obj: &Object, _addr: u64, directive: &Directive) -> Result<()> {
		match directive {
			Directive::Byte(v) => write!(self.0, "{v:02x} "),
			Directive::Short(v) => write!(self.0, "{v:04x} "),
			Directive::Word(v) => write!(self.0, "{v:08x} "),
			Directive::Quad(v) => write!(self.0, "{v:016x} "),

			Directive::Bytes(v) => {
				for b in v {
					write!(self.0, "{b:02x} ").unwrap();
				}

				Ok(())
			},

			_ => Ok(()),
		}.unwrap();

		Ok(())
	}
}

pub fn create(target: Box<dyn EmitterTarget>) -> Option<Box<dyn Emitter>> {
	Some(Box::new(Hex(target)))
}