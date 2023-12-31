use bibe_instr as isa;
use isa::Encode;
use crate::asm::object::Object;
use crate::asm::emitter::{
	Emitter,
	EmitterTarget,
	Result
};
use crate::asm as asm;

use log::debug;

pub struct Annotated(Box<dyn EmitterTarget>);

impl Emitter for Annotated {
	fn emit_isa_instruction(&mut self, _object: &Object, addr: u64, instr: &isa::Instruction) -> Result<()> {
		let encoded = instr.encode();
		debug!("Encoded {:?} as {:08x}", instr, encoded);

		let a = (encoded & 0xff) as u8;
		let b = ((encoded >> 8) & 0xff) as u8;
		let c = ((encoded >> 16) & 0xff) as u8;
		let d = ((encoded >> 24) & 0xff) as u8;

		write!(self.0, "{:08x} {:02x} {:02x} {:02x} {:02x} {:?}\n", addr, a, b, c, d, instr).unwrap();
		Ok(())
	}

	fn emit_asm_directive(&mut self, _object: &Object, addr: u64, directive: &asm::Directive) -> Result<()> {
		write!(self.0, "{:08x} {:?}\n", addr, directive).unwrap();
		Ok(())
	}
}

pub fn create(target: Box<dyn EmitterTarget>) -> Option<Box<dyn Emitter>> {
	Some(Box::new(Annotated(target)))
}