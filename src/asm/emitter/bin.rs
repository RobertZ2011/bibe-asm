use std::io::{
	Seek,
	SeekFrom,
	Write,
};

use bibe_instr::{
	self as isa,
	Encode,
};

use crate::asm::{
	self as asm,
	object::Object
};
use crate::asm::emitter::{
	Emitter,
	EmitterTarget,
	Result,
};

pub struct Bin(Box<dyn EmitterTarget>);

impl Emitter for Bin {
	fn emit_isa_instruction(&mut self, _object: &Object, addr: u64, instr: &isa::Instruction) -> Result<()> {
		let encoded = instr.encode();

		// TODO: handle endianess
		let a = (encoded & 0xff) as u8;
		let b = ((encoded >> 8) & 0xff) as u8;
		let c = ((encoded >> 16) & 0xff) as u8;
		let d = ((encoded >> 24) & 0xff) as u8;
		self.0.seek(SeekFrom::Start(addr)).unwrap();
		self.0.write(&[a, b, c, d]).unwrap();

		Ok(())
	}

	fn emit_asm_directive(&mut self, _object: &Object, _addr: u64, directive: &asm::Directive) -> Result<()> {
		// TODO: handle endianess
		use asm::Directive;
		match directive {
			Directive::Byte(_, v) => self.0.write(&[*v]),
			Directive::Short(_, v) => self.0.write(&[
				(v & 0xff) as u8, 
				((v >> 8) & 0xff) as u8
			]),
			Directive::Word(_, v) => self.0.write(&[
				(v & 0xff) as u8, 
				((v >> 8) & 0xff) as u8,
				((v >> 16) & 0xff) as u8,
				((v >> 24) & 0xff) as u8
			]),
			Directive::Quad(_, v) => self.0.write(&[
				(v & 0xff) as u8, 
				((v >> 8) & 0xff) as u8,
				((v >> 16) & 0xff) as u8,
				((v >> 24) & 0xff) as u8,
				((v >> 32) & 0xff) as u8,
				((v >> 40) & 0xff) as u8,
				((v >> 48) & 0xff) as u8,
				((v >> 56) & 0xff) as u8,
			]),

			_ => unimplemented!()
		}.unwrap();

		Ok(())
	}
}

pub fn create(target: Box<dyn EmitterTarget>) -> Option<Box<dyn Emitter>> {
	Some(Box::new(Bin(target)))
}