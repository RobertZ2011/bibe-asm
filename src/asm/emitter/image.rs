use std::{
	fs::File,
	io::Write,
};
use bibe_instr::{
	Encode,
	Instruction,
};

use log::debug;

pub fn emit_instruction(file: &mut File, _addr: u32, instruction: &Instruction) {
	let encoded = instruction.encode();
	debug!("Encoded {:?} as {:08x}", instruction, encoded);

	let a = (encoded & 0xff) as u8;
	let b = ((encoded >> 8) & 0xff) as u8;
	let c = ((encoded >> 16) & 0xff) as u8;
	let d = ((encoded >> 24) & 0xff) as u8;
	file.write(&[a, b, c, d]).unwrap();
}