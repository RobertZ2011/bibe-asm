/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
use crate::parser::StringID;
use crate::asm::object::Object;
use bibe_instr as isa;
use crate::asm as asm;

use std::collections::HashMap;
use std::fs::File;
use std::io::{
	Seek,
	Write
};

use log::debug;

use super::Immediate;

pub mod annotated;
pub mod bin;
pub mod hex;

#[derive(Clone, Copy, Debug)]
pub enum Error {
	UndefinedSymbol(StringID)
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait EmitterTarget: Seek + Write {}

impl EmitterTarget for File {}

fn resolve_immediate(symbols: &HashMap<StringID, u64>, addr: u64, imm: &Immediate) -> Result<i64> {
	if let Some(c) = imm.constant() {
		Ok(c as i64)
	} else {
		// Lookup the address of the symbol
		let id = imm.pc_rel().unwrap();
		let sym_addr = symbols.get(&id);
		if sym_addr.is_none() {
			return Err(Error::UndefinedSymbol(id))
		}

		//TODO: immediate overflow checking
		let sym_addr = *sym_addr.unwrap();
		debug!("sym_add {}, addr {}", sym_addr, addr);
		if sym_addr >= addr {
			Ok((sym_addr - addr) as i64)
		} else {
			Ok(-((addr - sym_addr) as i64))
		}
	}
}

pub fn link_instruction(symbols: &HashMap<StringID, u64>, addr: u64, instr: &asm::Instruction) -> Result<isa::Instruction> {
	match instr {
		asm::Instruction::Memory(mem) => Ok(isa::Instruction::Memory(*mem)),
		asm::Instruction::Rrr(rrr) => Ok(isa::Instruction::Rrr(*rrr)),
		asm::Instruction::Rri(rri_asm) => Ok(isa::Instruction::Rri(isa::rri::Instruction {
			op: rri_asm.op,
			cond: rri_asm.cond,
			dest: rri_asm.dest,
			src: rri_asm.src,
			imm: resolve_immediate(symbols, addr, &rri_asm.imm)? as i16,
		})),
		asm::Instruction::Csr(csr) => Ok(isa::Instruction::Csr(*csr)),
		asm::Instruction::Jump(jmp) => Ok(isa::Instruction::Jump(isa::jump::Instruction {
			imm: ((resolve_immediate(symbols, addr, &jmp.imm)? as u32) << 2) as i32,
		})),
	}
}

pub trait Emitter {
	fn emit_isa_instruction(&mut self, object: &Object, addr: u64, instr: &isa::Instruction) -> Result<()>;
	fn emit_asm_directive(&mut self, object: &Object, addr: u64, directive: &asm::Directive) -> Result<()>;

	fn emit_asm_instruction(&mut self, object: &Object, addr: u64, instr: &asm::Instruction) -> Result<isa::Instruction> {
		link_instruction(&object.symbols, addr, instr)
	}

	fn emit(&mut self, object: &Object) -> Result<()> {
		for (page_addr, page) in &object.pages {
			let mut skip_offset = 0;

			for statement in &page.statements {
				if statement.is_none() {
					skip_offset += 4;
					continue;
				}

				let (offset, statement) = statement.clone().unwrap();
				assert!(statement.size_of() != 0);

				let addr = page_addr + offset + skip_offset;
				use asm::Statement;
				match &statement {
					Statement::Instruction(instr) => {
						let isa_instr = self.emit_asm_instruction(object, addr, instr)?;
						self.emit_isa_instruction(object, addr, &isa_instr)?
					},
					Statement::Directive(directive) => self.emit_asm_directive(object, addr, directive)?,
				}
			}
		}

		return Ok(())
	}
}

pub fn create(name: &str, target: Box<dyn EmitterTarget>) -> Option<Box<dyn Emitter>> {
	match name {
		"annotated" => annotated::create(target),
		"bin" => bin::create(target),
		"hex" => hex::create(target),
		_ => None,
	}
}