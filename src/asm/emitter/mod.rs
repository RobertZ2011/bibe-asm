/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
use crate::parser::StringID;
use crate::asm::object::Object;
use bibe_instr as isa;
use crate::asm as asm;

use std::fs::File;
use std::io::{
	Seek,
	Write
};

use log::debug;

mod annotated;
mod bin;
mod hex;

#[derive(Clone, Copy, Debug)]
pub enum Error {
	UndefinedSymbol(StringID)
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait EmitterTarget: Seek + Write {}

impl EmitterTarget for File {}

pub trait Emitter {
	fn emit_isa_instruction(&mut self, object: &Object, addr: u64, instr: &isa::Instruction) -> Result<()>;
	fn emit_asm_directive(&mut self, object: &Object, addr: u64, directive: &asm::Directive) -> Result<()>;

	fn emit_asm_instruction(&mut self, object: &Object, addr: u64, instr: &asm::Instruction) -> Result<isa::Instruction> {
		match instr {
			asm::Instruction::Memory(mem) => Ok(isa::Instruction::Memory(*mem)),
			asm::Instruction::Rrr(rrr) => Ok(isa::Instruction::Rrr(*rrr)),
			asm::Instruction::Rri(rri_asm) => {
				let imm = if let Some(c) = rri_asm.imm.constant() {
					c
				} else {
					// Lookup the address of the symbol
					let id = rri_asm.imm.pc_rel().unwrap();
					let sym_addr = object.symbols.get(&id);
					if sym_addr.is_none() {
						return Err(Error::UndefinedSymbol(id))
					}

					//TODO: immediate overflow checking
					let sym_addr = *sym_addr.unwrap();
					debug!("sym_add {}, addr {}", sym_addr, addr);
					if sym_addr >= addr {
						 (sym_addr - addr) as i16
					} else {
						-((addr - sym_addr) as i16)
					}
				};

				debug!("Imm {}", imm);

				Ok(isa::Instruction::Rri(isa::rri::Instruction {
					op: rri_asm.op,
					cond: rri_asm.cond,
					dest: rri_asm.dest,
					src: rri_asm.src,
					imm: imm << rri_asm.imm_shl,
				}))
			},
			asm::Instruction::Csr(csr) => Ok(isa::Instruction::Csr(*csr)),
		}
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