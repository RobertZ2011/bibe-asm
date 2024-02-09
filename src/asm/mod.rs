use bibe_instr as isa;

use crate::parser::string_table::StringID;

pub mod emitter;
pub mod object;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Immediate {
	Constant(i64),
	PcRel(StringID),
}

impl Immediate {
	pub fn constant(&self) -> Option<i16> {
		match self {
			Immediate::Constant(c) => Some(*c as i16),
			_ => None
		}
	}

	pub fn pc_rel(&self) -> Option<StringID> {
		match self {
			Immediate::Constant(_) => None,
			Immediate::PcRel(id) => Some(*id),
		}
	}
}

pub mod rri {
	use bibe_instr::{ BinOp, Condition, Register};
	use super::Immediate;

	#[derive(Clone, Copy, Debug, PartialEq, Eq)]
	pub struct Instruction {
		pub op: BinOp,
		pub cond: Condition,
		pub dest: Register,
		pub src: Register,
		pub imm: Immediate,
	}
}

pub mod jump {
    use super::Immediate;

	#[derive(Clone, Copy, Debug, PartialEq, Eq)]
	pub struct Instruction {
		pub imm: Immediate,
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
	Memory(isa::memory::Instruction),
	Rrr(isa::rrr::Instruction),
	Rri(rri::Instruction),
	Csr(isa::csr::Instruction),
	Jump(jump::Instruction),
}

impl Instruction {
	pub fn size_of(&self) -> u64 {
		4
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Directive {
	Label(StringID),

	Origin(u64),
	Align(u64),

	Byte(u8),
	Short(u16),
	Word(u32),
	Quad(u64),

	Bytes(Vec<u8>),
}

impl Directive {
	pub fn size_of(&self) -> u64 {
		match self {
			Directive::Label(_) => 0,

			Directive::Origin(_) => 0,
			Directive::Align(_) => 0,

			Directive::Byte(..) => 1,
			Directive::Short(..) => 2,
			Directive::Word(..) => 4,
			Directive::Quad(..) => 8,

			Directive::Bytes(v) => v.len() as u64,
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
	Instruction(Instruction),
	Directive(Directive),
}

impl Statement {
	pub fn size_of(&self) -> u64 {
		match self {
			Statement::Instruction(instruction) => instruction.size_of(),
			Statement::Directive(directive) => directive.size_of(),
		}
	}

	pub fn instruction(&self) -> Option<Instruction> {
		match self {
			Statement::Instruction(instr) => Some(*instr),
			_ => None
		}
	}

	pub fn is_instruction(&self) -> bool {
		self.instruction().is_some()
	}

	pub fn directive(&self) -> Option<&Directive> {
		match self {
			Statement::Directive(directive) => Some(directive),
			_ => None
		}
	}

	pub fn is_directive(&self) -> bool {
		self.directive().is_some()
	}
}