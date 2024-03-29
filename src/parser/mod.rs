/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
mod token;
mod register;
mod mov;
mod csr;

use isa::{Shift, LoadStore, LoadStoreOp, Width};
pub use token::{
	Token,
	tokenize
};

use register::*;
use mov::*;
use csr::*;

pub mod string_table;
pub use string_table::StringID;

use nom::{
	branch::alt,
	combinator::{
		map,
		opt,
	},
	Err as NomErr,
	error::{
		ErrorKind,
		ParseError,
	},
	IResult,
	multi::many0,
};

use crate::parser::token::{
	Bracket,
	Punctuation,
	TokenStream,
};

use crate::asm;
use bibe_instr::{
	self as isa,
	BinOp,
	Condition,
	Register,
};

#[derive(Clone, Copy, Debug, PartialEq)]
struct MemOperandRr {
	rs: Register,
	rq: Register,
	shift: u8,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct MemOperandRi {
	rs: Register,
	imm: i16,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum MemOperand {
	Ri(MemOperandRi),
	Rr(MemOperandRr),
}

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
	ExpectedBinOp(Option<&'a Token<'a>>),
	ExpectedCsrOp(Option<&'a Token<'a>>),
	ExpectedMemOp(Option<&'a Token<'a>>),
	ExpectedBracket(Option<&'a Token<'a>>),
	ExpectedCondition(Option<&'a Token<'a>>),
	ExpectedName(Option<&'a Token<'a>>),
	ExpectedConstant(Option<&'a Token<'a>>),
	ExpectedStringConstant(Option<&'a Token<'a>>),
	ExpectedPunctuation(Option<&'a Token<'a>>, Punctuation),
	ExpectedRegister(Option<&'a Token<'a>>),
	ExpectedString(Option<&'a Token<'a>>, &'a str),

	InvalidOp(&'a str),
	ImmediateRange(i16),
	InvalidRegister(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum ErrorWrapper<'a, I> {
	Nom(I, ErrorKind),
	Err(Error<'a>),
}

impl<'a, I> ParseError<I> for ErrorWrapper<'a, I> {
	fn from_error_kind(input: I, kind: ErrorKind) -> Self {
		ErrorWrapper::Nom(input, kind)
	}
	
	fn append(_: I, _: ErrorKind, other: Self) -> Self {
		other
	}
}

pub type Result<'a, T> = IResult<&'a [Token<'a>], T, ErrorWrapper<'a, &'a [Token<'a>]>>; 

fn error<'a, T>(_s: &'a [Token], err: Error<'a>) -> Result<'a, T> {
	Err(NomErr::Error(ErrorWrapper::Err(err)))
}

fn slice_as_option<'a>(s: &'a [Token<'a>]) -> Option<&'a Token<'a>> {
	if s.len() == 0 { None } else { Some(&s[0]) }
}

fn name<'a>(s: &'a [Token<'a>]) -> Result<'a, &'a str> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedName(slice));
	if slice.is_none() {
		return err;
	}

	if let Token::String(v) = s[0] {
		return Ok((&s[1..], v));
	}

	err
}

fn identifier<'a>(s: &'a [Token<'a>]) -> Result<'a, StringID> {
	name(s).map(|(s, x)| (s, string_table::insert(x))) 
}

fn string<'a>(target: &'a str) -> impl Fn(&'a [Token<'a>]) -> Result<'a, &'a str> {
	move |s: &'a [Token]| {
		let slice = slice_as_option(s);
		let err = error(s, Error::ExpectedName(slice));
		if slice.is_none() {
			return err;
		}

		if let Token::String(v) = s[0] {
			if v == target {
				return Ok((&s[1..], target));
			}
		}

		err
	}
}

fn bracket<'a>(s: &'a [Token]) -> Result<'a, Bracket> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedBracket(slice));
	if slice.is_none() {
		return err;
	}

	if let Token::Bracket(b) = s[0] {
		return Ok((&s[1..], b));
	}

	err
}

fn constant<'a>(s: &'a [Token]) -> Result<'a, i64> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedConstant(slice));
	if slice.is_none() {
		return err;
	}

	if let Token::Constant(imm) = s[0] {
		return Ok((&s[1..], imm));
	}

	err
}

fn left_square_bracket<'a>(s: &'a [Token]) -> Result<'a, Bracket> {
	let err = error(s, Error::ExpectedBracket(Some(&s[0])));
	let (s, b) = bracket(s)?;

	if b == Bracket::LeftSquare {
		return Ok((s, b));
	}

	err
}

fn right_square_bracket<'a>(s: &'a [Token]) -> Result<'a, Bracket> {
	let err = error(s, Error::ExpectedBracket(Some(&s[0])));
	let (s, b) = bracket(s)?;

	if b == Bracket::RightSquare {
		return Ok((s, b));
	}

	err
}

fn string_constant<'a>(s: &'a [Token]) -> Result<'a, String> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedStringConstant(slice));
	if slice.is_none() {
		return err;
	}

	if let Token::StringConstant(string) = &s[0] {
		return Ok((&s[1..], string.clone()));
	}

	err
}

fn immediate_constant<'a>(s: &'a [Token]) -> Result<'a, asm::Immediate> {
	constant(s).map(|(s, c)| (s, asm::Immediate::Constant(c)))
}

fn immediate_pc_rel<'a>(s: &'a [Token]) -> Result<'a, asm::Immediate> {
	let (s, id) = identifier(s)?;
	Ok((s, asm::Immediate::PcRel(id)))
}

fn immediate<'a>(s: &'a [Token]) -> Result<'a, asm::Immediate> {
	alt((
		immediate_constant,
		immediate_pc_rel,
	))(s)
}

fn punctuation<'a>(punc: Punctuation) -> impl Fn(&'a [Token]) -> Result<'a, Punctuation> {
	move |s: &'a [Token]| {
		let slice = slice_as_option(s);
		let err = error(s, Error::ExpectedPunctuation(slice, punc));
		if slice.is_none() {
			return err;
		}

		if let Token::Punctuation(p) = s[0] {
			if p == punc {
				return Ok((&s[1..], punc));
			} else {
				return err;
			}
		}

		err
	}
}

fn colon<'a>(s: &'a [Token]) -> Result<'a, Punctuation> {
	punctuation(Punctuation::Colon)(s)
}

fn comma<'a>(s: &'a [Token]) -> Result<'a, Punctuation> {
	punctuation(Punctuation::Comma)(s)
}

fn period<'a>(s: &'a [Token]) -> Result<'a, Punctuation> {
	punctuation(Punctuation::Period)(s)
}

fn percent<'a>(s: &'a [Token]) -> Result<'a, Punctuation> {
	punctuation(Punctuation::Percent)(s)
}

fn binop<'a>(s: &'a [Token]) -> Result<'a, BinOp> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedBinOp(slice));

	let name = name(s);
	if name.is_err() {
		return err;
	}
	let (s, name) = name.unwrap();

	let op = match name {
		"add" => Some(BinOp::Add),
		"sub" => Some(BinOp::Sub),

		"mul" => Some(BinOp::Mul),
		"div" => Some(BinOp::Div),
		"mod" => Some(BinOp::Mod),

		"and" => Some(BinOp::And),
		"or" => Some(BinOp::Or),
		"xor" => Some(BinOp::Xor),

		"shl" => Some(BinOp::Shl),
		"shr" => Some(BinOp::Shr),
		"asl" => Some(BinOp::Asl),
		"asr" => Some(BinOp::Asr),
		"rol" => Some(BinOp::Rol),
		"ror" => Some(BinOp::Ror),

		"not" => Some(BinOp::Not),
		"neg" => Some(BinOp::Neg),

		"addcc" => Some(BinOp::Addcc),
		"subcc" => Some(BinOp::Subcc),

		_ => None,
	};

	if op.is_none() {
		return err;
	}

	Ok((s, op.unwrap()))
}

fn condition<'a>(s: &'a [Token]) -> Result<'a, Condition> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedCondition(slice));

	let (s, p) = opt(period)(s)?;
	if p.is_none() {
		return Ok((s, Condition::Always));
	}

	let name = name(s);
	if name.is_err() {
		return err;
	}
	let (s, name) = name.unwrap();

	let cond = match name {
		"al" => Some(Condition::Always),
		"v" => Some(Condition::Overflow),
		"c" => Some(Condition::Carry),
		"z" | "eq" => Some(Condition::Zero),
		"n" | "lt" => Some(Condition::Negative),
		"nz" | "ne" => Some(Condition::NotZero),
		"nn" | "ge" => Some(Condition::NotNegative),
		"gt" => Some(Condition::GreaterThan),
		_ => None
	};

	if cond.is_none() {
		return err;
	}

	Ok((s, cond.unwrap()))
}

fn branch_r<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("b")(s)?;
	let (s, rs) = register(s)?;
	Ok((s, asm::Instruction::Rrr(isa::rrr::Instruction {
		op: BinOp::Add,
		dest: Register::pc(),
		lhs: Register::r0(),
		rhs: rs,
		shift: isa::Shift::default(),
	})))
}

fn branch_i<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("b")(s)?;
	let (s, cond) = condition(s)?;
	let (s, imm) = immediate(s)?;

	Ok((s, asm::Instruction::Rri(asm::rri::Instruction {
		op: BinOp::Add,
		cond: cond,
		dest: Register::pc(),
		src: Register::pc(),
		imm: if imm.constant().is_some() {
			asm::Immediate::Constant((4 * imm.constant().unwrap()) as i64) 
		} else { imm }
	})))
}

fn branch<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	alt((
		branch_r,
		branch_i,
	))(s)
}

fn nop<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("nop")(s)?;
	Ok((s, asm::Instruction::Rrr(isa::rrr::Instruction {
		op: BinOp::Add,
		dest: Register::r0(),
		lhs: Register::r0(),
		rhs: Register::r0(),
		shift: isa::Shift::default(),
	})))
}

fn cmp_i<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("cmp")(s)?;
	let (s, cond) = condition(s)?;
	let (s, rs) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, imm) = immediate(s)?;
	Ok((s, asm::Instruction::Rri(asm::rri::Instruction {
		op: BinOp::Subcc,
		cond: cond,
		dest: Register::r0(),
		src: rs,
		imm: imm,
	})))
}

fn cmp_r<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("cmp")(s)?;
	let (s, rs) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, rq) = register(s)?;
	Ok((s, asm::Instruction::Rrr(isa::rrr::Instruction {
		op: BinOp::Subcc,
		dest: Register::r0(),
		lhs: rs,
		rhs: rq,
		shift: Shift::default(),
	})))
}

fn cmp<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	alt((
		cmp_r,
		cmp_i,
	))(s)
}

fn alias<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	alt((
		branch,
		mov,
		nop,
		cmp,
		csr_alias,
	))(s)
}

fn rrr<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, op) = binop(s)?;

	let (s, rd) = register(s)?;

	let (s, _) = comma(s)?;
	let (s, rs) = register(s)?;

	let (s, _) = comma(s)?;
	let (s, rq) = register(s)?;

	Ok((s, asm::Instruction::Rrr(
		isa::rrr::Instruction {
			op: op,
			dest: rd,
			lhs: rs,
			rhs: rq,
			shift: isa::Shift::default(),
		}
	)))
}

fn rri<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, op) = binop(s)?;
	let (s, cond) = condition(s)?;

	let (s, rd) = register(s)?;

	let (s, _) = comma(s)?;
	let (s, rs) = register(s)?;

	let (s, _) = comma(s)?;
	let (s, imm) = immediate(s)?;

	Ok((s, asm::Instruction::Rri(
		asm::rri::Instruction {
			op: op,
			cond: cond,
			dest: rd,
			src: rs,
			imm: imm,
		}
	)))
}

fn memop<'a>(s: &'a [Token]) -> Result<'a, LoadStoreOp> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedMemOp(slice));

	let name = name(s);
	if name.is_err() {
		return err;
	}
	let (s, name) = name.unwrap();

	let op = match name {
		"ldrb" => Some((LoadStore::Load, Width::Byte)),
		"ldrs" => Some((LoadStore::Load, Width::Short)),
		"ldrw" => Some((LoadStore::Load, Width::Word)),

		"strb" => Some((LoadStore::Store, Width::Byte)),
		"strs" => Some((LoadStore::Store, Width::Short)),
		"strw" => Some((LoadStore::Store, Width::Word)),

		_ => None,
	};

	if op.is_none() {
		return err;
	}

	let (op, width) = op.unwrap();
	Ok((s, LoadStoreOp {
		op: op,
		width: width,
	}))
}

fn mem_operand<'a>(s: &'a [Token]) -> Result<'a, MemOperand> {
	let (s, _) = left_square_bracket(s)?;
	let (s, rs) = register(s)?;
	let (s, offset) = opt(comma)(s)?;

	let (s, operand) = if offset.is_none() {
		(s, MemOperand::Ri(MemOperandRi {
			rs: rs,
			imm: 0 as i16,
		}))
	} else if let Ok((s, rq)) = register(s) {
		(s, MemOperand::Rr(MemOperandRr {
			rs: rs,
			rq: rq,
			shift: 0,
		}))
	} else {
		let (s, imm) = constant(s)?;
		(s, MemOperand::Ri(MemOperandRi {
			rs: rs,
			imm: imm as i16,
		}))
	};

	let (s, _) = right_square_bracket(s)?;
	Ok((s, operand))
}

pub fn memory<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, op) = memop(s)?;

	let (s, reg, operand) = if op.op == LoadStore::Load {
		let (s, reg) = register(s)?;
		let (s, _) = comma(s)?;
		let (s, operand) = mem_operand(s)?;
		(s, reg, operand)
	} else {
		let (s, operand) = mem_operand(s)?;
		let (s, _) = comma(s)?;
		let (s, reg) = register(s)?;
		(s, reg, operand)
	};

	let instr = match operand {
		MemOperand::Rr(rr) => {
			isa::memory::Instruction::Rr(isa::memory::rr::Instruction {
				op: op,
				rd: reg,
				rs: rr.rs,
				rq: rr.rq,
				shift: Shift::default(),
			})
		},
		MemOperand::Ri(ri) => {
			isa::memory::Instruction::Ri(isa::memory::ri::Instruction {
				op: op,
				rd: reg,
				rs: ri.rs,
				imm: ri.imm,
			})
		}
	};

	Ok((s, asm::Instruction::Memory(instr)))
}

pub fn instruction<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	alt((
		alias, 
		rrr,
		rri,
		csr,
		memory,
	))(s)
}

fn dot<'a>(target: &'a str) -> impl Fn(&'a [Token]) -> Result<'a, &'a str> {
	move |s: &'a [Token]| {
		let (s, _) = period(s)?;
		string(target)(s)
	}
}

fn origin<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("origin")(s)?;
	let (s, value) = constant(s)?;
	Ok((s, asm::Directive::Origin(value as u64)))
}

fn align<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("align")(s)?;
	let (s, value) = constant(s)?;
	Ok((s, asm::Directive::Align(value as u64)))
}

fn label<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, id) = identifier(s)?;
	let (s, _) = colon(s)?;
	Ok((s, asm::Directive::Label(id)))
}

fn byte<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("byte")(s)?;
	let (s, value) = constant(s)?;
	Ok((s, asm::Directive::Byte(value as u8)))
}

fn short<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("short")(s)?;
	let (s, value) = constant(s)?;
	Ok((s, asm::Directive::Short(value as u16)))
}

fn word<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("word")(s)?;
	let (s, value) = constant(s)?;
	Ok((s, asm::Directive::Word(value as u32)))
}

fn quad<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("quad")(s)?;
	let (s, value) = constant(s)?;
	Ok((s, asm::Directive::Quad(value as u64)))
}

fn ascii<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("ascii")(s)?;
	let (s, string) = string_constant(s)?;
	Ok((s, asm::Directive::Bytes(string.into_bytes())))
}

fn asciiz<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	let (s, _) = dot("asciiz")(s)?;
	let (s, string) = string_constant(s)?;
	let mut bytes = string.into_bytes();
	bytes.push(0);
	Ok((s, asm::Directive::Bytes(bytes)))
}

pub fn directive<'a>(s: &'a [Token]) -> Result<'a, asm::Directive> {
	alt((
		label,
		origin,
		align,

		byte,
		short,
		word,
		quad,

		ascii,
		asciiz,
	))(s)
}

pub type StatementStream = Vec<asm::Statement>;

pub fn statement<'a>(s: &'a [Token]) -> Result<'a, asm::Statement> {
	alt((
		map(directive, asm::Statement::Directive),
		map(instruction, asm::Statement::Instruction),
	))(s)
}

pub fn parse<'a>(s: &'a TokenStream<'a>) -> Result<'a, StatementStream> {
	many0(statement)(&s)
}