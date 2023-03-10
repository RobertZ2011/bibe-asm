/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
mod token;
pub use token::{
	Token,
	tokenize
};

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

use bibe_instr::{
	BinOp,
	Register,
	rrr,
	rri,
	Instruction,
	memory::{OpType as MemOpType, self},
	Width,
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
	ExpectedBracket(Option<&'a Token<'a>>),
	ExpectedCondition(Option<&'a Token<'a>>),
	ExpectedIdentifer(Option<&'a Token<'a>>),
	ExpectedImmediate(Option<&'a Token<'a>>),
	ExpectedMemOp(Option<&'a Token<'a>>),
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

fn identifier<'a>(s: &'a [Token<'a>]) -> Result<'a, &'a str> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedIdentifer(slice));
	if slice.is_none() {
		return err;
	}

	if let Token::Identifier(iden) = s[0] {
		return Ok((&s[1..], iden));
	}

	return err;
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

	return err;
}

fn left_square_bracket<'a>(s: &'a [Token]) -> Result<'a, Bracket> {
	let err = error(s, Error::ExpectedBracket(Some(&s[0])));
	let (s, b) = bracket(s)?;

	if b == Bracket::LeftSquare {
		return Ok((s, b));
	}

	return err;
}

fn right_square_bracket<'a>(s: &'a [Token]) -> Result<'a, Bracket> {
	let err = error(s, Error::ExpectedBracket(Some(&s[0])));
	let (s, b) = bracket(s)?;

	if b == Bracket::RightSquare {
		return Ok((s, b));
	}

	return err;
}

fn mem_operand_rr<'a>(s: &'a [Token]) -> Result<'a, MemOperand> {
	let (s, _) = left_square_bracket(s)?;
	let (s, rs) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, rq) = register(s)?;
	let (s, _) = right_square_bracket(s)?;

	Ok((s, MemOperand::Rr(MemOperandRr {
		rs: rs,
		rq: rq,
		shift: 0,
	})))
}

fn mem_operand_ri<'a>(s: &'a [Token]) -> Result<'a, MemOperand> {
	let (s, _) = left_square_bracket(s)?;
	let (s, rs) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, imm) = immediate(s)?;
	let (s, _) = right_square_bracket(s)?;

	Ok((s, MemOperand::Ri(MemOperandRi {
		rs: rs,
		imm: imm,
	})))
}

fn mem_operand<'a>(s: &'a [Token]) -> Result<'a, MemOperand> {
	alt((
		mem_operand_rr,
		mem_operand_ri,
	))(s)
}

fn mem_op<'a>(s: &'a [Token]) -> Result<'a, (MemOpType, Width)> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedMemOp(slice));
	if slice.is_none() {
		return err;
	}

	let (s, iden) = identifier(s)?;
	let op = match iden {
		"ldrb" => Some((MemOpType::Load, Width::Byte)),
		"ldrs" => Some((MemOpType::Load, Width::Short)),
		"ldrw" => Some((MemOpType::Load, Width::Word)),
		
		"strb" => Some((MemOpType::Store, Width::Byte)),
		"strs" => Some((MemOpType::Store, Width::Short)),
		"strw" => Some((MemOpType::Store, Width::Word)),
		_ => None,
	};

	if op.is_none() {
		return err;
	}

	Ok((s, op.unwrap()))
}

fn memory<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, (op, width)) = mem_op(s)?;

	let (s, rd, operand) = match op {
		MemOpType::Load => {
			let (s, rd) = register(s)?;
			let (s, _) = comma(s)?;
			let (s, operand) = mem_operand(s)?;
			(s, rd, operand)
		},
		MemOpType::Store => {
			let (s, operand) = mem_operand(s)?;
			let (s, _) = comma(s)?;
			let (s, rd) = register(s)?;
			(s, rd, operand)
		},
	};

	let instr = match operand {
		MemOperand::Ri(MemOperandRi { rs, imm }) => {
			Instruction::Memory(memory::Instruction::Ri(memory::ri::Instruction {
				op: (op, width),
				rd: rd,
				rs: rs,
				imm: imm,
			}))
		},
		MemOperand::Rr(MemOperandRr { rs, rq, shift }) => {
			Instruction::Memory(memory::Instruction::Rr(memory::rr::Instruction {
				op: (op, width),
				rd: rd,
				rs: rs,
				rq: rq,
				shift: shift,
			}))
		},
	};

	Ok((s, instr))
}

fn register<'a>(s: &'a [Token]) -> Result<'a, Register> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedRegister(slice));
	if slice.is_none() {
		return err;
	}

	if let Token::Identifier(iden) = s[0] {
		let reg = match iden {
			"pc" => Some(Register::pc()),
			"lr" => Some(Register::lr()),
			"fp" => Some(Register::fp()),
			"sp" => Some(Register::sp()),
			_ => if iden.len() == 2 {
				iden[1..2].parse::<u8>().map_or(None, |r| Some(Register::new(r).unwrap()))
			} else if iden.len() == 3 {
				iden[1..3].parse::<u8>().map_or(None, |r| Some(Register::new(r).unwrap()))
			} else {
				None
			}
		};

		if let Some(reg) = reg {
			return Ok((&s[1..], reg));
		}
	}

	return err;
}

fn immediate<'a>(s: &'a [Token]) -> Result<'a, i16> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedImmediate(slice));
	if slice.is_none() {
		return err;
	}

	if let Token::Signed(imm) = s[0] {
		return Ok((&s[1..], imm as i16));
	}

	return err;
}

fn punctuation<'a>(s: &'a [Token], punc: Punctuation) -> Result<'a, Punctuation> {
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

	return err;
}

fn comma<'a>(s: &'a [Token]) -> Result<'a, Punctuation> {
	punctuation(s, Punctuation::Comma)
}

fn period<'a>(s: &'a [Token]) -> Result<'a, Punctuation> {
	punctuation(s, Punctuation::Period)
}

fn binop<'a>(s: &'a [Token]) -> Result<'a, BinOp> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedBinOp(slice));
	if slice.is_none() {
		return err;
	}

	let (s, iden) = identifier(s)?;
	let op = match iden {
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
		"cmp" => Some(BinOp::Cmp),
		_ => None,
	};

	if op.is_none() {
		return err;
	}

	Ok((s, op.unwrap()))
}

fn string<'a>(value: &'a str) -> impl Fn(&'a [Token]) -> Result<'a, &'a str> {
	move |s: &'a [Token]| {
		let (s, ident) = identifier(s)?;
		if ident != value {
			return error(s, Error::ExpectedString(slice_as_option(s), value));
		}

		Ok((s, ident))
	}
}

fn rrr<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, op) = binop(s)?;
	let (s, dest) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, lhs) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, rhs) = register(s)?;
	Ok((s, Instruction::Rrr(rrr::Instruction {
		op: op,
		dest: dest,
		lhs: lhs,
		rhs: rhs,
		shift: rrr::Shift {
			kind: rrr::ShiftKind::Shl,
			shift: 0,
		}
	})))
}

fn condition<'a>(s: &'a [Token]) -> Result<'a, rri::Condition> {
	let (s, p) = opt(period)(s)?;
	if p.is_none() {
		return Ok((s, rri::Condition::Al));
	}

	let (s, iden) = identifier(s)?;
	let cond = match iden {
		"al" => Some(rri::Condition::Al),
		"eq" => Some(rri::Condition::Eq),
		"ne" => Some(rri::Condition::Ne),
		"gt" => Some(rri::Condition::Gt),
		"ge" => Some(rri::Condition::Ge),
		"lt" => Some(rri::Condition::Lt),
		"le" => Some(rri::Condition::Le),
		"nv" => Some(rri::Condition::Nv),
		_ => None
	};

	if cond.is_none() {
		return error(s, Error::ExpectedCondition(Some(&s[0])))
	}

	Ok((s, cond.unwrap()))
}

fn rri<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, op) = binop(s)?;
	let (s, cond) = condition(s)?;
	let (s, dest) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, src) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, imm) = immediate(s)?;
	Ok((s, Instruction::Rri(rri::Instruction {
		op: op,
		cond: cond,
		dest: dest,
		src: src,
		imm: imm,
	})))
}

fn jmp_r<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, _) = string("j")(s)?;
	let (s, rs) = register(s)?;
	return Ok((s, Instruction::Rrr(rrr::Instruction {
		op: BinOp::Add,
		dest: Register::pc(),
		lhs: Register::r0(),
		rhs: rs,
		shift: rrr::Shift {
			kind: rrr::ShiftKind::Shl,
			shift: 0,
		}
	})))
}

fn jmp_i<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, _) = string("j")(s)?;
	let (s, cond) = condition(s)?;
	let (s, imm) = immediate(s)?;
	return Ok((s, Instruction::Rri(rri::Instruction {
		op: BinOp::Add,
		cond: cond,
		dest: Register::pc(),
		src: Register::pc(),
		imm: 4 * imm,
	})))
}

fn jmp<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	alt((
		jmp_i,
		jmp_r,
	))(s)
}

fn mov_i<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, _) = string("mov")(s)?;
	let (s, cond) = condition(s)?;
	let (s, dest) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, imm) = immediate(s)?;
	return Ok((s, Instruction::Rri(rri::Instruction {
		op: BinOp::Add,
		cond: cond,
		dest: dest,
		src: Register::r0(),
		imm: imm,
	})))
}

fn mov_r<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, _) = string("mov")(s)?;
	let (s, dest) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, src) = register(s)?;
	return Ok((s, Instruction::Rrr(rrr::Instruction {
		op: BinOp::Add,
		dest: dest,
		lhs: Register::r0(),
		rhs: src,
		shift: rrr::Shift {
			kind: rrr::ShiftKind::Shl,
			shift: 0,
		}
	})))
}

fn nop<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	let (s, _) = string("nop")(s)?;
	return Ok((s, Instruction::Rrr(rrr::Instruction {
		op: BinOp::Add,
		dest: Register::r0(),
		lhs: Register::r0(),
		rhs: Register::r0(),
		shift: rrr::Shift {
			kind: rrr::ShiftKind::Shl,
			shift: 0,
		}
	})))
}

fn mov<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	alt((
		mov_i,
		mov_r,
	))(s)
}

fn alias<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	alt((
		jmp,
		mov,
		nop,
	))(s)
}

pub fn instruction<'a>(s: &'a [Token]) -> Result<'a, Instruction> {
	alt((
		alias, 
		rrr,
		rri,
		memory,
	))(s)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	Instruction(Instruction)
}

pub type StatementStream = Vec<Statement>;

pub fn statement<'a>(s: &'a [Token]) -> Result<'a, Statement> {
	map(instruction, |i| Statement::Instruction(i))(s)
}

pub fn parse<'a>(s: &'a TokenStream<'a>) -> Result<'a, StatementStream> {
	many0(statement)(&s)
}