mod token;
pub use token::{
	TokenStream,
	tokenize
};

use nom::{
	branch::alt,
	combinator::opt,
	Err as NomErr,
	error::{
		ErrorKind,
		ParseError,
	},
	IResult,
	multi::many0,
};

use crate::parser::token::{ 
	Punctuation,
	Token,
};

use bibe_instr::{
	BinOp,
	Register,
	rrr,
	rri,
	Instruction,
};

#[derive(Debug, PartialEq)]
pub enum Error<'a> {
	ExpectedBinOp(Option<&'a Token<'a>>),
	ExpectedCondition(Option<&'a Token<'a>>),
	ExpectedIdentifer(Option<&'a Token<'a>>),
	ExpectedImmediate(Option<&'a Token<'a>>),
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
			return error(s, Error::ExpectedString(Some(&s[0]), value));
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
		imm: imm,
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

fn instruction<'a>(s: &'a [Token]) -> Result<'a, Statement> {
	let (s, instr) = alt((
		alias, 
		rrr,
		rri,
	))(s)?;
	Ok((s, Statement::Instruction(instr)))
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
	Instruction(Instruction)
}

pub type StatementStream = Vec<Statement>;

fn statement<'a>(s: &'a [Token]) -> Result<'a, Statement> {
	instruction(s)
}

pub fn parse<'a>(s: &'a TokenStream<'a>) -> Result<'a, StatementStream> {
	many0(statement)(&s)
}