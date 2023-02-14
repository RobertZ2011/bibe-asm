/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
use nom::{
	branch::alt,
	character::complete::{
		alpha1,
		alphanumeric0,
		char,
		i32,
		multispace0,
		u32,
	},
	combinator::map,
	IResult,
	multi::many0,
};

use str_concat::concat;

use bibe_instr::{
	BinOp,
	memory::OpType as MemOpType,
	Width,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Punctuation {
	Period,
	Comma
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Bracket {
	LeftSquare,
	RightSquare,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
	BinOp(BinOp),
	Bracket(Bracket),
	Identifier(&'a str),
	MemOp(MemOpType, Width),
	Punctuation(Punctuation),
	Signed(i32),
	Unsigned(u32),
}

pub type TokenStream<'a> = Vec<Token<'a>>;

fn name(s: &str) -> IResult<&str, &str> {
	let (s, start) = alpha1(s)?;
	let (s, end) = alphanumeric0(s)?;
	let n = unsafe { concat(start, end).unwrap() };
	Ok((s, n))
}

fn bin_op(s: &str) -> IResult<&str, Token> {
	let (s, n) = name(s)?;
	let op = match n {
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

	}

	Ok((s, Token::BinOp(op.unwrap())))
}

fn mem_op(s: &str) -> IResult<&str, Token> {
	let (s, n) = name(s)?;
	let op = match n {
		"ldrb" => Some((MemOpType::Load, Width::Byte)),
		"ldrs" => Some((MemOpType::Load, Width::Short)),
		"ldrw" => Some((MemOpType::Load, Width::Word)),

		"strb" => Some((MemOpType::Store, Width::Byte)),
		"strs" => Some((MemOpType::Store, Width::Short)),
		"strw" => Some((MemOpType::Load, Width::Word)),

		_ => None,
	};

	if op.is_none() {

	}

	let (op, width) = op.unwrap();
	Ok((s, Token::MemOp(op, width)))
}

fn bracket(s: &str) -> IResult<&str, Token> {
	alt((
		map(char('['), |_| Token::Bracket(Bracket::LeftSquare)),
		map(char(']'), |_| Token::Bracket(Bracket::RightSquare)),
	))(s)
}

fn identifier(s: &str) -> IResult<&str, Token> {
	let (s, iden) = name(s)?;
	Ok((s, Token::Identifier(iden)))
}

fn signed(s: &str) -> IResult<&str, Token> {
	map(i32, Token::Signed)(s)
}

fn unsigned(s: &str) -> IResult<&str, Token> {
	map(u32, Token::Unsigned)(s)
}

fn constant(s: &str) -> IResult<&str, Token> {
	alt((signed, unsigned))(s)
}

fn punctuation(s: &str) -> IResult<&str, Token> {
	let (s, c) = alt((char('.'), char(',')))(s)?;
	Ok((s, Token::Punctuation(match c {
		'.' => Punctuation::Period,
		',' => Punctuation::Comma,
		_ => unreachable!(),
	})))
}

fn token(s: &str) -> IResult<&str, Token> {
	let (s, _) = multispace0(s)?;
	let (s, t) = alt((
		bracket,
		identifier,
		constant,
		punctuation,
	))(s)?;
	let (s, _) = multispace0(s)?;
	Ok((s, t))
}

pub fn tokenize(s: &str) -> IResult<&str, TokenStream> {
	many0(token)(s)
}