/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
use nom::{
	branch::alt,
	character::complete::{
		alpha1,
		alphanumeric0,
		char,
		i64,
		multispace0,
	},
	combinator::map,
	IResult,
	multi::many0,
};

use str_concat::concat;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Punctuation {
	Comma,
	Colon,
	Period,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Bracket {
	LeftSquare,
	RightSquare,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
	Bracket(Bracket),
	String(&'a str),
	Punctuation(Punctuation),
	Constant(i64),
}

pub type TokenStream<'a> = Vec<Token<'a>>;

fn string(s: &str) -> IResult<&str, Token> {
	let (s, start) = alpha1(s)?;
	let (s, end) = alphanumeric0(s)?;
	let n = unsafe { concat(start, end).unwrap() };
	Ok((s, Token::String(n)))
}

fn bracket(s: &str) -> IResult<&str, Token> {
	alt((
		map(char('['), |_| Token::Bracket(Bracket::LeftSquare)),
		map(char(']'), |_| Token::Bracket(Bracket::RightSquare)),
	))(s)
}

fn constant(s: &str) -> IResult<&str, Token> {
	map(i64, Token::Constant)(s)
}

fn punctuation(s: &str) -> IResult<&str, Token> {
	let (s, c) = alt((char('.'), char(','), char(':')))(s)?;
	Ok((s, Token::Punctuation(match c {
		':' => Punctuation::Colon,
		',' => Punctuation::Comma,
		'.' => Punctuation::Period,
		_ => unreachable!(),
	})))
}

fn token(s: &str) -> IResult<&str, Token> {
	let (s, _) = multispace0(s)?;
	let (s, t) = alt((
		bracket,
		string,
		constant,
		punctuation,
	))(s)?;
	let (s, _) = multispace0(s)?;
	Ok((s, t))
}

pub fn tokenize(s: &str) -> IResult<&str, TokenStream> {
	many0(token)(s)
}