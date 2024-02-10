/* Copyright 2023 Robert Zieba, see LICENSE file for full license. */
use nom::{
	branch::alt,
	character::complete::{
		alpha1,
		alphanumeric0,
		char,
		i64,
		multispace0, anychar,
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
	Percent,
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
	StringConstant(String),
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

fn string_constant(s: &str) -> IResult<&str, Token> {
	let (mut s, _) = char('"')(s)?;
	let mut res = String::with_capacity(256);

	loop {
		let (ns, c) = anychar(s)?;
		s = ns;

		let (_, c) = match c {
			'"' => return Ok((s, Token::StringConstant(res))),
			'\\' => {
				let (s, escaped) = anychar(s)?;
				let c = match escaped {
					'0' => '\0',
					't' => '\t',
					'n' => '\n',
					'\\' => '\\',
					'"' => '"',
					_ => todo!(),
				};

				(s, c)
			},
			_ => (s, c),
		};

		res.push(c);
		s = ns;
	}
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
	let (s, c) = alt((char('.'), char(','), char(':'), char('%')))(s)?;
	Ok((s, Token::Punctuation(match c {
		':' => Punctuation::Colon,
		',' => Punctuation::Comma,
		'.' => Punctuation::Period,
		'%' => Punctuation::Percent,
		_ => unreachable!(),
	})))
}

fn token(s: &str) -> IResult<&str, Token> {
	let (s, _) = multispace0(s)?;
	let (s, t) = alt((
		bracket,
		string,
		string_constant,
		constant,
		punctuation,
	))(s)?;
	let (s, _) = multispace0(s)?;
	Ok((s, t))
}

pub fn tokenize(s: &str) -> IResult<&str, TokenStream> {
	many0(token)(s)
}