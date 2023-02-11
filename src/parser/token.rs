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

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Punctuation {
	Period,
	Comma
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
	Identifier(&'a str),
	Punctuation(Punctuation),
	Signed(i32),
	Unsigned(u32),
}

pub type TokenStream<'a> = Vec<Token<'a>>;

fn identifier(s: &str) -> IResult<&str, Token> {
	let (s, start) = alpha1(s)?;
	let (s, end) = alphanumeric0(s)?;
	let iden = unsafe { concat(start, end).unwrap() };
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