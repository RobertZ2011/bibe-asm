use crate::parser::token::*;
use crate::parser::*;

pub fn register<'a>(s: &'a [Token]) -> Result<'a, Register> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedRegister(slice));
	if slice.is_none() {
		return err;
	}

	let (s, _) = percent(s)?;
	if let Token::String(iden) = s[0] {
		let reg = match iden {
			"pc" => Some(Register::pc()),
			"lr" => Some(Register::lr()),
			"fp" => Some(Register::fp()),
			"sp" => Some(Register::sp()),
			"z" => Some(Register::new(0).unwrap()),
			_ => {
				if let Ok(i) = &iden[1..].parse::<u8>() {
					match iden.chars().nth(0).unwrap() {
						'r' => Register::new(*i),
						'a' => Register::arg(*i),
						'o' => Register::out(*i),
						'l' => Register::local(*i),
						't' => Register::temp(*i),
						_ => None
					}
				} else {
					None
				}
			}
		};

		if let Some(reg) = reg {
			return Ok((&s[1..], reg));
		}
	}
	err
}

#[cfg(test)]
mod test {
	use super::*;

	// General purpose registers
	#[test]
	fn r0() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r0")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r0());
	}

	#[test]
	fn r1() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r1")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r1());
	}

	#[test]
	fn r2() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r2")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r2());
	}

	#[test]
	fn r3() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r3")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r3());
	}

	#[test]
	fn r4() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r4")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r4());
	}

	#[test]
	fn r5() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r5")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r5());
	}

	#[test]
	fn r6() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r6")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r6());
	}

	#[test]
	fn r7() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r7")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r7());
	}

	#[test]
	fn r8() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r8")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r8());
	}

	#[test]
	fn r9() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r9")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r9());
	}

	#[test]
	fn r10() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r10")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r10());
	}

	#[test]
	fn r11() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r11")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r11());
	}

	#[test]
	fn r12() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r12")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r12());
	}

	#[test]
	fn r13() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r13")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r13());
	}

	#[test]
	fn r14() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r14")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r14());
	}

	#[test]
	fn r15() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r15")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r15());
	}

	#[test]
	fn r16() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r16")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r16());
	}

	#[test]
	fn r17() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r17")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r17());
	}

	#[test]
	fn r18() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r18")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r18());
	}

	#[test]
	fn r19() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r19")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r19());
	}

	#[test]
	fn r20() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r20")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r20());
	}

	#[test]
	fn r21() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r21")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r21());
	}

	#[test]
	fn r22() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r22")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r22());
	}

	#[test]
	fn r23() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r23")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r23());
	}

	#[test]
	fn r24() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r24")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r24());
	}

	#[test]
	fn r25() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r25")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r25());
	}

	#[test]
	fn r26() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r26")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r26());
	}

	#[test]
	fn r27() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r27")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r27());
	}

	#[test]
	fn r28() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r28")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r28());
	}

	#[test]
	fn r29() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r29")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r29());
	}

	#[test]
	fn r30() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r30")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r30());
	}

	#[test]
	fn r31() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("r31")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::r31());
	}

	/// z
	#[test]
	fn z() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("z")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::z());
	}

	// Args
	#[test]
	fn a1() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("a1")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::a1());
	}

	#[test]
	fn a2() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("a2")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::a2());
	}

	#[test]
	fn a3() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("a3")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::a3());
	}

	#[test]
	fn a4() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("a4")]);
		println!("{res:?}");
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::a4());
	}

	#[test]
	fn a5() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("a5")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::a5());
	}

	#[test]
	fn a6() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("a6")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::a6());
	}

	// Outputs
	#[test]
	fn o0() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("o0")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::o0());
	}

	#[test]
	fn o1() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("o1")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::o1());
	}

	#[test]
	fn o2() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("o2")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::o2());
	}

	#[test]
	fn o3() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("o3")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::o3());
	}

	#[test]
	fn o4() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("o4")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::o4());
	}

	#[test]
	fn o5() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("o5")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::o5());
	}

	// Locals
	#[test]
	fn l0() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("l0")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::l0());
	}

	#[test]
	fn l1() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("l1")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::l1());
	}

	#[test]
	fn l2() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("l2")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::l2());
	}

	#[test]
	fn l3() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("l3")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::l3());
	}

	// Temps
	#[test]
	fn t0() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t0")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t0());
	}

	#[test]
	fn t1() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t1")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t1());
	}

	#[test]
	fn t2() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t2")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t2());
	}

	#[test]
	fn t3() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t3")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t3());
	}

	#[test]
	fn t4() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t4")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t4());
	}

	#[test]
	fn t5() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t5")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t5());
	}

	#[test]
	fn t6() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t6")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t6());
	}

	#[test]
	fn t7() {
		let res = register(&[Token::Punctuation(Punctuation::Percent), Token::String("t7")]);
		assert!(res.is_ok());
		let (_, res) = res.unwrap();
		assert_eq!(res, Register::t7());
	}
}