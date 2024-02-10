/* Copyright 2024 Robert Zieba, see LICENSE file for full license. */
use super::*;

use bibe_instr::csr::regs::*;

fn csr_op<'a>(s: &'a [Token]) -> Result<'a, LoadStoreOp> {
	let slice = slice_as_option(s);
	let err = error(s, Error::ExpectedCsrOp(slice));

	let name = name(s);
	if name.is_err() {
		return err;
	}
	let (s, name) = name.unwrap();

	let op = match name {
		"csrb" => Some((LoadStore::Load, Width::Byte)),
		"csrs" => Some((LoadStore::Load, Width::Short)),
		"csrw" => Some((LoadStore::Load, Width::Word)),

		"cswb" => Some((LoadStore::Store, Width::Byte)),
		"csws" => Some((LoadStore::Store, Width::Short)),
		"csww" => Some((LoadStore::Store, Width::Word)),

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

pub(super) fn csr<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, op) = csr_op(s)?;

	let (s, rd, imm) = if op.is_load() {
		let (s, rd) = register(s)?;
		let (s, _) = comma(s)?;
		let (s, imm) = constant(s)?;
		(s, rd, imm)

	} else {
		let (s, imm) = constant(s)?;
		let (s, _) = comma(s)?;
		let (s, rd) = register(s)?;
		(s, rd, imm)
	};

	Ok((s, asm::Instruction::Csr(isa::csr::Instruction {
		op: op, 
		reg: rd, 
		imm: imm as u32
	})))
}

fn swi_reg<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("swi")(s)?;
	let (s, src) = register(s)?;
	Ok((s, asm::Instruction::Csr(isa::csr::Instruction {
		op: LoadStoreOp {
			op: LoadStore::Store,
			width: Width::Word
		},
		reg: src,
		imm: ISR_ENTER_REG,
	})))
}

fn swi<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("swi")(s)?;
	Ok((s, asm::Instruction::Csr(isa::csr::Instruction {
		op: LoadStoreOp {
			op: LoadStore::Store,
			width: Width::Word
		},
		reg: Register::z(),
		imm: ISR_ENTER_REG,
	})))
}

pub(super) fn csr_alias<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	alt((
		swi_reg,
		swi,
	))(s)
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn swi_r() {
		let (_, tokens) = tokenize("swi %a4").unwrap();
		let res = swi_reg(&tokens);
		assert!(res.is_ok());
		let (_, instr) = res.unwrap();
		assert!(instr.is_csr());
		assert_eq!(instr.as_csr().unwrap(), &isa::csr::Instruction {
			op: LoadStoreOp {
				op: LoadStore::Store,
				width: Width::Word
			},
			reg: Register::a4(),
			imm: ISR_ENTER_REG,
		})
	}

	#[test]
	fn swi_none() {
		let (_, tokens) = tokenize("swi").unwrap();
		let res = swi(&tokens);
		assert!(res.is_ok());
		let (_, instr) = res.unwrap();
		assert!(instr.is_csr());
		assert_eq!(instr.as_csr().unwrap(), &isa::csr::Instruction {
			op: LoadStoreOp {
				op: LoadStore::Store,
				width: Width::Word
			},
			reg: Register::z(),
			imm: ISR_ENTER_REG,
		})
	}
}