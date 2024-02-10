/* Copyright 2024 Robert Zieba, see LICENSE file for full license. */
use super::*;

use bibe_instr::csr::regs::*;

fn mov_i<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("mov")(s)?;
	let (s, cond) = condition(s)?;
	let (s, dest) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, imm) = immediate(s)?;
	Ok((s, asm::Instruction::Rri(asm::rri::Instruction {
		op: BinOp::Add,
		cond: cond,
		dest: dest,
		src: if imm.pc_rel().is_none() { Register::z() } else { Register::pc() },
		imm: imm,
	})))
}

fn mov_r<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("mov")(s)?;
	let (s, dest) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, src) = register(s)?;
	Ok((s, asm::Instruction::Rrr(isa::rrr::Instruction {
		op: BinOp::Add,
		dest: dest,
		lhs: src,
		rhs: Register::z(),
		shift: isa::Shift::default(),
	})))
}

fn mov_reg_psr<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("mov")(s)?;
	let (s, dest) = register(s)?;
	let (s, _) = comma(s)?;
	let (s, _) = percent(s)?;
	let (s, _) = string("psr")(s)?;
	Ok((s, asm::Instruction::Csr(isa::csr::Instruction {
		op: LoadStoreOp {
			op: LoadStore::Load,
			width: Width::Word
		},
		reg: dest,
		imm: PSR_PSR0_REG,
	})))
}

fn mov_psr_reg<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	let (s, _) = string("mov")(s)?;
	let (s, _) = percent(s)?;
	let (s, _) = string("psr")(s)?;
	let (s, _) = comma(s)?;
	let (s, src) = register(s)?;
	Ok((s, asm::Instruction::Csr(isa::csr::Instruction {
		op: LoadStoreOp {
			op: LoadStore::Store,
			width: Width::Word
		},
		reg: src,
		imm: PSR_PSR0_REG,
	})))
}

pub(super) fn mov<'a>(s: &'a [Token]) -> Result<'a, asm::Instruction> {
	alt((
		mov_r,
		mov_i,
		mov_reg_psr,
		mov_psr_reg,
	))(s)
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn psr_reg() {
		let (_, tokens) = tokenize("mov %psr, %l2").unwrap();
		let res = mov(&tokens);
		assert!(res.is_ok());
		let (_, instr) = res.unwrap();
		assert!(instr.is_csr());
		assert_eq!(instr.as_csr().unwrap(), &isa::csr::Instruction {
			op: LoadStoreOp {
				op: LoadStore::Store,
				width: Width::Word
			},
			reg: Register::l2(),
			imm: PSR_PSR0_REG,
		})
	}

	#[test]
	fn reg_psr() {
		let (_, tokens) = tokenize("mov %t5, %psr").unwrap();
		let res = mov(&tokens);
		assert!(res.is_ok());
		let (_, instr) = res.unwrap();
		assert!(instr.is_csr());
		assert_eq!(instr.as_csr().unwrap(), &isa::csr::Instruction {
			op: LoadStoreOp {
				op: LoadStore::Load,
				width: Width::Word
			},
			reg: Register::t5(),
			imm: PSR_PSR0_REG,
		})
	}

	#[test]
	fn reg_reg() {
		let (_, tokens) = tokenize("mov %t5, %a3").unwrap();
		let res = mov(&tokens);
		assert!(res.is_ok());
		let (_, instr) = res.unwrap();
		assert!(instr.is_rrr());
		assert_eq!(instr.as_rrr().unwrap(), &isa::rrr::Instruction {
			op: BinOp::Add,
			dest: Register::t5(),
			lhs: Register::a3(),
			rhs: Register::z(),
			shift: Shift::default(),
		})
	}

	#[test]
	fn reg_imm() {
		let (_, tokens) = tokenize("mov %o0, 43").unwrap();
		let res = mov(&tokens);
		assert!(res.is_ok());
		let (_, instr) = res.unwrap();
		assert!(instr.is_rri());
		assert_eq!(instr.as_rri().unwrap(), &asm::rri::Instruction {
			op: BinOp::Add,
			cond: Condition::Always,
			dest: Register::o0(),
			src: Register::z(),
			imm: asm::Immediate::Constant(43),
		})
	}

	#[test]
	fn reg_imm_nz() {
		let (_, tokens) = tokenize("mov.nz %o0, 43").unwrap();
		let res = mov(&tokens);
		assert!(res.is_ok());
		let (_, instr) = res.unwrap();
		assert!(instr.is_rri());
		assert_eq!(instr.as_rri().unwrap(), &asm::rri::Instruction {
			op: BinOp::Add,
			cond: Condition::NotZero,
			dest: Register::o0(),
			src: Register::z(),
			imm: asm::Immediate::Constant(43),
		})
	}
}