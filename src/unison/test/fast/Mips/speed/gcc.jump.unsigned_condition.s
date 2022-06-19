	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"../unison/src/unison/test/fast/Mips/speed/gcc.jump.unsigned_condition.ll"
	.text
	.globl	unsigned_condition
	.align	2
	.type	unsigned_condition,@function
	.set	nomicromips
	.set	nomips16
	.ent	unsigned_condition
unsigned_condition:                     # @unsigned_condition
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, _gp_disp
	addiu	$1, $4, -102
	addiu	$2, $2, _gp_disp
	sltiu	$3, $1, 10
	bnez	$3, $BB0_2
	addu	$gp, $2, $25
# BB#1:
	lw	$4, .str($gp)
	lw	$6, __FUNCTION__.unsigned_condition($gp)
	lw	$25, fancy_abort($gp)
	jalr	$25
	addiu	$5, $zero, 951
$BB0_2:
	sll	$1, $1, 2
	lw	$2, switch.table.4($gp)
	addu	$1, $2, $1
	lw	$2, 0($1)
	jr	$ra
	nop
	.set	at
	.set	macro
	.set	reorder
	.end	unsigned_condition
$func_end0:
	.size	unsigned_condition, ($func_end0)-unsigned_condition

	.hidden	.str
	.hidden	__FUNCTION__.unsigned_condition
	.hidden	switch.table.4

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
