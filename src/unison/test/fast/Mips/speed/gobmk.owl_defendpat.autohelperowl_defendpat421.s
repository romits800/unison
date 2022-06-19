	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"../unison/src/unison/test/fast/Mips/speed/gobmk.owl_defendpat.autohelperowl_defendpat421.ll"
	.text
	.hidden	autohelperowl_defendpat421
	.globl	autohelperowl_defendpat421
	.align	2
	.type	autohelperowl_defendpat421,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperowl_defendpat421
autohelperowl_defendpat421:             # @autohelperowl_defendpat421
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, _gp_disp
	addiu	$sp, $sp, -4
	addiu	$2, $2, _gp_disp
	sw	$ra, -4($sp)
	addu	$gp, $2, $25
	sll	$1, $4, 2
	lw	$2, transformation($gp)
	addu	$1, $2, $1
	lw	$2, 18336($1)
	addiu	$1, $zero, 3
	addu	$4, $2, $5
	lw	$25, safe_move($gp)
	jalr	$25
	subu	$5, $1, $6
	sltiu	$2, $2, 1
	lw	$1, -4($sp)
	jr	$1
	addiu	$sp, $sp, 4
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperowl_defendpat421
$func_end0:
	.size	autohelperowl_defendpat421, ($func_end0)-autohelperowl_defendpat421


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
