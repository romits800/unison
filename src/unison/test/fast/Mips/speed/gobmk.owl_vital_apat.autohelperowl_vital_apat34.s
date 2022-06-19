	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"../unison/src/unison/test/fast/Mips/speed/gobmk.owl_vital_apat.autohelperowl_vital_apat34.ll"
	.text
	.hidden	autohelperowl_vital_apat34
	.globl	autohelperowl_vital_apat34
	.align	2
	.type	autohelperowl_vital_apat34,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperowl_vital_apat34
autohelperowl_vital_apat34:             # @autohelperowl_vital_apat34
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, _gp_disp
	addu	$gp, $2, $25
	addiu	$2, $2, _gp_disp
	lw	$1, transformation($gp)
	sll	$2, $4, 2
	addu	$1, $1, $2
	lw	$1, 24256($1)
	addiu	$sp, $sp, -4
	sw	$5, 20($sp)
	addu	$1, $1, $5
	sw	$1, 16($sp)
	sw	$ra, -4($sp)
	move	 $7, $5
	move	 $4, $6
	addiu	$5, $zero, 0
	lw	$25, play_attack_defend2_n($gp)
	jalr	$25
	addiu	$6, $zero, 1
	lw	$1, -4($sp)
	jr	$1
	addiu	$sp, $sp, 4
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperowl_vital_apat34
$func_end0:
	.size	autohelperowl_vital_apat34, ($func_end0)-autohelperowl_vital_apat34


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
