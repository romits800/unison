	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"../unison/src/unison/test/fast/Mips/speed/gobmk.owl_attackpat.autohelperowl_attackpat68.ll"
	.text
	.hidden	autohelperowl_attackpat68
	.globl	autohelperowl_attackpat68
	.align	2
	.type	autohelperowl_attackpat68,@function
	.set	nomicromips
	.set	nomips16
	.ent	autohelperowl_attackpat68
autohelperowl_attackpat68:              # @autohelperowl_attackpat68
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
	move	 $7, $5
	sw	$ra, -4($sp)
	addu	$gp, $2, $25
	sll	$1, $4, 2
	move	 $4, $6
	lw	$2, transformation($gp)
	addu	$2, $2, $1
	lw	$1, 21856($2)
	lw	$2, 20672($2)
	sw	$5, 20($sp)
	addiu	$6, $zero, 2
	addu	$2, $2, $5
	sw	$2, 24($sp)
	addu	$1, $1, $5
	sw	$1, 16($sp)
	lw	$25, play_attack_defend2_n($gp)
	jalr	$25
	addiu	$5, $zero, 0
	lw	$1, -4($sp)
	jr	$1
	addiu	$sp, $sp, 4
	.set	at
	.set	macro
	.set	reorder
	.end	autohelperowl_attackpat68
$func_end0:
	.size	autohelperowl_attackpat68, ($func_end0)-autohelperowl_attackpat68


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
