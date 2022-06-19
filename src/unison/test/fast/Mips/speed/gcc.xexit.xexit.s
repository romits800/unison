	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"../unison/src/unison/test/fast/Mips/speed/gcc.xexit.xexit.ll"
	.text
	.globl	xexit
	.align	2
	.type	xexit,@function
	.set	nomicromips
	.set	nomips16
	.ent	xexit
xexit:                                  # @xexit
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, _gp_disp
	addu	$16, $2, $25
	addiu	$2, $2, _gp_disp
	lw	$1, _xexit_cleanup($16)
	lw	$25, 0($1)
	beqz	$25, $BB0_2
	addiu	$sp, $sp, 0
# BB#1:
	jalr	$25
	move	 $17, $4
	move	 $4, $17
$BB0_2:
	lw	$25, exit($16)
	jalr	$25
	move	 $gp, $16
	.set	at
	.set	macro
	.set	reorder
	.end	xexit
$func_end0:
	.size	xexit, ($func_end0)-xexit


	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
