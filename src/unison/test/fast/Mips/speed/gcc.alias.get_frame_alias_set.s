	.text
	.abicalls
	.section	.mdebug.abi32,"",@progbits
	.nan	legacy
	.file	"../unison/src/unison/test/fast/Mips/speed/gcc.alias.get_frame_alias_set.ll"
	.text
	.globl	get_frame_alias_set
	.align	2
	.type	get_frame_alias_set,@function
	.set	nomicromips
	.set	nomips16
	.ent	get_frame_alias_set
get_frame_alias_set:                    # @get_frame_alias_set
	.frame	$sp,0,$ra
	.mask 	0x00000000,0
	.fmask	0x00000000,0
	.set	noreorder
	.set	nomacro
	.set	noat
# BB#0:
	lui	$2, _gp_disp
	addiu	$2, $2, _gp_disp
	addu	$4, $2, $25
	lw	$3, get_frame_alias_set.set($4)
	lw	$2, 0($3)
	addiu	$1, $zero, -1
	bne	$2, $1, $BB0_4
# BB#1:
	lw	$1, flag_strict_aliasing($4)
	lw	$1, 0($1)
	beqz	$1, $BB0_3
	addiu	$2, $zero, 0
# BB#2:
	lw	$1, new_alias_set.last_alias_set($4)
	lw	$2, 0($1)
	addiu	$2, $2, 1
	sw	$2, 0($1)
$BB0_3:
	sw	$2, 0($3)
$BB0_4:
	jr	$ra
	.set	at
	.set	macro
	.set	reorder
	.end	get_frame_alias_set
$func_end0:
	.size	get_frame_alias_set, ($func_end0)-get_frame_alias_set

	.hidden	new_alias_set.last_alias_set
	.hidden	get_frame_alias_set.set

	.ident	"clang version 3.8.0 (http://llvm.org/git/clang.git 2d49f0a0ae8366964a93e3b7b26e29679bee7160) (http://llvm.org/git/llvm.git 60bc66b44837125843b58ed3e0fd2e6bb948d839)"
	.section	".note.GNU-stack","",@progbits
	.text
