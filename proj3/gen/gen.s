.text
.global _putchar, _getchar, _entry_point

################# FUNCTIONS #####################
_f:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq $2, %rdx
	movq %rdx, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret

_g:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq $5, %rcx
	movq $2, %r8
	addq %r8, %rcx
	movq %rcx, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret

#################################################


###################### MAIN #####################
_entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	movq $5, %rdi
	movq %rdi, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret
#################################################


#################### DATA #######################

.data
heap:	.quad 0
#################################################
