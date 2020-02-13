.text
.global _putchar, _getchar, _entry_point

################# FUNCTIONS #####################
#################################################


###################### MAIN #####################
_entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	movq $0, %rdi
	movq %rdi, %rsi
	movq $2, %rdx
	addq %rdx, %rsi
	movq %rsi, %rdi
	movq $1, %rdx
	subq %rdx, %rsi
	movq %rsi, %rdi
	movq %rsi, %rdi
	movq %rdi, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret
#################################################


#################### DATA #######################

.data
heap:	.quad 0
#################################################
