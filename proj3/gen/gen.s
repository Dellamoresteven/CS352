.text
.global _putchar, _getchar, _entry_point

################# FUNCTIONS #####################
#################################################


###################### MAIN #####################
_entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	movq $10, %rdi
	movq $5, %rsi
	movq %rdi, %rax
	pushq %rdx
	movq %rsi, %rbx
	cqto
	idiv %rbx
	popq %rdx
	movq %rax, %rdi
	movq %rdi, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret
#################################################


#################### DATA #######################

.data
heap:	.quad 0
#################################################
