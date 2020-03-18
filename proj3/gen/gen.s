.text
.global _putchar, _getchar, _entry_point

################# FUNCTIONS #####################
#################################################


###################### MAIN #####################
_entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	movq heap(%rip), %rdi
	movq $10, %rsi
	leaq (%rdi, %rsi, 8), %rdi
	movq %rdi, heap(%rip)
	movq %rdi, %rsi
	movq $1, %rdx
	movq $1, %rcx
	movq %rcx, (%rsi, %rdx, 8)
	movq %rdi, %rsi
	movq $1, %rdx
	movq (%rsi, %rdx, 8), %rsi
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
