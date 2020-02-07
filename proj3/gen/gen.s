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
	movq heap(%rip), %rdi
	movq %rdi, %rsi
	movq $3, %rdx
	movq $826, %rcx
	movq (%rsi, %rdx, 8), %rsi
	movq %rcx, heap(%rip)
	movq %rdi, %rsi
	movq $3, %rdx
	leaq (%rsi, %rdx, 8), %rsi
	movq heap(%rip), %rsi
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
