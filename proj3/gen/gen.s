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
	movq $4, %rsi
	movq heap(%rip), %rdx
	movq (%rdx, %rsi, 8), %rdx
	movq heap(%rip), %rsi
	movq $4, %rdx
	movq heap(%rip), %rcx
	movq (%rcx, %rdx, 8), %rcx
	movq %rsi, %rdx
	movq $0, %rcx
	movq $8, %r8
	movq  %r8, (%rdx, %rcx, 8)
	movq %rsi, %rdx
	movq $1, %rcx
	movq $9, %r8
	movq  %r8, (%rdx, %rcx, 8)
	movq %rsi, %rdx
	movq $2, %rcx
	movq $10, %r8
	movq  %r8, (%rdx, %rcx, 8)
	movq %rsi, %rdx
	movq $3, %rcx
	movq $11, %r8
	movq  %r8, (%rdx, %rcx, 8)
	movq %rdi, %rdx
	movq $0, %rcx
	movq $5, %r8
	movq  %r8, (%rdx, %rcx, 8)
	movq %rsi, %rdx
	movq $2, %rcx
	movq (%rdx, %rcx, 8), %rdx
	movq %rdx, %rsi
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
