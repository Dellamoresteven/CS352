.text
.global _putchar, _getchar, _entry_point

################# FUNCTIONS #####################
_f:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, %rsi
	movq $0, %rdx
	movq (%rsi, %rdx, 8), %rsi
	movq %rsi, %rax
	movq %rbp, %rsp	# reset frame
	popq %rbp
	ret

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
	movq %rdi, %rsi
	movq $0, %rdx
	movq $100, %rcx
	movq  %rcx, (%rsi, %rdx, 8)
	movq %rdi, %rsi
	push %rdi
	push %rsi
	push %rdx
	push %rcx
	push %r8
	push %r9
	push %r10
	push %r11
	push %r12
	push %r13
	push %r14
	push %r15
	movq %rsi, %rdi
	call _f
	pop %r15
	pop %r14
	pop %r13
	pop %r12
	pop %r11
	pop %r10
	pop %r9
	pop %r8
	pop %rcx
	pop %rdx
	pop %rsi
	pop %rdi
	movq %rax, %rsi
	movq %rsi, %rdx
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
