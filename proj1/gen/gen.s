.text
#if (__APPLE__)
	.global _entry_point

_entry_point:
#else
	.global entry_point

entry_point:
#endif
	push %rbp	# save stack frame for C convention
	mov %rsp, %rbp

	# beginning generated code
	movq $4, %rax
	pushq %rax
	movq $9, %rax
	pushq %rax
	movq $0, %rax
	popq %rbx
	subq %rbx, %rax
	cqto
	popq %rbx
	idivq %rbx
	# end generated code
	# %rax contains the result

	mov %rbp, %rsp	# reset frame
	pop %rbp
	ret



