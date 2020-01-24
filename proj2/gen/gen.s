.text
#if(__APPLE__)
	.global _entry_point

_entry_point:
#else
	.global entry_point

entry_point:
#endif
	push %rbp	# save stack frame for C convention
	mov %rsp, %rbp

	# beginning generated code
	movq $5, %rbx
	movq $5, %rcx
	movq %rbx, %rdi
	movq %rcx, %rsi
	cmpq %rsi, %rdi
	je if1_body
	movq $10, %rdi
	jmp if1_after
if1_body :
	movq $5, %rdi
if1_after :
	movq %rdi, %rcx
	movq %rcx, %rbx
	movq %rbx, %rax
	# end generated code
	# %rax contains the result

	mov %rbp, %rsp	# reset frame
	pop %rbp
	ret



