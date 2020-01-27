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
	movq $2, %rbx
	movq $0, %rcx
	jmp loop1_cond
loop1_body:
	movq %rbx, %rdi
	movq %rbx, %rsi
	imulq %rsi, %rdi
	movq %rdi, %rbx
	movq %rcx, %rsi
	movq $1, %r8
	addq %r8, %rsi
	movq %rsi, %rcx
	movq %rsi, %rdi
loop1_cond:
	movq %rcx, %rdi
	movq $5, %rsi
	cmpq %rsi, %rdi
	jl loop1_body
	movq %rbx, %rdi
	movq %rdi, %rcx
	movq %rcx, %rbx
	movq %rbx, %rax
	# end generated code
	# %rax contains the result

	mov %rbp, %rsp	# reset frame
	pop %rbp
	ret



