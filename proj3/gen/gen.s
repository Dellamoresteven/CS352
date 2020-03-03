.text
.global _putchar, _getchar, _entry_point

################# FUNCTIONS #####################
#################################################


###################### MAIN #####################
_entry_point:
	pushq %rbp	# save stack frame for calling convention
	movq %rsp, %rbp
	movq %rdi, heap(%rip)
	movq $2, %rdi
	movq $1, %rsi
	movq $1, %rdx
	cmp %rdx, %rsi
	sete %al
	movzbq %al, %rsi
	test %rsi, %rsi
	jnz if1_then
	jmp if1_end
if1_then:
	movq $3, %rsi
	movq %rsi, %rdi
if1_end:
	movq %rdi, %rsi
	movq $2, %rdx
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
