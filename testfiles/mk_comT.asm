.model small
	
.code
org    100H	

JUMPS	
	
begin:

	jmp main
	

main    proc    near        ; <=== Entry point (main function)
	ret 1
	ret 2

	nop
	nop
	nop

	shl byte ptr ds:[01h], 1
	shl byte ptr ds:[1000h], 1
	

	shl word ptr ds:[01h], 1
	shl word ptr ds:[1000h], 1

	nop
	nop
	nop

	sbb ax, 1010h
	sbb ax, 1010h

	adc bp, 9000h
	adc ax, 0000h
	
	or ax, 90h
	add ah, 8h
	or ah, 17h
	add ax, 1111h


	movsb
	movsw
	stosb
	stosw
	lodsb
	lodsw
	scasb
	scasw

	
	add cx, 09h
	or word ptr [bp + di + 900h], 9h
	adc bl, 3h
	sbb cx, 40h
	and cx, 19h
	sub word ptr [bp + di], 80h
	xor bx, 90h
	cmp word ptr [di], 900h
	
	
	shr ax, 07h
	shr byte ptr [si + 90h], 3
	
	rol byte ptr [si], 02h

	shl byte ptr [bp + 90h], 1
	

	add byte ptr ds:[si], 09h
	add byte ptr es:[si], 09h
	add byte ptr cs:[78], 90h

	and ax, 09h
	and ax, 0FFh
	and ax, 0ECh
	and al, 00h
	
	sub al, 1
	sub al, 0CFh
	
	sub dx, ax
	sub byte ptr [si + 0FFh], ch
	
	cmp ax, 0AAh
	cmp bx, 0AAh
	
	xor word ptr [di], cx
	xor ch, byte ptr [di]
	xor word ptr [bx + si + 0Ah], dx
	xor ah, 090h
	xor al, 0FFh
	
	add cl, 01h
	add ah, 0FFh

	
main    endp                ;<=== End function
	
end begin                ;<=== End program


	;; tasm mk_com
	;; tlink /t mk_com
