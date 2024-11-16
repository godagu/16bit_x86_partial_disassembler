.model small
	
.code
org    100H	
begin:
	jmp    main

main    proc    far        ; <=== Entry point (main function)
	nop
	nop
	nop

	rol al, cl

	nop
	nop
	nop

	rol dx, 1

	nop 
	nop
	nop 

	rol byte ptr es:[bp+di+2], 1

	nop
	nop
	nop

	nop
	nop
	nop

	rol byte ptr [si], 1

	nop
	nop
	nop

	mov byte ptr ds:[2], 1

	nop

	mov word ptr ds:[2], 0FFFh

	nop

	add DS:[di], ax

	nop

	add SS:[SI], bx

	nop

	add ES:[SI], cx

	nop

	add CS:[SI], ax

	rol byte ptr [bx+si], 1
	ror word ptr [bx+di], 1
	rcl word ptr [bp+si], 1
	rcr byte ptr [bp+di], 1
	shl byte ptr [si], 1
	shr byte ptr [di], 1
	sar byte ptr ds:[01], 1
	sar byte ptr [bx], 1

	




    mov    ax,4c00H
    int    21H

	movsb 

	movsw
	lodsb
	lodsw
	cmpsb
	cmpsw
	scasb
	scasw


	xor word ptr cs:[bx+5], 5

main    endp                ;<=== End function
	
end begin                ;<=== End program


	;; tasm mk_com
	;; tlink /t mk_com
