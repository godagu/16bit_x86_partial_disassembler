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

	xor word ptr cs:[bx+5], 5

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


main    endp                ;<=== End function
	
end begin                ;<=== End program


	;; tasm mk_com
	;; tlink /t mk_com
