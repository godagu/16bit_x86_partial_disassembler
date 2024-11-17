.model small
	
.code
org    100H	

JUMPS	
	
begin:

	jmp main
	

main    proc    near        ; <=== Entry point (main function)
; 	jmp b
; 	b:
; 	xor ax, 32h
; 	xor word ptr es:[bx+di+0FFAh], 16
; 	aam
; 	clc
; 	nop
; 	nop
; 	nop
; 	call b
;
; 	b:

	xor ax, 32h
	;test cx, [bp+di]

; 	nop
; 	nop
; 	nop

	
main    endp                ;<=== End function
	
end begin                ;<=== End program


	;; tasm mk_com
	;; tlink /t mk_com
