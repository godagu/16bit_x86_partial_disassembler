
.model small

.code
org    100H
begin:
	jmp    main

main    proc    far        ; <=== Entry point (main function)
    nop
    nop
    nop
    call bx
    nop
    nop
    nop
    push [bp+di]

    push [bp+di+1111h]

    push es:[2]

    inc es:[1]

    inc al
    inc dx
    dec ah
    dec byte ptr [bp+di]

    shr ax, cl

    xchg ax, ax


    nop
    nop
    nop





main    endp                ;<=== End function

end begin                ;<=== End program


	;; tasm mk_com
	;; tlink /t mk_com
