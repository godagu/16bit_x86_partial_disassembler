
.model small

.code
org    100H
begin:
	jmp    main

main    proc    far        ; <=== Entry point (main function)
    inc byte ptr [bp+di+0AfAfh]

    call bx
    push [bp+di]

    push [bp+di+1111h]

    sub cx, -1
    sub dx, 0


    push es:[0FFAAh]

    inc byte ptr es:[1]

    inc dx
    dec ah
    dec byte ptr [bp+di]

    shr ax, cl

    nop
    nop

      ; JMP instructions
    jmp short label1
    jmp word ptr [bx+si]
    jmp word ptr [bx+1234h]

    label1:
        nop

    nop
    nop


    xor word ptr [bx+97E4h], dx
    xor byte ptr [bp+di], 0AAh


    nop
    nop


    ; INC (memory)
    inc word ptr [bp+si]
    inc byte ptr [bp+si+0FFh]

    nop
    nop


    ; DEC (memory)
    dec word ptr [bx+di]
    dec byte ptr [di+5678h]

    nop
    nop


    ; CALL (opcode 1111xxxx)
    call word ptr [si]
    call word ptr [bp+124h]

    nop
    nop


    ; PUSH (reg/mem)
    push ax
    push bp
    push word ptr es:[bx+si+10h]

    nop

    nop
    nop

    nop

    adc word ptr cs:[si+10h], 01h

    nop
    nop

    sbb cl, 22h
    sbb word ptr [bx+di], 09h

    nop
    nop

    and al, 0F0h

    nop
    and word ptr es:[di], 0F0Fh

    nop
    nop


    ; SUB (opcode 1000xxxx)
    sub ax, dx
    sub bl, 15h
    nop

    sub word ptr [bp+si], bx


    nop
    nop

    cmp word ptr [si], 23h

    nop
    nop


    ; MOVS
    movsb
    movsw

    ; CMPS
    cmpsb
    cmpsw

    ; STOS
    stosb
    stosw

    ; LODS
    lodsb
    lodsw

    ; SCAS
    scasb
    scasw


    nop
    nop

    nop
    nop

    ; ROL instructions
    rol al, 1
    rol word ptr [si], cl


    nop


    ; ROR instructions
    ror ah, 2
    ror word ptr [bx], cl


    nop
    nop


    ; RCL instructions
    rcl al, 1
    rcl word ptr [bp+di], cl


    nop
    nop


    ; RCR instructions
    rcr ah, 2
    rcr word ptr [bp], cl


    nop
    nop


    ; SHL instructions
    shl al, 3
    shl word ptr [di+1234h], 1


    nop
    nop


    ; SHR instructions
    shr al, 1
    shr word ptr [bx+si], cl


    nop
    nop


    ; SAR instructions
    sar al, 1
    sar word ptr [bp+di], cl






main    endp                ;<=== End function

end begin                ;<=== End program


	;; tasm mk_com
	;; tlink /t mk_com
