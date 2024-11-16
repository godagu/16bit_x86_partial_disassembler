.model small
.stack 100H

BUFFER_READ_SIZE = 02H
NEW_LINE macro
        mov dl, 13
        mov ah, 02h
        int 21h

        mov dl, 10
        mov ah, 02h
        int 21h
endm

SPACE macro
        mov dl, 32
        mov ah, 02h
        int 21h
endm

COMMA macro
        mov dl, 44
        mov ah, 02h
        int 21h

endm

ONE macro
        mov dl, 49
        mov ah, 02h
        int 21h
endm




JUMPS
.data

err_s db 'Source file could not be opened', 13, 10, '$'
err_d db 'Destination file could not be opened', 13, 10, '$'

msg db 'helloooo', 13, 10, '$'

sourceF db 13 dup(0)
sourceHandle dw ?
_next_file dw 0081h

destF db 13 dup(0)
destHandle dw ?

buffer db 2 dup(0)
current_buffer_size db 2

_w db ?
symbol_w db 'w'
symbol_b db 'b'

_v db ?

_mod db ?

_rm db ?

_s db ?

_offset_value db ?

_cl db 'CL$'



opp_1010 db 02h, 'movs'
         db 03h, 'cmps'
         db 05h, 'stos'
         db 06h, 'lods'
         db 07h, 'scas'

opp_1101 db 00h, 'rol'
        db 01h, 'ror'
        db 02h, 'rcl'
        db 03h, 'rcr'
        db 04h, 'shl'
        db 05h, 'shr'
        db 07h, 'sar'


opp_1000 db 00h, 'add'
        db 01h, 'or '
        db 02h, 'adc'
        db 03h, 'sbb'
        db 04h, 'and'
        db 05h, 'sub'
        db 06h, 'xor'
        db 07h, 'cmp'

segment_prefix_table db 00h, 'ES:'
                     db 01h, 'CS:'
                     db 02h, 'SS:'
                     db 03h, 'DS:'
        
register_table_w_0 db 00h, 'AL'
                  db 01h, 'CL'
                  db 02h, 'DL'
                  db 03h, 'BL'
                  db 04h, 'AH'
                  db 05h, 'CH'
                  db 06h, 'DH'
                  db 07h, 'BH'

register_table_w_1 db 00h, 'AX'
                    db 01h, 'CX'
                    db 02h, 'DX'
                    db 03h, 'BX'
                    db 04h, 'SP'
                    db 05h, 'BP'
                    db 06h, 'SI'
                    db 07h, 'DI'

reg_memory_table_mod_00 db 00h, 'BX+SI'
                        db 01h, 'BX+DI'
                        db 02h, 'BP+SI'
                        db 03h, 'BP+DI'
                        db 04h, 'SI   '
                        db 05h, 'DI   '
                        db 06h, '     '
                        db 07h, 'BX   '

reg_memory_table_mod_01_10 db 00h,  'BX+SI+'
                            db 01h, 'BX+DI+'
                            db 02h, 'BP+SI+'
                            db 03h, 'BP+DI+'
                            db 04h, 'SI+   '
                            db 05h, 'DI+   '
                            db 06h, 'BP+   '
                            db 07h, 'BX+   '



.code
START:

;; liko JMP, ROL, XOR, SAR, segmento registro keitimo prefiksaa


mov ax, @data
mov es, ax

call skip_spaces

lea di, es:[destF]
call read_filename

cmp byte ptr es:[destF], 0
je stop

lea di, es:[sourceF]
call read_filename

;; if not entered --> help
cmp byte ptr es:[sourceF], 0
je stop

push ds si

mov ax, @data
mov ds, ax

source_from_file:
    lea dx, ds:[sourceF]
    mov ah, 3dh
    mov al, 00h
    int 21h

    jc err_source

    mov [sourceHandle], ax

    call read_buffer

; dailas atidarytas
; i si ikelta buffer adresas --> t.y. ikeltas pirmas baitas kuri reik parsint
parse_loop:
    mov bl, byte ptr ds:[si]
    and bl, 0E7h ;; for identifying segment reg change prefix
    cmp bl, 26h
    je _001sr

    mov al, byte ptr ds:[si]
    shr al, 04h

    cmp al, 0Ah
    je _1010

    cmp al, 0Dh
    je _1101

    cmp al, 08h
    je _1000

    jmp continue_loop

    _1010:
        call handle_1010
        jmp continue_loop

    _001sr:
        call handle_001sr
        jmp continue_loop

    _1101:
        call handle_1101
        jmp continue_loop

    _1000:
        call handle_1000
        jmp continue_loop


continue_loop:
    call handle_buffer
    jmp parse_loop


handle_1010 PROC
    mov al, byte ptr ds:[si]
    and al, 0Fh ;; and with 00001111 to unmask the last 4 bits

    call get_w ;; find out w

    shr al, 0001h ;;bit shift

    lea bx, opp_1010 ;; load array
    mov cx, 05h ;; how many elements in array

    _handle_1010_array_loop:
        cmp al, byte ptr [bx]
        je _handle_1010_opcode_found


        add bx, 05h ;; how long is the element in array

    loop _handle_1010_array_loop

    _handle_1010_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h
        mov cx, 04h ;;kiek bitu rasysim
        int 21h

        cmp byte ptr ds:[_w], 00H ;;palyginam ar w yra 0 ar 1
        je write_b

        mov dl, byte ptr ds:[symbol_w]
        mov ah, 02h
        int 21h 

        ;; newline
        NEW_LINE

        ;; irasyti w

        ret
       
    write_b:
        mov dl, byte ptr ds:[symbol_b]
        mov ah, 02h
        int 21h 

        NEW_LINE


    ret


handle_1010 ENDP

handle_001sr PROC
    mov al, byte ptr ds:[si]
    and al, 18h ; and su 000 11 000 to get the sr

    shr al, 0003h ; byte shift to allign

    lea bx, segment_prefix_table
    mov cx, 04h

    _handle_001sr_array_loop:
        cmp al, byte ptr [bx]
        je _handle_001sr_opcode_found

        add bx, 04h ;pridedam tiek, kiek zodzio ilgis array

    loop _handle_001sr_array_loop

    _handle_001sr_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 03h ;;kiek bitu rasysim
        int 21h

        ;; newline
        NEW_LINE

        ret

handle_001sr ENDP

handle_1101 PROC
    ;; get w and v
    mov al, byte ptr ds:[si]

    call get_w
    call get_v

    call handle_buffer
    mov al, byte ptr ds:[si]

    call get_mod
    call get_rm

    and al, 38h
    shr al, 3

    lea bx, opp_1101
    mov cx, 07h

    _handle_1101_array_loop:
        cmp al, byte ptr [bx]
        je _handle_1101_opcode_found

        add bx, 04h

    loop _handle_1101_array_loop

    _handle_1101_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 03h ;;kiek bitu rasysim
        int 21h

        SPACE

        ;; handle_mod_00 through handle_mod_11
        ;;

        ;; paskutinis zinsnis, ziuret pagal v reiksme ar bus , CL ar bus , 1

        call handle_mod

        SPACE

        cmp byte ptr ds:[_v], 01h
        je print_CL

        ONE
        jmp continue


        print_CL:
            lea dx, _cl
            mov ah, 40h
            mov bx, 0001h
            mov cx, 02h
            int 21h

        continue:
            NEW_LINE
            ret

handle_1101 ENDP

handle_1000 PROC
        mov al, byte ptr ds:[si]
        call get_s
        call get_w

        call handle_buffer
        mov al, byte ptr ds:[si]

        call get_mod
        call get_rm


        and al, 38h
        shr al, 3

        lea bx, opp_1000
        mov cx, 08h

        _handle_1000_array_loop:
            cmp al, byte ptr [bx]
            je _handle_1000_opcode_found

            add bx, 04h

        loop _handle_1000_array_loop

        _handle_1000_opcode_found:
            inc bx
            mov dx, bx
            mov ah, 40h
            mov bx, 0001h ;; kol kas i stdout
            mov cx, 03h ;;kiek bitu rasysim
            int 21h

            SPACE


            call handle_mod


            SPACE
            ;; s = 0, w = 1 skaitom 2 baitus
            cmp byte ptr [_s], 0h
            je _handle_1000_s_0

            cmp byte ptr [_w], 0h
            je _handle_1000_s_0

            ;; handle special case s=1, w=1

            call handle_buffer
            mov al, byte ptr ds:[si]

            cmp al, 80h
            jae _handle_1000_spec_case

            xor ah, ah
            call print_ax_hex

            jmp _handle_1000_end

            _handle_1000_spec_case:
                mov ah, 0FFh
                call print_ax_hex

                jmp _handle_1000_end

            _handle_1000_w_0:
                call handle_buffer
                mov al, byte ptr ds:[si]

                mov ah, 00h
                call print_ax_hex

                jmp _handle_1000_end


            _handle_1000_s_0:
                ;; jei w = 0 skaitom 1
                cmp byte ptr [_w], 0h
                je _handle_1000_w_0

                call handle_buffer
                mov al, byte ptr ds:[si]

                call handle_buffer
                mov ah, byte ptr ds:[si]

                call print_ax_hex

                jmp _handle_1000_end


        ;; 4 atvejai
        ;; s = 0 w = 0 --> skaitom tik 1 ir ji rasom
        ;; s = 0 w = 1 --> skaitau 2 baitus
        ;;spec case: nuskaitom viena baita, isidedam ji kazkur (pvz al) ir uzpildom ah arba 00 arba FF pagal most significant byte
        ;; s = 1 w = 0 --> skaitom tik 1 ir ji rasom
        ;; s = 1 w = 1 --> spec case: nuskaitom viena baita, isidedam ji kazkur (pvz al) ir uzpildom ah arba 00 arba FF pagal most significant byte


        _handle_1000_end:
            NEW_LINE
            ret


handle_1000 ENDP

get_w PROC
    push ax
        and al, 0001h
        mov byte ptr ds:[_w], al
    pop ax
    ret
get_w ENDP

get_v PROC
    push ax
        and al, 02h
        shr al, 1
        mov byte ptr ds:[_v], al
    pop ax
    ret
get_v ENDP

get_s PROC
    push ax
        and al, 02h
        shr al, 1
        mov byte ptr ds:[_s], al
    pop ax
    ret


get_s ENDP

get_mod PROC
    push ax
        shr al, 06h
        mov byte ptr ds:[_mod], al
    pop ax
    ret
get_mod ENDP

get_rm PROC
    push ax
        and al, 07h
        mov byte ptr ds:[_rm], al
    pop ax
    ret
get_rm ENDP

handle_mod PROC
    cmp byte ptr ds:[_mod], 0h
        je handle_mod_00_label
    cmp byte ptr ds:[_mod], 1h
        je handle_mod_01_label
    cmp byte ptr ds:[_mod], 2h
        je handle_mod_10_label
    cmp byte ptr ds:[_mod], 3h
        je handle_mod_11_label

    handle_mod_00_label:
        call handle_mod_00
        ret
    handle_mod_01_label:
        call handle_mod_01
        ret
    handle_mod_10_label:
        call handle_mod_10
        ret
    handle_mod_11_label:
        call handle_mod_11
        ret

handle_mod ENDP


handle_mod_00 PROC
    mov cx, 08h
    mov al, ds:[_rm]

    lea bx, reg_memory_table_mod_00


    _handle_mod_00_array_loop:
        cmp al, byte ptr [bx]
        je _handle_mod_00_opcode_found

        add bx, 06h
    loop _handle_mod_00_array_loop

    _handle_mod_00_opcode_found:
        cmp al, 06h
        je _handle_mod_00_spec_case

        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 5h ;;kiek bitu rasysim
        int 21h

        COMMA

        ret

    ;; when rm 110, direct address
    _handle_mod_00_spec_case:
        call handle_buffer ;; pasiimu kitus du baitus
        mov al, byte ptr ds:[si]

        call handle_buffer
        mov ah, byte ptr ds:[si]

        call print_ax_hex

        COMMA

        ret

        ;; cia reikia skirti du baitus ?? ir is kazkur patraukt ju reiksme? poslinkis?

handle_mod_00 ENDP


;; procedure to print ax
print_ax_hex PROC
	push ax	 ; will need it later

	; parse and print ah
	shr ax, 8 ;; byte shift to right to make printing easy
	mov bh, 16 ;; divide to get hex
	div bh ; al / bh    ah - remainder, al - quetient

        push ax
	;mov bl, ah ; save ah for later (remainder)

	;; handle quotient
	cmp al, 10
	jb _print_ax_hex_first_number

	add al, 7

	_print_ax_hex_first_number:
        add al, '0'
        mov byte ptr ds:[_offset_value], al
        lea dx, _offset_value
        mov ah, 40h
        mov bx, 0001h
        mov cx, 1h
        int 21h

    ;; handle remainder
        pop ax
        mov bl, ah
	cmp bl, 10
	jb	_print_ax_hex_second_number

	add bl, 7

	_print_ax_hex_second_number:
        add bl, '0'
        mov byte ptr ds:[_offset_value], bl
        lea dx, _offset_value
        mov ah, 40h
        mov bx, 0001h
        mov cx, 1h
        int 21h

	; parse and print al
	pop ax
	and ax, 00FFh ;; and with 00001111 to unmask the last bits of ax
	mov bh, 16
	div bh
	;; dividing by 16

	;mov bl, ah ; save ah for later
        push ax

	;; print quotient
	cmp al, 10
	jb _print_ax_hex_third_number

	add al, 7 ;; if letter add 7

	_print_ax_hex_third_number:
        add al, '0'
        mov byte ptr ds:[_offset_value], al
        lea dx, _offset_value
        mov ah, 40h
        mov bx, 0001h
        mov cx, 1h
        int 21h

    ;; print remainder
        pop ax
        mov bl, ah
	cmp bl, 10
	jb	_print_ax_hex_fourth_number

	add bl, 7

	_print_ax_hex_fourth_number:
        add bl, '0'
        mov byte ptr ds:[_offset_value], bl
        lea dx, _offset_value
        mov ah, 40h
        mov bx, 0001h
        mov cx, 1h
        int 21h

	ret

print_ax_hex ENDP


handle_mod_01 PROC
    mov cx, 08h
    mov al, ds:[_rm]

    lea bx, reg_memory_table_mod_01_10

    _handle_mod_01_array_loop:
        cmp al, byte ptr [bx]
        je _handle_mod_01_opcode_found

        add bx, 07h
    loop _handle_mod_01_array_loop

    _handle_mod_01_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 6h ;;kiek bitu rasysim
        int 21h

        call handle_buffer ;; pasiimu kita baita (poslinkis tik per viena)
        mov al, byte ptr ds:[si]

        mov ah, 00h
        call print_ax_hex

        COMMA

    ;; 1 baito poslinkis
    ret
handle_mod_01 ENDP


handle_mod_10 PROC
    mov cx, 08h
    mov al, ds:[_rm]

    lea bx, reg_memory_table_mod_01_10

    _handle_mod_10_array_loop:
        cmp al, byte ptr [bx]
        je _handle_mod_10_opcode_found

        add bx, 07h
    loop _handle_mod_10_array_loop

    _handle_mod_10_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 6h ;;kiek bitu rasysim
        int 21h

        call handle_buffer ;; pasiimu kita baita (poslinkis tik per viena)
        mov al, byte ptr ds:[si]

        call handle_buffer
        mov ah, byte ptr ds:[si]

        call print_ax_hex

        COMMA

    ;; 1 baito poslinkis
    ret
    ;; dvieju baitu poslinkis
    ret
handle_mod_10 ENDP


handle_mod_11 PROC
    mov cx, 08h
    mov al, ds:[_rm]

    cmp byte ptr ds:[_w], 0h
    je handle_w_0

    lea bx, register_table_w_1
    jmp _handle_mod_11_array_loop

    handle_w_0:
         lea bx, register_table_w_0

    _handle_mod_11_array_loop:
        cmp al, byte ptr [bx]
        je _handle_mod_11_opcode_found

        add bx, 03h
    loop _handle_mod_11_array_loop

    _handle_mod_11_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 2h ;;kiek bitu rasysim
        int 21h

        COMMA

        ret

handle_mod_11 ENDP


handle_buffer PROC
    inc si
    dec [current_buffer_size]
    cmp byte ptr [current_buffer_size], 0
    ja handle_buffer_return

    call read_buffer ;; if nothing more to read

    handle_buffer_return:
        ret

handle_buffer ENDP

read_buffer PROC
    push ax
    mov ah, 3Fh
    lea dx, buffer
    mov cx, BUFFER_READ_SIZE
    mov bx, [sourceHandle]
    int 21h
    cmp ax, 00H
    je stop

    lea si, buffer
    mov ds:[current_buffer_size], al
    pop ax
    ret
read_buffer ENDP



read_filename PROC
    push ds es ;; pasipushinam kad nesugadintume

    mov ax, @data 
    mov es, ax

    mov ah, 62h ;; get PSP address
    int 21h

    mov ds, bx
    push ax

    mov si, es:[_next_file] ;; pradzia parametru
    call skip_spaces ;; praskipinam spaceus

    mov cx, 0

    read_filename_start:
        cmp byte ptr ds:[si], 13 ;; jeigu nebera ka skaityt
        je read_filename_end

        inc cx

        cmp cx, 13 ;; jei ilgesnis nei 13 terminate§1
        ja stop

        cmp byte ptr ds:[si], ' ' ;; jei nuskaitem visa faila
        jne read_filename_next

    read_filename_end:
        mov al, 0 ;;paclearinam ax nes cia storinsim

        stosb ;; storinam file name'a akumuliatoriuje
        pop ax
        mov es:[_next_file], si ;;tolimesniems failams

        pop es ds ;; grazinam
        ret
    
    read_filename_next:
        lodsb ;;paloadinam baita i akumulaiatoriu
        stosb ;;akumuliatoriaus reiksme storina es:di

        jmp read_filename_start
read_filename ENDP


skip_spaces PROC
    mov si, es:[_next_file]

    skip_spaces_loop:
        cmp byte ptr ds:[si], ' '
        jne skip_spaces_end
        inc si
        jmp skip_spaces_loop

    skip_spaces_end:
        mov es:[_next_file], si
        ret
skip_spaces ENDP



;; terminate program
stop:
    mov ax, 4c00h 
    int 21h 


err_dest:
    mov ax, @data
    mov ds, ax
    
    lea dx, err_d
    mov ah, 09h
    int 21h

    lea dx, destF
    mov ah, 09h
    int 21h

    mov ax, 4c01h ;; return with 1
    int 21h

err_source:
    mov ax, @data
    mov ds, ax
    
    lea dx, err_s
    mov ah, 09h
    int 21h

    mov ax, 4c01h ; return with 1
    int 21h

end START
