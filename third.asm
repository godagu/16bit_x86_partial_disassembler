.model small
.stack 100H

;; ------------------------------------------------------------
;; |    /\  /\         /\       /----  |--\  /---\     /----- |
;; |   /  \/  \       /  \     /       |__|  |   |     |      |
;; |  /        \     /----\    \       |\    |   |     \----\ |
;; | /          \   /      \    \----  | \   \---/      ----/ |
;; ------------------------------------------------------------
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

COLON macro
    mov dl, 58
    mov ah, 02h
    int 21h
endm

H macro
    mov dl, 'h'
    mov ah, 02h
    int 21h
endm

JUMPS
.data

;; error messages
err_s db 'Source file could not be opened', 13, 10, '$'
err_d db 'Destination file could not be opened', 13, 10, '$'

;; other printed strings
command_not_identified db 'This command could not be identified'
    command_not_identified_len  equ $ - command_not_identified ;; to get size
msg_bytes_parsed db 'Bytes parsed:'
    msg_bytes_parsed_len equ $ - msg_bytes_parsed

about db 'This program disassembles some of x86_16bit commands', 13, 10, 'third.exe destinationFile sourceFile', 13, 10, 13, 10, 'third /? - help', 13, 10, '$'

input_command_line db 'Please enter commands in HEX to disassemble:', 13, 10, '$'

buffer_reading_cmnd_line db 255, 0, 255 dup(?)



;; handling files
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
_d db ?
_reg db ?

_offset_value db ?

;; for single commands

_cl db 'cl$'
_xor db 'xor$'
_jmp db 'jmp$'

temp_file db 'temp.txt', 0


;; for handling printing address or bytes parses
_address dw 00FFh
bytes_bfr db 16 dup (?)
bytes_bfr_size db 0


;; tables for different opp codes
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


opp_1111_1111 db 00h, 'inc '
                db 01h, 'dec '
                db 02h, 'call'
                db 03h, 'call'
                db 04h, 'jmp '
                db 05h, 'jmp '
                db 06h, 'push'


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

;; liko help msg, jei be parametru skaityt tiesiogiai is stdin

mov ax, @data
mov es, ax

call skip_spaces

;; if /? entered --> help
mov ax, word ptr ds:[si]
cmp ax, 3F2Fh
je help

;; read destination file name
lea di, es:[destF]
call read_filename

;cmp byte ptr es:[destF], 0
;je stop

push ds

mov ax, @data
mov ds, ax

lea dx, ds:[destF]
mov ah, 3Ch
xor cx, cx
mov al, 00h
int 21h

pop ds

jc err_dest

mov es:[destHandle], ax

;; redirect file to stdout

;; read source file name
lea di, es:[sourceF]
call read_filename

;; if not entered --> help
cmp byte ptr es:[sourceF], 0
jne aaa10

call read_command_line

aaa10:

cmp byte ptr es:[destF], 0
je aaa11

mov bx, word ptr es:[destHandle]
mov ah, 46h
mov cx, 0001h
int 21h

aaa11:

;;JEI NEI VIENAS FAILAS NENURODYTAS SKAITYT IS SDIN???????

push ds si

mov ax, @data
mov ds, ax

source_from_file:
    ;; open file for reading
    lea dx, ds:[sourceF]
    mov ah, 3dh
    mov al, 00h
    int 21h

    jc err_source

    mov [sourceHandle], ax

    ;; read first byte
    call read_buffer
    inc [_address]

    ;; move to si first byte to read
    mov al, byte ptr ds:[si]
    call mov_current_byte_buff_out

; dailas atidarytas
; i si ikelta buffer adresas --> t.y. ikeltas pirmas baitas kuri reik parsint
parse_loop:
    call print_address

    ;; identifying segment reg change prefix
    mov bl, byte ptr ds:[si]
    and bl, 0E7h ;; for identifying segment reg change prefix
    cmp bl, 26h
    je _001sr

    ;; load again, shift right to get the first party
    mov al, byte ptr ds:[si]
    shr al, 04h

    cmp al, 0Ah
    je _1010

    cmp al, 0Dh
    je _1101

    cmp al, 08h
    je _1000

    cmp al, 03h
    je _0011

    cmp al, 0Eh
    je _1110

    cmp al, 0Fh
    je _1111_1111


    ; id none of the above, print commmand not identified
    lea dx, command_not_identified
    mov ah, 40h
    mov bx, 0001h
    mov cx, command_not_identified_len
    int 21h

    SPACE
    call print_byte_buff

    NEW_LINE

    ;; read more
    jmp continue_loop

    _1010:
        ;; MOVSB, MOVSW, CMPSB, CMPSW, STOSB, STOSW, LODSB, LODSW, SCASB, SCASW

        mov al, byte ptr ds:[si]
        and al, 0Fh
        shr al, 1h
        ;; chech if not 0100


        ;; rule out test command
        cmp al, 4h
        je _parse_loop_handle_1010_not_found

        ;; rule out mov acc <- memory
        cmp al, 0h
        je _parse_loop_handle_1010_not_found


        ;;rule out mov memory <- acc
        cmp al, 1h
        je _parse_loop_handle_1010_not_found


        call handle_1010
        jmp continue_loop

        _parse_loop_handle_1010_not_found:

            lea dx, command_not_identified
            mov ah, 40h
            mov bx, 0001h
            mov cx, command_not_identified_len
            int 21h

            SPACE
            call print_byte_buff

            NEW_LINE
            jmp continue_loop

    _001sr:
        ;; SEGMENT REGISTER CHANGE PREFIX
        mov al, byte ptr ds:[si]
        and al, 07h

        ;; check if ends in 001x x110
        cmp al, 6h
        je _parse_loop_handle_001sr

        ;; if not segment change prefix command, then print not identified
        lea dx, command_not_identified
        mov ah, 40h
        mov bx, 0001h
        mov cx, command_not_identified_len
        int 21h

        SPACE
        call print_byte_buff

        NEW_LINE
        jmp continue_loop

        ;; if correct command continue
        _parse_loop_handle_001sr:
            call handle_001sr
            jmp continue_loop

    _1101:
        ;; ROl, ROR, RCL, RCR, SHL/SAL, SHR, SAR
        mov al, byte ptr ds:[si]
        and al, 0Fh
        shr al, 2h

        ;; rule out aam, aad, xlat, esc
        cmp al, 00h
        je _parse_loop_handle_1101


        ;; not identified
        lea dx, command_not_identified
        mov ah, 40h
        mov bx, 0001h
        mov cx, command_not_identified_len
        int 21h

        SPACE
        call print_byte_buff

        NEW_LINE

        jmp continue_loop

        ;; all good - continue
        _parse_loop_handle_1101:
            call handle_1101
            jmp continue_loop

    _1000:
        ;; ADD, OR, ADC, SBB, AND, SUB, XOR, CMP
        mov al, byte ptr ds:[si]
        and al, 0Fh
        shr al, 2h

        ;; rule out test through pop
        cmp al, 0h
        je _parse_loop_handle_1000


        ;; not identified
        lea dx, command_not_identified
        mov ah, 40h
        mov bx, 0001h
        mov cx, command_not_identified_len
        int 21h

        SPACE
        call print_byte_buff

        NEW_LINE
        jmp continue_loop


        ;; identified, lets go
        _parse_loop_handle_1000:
            call handle_1000
            jmp continue_loop

    _0011:
        ;;XOR (reg->reg/mem), XOR (acc, betarpiskas)
        mov al, byte ptr ds:[si]

        and al, 0Ch
        shr al, 2h

        ;; check for XOR reg, reg/mem
        cmp al, 0h
        je _parse_loop_handle_0011

        mov al, byte ptr ds:[si]
        and al, 0Fh
        shr al, 1h

        ;; check for XOR acc, immediate
        cmp al, 2h
        je _parse_loop_handle_0011

        ;; all the ohters in 0011 fam not identified :(
        lea dx, command_not_identified
        mov ah, 40h
        mov bx, 0001h
        mov cx, command_not_identified_len
        int 21h

        SPACE
        call print_byte_buff
        NEW_LINE

        ;; if not the xors continue
        jmp continue_loop

        ;; top security clearance checked. go through.
        _parse_loop_handle_0011:
            call handle_0011
            jmp continue_loop

    _1110:
        ;; JMP (vidinis tiesioginis), JMP(isorinis tiesioginis), JMP (vidinis artimas)
        mov al, byte ptr ds:[si]

        and al, 0Fh

        ;; check for JMP label (inner direct)
        cmp al, 09h
        je _parse_loop_handle_1110

        ;; check for JMP label (outter indirect)
        cmp al, 0Ah
        je _parse_loop_handle_1110

;       ;; check for JMP label (inner near)
        cmp al, 0Bh
        je _parse_loop_handle_1110

        ;; if not any of the JMPs: mop mop moooop
        lea dx, command_not_identified
        mov ah, 40h
        mov bx, 0001h
        mov cx, command_not_identified_len
        int 21h

        SPACE
        call print_byte_buff
        NEW_LINE


        jmp continue_loop

        ;; if(JMP==true){}
        _parse_loop_handle_1110:
            call handle_1110
            jmp continue_loop

    _1111_1111:
        ;; INC, DEC, CAll (in), CALL (out), JMP (in), JMP (out), PUSH
        mov al, byte ptr [si]

        ;; rule out not in he 1111_111x fam
        and al, 0Eh
        cmp al, 0Eh
        je _parse_loop_handle_1111_1111

        ;; not identified, amigo
        lea dx, command_not_identified
        mov ah, 40h
        mov bx, 0001h
        mov cx, command_not_identified_len
        int 21h

        SPACE
        call print_byte_buff
        NEW_LINE

        jmp continue_loop

        ;; you are accepted for the job
        _parse_loop_handle_1111_1111:

            call handle_1111_1111
            jmp continue_loop


continue_loop:
    ;; get more bytes
    call handle_buffer
    jmp parse_loop

;; baigiau cia

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

        SPACE
        call print_byte_buff
        ;; newline
        NEW_LINE

        ;; irasyti w

        ret
       
    write_b:
        mov dl, byte ptr ds:[symbol_b]
        mov ah, 02h
        int 21h 

        SPACE
        call print_byte_buff
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


        SPACE
        call print_byte_buff
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
        COMMA

        SPACE

        cmp byte ptr ds:[_v], 01h
        je print_CL

        ONE
        H

        jmp continue


        print_CL:
            lea dx, _cl
            mov ah, 40h
            mov bx, 0001h
            mov cx, 02h
            int 21h

        continue:
            SPACE
            call print_byte_buff
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

            COMMA


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
            SPACE
            call print_byte_buff
            NEW_LINE
            ret


handle_1000 ENDP


handle_0011 PROC

      mov al, byte ptr ds:[si]
        and al, 0Fh

        shr al, 2h

        cmp al, 0h
        je _handle_1000_00dw

        cmp al, 1h
        je _handle_1000_010w

        ret

        _handle_1000_00dw:
            mov al, byte ptr ds:[si]
            call get_d
            call get_w

            lea dx, _xor
            mov ah, 40h
            mov bx, 0001h
            mov cx, 03h
            int 21h

            SPACE

            call handle_buffer
            mov al, byte ptr ds:[si]

            call get_mod
            call get_reg
            call get_rm


            cmp [_d], 1h
            je _handle_0011_d_1

            ;; else handle d=1
            call handle_mod

            COMMA
            SPACE

            call handle_mod_11_reg

            jmp _handle_0011_end

        _handle_0011_d_1:
            call handle_mod_11_reg

            COMMA
            SPACE

            call handle_mod

            jmp _handle_0011_end


        _handle_1000_010w:
            mov al, byte ptr ds:[si]
            call get_w

            lea dx, _xor
            mov ah, 40h
            mov bx, 0001h
            mov cx, 03h
            int 21h

            SPACE

            lea dx, register_table_w_1
            inc dx
            mov ah, 40h
            mov bx, 0001h
            mov cx, 02h
            int 21h

            COMMA
            SPACE

            call handle_buffer
            mov al, byte ptr ds:[si]

            xor ah, ah

            cmp [_w], 0h
            je _handle_1000_print_ax_hex


            call handle_buffer
            mov ah, byte ptr ds:[si]


            _handle_1000_print_ax_hex:
            ;; for bojb [bovb] ?????
                call print_ax_hex


        _handle_0011_end:
            SPACE
            call print_byte_buff
            NEW_LINE
            ret

handle_0011 ENDP

handle_1110 PROC

        ;; print jmp
        lea dx, _jmp
        mov ah, 40h
        mov bx, 0001h
        mov cx, 03h
        int 21h

        SPACE

        mov al, byte ptr ds:[si]
        and al, 0Fh

        cmp al, 09h
        je _handle_1110_1001

        cmp al, 0Ah
        je _handle_1110_1010

        cmp al, 0Bh
        je _handle_1110_1011


        _handle_1110_1001:
            call handle_buffer
            mov al, byte ptr ds:[si]


            call handle_buffer
            mov ah, byte ptr ds:[si]


            call print_ax_hex

            SPACE
            call print_byte_buff
            NEW_LINE

            ret

        _handle_1110_1010:
            call handle_buffer
            mov al, byte ptr ds:[si]


            call handle_buffer
            mov ah, byte ptr ds:[si]


            call print_ax_hex

            call handle_buffer
            mov al, byte ptr ds:[si]


            call handle_buffer
            mov ah, byte ptr ds:[si]


            call print_ax_hex

            SPACE
            call print_byte_buff
            NEW_LINE

            ;; keturi baitus atspausdint
            ret

        _handle_1110_1011:
            call handle_buffer
            mov al, byte ptr ds:[si]

                call print_ax_hex_1_byte
            ;xor ah, ah

            ;call print_ax_hex
            SPACE
            call print_byte_buff
            NEW_LINE


            ret


handle_1110 ENDP

handle_1111_1111 PROC
    mov al, byte ptr ds:[si]

    call get_w

    call handle_buffer
    mov al, byte ptr ds:[si]

    and al, 38h
    shr al, 3h

    lea bx, opp_1111_1111
    mov cx, 07h

    _handle_1111_1111_array_loop:
        cmp al, byte ptr [bx]
        je _handle_1111_1111_opcode_found

        add bx, 05h

    loop _handle_1111_1111_array_loop

    _handle_1111_1111_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 04h ;;kiek bitu rasysim
        int 21h

        mov al, byte ptr ds:[si]

        call get_mod

        call get_rm
        SPACE

        call handle_mod

        SPACE
        call print_byte_buff
        NEW_LINE

    _handle_1111_1111_end:
        ret


handle_1111_1111 ENDP

handle_mod_11_reg PROC
    mov cx, 08h
    mov al, ds:[_reg]

    cmp byte ptr ds:[_w], 0h
    je _handle_mod_11_handle_w_0

    lea bx, register_table_w_1
    jmp _handle_mod_11_xor_array_loop

    _handle_mod_11_handle_w_0:
         lea bx, register_table_w_0

    _handle_mod_11_xor_array_loop:
        cmp al, byte ptr [bx]
        je _handle_mod_11_xor_opcode_found

        add bx, 03h
    loop _handle_mod_11_xor_array_loop

    _handle_mod_11_xor_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 2h ;;kiek bitu rasysim
        int 21h

        ret

handle_mod_11_reg ENDP


handle_rm PROC
    mov cx, 08h
    mov al, ds:[_rm]

    cmp byte ptr ds:[_w], 0h
    je _handle_rm_handle_w_0

    lea bx, register_table_w_1
    jmp _handle_rm_array_loop

    _handle_rm_handle_w_0:
         lea bx, register_table_w_0

    _handle_rm_array_loop:
        cmp al, byte ptr [bx]
        je _handle_rm_opcode_found

        add bx, 03h
    loop _handle_rm_array_loop

    _handle_rm_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h ;; kol kas i stdout
        mov cx, 2h ;;kiek bitu rasysim
        int 21h

        ret

handle_rm ENDP

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

get_d PROC

    push ax
        and al, 02h
        shr al, 1
        mov byte ptr ds:[_d], al
    pop ax
    ret


get_d ENDP


get_mod PROC
    push ax
        shr al, 06h
        mov byte ptr ds:[_mod], al
    pop ax
    ret
get_mod ENDP

get_reg PROC
    push ax
        and al, 38h
        shr al, 3h
        mov byte ptr ds:[_reg], al
    pop ax
    ret
get_reg ENDP

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

        ret

    ;; when rm 110, direct address
    _handle_mod_00_spec_case:
        call handle_buffer ;; pasiimu kitus du baitus
        mov al, byte ptr ds:[si]

        call handle_buffer
        mov ah, byte ptr ds:[si]

        call print_ax_hex

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
        H

	ret

	H

print_ax_hex ENDP

;; procedure to print ax
print_ax_hex_1_byte PROC
	push ax	 ; will need it later

	; parse and print ah
	;shr ax, 8 ;; byte shift to right to make printing easy
	xor ah, ah
	mov bh, 16 ;; divide to get hex
	div bh ; al / bh    ah - remainder, al - quetient

        push ax
	;mov bl, ah ; save ah for later (remainder)

	;; handle quotient
	cmp al, 10
	jb _print_ax_hex_1_byte_first_number

	add al, 7

	_print_ax_hex_1_byte_first_number:
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
	jb	_print_ax_hex_1_byte_second_number

	add bl, 7

	_print_ax_hex_1_byte_second_number:
        add bl, '0'
        mov byte ptr ds:[_offset_value], bl
        lea dx, _offset_value
        mov ah, 40h
        mov bx, 0001h
        mov cx, 1h
        int 21h

	; parse and print al
	pop ax

	H

	ret

print_ax_hex_1_byte ENDP


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

            call print_ax_hex_1_byte
        ;mov ah, 00h
        ;call print_ax_hex

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
        mov al, byte ptr ds:[si] ;;apkeiciau ah ir al!!!!!!!

        call handle_buffer
        mov ah, byte ptr ds:[si]

        call print_ax_hex

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

        ret

handle_mod_11 ENDP


handle_buffer PROC
push ax
    inc [_address]
    inc si
    dec [current_buffer_size]
    cmp byte ptr [current_buffer_size], 0
    ja handle_buffer_return

    call read_buffer ;; if nothing more to read

    handle_buffer_return:
        mov al, byte ptr ds:[si]

        call mov_current_byte_buff_out

    pop ax
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

print_address PROC
    mov ax, word ptr ds:[_address]
    call print_ax_hex
    COLON
    SPACE
    ret
print_address ENDP


mov_current_byte_buff_out PROC
	push si ax	 ; will need it later

	lea si, ds:[bytes_bfr]

	mov cl, byte ptr ds:[bytes_bfr_size]
	xor ch, ch

	add si, cx

	; parse and print ah
	;shr ax, 8 ;; byte shift to right to make printing easy
	xor ah, ah
	mov bh, 16 ;; divide to get hex
	div bh ; al / bh    ah - remainder, al - quetient

        push ax
	;mov bl, ah ; save ah for later (remainder)

	;; handle quotient
	cmp al, 10
	jb _move_current_byte_buff_out_first_number

	add al, 7

	_move_current_byte_buff_out_first_number:
        add al, '0'
        mov byte ptr [si], al
        inc si
        inc byte ptr ds:[bytes_bfr_size]


    ;; handle remainder
        pop ax
        mov bl, ah
	cmp bl, 10
	jb	_move_current_byte_buff_out_second_number
	add bl, 7

	_move_current_byte_buff_out_second_number:
        add bl, '0'
        mov byte ptr [si], bl
        inc si
        inc byte ptr ds:[bytes_bfr_size]


	; parse and print al
	pop ax si

	ret

mov_current_byte_buff_out ENDP

print_byte_buff PROC

    lea dx, msg_bytes_parsed
    mov ah, 40h
    mov bx, 0001h
    mov cx, msg_bytes_parsed_len
    int 21h

    SPACE


    lea dx, bytes_bfr
    mov ah, 40h
    mov bx, 0001h ;; kol kas i stdout
    xor ch, ch
    mov cl, byte ptr ds:[bytes_bfr_size] ;;kiek bitu rasysim
    int 21h

    H

    mov byte ptr ds:[bytes_bfr_size], 0h
    ret


print_byte_buff ENDP


read_command_line PROC
    push ds
    mov ax, @data
    mov ds, ax

    mov dx, offset input_command_line
    mov ah, 09h
    int 21h

    lea dx, buffer_reading_cmnd_line
    mov ah, 0Ah
    int 21h

    call check_buffer
    ;; atspausidnt zinute pls ivesk cia
    ;; nuskaityti ka iraso i kazkoki BUFFER_READ_SIZE
    ;; pereit per bufer ir jei ivestas ne 16taines - atspausinf error msg ir baigt darba
    ;; sukurti temp faila ir i ji nurasyto
    ;; sourec file'a nukreipti i sita faila

    pop ds
    ret
read_command_line ENDP


check_buffer PROC
    xor cx, cx
    mov ah, 3Ch
    lea dx, temp_file
    int 21h
    jc err_source

    mov word ptr ds:[sourceHandle], ax

    xor ch, ch
    mov cl, byte ptr ds:[buffer_reading_cmnd_line + 1]

    ;; susikurt temp faila ir atisdaryt reaad write

    check_buffer_loop:
        ;; patikrinti ar yra simbolis nuo A-
        ;; paverst i koda :) kazkiek atimt

        ;; patkrint ar a-f
        ;; kazkiek atimt

        ;; patikrint ar 0-9
        ;; atimt '0'

        ;; jeigu komanda yra daugiau uz 16 --> end
        ;; kitu atveju irasyti i temp faila

        mov ah, 40h
        mov bx, word ptr ds:[sourceHandle]
        lea dx, buffer_reading_cmnd_line
        add dx, cx
        int 21h

    loop check_buffer_loop
    ret
check_buffer ENDP


;; help message print
help:
    mov ax, @data
    mov ds, ax

    mov dx, offset about
    mov ah, 09h
    int 21h

    jmp stop


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
