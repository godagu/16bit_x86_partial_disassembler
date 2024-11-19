.model small
.stack 100H

; .88b  d88.  .d8b.   .o88b. d8888b.  .d88b.  .d8888.
; 88'YbdP`88 d8' `8b d8P  Y8 88  `8D .8P  Y8. 88'  YP
; 88  88  88 88ooo88 8P      88oobY' 88    88 `8bo.
; 88  88  88 88~~~88 8b      88`8b   88    88   `Y8b.
; 88  88  88 88   88 Y8b  d8 88 `88. `8b  d8' db   8D
; YP  YP  YP YP   YP  `Y88P' 88   YD  `Y88P'  `8888Y'


BUFFER_READ_SIZE = 02H

BUFFER_OUT_MAX_SIZE = 80h

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

SPACE_BFR macro
        push bx

        lea bx, buffer_out
        add bl, byte ptr [buffer_out_size]
        mov byte ptr [bx], 32

        inc byte ptr ds:[buffer_out_size]
        pop bx
endm


COMMA macro
        mov dl, 44
        mov ah, 02h
        int 21h

endm

COMMA_BFR macro
        push bx

        lea bx, buffer_out
        add bl, byte ptr [buffer_out_size]
        mov byte ptr [bx], 44

        inc byte ptr ds:[buffer_out_size]
        pop bx
endm

ONE macro
        mov dl, 49
        mov ah, 02h
        int 21h
endm

ONE_BFR macro
        push bx

        lea bx, buffer_out
        add bl, byte ptr [buffer_out_size]
        mov byte ptr [bx], 49

        inc byte ptr ds:[buffer_out_size]
        pop bx
endm


COLON macro
    mov dl, 58
    mov ah, 02h
    int 21h
endm

COLON_BFR macro
        push bx

        lea bx, buffer_out
        add bl, byte ptr [buffer_out_size]
        mov byte ptr [bx], 58

        inc byte ptr ds:[buffer_out_size]
        pop bx
endm


H macro
    mov dl, 'h'
    mov ah, 02h
    int 21h
endm

H_BFR macro
        push bx

        lea bx, buffer_out
        add bl, buffer_out_size
        mov byte ptr [bx], 'h'

        inc byte ptr ds:[buffer_out_size]
        pop bx
endm


JUMPS



; d8888b.  .d8b.  d888888b  .d8b.
; 88  `8D d8' `8b `~~88~~' d8' `8b
; 88   88 88ooo88    88    88ooo88
; 88   88 88~~~88    88    88~~~88
; 88  .8D 88   88    88    88   88
; Y8888D' YP   YP    YP    YP   YP

.data

;; error messages
err_s db 'Source file could not be opened', 13, 10, '$'
err_d db 'Destination file could not be opened', 13, 10, '$'
msg_wrong_input db 'Wrong input. Make sure to enter values in the ranges 0-9, A-H, a-h', 13, 10, '$'

msg_internal_err db 'Internal error. I suggest start crying.', 13, 10, '$'

;; other printed strings
command_not_identified db 'This command could not be identified'
    command_not_identified_len  equ $ - command_not_identified ;; to get size
msg_bytes_parsed db 'Bytes parsed:'
    msg_bytes_parsed_len equ $ - msg_bytes_parsed

about db 'This program disassembles some of x86_16bit commands', 13, 10, 'third.exe destinationFile sourceFile', 13, 10, 13, 10, 'third /? - help', 13, 10, '$'

input_command_line db 'Please enter commands in HEX to disassemble:', 13, 10, '$'

buffer_reading_cmnd_line db 255, 0, 255 dup(?)

buffer_out db BUFFER_OUT_MAX_SIZE dup(?)
buffer_out_size db 0

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


;;
temp_file db 'xy5.tt', 0 ;; keiciau cia
temp_file_name_len equ $ - temp_file


;; for handling printing address or bytes parses
_address dw 00FFh
bytes_bfr db 16 dup (?)
bytes_bfr_size db 0


; db       .d88b.   .d88b.  db   dD  db    db d8888b.      d888888b  .d8b.  d8888b. db      d88888b .d8888.
; 88      .8P  Y8. .8P  Y8. 88 ,8P'  88    88 88  `8D      `~~88~~' d8' `8b 88  `8D 88      88'     88'  YP
; 88      88    88 88    88 88,8P    88    88 88oodD'         88    88ooo88 88oooY' 88      88ooooo `8bo.
; 88      88    88 88    88 88`8b    88    88 88~~~           88    88~~~88 88~~~b. 88      88~~~~~   `Y8b.
; 88booo. `8b  d8' `8b  d8' 88 `88.  88b  d88 88              88    88   88 88   8D 88booo. 88.     db   8D
; Y88888P  `Y88P'   `Y88P'  YP   YD  ~Y8888P' 88              YP    YP   YP Y8888P' Y88888P Y88888P `8888Y'

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

cmp byte ptr es:[destF], 0
je des_file_not_specified

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

;; read source file name
des_file_not_specified:
    lea di, es:[sourceF]
    call read_filename

    ;; see if source file is specidied
    cmp byte ptr es:[sourceF], 0
    jne source_file_specified

    ;; if source file (and destination) not specified, read from command line
    call read_command_line

source_file_specified:
    ;; double check if destination file is specified
    cmp byte ptr es:[destF], 0
    je source_file_not_specified

    ;; if specified, then redirect the the stdout to file (if not, print to console)
    mov bx, word ptr es:[destHandle]
    mov ah, 46h
    mov cx, 0001h
    int 21h


source_file_not_specified:
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

    mov ax, @data
    mov es, ax

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


; d8888b. d8888b.  .d88b.   .o88b. d88888b d8888b. db    db d8888b. d88888b .d8888.
; 88  `8D 88  `8D .8P  Y8. d8P  Y8 88'     88  `8D 88    88 88  `8D 88'     88'  YP
; 88oodD' 88oobY' 88    88 8P      88ooooo 88   88 88    88 88oobY' 88ooooo `8bo.
; 88~~~   88`8b   88    88 8b      88~~~~~ 88   88 88    88 88`8b   88~~~~~   `Y8b.
; 88      88 `88. `8b  d8' Y8b  d8 88.     88  .8D 88b  d88 88 `88. 88.     db   8D
; 88      88   YD  `Y88P'   `Y88P' Y88888P Y8888D' ~Y8888P' 88   YD Y88888P `8888Y'

;; procedures for handling different families
;; updated for buffer_out
handle_1010 PROC
    mov al, byte ptr ds:[si]
    and al, 0Fh ;; and with 00001111 to unmask the last 4 bits

    call get_w ;; find out w

    shr al, 0001h ;;bit shift

    lea bx, opp_1010 ;; load array
    mov cx, 05h ;; how many elements in array

    ;; loop througha array to find the needed command name
    _handle_1010_array_loop:
        cmp al, byte ptr [bx]
        je _handle_1010_opcode_found

        add bx, 05h ;; how long is the element in array

    loop _handle_1010_array_loop

    _handle_1010_opcode_found:
        inc bx
        mov cx, 04h

        call mov_cl_bytes_to_bfr_out_from_bx

        cmp byte ptr ds:[_w], 00H ;;palyginam ar w yra 0 ar 1
        je write_b


        mov dl, byte ptr ds:[symbol_w]
        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]

        mov byte ptr [bx], dl
        inc byte ptr ds:[buffer_out_size]

        SPACE_BFR

        call print_buffer_out

        call print_byte_buff

        NEW_LINE

        ret
       
    ;; else write b symbol
    write_b:
        mov dl, byte ptr ds:[symbol_b]
        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]

        mov byte ptr [bx], dl
        inc byte ptr ds:[buffer_out_size]

        SPACE_BFR

        call print_buffer_out

        call print_byte_buff
        NEW_LINE

    ret

handle_1010 ENDP

mov_cl_bytes_to_bfr_out_from_bx PROC
        push ax si bx
        xor ah, ah
        mov al, byte ptr ds:[buffer_out_size]

        mov si, bx
        lea di, buffer_out
        add di, ax

        add byte ptr ds:[buffer_out_size], cl

        rep movsb
        pop bx si ax
        ret
ENDP

; updated for buffer
handle_001sr PROC
    mov al, byte ptr ds:[si]
    and al, 18h                                             ; and su 000 11 000 to get the sr

    shr al, 0003h                                           ; byte shift to allign

    ;; load and loop through segment prefix table
    lea bx, segment_prefix_table
    mov cx, 04h

    _handle_001sr_array_loop:
        cmp al, byte ptr [bx]
        je _handle_001sr_opcode_found

        add bx, 04h                                         ;pridedam tiek, kiek zodzio ilgis array

    loop _handle_001sr_array_loop

    _handle_001sr_opcode_found:
        inc bx
        mov cx, 03h

        call mov_cl_bytes_to_bfr_out_from_bx

        SPACE_BFR

        call print_buffer_out

        SPACE
        call print_byte_buff

        NEW_LINE
        mov byte ptr ds:[buffer_out_size], 0

        ret

handle_001sr ENDP

;; updated for buffer_out
handle_1101 PROC
    ;; get w and v
    mov al, byte ptr ds:[si]

    call get_w
    call get_v

    ;; load further byte
    call handle_buffer
    mov al, byte ptr ds:[si]

    call get_mod
    call get_rm

    ;; unmask and shift bits
    and al, 38h
    shr al, 3

    ;; loop through table
    lea bx, opp_1101
    mov cx, 07h

    _handle_1101_array_loop:
        cmp al, byte ptr [bx]
        je _handle_1101_opcode_found

        add bx, 04h

    loop _handle_1101_array_loop

    _handle_1101_opcode_found:
        inc bx
        mov cx, 03h ;;kiek bitu rasysim

        call mov_cl_bytes_to_bfr_out_from_bx

        SPACE_BFR

        ;; handle different mods
        call handle_mod
        COMMA_BFR

        SPACE_BFR

        ;; check v and determine wheter to write , CL or , 1
        cmp byte ptr ds:[_v], 01h
        je print_CL

        ONE_BFR

        H_BFR

        jmp continue

        print_CL:
            lea bx, _cl
            mov cx, 02h
            call mov_cl_bytes_to_bfr_out_from_bx

        continue:
            SPACE_BFR
            call print_buffer_out

            call print_byte_buff
            NEW_LINE
            ret

handle_1101 ENDP

;; updated for buffer_out
handle_1000 PROC
        mov al, byte ptr ds:[si]
        call get_s
        call get_w

        ;; load further bytes
        call handle_buffer
        mov al, byte ptr ds:[si]

        call get_mod
        call get_rm

        ;; unmask and shift
        and al, 38h
        shr al, 3

        ;; loop through array
        lea bx, opp_1000
        mov cx, 08h

        _handle_1000_array_loop:
            cmp al, byte ptr [bx]
            je _handle_1000_opcode_found

            add bx, 04h

        loop _handle_1000_array_loop

        _handle_1000_opcode_found:
            inc bx
            mov cx, 03h ;;kiek bitu rasysim
            call mov_cl_bytes_to_bfr_out_from_bx

            SPACE_BFR

            call handle_mod

            COMMA_BFR

            SPACE_BFR

            ;; s = 0, w = 1 skaitom 2 baitus
            cmp byte ptr [_s], 0h
            je _handle_1000_s_0

            cmp byte ptr [_w], 0h
            je _handle_1000_s_0

            ;; handle special case s=1, w=1

            call handle_buffer
            mov al, byte ptr ds:[si]

            ;; if first bit is one, ten add FF
            cmp al, 80h
            jae _handle_1000_spec_case

            ;; if first bit is zero, add 00
            xor ah, ah
            call buffer_out_ax_hex

            jmp _handle_1000_end

            _handle_1000_spec_case:
                mov ah, 0FFh
                call buffer_out_ax_hex

                jmp _handle_1000_end

            ;; other cases (described below)
            _handle_1000_w_0:
                call handle_buffer
                mov al, byte ptr ds:[si]

                ;mov ah, 00h
                call buffer_out_ax_hex_1_byte

                jmp _handle_1000_end


            _handle_1000_s_0:
                ;; jei w = 0 skaitom 1
                cmp byte ptr [_w], 0h
                je _handle_1000_w_0

                call handle_buffer
                mov al, byte ptr ds:[si]

                call handle_buffer
                mov ah, byte ptr ds:[si]

                call buffer_out_ax_hex

                jmp _handle_1000_end


        ;; 4 atvejai
        ;; s = 0 w = 0 --> skaitom tik 1 ir ji rasom
        ;; s = 0 w = 1 --> skaitau 2 baitus
        ;;spec case: nuskaitom viena baita, isidedam ji kazkur (pvz al) ir uzpildom ah arba 00 arba FF pagal most significant byte
        ;; s = 1 w = 0 --> skaitom tik 1 ir ji rasom
        ;; s = 1 w = 1 --> spec case: nuskaitom viena baita, isidedam ji kazkur (pvz al) ir uzpildom ah arba 00 arba FF pagal most significant byte


        _handle_1000_end:
            SPACE_BFR
            call print_buffer_out

            call print_byte_buff
            NEW_LINE
            ret


handle_1000 ENDP

;; updated for buffer_out
handle_0011 PROC

    mov al, byte ptr ds:[si]
    and al, 0Fh
    shr al, 2h

    ;; two different commands cases
    cmp al, 0h
    je _handle_1000_00dw

    cmp al, 1h
    je _handle_1000_010w

            ;ret ;; should print a message command not undetified probably (should woek checked in the loop)

    ;; XOR 1st case
    _handle_1000_00dw:
        mov al, byte ptr ds:[si]
        call get_d
        call get_w

        ;; print xor command
        lea bx, _xor
        mov cx, 03h
        call mov_cl_bytes_to_bfr_out_from_bx

        SPACE_BFR

        ;; load other bytes
        call handle_buffer
        mov al, byte ptr ds:[si]

        call get_mod
        call get_reg
        call get_rm

        ;; d = 0 or d = 1, handle different cases
        cmp [_d], 1h
        je _handle_0011_d_1

        ;; else handle d=0
        call handle_mod

        COMMA_BFR
        SPACE_BFR

        call handle_mod_11_reg

        jmp _handle_0011_end

    _handle_0011_d_1:
        call handle_mod_11_reg

        COMMA_BFR
        SPACE_BFR

        call handle_mod

        jmp _handle_0011_end


    ;; XOR 2nd case
    _handle_1000_010w:
        mov al, byte ptr ds:[si]
        call get_w

        ;; print xor command
        lea bx, _xor
        mov cx, 03h

        call mov_cl_bytes_to_bfr_out_from_bx

        SPACE_BFR

        ;; get ax from the table
        lea bx, register_table_w_1
        mov cx, 02h
        call mov_cl_bytes_to_bfr_out_from_bx

        COMMA_BFR
        SPACE_BFR

        ;; load new bytes
        call handle_buffer
        mov al, byte ptr ds:[si]

        xor ah, ah

        ;; check w
        cmp [_w], 0h
        je _handle_1000_print_ax_hex

        ;; if w = 1, load one more byte
        call handle_buffer
        mov ah, byte ptr ds:[si]


        _handle_1000_print_ax_hex:
            call buffer_out_ax_hex


    _handle_0011_end:
        SPACE_BFR
        call print_buffer_out

        call print_byte_buff
        NEW_LINE
        ret

handle_0011 ENDP

;; updated for buffer_out
handle_1110 PROC

        ;; print jmp
        lea bx, _jmp
        mov cx, 03h
        call mov_cl_bytes_to_bfr_out_from_bx

        SPACE_BFR

        mov al, byte ptr ds:[si]
        and al, 0Fh

        ;; check 3 different cases
        cmp al, 09h
        je _handle_1110_1001

        cmp al, 0Ah
        je _handle_1110_1010

        cmp al, 0Bh
        je _handle_1110_1011


        _handle_1110_1001:
            ;; get 2 bytes
            call handle_buffer
            mov al, byte ptr ds:[si]


            call handle_buffer
            mov ah, byte ptr ds:[si]


            call buffer_out_ax_hex

            SPACE_BFR
            call print_buffer_out

            call print_byte_buff
            NEW_LINE

            ret

        _handle_1110_1010:
            ;; get 4 bytes
            call handle_buffer
            mov al, byte ptr ds:[si]


            call handle_buffer
            mov ah, byte ptr ds:[si]


            call buffer_out_ax_hex

            call handle_buffer
            mov al, byte ptr ds:[si]


            call handle_buffer
            mov ah, byte ptr ds:[si]


            call buffer_out_ax_hex

            SPACE_BFR
            call print_buffer_out

            call print_byte_buff
            NEW_LINE

            ret

        _handle_1110_1011:
            ;; get 1 byte
            call handle_buffer
            mov al, byte ptr ds:[si]

            call buffer_out_ax_hex_1_byte

            SPACE_BFR
            call print_buffer_out

            call print_byte_buff
            NEW_LINE

            ret

handle_1110 ENDP

;; updated for buffer_out
handle_1111_1111 PROC
    mov al, byte ptr ds:[si]
    call get_w

    ;; load more
    call handle_buffer
    mov al, byte ptr ds:[si]

    ;; unmask and shift
    and al, 38h
    shr al, 3h

     ;; loop till correct command found
    lea bx, opp_1111_1111
    mov cx, 07h

    _handle_1111_1111_array_loop:
        cmp al, byte ptr [bx]
        je _handle_1111_1111_opcode_found

        add bx, 05h

    loop _handle_1111_1111_array_loop

    _handle_1111_1111_opcode_found:
        inc bx
        mov cx, 04h ;;kiek bitu rasysim

        call mov_cl_bytes_to_bfr_out_from_bx

        mov al, byte ptr ds:[si]

        call get_mod
        call get_rm

        SPACE_BFR

        call handle_mod

        SPACE_BFR
        call print_buffer_out

        call print_byte_buff
        NEW_LINE

    _handle_1111_1111_end:
        ret


handle_1111_1111 ENDP


;; helper function for opp 0011
;; prints register according to w value
;; updated for buffer_out
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
        mov cx, 2h ;;kiek bitu rasysim
        call mov_cl_bytes_to_bfr_out_from_bx

        ret

handle_mod_11_reg ENDP

;; handles r/m (register/memory) according to w
;; updated for buffer_out
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
        mov cx, 2h
        call mov_cl_bytes_to_bfr_out_from_bx

        ret

handle_rm ENDP

;; getters for w, v, s, d
get_w PROC
    push ax;; updated for buffer_out
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

;; getters for mod, reg and rm
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

;; checks the mod and redirects further
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

; if mod=00
; update for buffer_out
handle_mod_00 PROC
    mov cx, 08h
    mov al, ds:[_rm]

    ;; loop thru table
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
        mov cx, 5h
        call mov_cl_bytes_to_bfr_out_from_bx

        ret

    ;;  special vase when rm 110, direct address
    _handle_mod_00_spec_case:
        ;; load two other bytes
        call handle_buffer
        mov al, byte ptr ds:[si]

        call handle_buffer
        mov ah, byte ptr ds:[si]

        call buffer_out_ax_hex

        ret

handle_mod_00 ENDP


;; procedure to print ax (2 bytes_)
print_ax_hex PROC
	push ax	 ; will need it later

	; parse and print ah
	shr ax, 8 ;; byte shift to right to make printing easy
	mov bh, 16 ;; divide to get hex
	div bh ; al / bh    ah - remainder, al - quetient

    push ax

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

    pop ax

    ;; handle remainder
    mov bl, ah
	cmp bl, 10
	jb	_print_ax_hex_second_number

	add bl, 7 ;; if letter add 7

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


;; same as previous but to buffer
buffer_out_ax_hex PROC
	push ax	 ; will need it later

	; parse and print ah
	shr ax, 8 ;; byte shift to right to make printing easy
	mov bh, 16 ;; divide to get hex
	div bh ; al / bh    ah - remainder, al - quetient

    push ax

	;; handle quotient
	cmp al, 10
	jb _buffer_out_ax_hex_first_number

	add al, 7

	_buffer_out_ax_hex_first_number:
        add al, '0'

        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]
        mov byte ptr [bx], al
        inc byte ptr ds:[buffer_out_size]

    pop ax

    ;; handle remainder
    mov dl, ah
	cmp dl, 10
	jb	_buffer_out_ax_hex_second_number

	add dl, 7 ;; if letter add 7

	_buffer_out_ax_hex_second_number:
        add dl, '0'

        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]
        mov byte ptr [bx], dl
        inc byte ptr ds:[buffer_out_size]

	; parse and print al
	pop ax
	and ax, 00FFh ;; and with 00001111 to unmask the last bits of ax
	mov bh, 16
	div bh
	;; dividing by 16


    push ax

	;; print quotient
	cmp al, 10
	jb _buffer_out_ax_hex_third_number

	add al, 7 ;; if letter add 7

	_buffer_out_ax_hex_third_number:
        add al, '0'

        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]
        mov byte ptr [bx], al
        inc byte ptr ds:[buffer_out_size]

    ;; print remainder
        pop ax
        mov dl, ah
	cmp dl, 10
	jb	_buffer_out_ax_hex_fourth_number

	add dl, 7

	_buffer_out_ax_hex_fourth_number:
        add dl, '0'

        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]
        mov byte ptr [bx], dl
        inc byte ptr ds:[buffer_out_size]

        H_BFR

	ret

buffer_out_ax_hex ENDP

;; procedure to print ax (1 byte), same as above but only one byte
print_ax_hex_1_byte PROC
	push ax	 ; will need it later

	xor ah, ah
	mov bh, 16 ;; divide to get hex
	div bh ; al / bh    ah - remainder, al - quetient

    push ax

	;; handle quotient
	cmp al, 10
	jb _print_ax_hex_1_byte_first_number

	add al, 7 ;;if letter add 7

	_print_ax_hex_1_byte_first_number:
        add al, '0'
        mov byte ptr ds:[_offset_value], al
        lea dx, _offset_value
        mov ah, 40h
        mov bx, 0001h
        mov cx, 1h
        int 21h

    pop ax

    ;; handle remainder
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

	pop ax

	H

	ret

print_ax_hex_1_byte ENDP


;; same as abpve but to buffer
buffer_out_ax_hex_1_byte PROC
	push ax	 ; will need it later

	xor ah, ah
	mov bh, 16 ;; divide to get hex
	div bh ; al / bh    ah - remainder, al - quetient

    push ax

	;; handle quotient
	cmp al, 10
	jb _buffer_out_ax_hex_1_byte_first_number

	add al, 7 ;;if letter add 7

	_buffer_out_ax_hex_1_byte_first_number:
        add al, '0'

        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]
        mov byte ptr [bx], al
        inc byte ptr ds:[buffer_out_size]

    pop ax

    ;; handle remainder
    mov dl, ah
	cmp dl, 10
	jb	_buffer_out_ax_hex_1_byte_second_number

	add dl, 7

	_buffer_out_ax_hex_1_byte_second_number:
        add dl, '0'

        lea bx, buffer_out
        add bl, byte ptr ds:[buffer_out_size]
        mov byte ptr [bx], dl
        inc byte ptr ds:[buffer_out_size]

	pop ax

	H_BFR

	ret

buffer_out_ax_hex_1_byte ENDP


;; if mod  = 01
;; updated for buffer_out
handle_mod_01 PROC
    ;; same as mod=00, but + offset
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
        mov cx, 6h

        call mov_cl_bytes_to_bfr_out_from_bx

        ;;get new byte (offset)
        call handle_buffer ;; pasiimu kita baita (poslinkis tik per viena)
        mov al, byte ptr ds:[si]

        call buffer_out_ax_hex_1_byte
    ret
handle_mod_01 ENDP

;; if mod =10
;;; updated for buffer_out
handle_mod_10 PROC
    ;; similar as the 01 but offset is 2 bytes
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
        mov cx, 6h
        call mov_cl_bytes_to_bfr_out_from_bx

        ;; get bytes for the offset
        call handle_buffer
        mov al, byte ptr ds:[si]

        call handle_buffer
        mov ah, byte ptr ds:[si]

        call buffer_out_ax_hex

    ret
handle_mod_10 ENDP

;; if mod=11
;; updated for buffer_out
handle_mod_11 PROC
    mov cx, 08h
    mov al, ds:[_rm]

    ;; check value of w (and load corresponding table)
    cmp byte ptr ds:[_w], 0h
    je handle_w_0

    ;; w=1
    lea bx, register_table_w_1
    jmp _handle_mod_11_array_loop

    ; w=0
    handle_w_0:
    lea bx, register_table_w_0

    ;; loop
    _handle_mod_11_array_loop:
        cmp al, byte ptr [bx]
        je _handle_mod_11_opcode_found

        add bx, 03h
    loop _handle_mod_11_array_loop

    ;; print
    _handle_mod_11_opcode_found:
        inc bx
        mov cx, 2h ;;kiek bitu rasysim
        call mov_cl_bytes_to_bfr_out_from_bx

        ret

handle_mod_11 ENDP

;; ckecks the current buffer size and reads more if there is nothing to read
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

;; reads more bytes
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


;; function for reading file_name
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

        cmp cx, 13 ;; jei ilgesnis nei 13 terminate
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

;; function to skip extra spaces
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

;; for printing address at the end of each line when disassembling
print_address PROC
    mov ax, word ptr ds:[_address]
    call print_ax_hex
    COLON
    SPACE
    ret
print_address ENDP

;; move to buffer what bytes wehre parsed
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

;; print the bytes parsed buffer
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

;; for reading input from the command line
read_command_line PROC
    push ds es
    mov ax, @data
    mov ds, ax

    ;; print messages
    mov dx, offset input_command_line
    mov ah, 09h
    int 21h

    lea dx, buffer_reading_cmnd_line
    mov ah, 0Ah
    int 21h

    call check_buffer

    ;; moce temp_file to souce_file
    mov ax, @data
    mov es, ax
    lea si, temp_file
    lea di, sourceF
    mov cx, temp_file_name_len

    rep movsb

    pop es ds
    ret
read_command_line ENDP

;; checks the buffer and writes to temp file
check_buffer PROC
    ;; create temo file
    push bx
    xor cx, cx
    mov ah, 3Ch
    lea dx, temp_file
    int 21h
    jc err_internal

    mov word ptr ds:[sourceHandle], ax

    xor ch, ch
    mov cl, byte ptr ds:[buffer_reading_cmnd_line + 1]              ;; load how many arguments where (count)

    lea bx, buffer_reading_cmnd_line
    add bx, 02h

    ;; loop through buffer
    check_buffer_loop:
        mov al, byte ptr [bx]

        ;; read one byte
        call convert_to_real_expression
        mov ah, al

        dec cx
        cmp cx, 0
        je one_missing

        ;; read another byte
        inc bx
        mov al, byte ptr [bx]
        call convert_to_real_expression

        ;; join two read bytes to one
        shl ah, 4
        and al, 0Fh
        or al, ah
        jmp write

        ;; uneven amount of bytes, add extra 0 to the end
        one_missing:
        shl ah, 4
        xor al, al
        or al, ah

        ;; irasom i temp file'a
        write:
        push bx cx
        mov byte ptr ds:[buffer], al

        mov ah, 40h
        mov bx, word ptr ds:[sourceHandle]
        mov cx, 0001h
        lea dx, buffer
        int 21h

        pop cx bx

        cmp cx, 0
        je end_loop

        inc bx
    loop check_buffer_loop

    end_loop:
    mov bx, word ptr ds:[sourceHandle]
    mov ah, 3Eh
    int 21h

    pop bx
    ret
check_buffer ENDP

; expects argument to be in al
; return argument in al
;; checks if argument in ranges 0-9 A-H a-h
convert_to_real_expression PROC
    cmp al, 'a'
    jb big_letter

    cmp al, 'f'
    ja err_wrong_input

    sub al, 87
    jmp _convert_to_real_expression_ret

    big_letter:
    cmp al, 'A'
    jb num

    cmp al, 'F'
    ja err_wrong_input

    sub al, 55
    jmp _convert_to_real_expression_ret

    num:
    cmp al, '0'
    jb err_wrong_input

    cmp al, '9'
    ja err_wrong_input

    sub al, 48
    _convert_to_real_expression_ret:
    ret
convert_to_real_expression ENDP

;; write the buffer
print_buffer_out PROC
    lea dx, buffer_out
    xor ch, ch
    mov cl, byte ptr ds:[buffer_out_size]
    mov bx, 0001h
    mov ah, 40h
    int 21h

    mov byte ptr ds:[buffer_out_size], 00h
    ret
print_buffer_out ENDP


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
    mov ah, 3Eh
    mov bx, word ptr [sourceHandle]
    int 21h

    ; delete temp file either way
    mov ah, 41h
    lea dx, temp_file
    int 21h

    mov ah, 3Eh
    mov bx, word ptr [destHandle]
    int 21h

    mov ax, 4c00h 
    int 21h 

;; destination error
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

;; source error
err_source:
    mov ax, @data
    mov ds, ax
    
    lea dx, err_s
    mov ah, 09h
    int 21h

    mov ax, 4c01h ; return with 1
    int 21h

;; wrong input error
err_wrong_input:
    mov ax, @data
    mov ds, ax

    mov dx, offset msg_wrong_input
    mov ah, 09h
    int 21h

    jmp stop

;; some other internal error
err_internal:
    mov ax, @data
    mov ds, ax

    mov dx, offset msg_internal_err
    mov ah, 09h
    int 21h

    jmp stop

end START
