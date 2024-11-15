.model small
.stack 100H

BUFFER_READ_SIZE = 02H

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


opp_1010 db 02h, 'movs'
         db 03h, 'cmps'
         db 05h, 'stos'
         db 06h, 'lods'
         db 07h, 'scas'
        

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
    mov al, byte ptr ds:[si]
    shr al, 04h

    cmp al, 0Ah
    jne continue_loop

    call handle_1010

continue_loop:
    call handle_buffer
    jmp parse_loop


handle_1010 PROC near
    mov al, byte ptr ds:[si]
    AND al, 0Fh

    call get_w

    shr al, 0001h

    lea bx, opp_1010
    mov cx, 05h

    _handle_1010_array_loop:
        cmp al, byte ptr [bx]
        je _handle_1010_opcode_found

        add bx, 05h

    loop _handle_1010_array_loop

    _handle_1010_opcode_found:
        inc bx
        mov dx, bx
        mov ah, 40h
        mov bx, 0001h
        mov cx, 04h
        int 21h

        cmp byte ptr ds:[_w], 00H
        je write_b

        mov dl, byte ptr ds:[symbol_w]
        mov ah, 02h
        int 21h 

        mov dl, 13
        mov ah, 02h
        int 21h

        mov dl, 10
        mov ah, 02h
        int 21h

        ;; irasyti w

        ret
       
    write_b:
        mov dl, byte ptr ds:[symbol_b]
        mov ah, 02h
        int 21h 

        mov dl, 13
        mov ah, 02h
        int 21h

        mov dl, 10
        mov ah, 02h
        int 21h


    ret
    ;; loop (5x)
    ;; isikelti i bx masyva (tas kur birsuj)
    ;; jei komanda titinka ja atpausind ir ret
    ;; jei ne prideti penkis ir vel loop


handle_1010 ENDP

get_w PROC near
    push ax
        AND al, 0001h
        mov byte ptr ds:[_w], al
    pop ax
    ret
get_w ENDP


handle_buffer PROC near
    inc si
    dec [current_buffer_size]
    cmp byte ptr [current_buffer_size], 0
    ja handle_buffer_return

    call read_buffer ;; if nothing more to read

    handle_buffer_return:
        ret

handle_buffer ENDP

read_buffer PROC near
    mov ah, 3Fh
    lea dx, buffer
    mov cx, BUFFER_READ_SIZE
    mov bx, [sourceHandle]
    int 21h
    cmp ax, 00H
    je stop

    lea si, buffer
    mov ds:[current_buffer_size], al

    ret
read_buffer ENDP



read_filename PROC near
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


skip_spaces PROC near
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