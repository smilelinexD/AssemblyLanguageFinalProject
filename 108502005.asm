INCLUDE Irvine32.inc

main          EQU start@0

.data
	board BYTE 81 DUP('.')
    turnmsg BYTE "'s turn.", 0
    invalidmsg BYTE "Invalid input, try again.", 0
    oWinmsg BYTE "O won!", 0
    xWinmsg BYTE "X won!", 0
    drawmsg BYTE "It was a draw!", 0
    againmsg BYTE "Want to play again? [Y/N]", 0
    boardRow BYTE 20 DUP(?), 0

    xyInit COORD <10, 4>
    xyPos COORD <10, 4>
    consoleHandle DWORD ?
    nlength DWORD 20
    count DWORD 0

    chessonboard BYTE 0

    cursorPos COORD <12, 6>
    cursorInit COORD <12, 6>
.code

clearBoard PROTO
printBoard PROTO
setBoardRow PROTO, rowInd: BYTE
attempt PROTO, chess: BYTE
checkWin PROTO, chess: BYTE

main PROC
    invoke GetStdHandle, STD_OUTPUT_HANDLE
    mov consoleHandle, eax

    call Clrscr
    call printBoard

    mov chessonboard, 0
    mainLoop:
        invoke attempt, 'O'
        call Clrscr
        call printBoard
        invoke checkWin, 'O'
        cmp bl, 1
        je Owin
        inc chessonboard
        cmp chessonboard, 81
        je draw
        invoke attempt, 'X'
        call Clrscr
        call printBoard
        invoke checkWin, 'X'
        cmp bl, 1
        je Xwin
        inc chessonboard
        jmp mainLoop
    
    Owin:
        mov edx, OFFSET oWinmsg
        call writeString
        call Crlf
        jmp endgame
    Xwin:
        mov edx, OFFSET xWinmsg
        call writeString
        call Crlf
        jmp endgame
    draw:
        mov edx, OFFSET drawmsg
        call writeString
        call Crlf
        jmp endgame

    endgame:
        mov edx, OFFSET againmsg
        call writeString
        call Crlf
        call ReadChar
        call WriteChar
        call Crlf
        cmp al, 'Y'
        jne realEnd
        call clearBoard
        call Clrscr
        call printBoard
        mov chessonboard, 0
        jmp mainLoop

    realEnd:
        call waitMsg
main ENDP

clearBoard PROC USES eax ecx edi
; Clear the game board
    mov edi, OFFSET board
    mov al, '.'
    mov ecx, LENGTHOF board     ; Repeat 81 times
    rep stosb
    RET
clearBoard ENDP

printBoard PROC USES eax ecx
; Print out the game board
    mov ax, xyInit.x        ; Initialize
    mov xyPos.x, ax
    mov ax, xyInit.y
    mov xyPos.y, ax

    mov ecx, 20             ; Board on screen has 20 rows
    xor eax, eax            ; Zeroes eax (al uses as row index)
    printBoard_Loop:
        invoke setBoardRow, al
        push eax
        push ecx
        invoke WriteConsoleOutputCharacter,
            consoleHandle,
            ADDR boardRow,
            nlength,
            xyPos,
            ADDR count
        pop ecx
        pop eax
        inc al
        inc xyPos.y
        Loop printBoard_Loop
    RET

printBoard ENDP

setBoardRow PROC USES eax ebx ecx esi edi, rowInd: BYTE
; Sets up boardRow to acoordingly string
; Receives a byte as row index
    mov edi, OFFSET boardRow
    cmp rowInd, 0
    jne wRow1
    ;wRow0:
        mov al, ' '
        mov ecx, 2
        rep stosb
        mov al, 'A'
        mov ecx, 9
    wRow0_Loop:
        stosb
        push eax
        mov al, ' '
        stosb
        pop eax
        inc al
        Loop wRow0_Loop
        RET
    wRow1:
        cmp rowInd, 1
        jne wRow2to18
        mov al, ' '
        stosb
        mov al, 0DAh            ;┌
        stosb
        mov al, 0C4h            ;─
        stosb
        mov ecx, 8
        wRow1_Loop:
            mov al, 0C2h        ;┬
            stosb
            mov al, 0C4h        ;─
            stosb
            Loop wRow1_Loop      
        mov al, 0BFh            ;┐
        stosb
        RET
    wRow2to18:
        cmp rowInd, 19
        je wRow19
        mov al, rowInd
        mov bl, 2
        div bl
        cmp ah, 0
        jne wOdd
        ;wEven:     ;w/ data
            mov esi, OFFSET board
            add al, 48
            stosb
            sub al, 49
            mov bl, 9
            mul bl
            mov bl, TYPE BYTE
            mul bl
            add esi, eax
            mov al, 0B3h        ;│
            stosb
            mov ecx, 9
            wEven_Loop:
                mov al, [esi]
                stosb
                mov al, 0B3h    ;│
                stosb
                add esi, TYPE BYTE
                Loop wEven_Loop
            RET
        wOdd:
            mov al, ' '
            stosb
            mov al, 0C3h        ;├
            stosb
            mov al, 0C4h        ;─
            stosb
            mov ecx, 8
            wOdd_Loop:
                mov al, 0C5h    ;┼
                stosb
                mov al, 0C4h    ;─
                stosb
                Loop wOdd_Loop
            mov al, 0B4h        ;┤
            stosb
            RET
    wRow19:
        mov al, ' '
        stosb
        mov al, 0C0h            ;└
        stosb
        mov al, 0C4h            ;─
        stosb
        mov ecx, 8
        wRow19_Loop:
            mov al, 0C1h        ;┴
            stosb
            mov al, 0C4h        ;─
            stosb
            Loop wRow19_Loop
        mov al, 0D9h            ;┘
        stosb
        RET
setBoardRow ENDP

attempt PROC USES ebx ecx edx, chess: BYTE
; Try to put a chess on board
; Receives a BYTE as chess symbol
; Returns eax (remains the index of which just placed)
    ; initialize

        mov ax, cursorInit.x
        mov cursorPos.x, ax
        mov ax, cursorInit.y
        mov cursorPos.y, ax

        xor edx, edx                    ; edx uses as index
    INIT:                               ; Set up starting point
        mov eax, edx
        mov bl, 9
        div bl
        mov bh, al                      ; Saves current row to bh
        .IF board[edx] != '.'
            inc edx
            add cursorPos.x, 2
            mov eax, edx
            div bl
            .IF al != bh                ; Moved to another row
                add cursorPos.y, 2
                mov cursorPos.x, 12
            .ENDIF
            jmp INIT
        .ENDIF

    Read:
        push eax
        push edx
        call Clrscr

        mov al, chess
        call WriteChar
        mov edx, OFFSET turnmsg
        call writeString
        
        call printBoard
        invoke SetConsoleCursorPosition, consoleHandle, cursorPos
        pop edx
        pop eax

        mov eax, edx
        mov bl, 9
        div bl
        mov bh, al                              ; Saves current row to bh

        call ReadChar

        .IF ax == 4800h     ; up
            mov ecx, 1                          ; Saves how many blockes moved
            Up_Point:
                sub edx, 9
                sub cursorPos.y, 2
                cmp edx, 0
                jnl Up_Continue
                    Up_Loop:                    ; Out of bound
                        add edx, 9
                        add cursorPos.y, 2
                        Loop Up_Loop
                    jmp Read
                Up_Continue:
                    .IF board[edx] != '.'
                        inc ecx
                        jmp Up_Point
                    .ENDIF
        .ENDIF
        .IF ax == 5000h     ; down
            mov ecx, 1                          ; Similar to up one
            Down_Point:
                add edx, 9
                add cursorPos.y, 2
                .IF edx > 80
                    Down_Loop:
                        sub edx, 9
                        sub cursorPos.y, 2
                        Loop Down_Loop
                    jmp Read
                .ENDIF
                .IF board[edx] != '.'
                    inc ecx
                    jmp Down_Point
                .ENDIF
        .ENDIF
        .IF ax == 4B00h     ; left
            mov ecx, 1                          ; Saves how many blocks moved
            Left_Point:
                dec edx
                sub cursorPos.x, 2
                cmp edx, 0
                jl Left_Loop                    ; Out of bound
                mov eax, edx
                div bl
                .IF bh != al                    ; Moved to another row
                    Left_Loop:
                        inc edx
                        add cursorPos.x, 2
                        Loop Left_Loop
                    jmp Read
                .ENDIF
                .IF board[edx] != '.'
                    inc ecx
                    jmp Left_Point
                .ENDIF
        .ENDIF
        .IF ax == 4D00h     ;right
            mov ecx, 1                          ; Similar to left one
            Right_Point:
                inc edx
                add cursorPos.x, 2
                mov eax, edx
                div bl
                .IF bh != al
                    Right_Loop:
                        dec edx
                        sub cursorPos.x, 2
                        Loop Right_Loop
                    jmp Read
                .ENDIF
                .IF board[edx] != '.'
                    inc ecx
                    jmp Right_Point
                .ENDIF
        .ENDIF
        .IF ax == 1C0Dh     ; enter
            mov bl, chess
            mov board[edx], bl
            mov eax, edx
            RET
        .ENDIF
        jmp Read
attempt ENDP

checkWin PROC USES edx, chess: BYTE
; Checks whether a player wins
; Receives eax as board index, dl as player O or X
; Returns bl = 1 if win, bl = 0 otherwise
    mov dl, chess
    ; Vertical
    push eax
    mov ebx, 1                  ; saves how many chess found successive
    Up:
        sub eax, 9
        cmp eax, 0
        jl Up_Failed            ; out of bound
        cmp board[eax], dl      ; chess not equal to one just placed
        jne Up_Failed
    ; Up_Caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp Up                  ; find upper one
    Up_Failed:
        pop eax
    push eax
    Down:                       ; similar to up one
        add eax, 9
        cmp eax, 80
        jg Down_Failed
        cmp board[eax], dl
        jne Down_Failed
    ;DownO_Caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp Down
    Down_Failed:
        pop eax
    ; Horizental
    push eax
    mov ebx, 1                  ; saves how many chess found successive
    push eax
    mov cl, 9
    div cl                      ; saves the chess' row
    mov ch, al                  ; to ch
    pop eax
    Left:
        dec eax
        cmp eax, 0              
        jl Left_Failed          ; out of bound
        push eax
        div cl
        cmp ch, al              
        pop eax
        jne Left_Failed         ; decreases to another row
        cmp board[eax], dl      ; chess not equal to one just placed
        jne Left_Failed
    ; Left_Caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp Left
    Left_Failed:
        pop eax
    push eax
    Right:                      ; similar to left one
        inc eax
        cmp eax, 80
        jg Right_Failed
        push eax
        div cl
        cmp al, ch
        pop eax
        jne Right_Failed
        cmp board[eax], dl
        jne Right_Failed
    ; Right_caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp Right
    Right_Failed:
        pop eax
    ; NW to SE
    push eax
    mov cl, 9                   ; dividor
    mov ebx, 1                  ; saves how many chess found successive
    UpLeft:
        sub eax, 10
        cmp eax, 0
        jl UpLeft_Failed        ; out of bound
        push eax
        div cl
        cmp ah, 8               
        pop eax
        je UpLeft_Failed        ; decreases from leftmost column to rightmost column
        cmp board[eax], dl      ; chess not equal to one just placed
        jne UpLeft_Failed
    ; UpLeft_Caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp UpLeft
    UpLeft_Failed:
        pop eax
    push eax
    DownRight:                  ; similar to UpLeft one
        add eax, 10
        cmp eax, 80
        jg DownRight_Failed
        push eax
        div cl
        cmp ah, 0
        pop eax
        je DownRight_Failed
        cmp board[eax], dl
        jne DownRight_Failed
    ; DownRight_Caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp DownRight
    DownRight_Failed:
        pop eax
    ; NE to SW                  ; similar to (NW to SE)
    push eax
    mov cl, 9
    mov ebx, 1
    UpRight:
        sub eax, 8
        cmp eax, 0
        jl UpRight_Failed
        push eax
        div cl
        cmp ah, 0
        pop eax
        je UpRight_Failed
        cmp board[eax], dl
        jne UpRight_Failed
    ; UpRight_Caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp UpRight
    UpRight_Failed:
        pop eax
    push eax
    DownLeft:
        add eax, 8
        cmp eax, 80
        jg DownLeft_Failed
        push eax
        div cl
        cmp ah, 8
        pop eax
        je DownLeft_Failed
        cmp board[eax], dl
        jne DownLeft_Failed
    ; DownLeft_Caught
        inc ebx
        cmp ebx, 5
        je Win
        jmp DownLeft
    DownLeft_Failed:
        pop eax

    ; NotWin
    mov bl, 0
    RET

    Win:
        pop eax
        mov bl, 1
        RET
checkWin ENDP

END main
