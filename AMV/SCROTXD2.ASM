.MODEL medium,basic
.STACK 100h
.CODE

PUBLIC scrolldowntxt
   scrolldowntxt PROC
        mov al,01h      ;one line to scroll
        mov ah,07h      ;scrolldown

        mov cl,02h     ;col upper left= 3
        mov ch,0Ah     ;row upper left= 11
        mov dl,4Eh     ;col lower right=78
        mov dh,16h     ;row lower right=23

        mov bh,00h     ;blank line=up/down
        int 10h        ;do!
        ret
   scrolldowntxt ENDP

END

