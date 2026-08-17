.MODEL medium,basic
.STACK 100h
.CODE

PUBLIC scrolluptxt
   scrolluptxt PROC
        mov al,01h
        mov ah,06h

        mov cl,02h     ;col upper left= 3
        mov ch,0Ah     ;row upper left= 11
        mov dl,4Eh     ;col lower right=78
        mov dh,16h     ;row lower right=23

        mov bh,00h
        int 10h
        ret
   scrolluptxt ENDP

END

