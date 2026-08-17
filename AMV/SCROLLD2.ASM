;ax              dx              cx              bh         int        ret
;&HB8,&H01,&H07, &HB9,&H02,&H02, &HBA,&H4d,&H16, &HB7,&H00, &HCD,&H10, &HCB
;1617181920212223
;1011121314151617
.MODEL medium,basic
.STACK 100h
.CODE

PUBLIC scrolldownall
   scrolldownall PROC
        mov al,01h      ;one line to scroll
        mov ah,07h      ;scrolldown

        mov cl,00h     ;col upper left= 1
        mov ch,04h     ;row upper left= 4
        mov dl,4Fh     ;col lower right=80
        mov dh,17h     ;row lower right=23

        mov bh,00h     ;blank line=up/down
        int 10h        ;do!
        ret
   scrolldownall ENDP

END

