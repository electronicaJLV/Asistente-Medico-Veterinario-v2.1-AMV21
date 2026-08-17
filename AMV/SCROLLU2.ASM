;up
;ax              dx              cx              bh         int        ret
;&HB8,&H01,&H06, &HB9,&H02,&H02, &HBA,&H4d,&H16, &HB7,&H00, &HCD,&H10, &HCB
.MODEL medium,basic
.STACK 100h
.CODE

PUBLIC scrollupall
   scrollupall PROC
        mov al,01h
        mov ah,06h

        mov cl,00h     ;col upper left=1
        mov ch,04h     ;row upper left=2
        mov dl,4Fh     ;col lower right=79
        mov dh,17h     ;row lower right=23

        mov bh,00h
        int 10h
        ret
   scrollupall ENDP

END

