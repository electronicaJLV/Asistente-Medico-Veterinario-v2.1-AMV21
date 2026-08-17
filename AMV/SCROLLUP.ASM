;up
;ax              dx              cx              bh         int        ret
;&HB8,&H01,&H06, &HB9,&H02,&H02, &HBA,&H4d,&H16, &HB7,&H00, &HCD,&H10, &HCB
.MODEL medium,basic
.STACK 100h
.CODE

PUBLIC scrollup
   scrollup PROC
        mov al,01h
        mov ah,06h
        mov cl,02h
        mov ch,02h
        mov dl,4Dh
        mov dh,16h
        mov bh,00h
        int 10h
        ret
   scrollup ENDP

END

