;ax              dx              cx              bh         int        ret
;&HB8,&H01,&H07, &HB9,&H02,&H02, &HBA,&H4d,&H16, &HB7,&H00, &HCD,&H10, &HCB
.MODEL medium,basic
.STACK 100h
.CODE

PUBLIC scrolldown
   scrolldown PROC
        mov al,01h
        mov ah,07h
        mov cl,02h     ;col upper left=1
        mov ch,02h     ;row upper left=2
        mov dl,4Dh    ;col lower right=80
        mov dh,16h    ;row lower right=23
        mov bh,00h
        int 10h
        ret
   scrolldown ENDP

END

