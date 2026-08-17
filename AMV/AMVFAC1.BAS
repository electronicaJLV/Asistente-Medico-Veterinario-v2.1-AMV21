DECLARE SUB sort ()
DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE SUB LlistPa ()
DECLARE SUB NOHAY ()
DECLARE SUB LOGIN (ZZ%)
DECLARE SUB Lineas ()
DECLARE SUB Question (u%, Fo$, B%)
DECLARE SUB comppa ()
DECLARE SUB Invents (what$)
DECLARE SUB listPa (yy%, recupera%)
DECLARE SUB CompSaldo ()
DECLARE SUB QuickInvent ()
DECLARE SUB Espere (y%)
DECLARE SUB PintarVar ()
DECLARE SUB EmitirFac (file$)
DECLARE SUB Grabar ()
DECLARE SUB LlistFa (CUANDO$)
DECLARE SUB Resumen ()
DECLARE SUB PRINTGASTO (file$, CUANDO$)
DECLARE SUB GastosFondo ()
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'

DEFINT A-Z
SUB Checkout
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #2 LEN = LEN(Pets)
IF LOF(2) = 0 THEN CLOSE #2: CALL NOHAY: EXIT SUB
CLOSE #2
CALL LOGIN(1): IF EnGtCla% = 0 THEN EXIT SUB
1559
GOSUB Fondo
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "ESC: Salir  ³ F4: Listado"
aquicf$ = "": REGNUMERO& = 0: REGNOMBRE$ = "":
Question 0, "Pa", 7
IF salir% = 1 THEN
    salir% = 0: EXIT SUB
END IF
IF REGNOMBRE$ = "" THEN GOTO 1559
145
GOSUB Fondo
Espere 0: CALL comppa
1777
IF PetShow.id = "             " THEN
    GOSUB lmmalo:
    IF rgero$ = "" THEN
        GOTO 1559
    ELSE REGNOMBRE$ = rgero$: GOTO 145
    END IF
END IF
GOSUB Fondo: rgero$ = TRIM$(PetShow.id)
OPEN "AMVSYS\AMVFA.SYS" FOR APPEND AS #2: CLOSE #2
OPEN "AMVSYS\AMVFA.SYS" FOR INPUT AS #2
DO WHILE NOT EOF(2): INPUT #2, fac&: LOOP: CLOSE #2
fac& = fac& + 1: LOCATE 2, 11: COLOR 0, 3: PRINT fac&
CALL PintarVar: GOSUB REVISA
11777
LARGO = 9
    OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #4:
    CALL Invents(PetShow.id)
    textocf 1, 25, 3, 3, STRING$(80, " "): textocf 2, 25, 0, 3, "ESC: Salir ³ F1: Imprimir ³ F3: Cargar ³ F4: Listado ³ F10: Facturar"
    DO
        kbd$ = INKEY$
        SELECT CASE kbd$
            CASE IS = CHR$(27): CLOSE #4: EXIT SUB
            CASE IS = CHR$(0) + ";"
                Espere 0:    Cargo.fac = "             "
                CLOSE #4: EmitirFac "AMVDAT\AMVCA.DAT": GOTO 1777
            CASE IS = CHR$(0) + "D": CompSaldo
                IF mttl& > 0 OR mttl& < 0 THEN
                    Alert 1, 3, 26, 5, 26, 7
                    textocf 28, 6, 0, 3, "Saldo no est  en cero..."
                    BEEP: SLEEP 1: GOTO 1777
                ELSE
                    Espere 0: CLOSE #4: CALL Grabar
                END IF
                GOTO 145
            CASE IS = CHR$(0) + "=":
                CLOSE #4: CALL QuickInvent
                REGNOMBRE$ = rgero$: GOTO 1777
        CASE IS = CHR$(0) + ">"
            CLOSE #4: listPa 1, 0
                salir% = 0
                IF REGNOMBRE$ = "" AND REGNUMERO& = 0 OR REGNOMBRE$ = "0" THEN REGNOMBRE$ = rgero$
                IF PetShow.id = "" THEN REGNOMBRE$ = rgero$
                mttl& = 0: GOTO 145
           CASE IS = CHR$(0) + "H"
                CLOSE #4: OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #4: Invents (PetShow.id)
           CASE IS = CHR$(0) + "P"
                CALL Invents(PetShow.id)
            CASE ELSE
        END SELECT
    LOOP
EXIT SUB

lmmalo:
    GOSUB Fondo
    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1
    CLOSE #1
RETURN

Fondo:
CALL Lineas
LOCATE 2, 2: COLOR 0, 3: PRINT "FACTURA #";
textocf 34, 2, 14, 3, "FACTURACION"
textocf 2, 3, 15, 1, "PACIENTE"
textocf 3, 4, 14, 1, "CLIENTE:"
textocf 40, 4, 14, 1, "MASCOTA:"
textocf 40, 5, 14, 1, "ENTRADA:"
textocf 3, 5, 14, 1, "RIF-NIT-CI:"
textocf 3, 6, 14, 1, "DIRECCION/TELEF:"
textocf 1, 8, 3, 3, STRING$(80, " ")
textocf 3, 8, 14, 3, "FECHA"
textocf 12, 8, 14, 3, "OP."
textocf 18, 8, 14, 3, "CODIGO"
textocf 32, 8, 14, 3, "CONCEPTO"
textocf 53, 8, 14, 3, "CANT."
textocf 65, 8, 14, 3, "MONTO"
FOR y% = 9 TO 24
    textocf 1, y%, 7, 7, STRING$(80, " ")
    textocf 11, y%, 1, 7, STRING$(1, 179)
    textocf 15, y%, 1, 7, STRING$(1, 179)
    textocf 25, y%, 1, 7, STRING$(1, 179)
    textocf 52, y%, 1, 7, STRING$(1, 179)
    textocf 58, y%, 1, 7, STRING$(1, 179)
NEXT
RETURN

REVISA:
CompSaldo
IF mttl& > 0 THEN
    textocf 55, 7, 12, 0, " Saldo Deudor:    "
    LOCATE 7, 69: COLOR 12, 0: PRINT USING AS$; mttl&;
    l = 6
END IF
IF mttl& < 0 THEN
    textocf 54, 7, 3, 0, " Saldo a Favor:   "
    LOCATE 7, 69: COLOR 3, 0: PRINT USING AS$; mttl&;
    l = 6
END IF
RETURN
END SUB

SUB EmitirFac (file$)
WIDTH LPRINT 120
M = 1
HH$ = "####"
tl$ = "\                              \"
dr$ = "\                                       \"
Em$ = "\                         \"
rf$ = "\              \"
P$ = "\           \"
pppp$ = "\            \"
ppp$ = "\                 \"
pp$ = "\                    \"
PPPPP$ = "\                                                  \"
FF$ = " \       \" + "³"
FE$ = " \ \" + "³"
FA$ = " \   \" + "³"
FD$ = " \                       \" + "³"
FS$ = " ##,###,###.##"
A = 11
GOSUB Head
OPEN file$ FOR INPUT AS #4:
IF EOF(4) THEN GOTO 9
FOR I% = 1 TO LOF(4)
9
     IF EOF(4) THEN
        FOR juan% = 1 TO A
            LPRINT "          ³    ³          ³                          ³      ³"
        NEXT juan%
        LPRINT "ÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ"
        LPRINT " OPER. "; Iniciales$; " ³ "; "PAG:";
        LPRINT USING HH$; M;
        LPRINT "                                    TOTAL:"; STR$(mottl&)
        LPRINT "ÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
        LPRINT CHR$(15); " E-Mail: ";
        LPRINT USING Em$; email$
        IF Cargo.fac = "             " THEN
            LPRINT CHR$(18); "ESTE SOPORTE SOLO ES INFORMATIVO, NO TIENE NINGUN VALOR COMERCIAL..."
        END IF
        LPRINT CHR$(18):
        LPRINT : LPRINT : LPRINT
        LPRINT : LPRINT : LPRINT
        LPRINT : CLOSE #4: EXIT SUB
    END IF

     IF j = 1 THEN
        j = 0:
            LPRINT "          ³    ³          ³                 VIENEN...³      ³ "; STR$(mottl&)
     END IF
     IF A = 0 THEN
        LPRINT "ÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ"
        LPRINT " OPER. "; Iniciales$; " ³ "; "PAG:";
        LPRINT USING HH$; M;
        LPRINT "                                      VAN:"; STR$(mottl&)
        LPRINT "ÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
        LPRINT CHR$(15); " E-Mail: ";
        LPRINT USING Em$; email$
       
        IF Cargo.fac = "             " THEN
            LPRINT CHR$(18); "ESTE SOPORTE SOLO ES INFORMATIVO, NO TIENE NINGUN VALOR COMERCIAL..."
        END IF
        LPRINT CHR$(18):
        LPRINT : LPRINT : LPRINT
        LPRINT : LPRINT : LPRINT
        LPRINT : M = M + 1: A = 10: GOSUB Head: j = 1: GOTO 9
     END IF
    INPUT #4, serial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
       IF file$ = "AMVDAT\AMVCA.DAT" THEN
            IF serial$ <> TRIM$(PetShow.id) THEN GOTO 9
       END IF
       IF file$ = "AMVDAT\AMVFB.DAT" THEN
            IF serial$ <> TRIM$(Cargo.fac) THEN GOTO 9
       END IF
    A = A - 1
    LPRINT USING FF$; myfecha$;
    LPRINT USING FE$; myclave$;
    LPRINT USING FF$; mycodigo$;
    LPRINT USING FD$; mynombre$;
    LPRINT USING FA$; mycantidad$;
    LPRINT USING FS$; mysubtotal&
    mottl& = mottl& + mysubtotal&
    NEXT
EXIT SUB

Head:
LPRINT CHR$(14); TRIM$(nombre$)
LPRINT CHR$(15);
LPRINT USING dr$; direcc$;
IF Cargo.fac <> "             " THEN
    LPRINT CHR$(14); CHR$(15); STRING$(25, 32);
    LPRINT "FACTURA #";
    LPRINT USING P$; LEFT$("0000000000000", 13 - LEN(TRIM$(Cargo.fac))) + TRIM$(Cargo.fac)
END IF
IF Cargo.fac = "             " THEN
    LPRINT CHR$(14); CHR$(15); STRING$(24, 32); "<ESTADO DE CUENTA>"
END IF
LPRINT CHR$(15); "TELEFONOS: ";
LPRINT telefonos$
LPRINT "RIF: ";
LPRINT USING rf$; rif$;
LPRINT " NIT: ";
LPRINT USING rf$; nit$;
LPRINT CHR$(18); STRING$(28, 32);
LPRINT "PACIENTE:";
LPRINT USING P$; LEFT$("0000000000000", 13 - LEN(TRIM$(PetShow.id))) + TRIM$(PetShow.id)
LPRINT "CLIENTE: ";
LPRINT USING pp$; (TRIM$(Cargo.dueno));
LPRINT STRING$(24, 32);
LPRINT "ENTRADA:";
LPRINT USING P$; Cargo.checkin
LPRINT "MASCOTA: ";
LPRINT USING pp$; (TRIM$(Cargo.nombr));
LPRINT STRING$(24, 32);
IF Cargo.fac > "             " THEN
    LPRINT "SALIDA:";
    LPRINT USING P$; TRIM$(Cargo.fecha)
END IF
IF Cargo.fac = "             " THEN
    LPRINT "FECHA: ";
    LPRINT USING P$; TRIM$(FECCHA$)
END IF
LPRINT USING pppp$; "RIF-NIT-CI: ";
LPRINT USING pp$; TRIM$(Cargo.cedula)
LPRINT USING ppp$; "DIRECCION/TELEF: ";
LPRINT USING PPPPP$; TRIM$(Cargo.direcc)
LPRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
LPRINT CHR$(14); CHR$(15); " FECHA    OP.  CODIGO        CONCEPTO          CANT.    MONTO"
LPRINT CHR$(18); "ÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
RETURN
END SUB

SUB Gastos
c1 = 1: c2 = 1:
19
neto& = 0
Alert 1, 3, 26, 10, 26, 12
    textocf 28, 11, 0, 3, "Ingrese Fecha: ":
    CUANDO$ = FECCHA$
    textocf 43, 11, 0, 3, CUANDO$
    kbd$ = VERDATE(CUANDO$, 43, 11)
    file$ = "AMVHIS\GA" + RIGHT$(CUANDO$, 5) + ".DAT"
    OPEN file$ FOR APPEND AS #4: CLOSE #4
14
neto& = 0: saldo& = 0
OPEN file$ FOR INPUT AS #1:
DO WHILE NOT EOF(1)
IF EOF(1) THEN EXIT DO
    INPUT #1, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
    IF RIGHT$(myfecha$, 5) = RIGHT$(CUANDO$, 5) THEN neto& = neto& + mysubtotal&
    IF myfecha$ = CUANDO$ THEN saldo& = saldo& + mysubtotal&
LOOP
CLOSE #1
OPEN file$ FOR INPUT AS #4:
181
CALL GastosFondo
textocf 2, 3, 7, 1, "Saldo: ": COLOR 3, 1: PRINT USING AS$; saldo&; : COLOR 7, 1: PRINT " Bs."; : PRINT " al "; CUANDO$; : COLOR 3, 1: PRINT "  ³ "; : COLOR 7, 1: PRINT "Saldo Neto: "; : COLOR 3, 1: PRINT USING AS$; neto&
FOR I% = 5 TO 24
99
    IF EOF(4) THEN
        textocf 2, I%, 1, 7, "********": textocf 12, I%, 1, 7, "**": textocf 16, I%, 1, 7, "*************": textocf 31, I%, 1, 7, "*************************": textocf 58, I%, 1, 7, "****": textocf 64, I%, 1, 7, "*************"
        EXIT FOR
    END IF
        INPUT #4, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
    IF myfecha$ <> CUANDO$ THEN GOTO 99
        d = 0: c = 4
        IF VAL(mycodigo$) < 50 THEN
            d = 1: c = 1
        END IF
        textocf 2, I%, 0, 7, myfecha$: textocf 12, I%, 0, 7, myclave$: textocf 16, I%, 0, 7, mycodigo$: textocf 31, I%, d, 7, mynombre$: textocf 58, I%, 0, 7, mycantidad$: LOCATE I%, 67: COLOR c, 7: PRINT USING AS$; mysubtotal&;
NEXT
textocf 2, 3, 7, 1, "Saldo: ": COLOR 3, 1: PRINT USING AS$; saldo&; : COLOR 7, 1: PRINT " Bs."; : PRINT " al "; CUANDO$; : COLOR 3, 1: PRINT "  ³ "; : COLOR 7, 1: PRINT "Saldo Neto: "; : COLOR 3, 1: PRINT USING AS$; neto&
IF EnGtCla% = 1 THEN CLOSE #4: EnGtCla% = 0: CUANDO$ = FECCHA$: kargo% = 1: GOTO 14
IF kargo% = 1 THEN CLOSE #4: kargo = 0: CALL QuickInvent: GOTO 14
    DO
    kbd$ = INKEY$
        SELECT CASE kbd$
            CASE IS = CHR$(27): CLOSE #4:
            OPEN file$ FOR APPEND AS #4:
            IF LOF(4) = 0 THEN
                CLOSE #4: KILL file$
            END IF: CLOSE #4
            EXIT SUB
            CASE IS = CHR$(0) + "<": CLOSE #4: saldo& = 0:
            OPEN file$ FOR APPEND AS #4:
            IF LOF(4) = 0 THEN
                CLOSE #4: KILL file$
            END IF: CLOSE #4
            c1 = 7: c2 = 7:
            GOTO 19
            CASE IS = CHR$(0) + ">":  saldo& = 0: neto& = 0: CLOSE #4:
            CLOSE #4
            PRINTGASTO file$, CUANDO$
            GOTO 14
            CASE IS = CHR$(0) + "=": saldo& = 0:
                CLOSE #4:
                CALL LOGIN(1)
                PetShow.id = "             "
                Pet.id = "             "
                IF EnGtCla% = 1 THEN GOTO 14
                 CUANDO$ = FECCHA$
                 neto& = 0: GOTO 14
           CASE IS = CHR$(0) + "H": saldo& = 0:
                CLOSE #4
                neto& = 0: GOTO 14
            CASE IS = CHR$(0) + "P"
                GOTO 181
        END SELECT
    LOOP
EXIT SUB


primirgastos:
FF$ = " \       \"
FE$ = " \ \"
FA$ = " \   \"
FD$ = " \                       \"
LPRINT CHR$(14); TRIM$(nombre$): LPRINT CHR$(18); MFECHA$: LPRINT STRING$(27, 32); CHR$(14); CHR$(15); "INVENTARIO GENERAL"
LPRINT CHR$(18); TIME$; "                                                      Pag. #"; pag%
LPRINT STRING$(78, 196)
   
    OPEN file$ FOR INPUT AS #4:
FOR I% = 1 TO LOF(4)
20
    IF EOF(4) THEN
        EXIT FOR
    END IF
    INPUT #4, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
        IF myfecha$ <> CUANDO$ THEN
            GOTO 20
        END IF
        IF VAL(myserial$) > 0 THEN
            GOTO 20
        END IF
    LPRINT USING FF$; myfecha$; : LPRINT USING FE$; myclave$; : LPRINT USING FF$; mycodigo$; : LPRINT USING FD$; mynombre$; : LPRINT USING FA$; mycantidad$; : LPRINT USING AS$; mysubtotal&
NEXT
RETURN
END SUB

SUB GastosFondo
    textocf 1, 3, 1, 1, STRING$(80, " "): textocf 1, 2, 3, 3, STRING$(80, " ")
    textocf 30, 2, 0, 3, "RELACION DE GASTOS"
    textocf 1, 4, 3, 3, STRING$(80, " ")
    textocf 3, 4, 0, 3, "FECHA": textocf 12, 4, 0, 3, "OP.": textocf 20, 4, 0, 3, "CODIGO"
    textocf 38, 4, 0, 3, "CONCEPTO": textocf 58, 4, 0, 3, "CANT.": textocf 68, 4, 0, 3, "MONTO"
FOR y% = 5 TO 24
    IF y% = 11 THEN
        S% = 14
    END IF
    textocf 1, y%, 7, 7, STRING$(80, " "): textocf 11, y%, 1, 7, STRING$(1, 179): textocf 15, y%, 1, 7, STRING$(1, 179): textocf 30, y%, 1, 7, STRING$(1, 179): textocf 57, y%, 1, 7, STRING$(1, 179): textocf 63, y%, 1, 7, STRING$(1, 179)
NEXT
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "ESC: Salir   ³ F2: Seleccionar Fecha  ³ F3: Cargar  ³ F4: Imprimir       "
textocf 2, 3, 7, 1, "Saldo: ": COLOR 3, 1: PRINT "           "; : COLOR 7, 1: PRINT " Bs."; : PRINT " al " + FECCHA$; : COLOR 3, 1: PRINT "  ³ "; : COLOR 7, 1: PRINT "Saldo Neto: "; : COLOR 3, 1: PRINT "          "
END SUB

SUB GastosInvents
file$ = "AMVHIS\GA" + RIGHT$(FECCHA$, 5) + ".DAT"
OPEN file$ FOR INPUT AS #1:
DO WHILE NOT EOF(1)
IF EOF(1) THEN EXIT DO
    INPUT #1, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
    IF RIGHT$(myfecha$, 5) = RIGHT$(FECCHA$, 5) THEN neto& = neto& + mysubtotal&
    IF myfecha$ = FECCHA$ THEN saldo& = saldo& + mysubtotal&
LOOP
CLOSE #1
OPEN file$ FOR INPUT AS #4:
CALL GastosFondo
textocf 2, 3, 7, 1, "Saldo: ": COLOR 3, 1: PRINT USING AS$; saldo&; : COLOR 7, 1: PRINT " Bs."; : PRINT " al "; FECCHA$; : COLOR 3, 1: PRINT "  ³ "; : COLOR 7, 1: PRINT "Saldo Neto: "; : COLOR 3, 1: PRINT USING AS$; neto&
FOR I% = 5 TO 24
199
    IF EOF(4) THEN
        textocf 2, I%, 1, 7, "********": textocf 12, I%, 1, 7, "**": textocf 16, I%, 1, 7, "*************": textocf 31, I%, 1, 7, "*************************": textocf 58, I%, 1, 7, "****": textocf 64, I%, 1, 7, "*************"
       EXIT FOR
    END IF
        INPUT #4, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
    IF myfecha$ <> FECCHA$ THEN GOTO 199
        d = 0: c = 4
        IF VAL(mycodigo$) < 50 THEN
            d = 1: c = 1
        END IF
        textocf 2, I%, 0, 7, myfecha$: textocf 12, I%, 0, 7, myclave$: textocf 16, I%, 0, 7, mycodigo$: textocf 31, I%, d, 7, mynombre$: textocf 58, I%, 0, 7, mycantidad$: LOCATE I%, 67: COLOR c, 7: PRINT USING AS$; mysubtotal&;
NEXT
textocf 2, 3, 7, 1, "Saldo: ": COLOR 3, 1: PRINT USING AS$; saldo&; : COLOR 7, 1: PRINT " Bs."; : PRINT " al "; FECCHA$; : COLOR 3, 1: PRINT "  ³ "; : COLOR 7, 1: PRINT "Saldo Neto: "; : COLOR 3, 1: PRINT USING AS$; neto&
CLOSE #4
END SUB

SUB Grabar
textocf 70, 25, 0, 3, "ESC: Salir"
OPEN "AMVSYS\AMVFA.SYS" FOR APPEND AS #2: CLOSE #2
OPEN "AMVSYS\AMVFA.SYS" FOR INPUT AS #2
DO WHILE NOT EOF(2): INPUT #2, fac&: LOOP: CLOSE #2
IF fac& = 1999999999 THEN fac& = 0
fac& = fac& + 1:
Cargo.fac = TRIM$(STR$(fac&))
fec$ = RIGHT$(FECCHA$, 5)
filefh$ = "AMVHIS\FH" + fec$ + ".dat"
filefb$ = "AMVHIS\FB" + fec$ + ".dat"

LOCATE 2, 2: COLOR 4, 3: PRINT "FACTURA # "; Cargo.fac
CLOSE #1
OPEN "AMVSYS\AMVFA.SYS" FOR OUTPUT AS #4: WRITE #4, fac&: CLOSE #4
OPEN filefh$ FOR RANDOM ACCESS WRITE AS #1 LEN = LEN(Cargo)
PUT #1, fac&, Cargo: CLOSE #1

OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #12:
OPEN filefb$ FOR APPEND AS #20
FOR d% = 1 TO LOF(12)
9999
        IF EOF(12) THEN EXIT FOR
        INPUT #12, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            IF myserial$ <> TRIM$(PetShow.id) THEN GOTO 9999
            IF myserial$ = TRIM$(PetShow.id) THEN
            myserial$ = Cargo.fac
                WRITE #20, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            END IF
    NEXT
    CLOSE #12: CLOSE #20
OPEN "AMVDAT\AMV.TMP" FOR APPEND AS #2:
OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #12:
FOR d% = 1 TO LOF(12)
999
        IF EOF(12) THEN EXIT FOR
        INPUT #12, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            IF myserial$ <> TRIM$(PetShow.id) THEN
                WRITE #2, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            END IF
            IF myserial$ = TRIM$(PetShow.id) THEN GOTO 999
NEXT
    CLOSE #12: CLOSE #2
KILL "AMVDAT\AMVCA.DAT"
NAME "AMVDAT\AMV.TMP" AS "AMVDAT\AMVCA.DAT"
EmitirFac (filefb$)
END SUB

SUB ListFa
18
neto& = 0
Alert 1, 3, 26, 10, 26, 12
    textocf 28, 11, 0, 3, "Ingrese Fecha: ":
    CUANDO$ = FECCHA$
    textocf 43, 11, 0, 3, CUANDO$
    kbd$ = VERDATE(CUANDO$, 43, 11)
118
CALL Lineas
textocf 2, 25, 0, 3, "ESC: Salir   ³ F2: Seleccionar Fecha  ³ F3: Resumen  ³ F4: Imprimir"
IF yy% = 1 THEN
    textocf 2, 25, 0, 3, "ESC: Salir  ³ ENTER: Seleccionar"
END IF
textocf 1, 2, 3, 3, STRING$(80, " ")
textocf 22, 2, 14, 3, "MOVIMIENTO DE InventS AL " + CUANDO$
textocf 1, 4, 3, 3, STRING$(80, " ")
textocf 2, 4, 0, 3, "PACIENTE"
textocf 20, 4, 0, 3, "CODIGO"
textocf 37, 4, 0, 3, "DESCRIPCION"
textocf 58, 4, 0, 3, "CANT."
textocf 67, 4, 0, 3, "MONTO"
textocf 77, 4, 0, 3, "OP."
Z = 1
GOSUB FaLstBck
FOR y% = 5 TO 24:
    textocf 16, y%, 1, 7, STRING$(1, 179)
    textocf 30, y%, 1, 7, STRING$(1, 179)
    textocf 57, y%, 1, 7, STRING$(1, 179)
    textocf 62, y%, 1, 7, STRING$(1, 179)
    textocf 76, y%, 1, 7, STRING$(1, 179)
NEXT
f$ = RIGHT$(FECCHA$, 5)
file$ = "AMVHIS\FB" + f$ + ".DAT"
OPEN file$ FOR APPEND AS #1: CLOSE #1
OPEN "AMVDAT\AMVCA.DAT" FOR APPEND AS #1: CLOSE #1
OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #1
OPEN file$ FOR INPUT AS #2
230

FOR I = 1 TO 20
299
        IF EOF(1) THEN EXIT FOR
        INPUT #1, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            IF myfecha$ <> TRIM$(CUANDO$) THEN GOTO 299
    S = 0
    IF VAL(LEFT$(mycodigo$, 5)) < 50 THEN
        S = 1
    END IF
    IF myserial$ = "" THEN
        myserial$ = "Cl¡nica"
        S = 4
    END IF
    LOCATE 4 + I, 2: COLOR S, 7: PRINT myserial$;
    LOCATE 4 + I, 17: COLOR S, 7: PRINT mycodigo$;
    LOCATE 4 + I, 31: COLOR S, 7: PRINT mynombre$;
    LOCATE 4 + I, 58: COLOR S, 7: PRINT mycantidad$;
    LOCATE 4 + I, 63: COLOR S, 7: PRINT USING AS$; mysubtotal&;
    LOCATE 4 + I, 77: COLOR S, 7: PRINT myclave$;
NEXT

Z = I
FOR I = Z TO 20
300
      IF EOF(2) THEN
   textocf 2, 4 + I, 1, 7, "*************"
   textocf 17, 4 + I, 1, 7, "*************"
   textocf 31, 4 + I, 1, 7, "*************************"
   textocf 58, 4 + I, 1, 7, "****"
   textocf 63, 4 + I, 1, 7, "*************"
   textocf 77, 4 + I, 1, 7, "**"
           EXIT FOR
      END IF
       INPUT #2, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            IF myfecha$ <> TRIM$(CUANDO$) THEN GOTO 300
    S = 0
    IF VAL(LEFT$(mycodigo$, 5)) < 50 THEN
        S = 1
    END IF
    LOCATE 4 + I, 2: COLOR S, 7: PRINT myserial$;
    LOCATE 4 + I, 17: COLOR S, 7: PRINT mycodigo$;
    LOCATE 4 + I, 31: COLOR S, 7: PRINT mynombre$;
    LOCATE 4 + I, 58: COLOR S, 7: PRINT mycantidad$;
    LOCATE 4 + I, 63: COLOR S, 7: PRINT USING AS$; mysubtotal&;
    LOCATE 4 + I, 77: COLOR S, 7: PRINT myclave$;
    LOCATE 4 + I, 75: COLOR 11, 7: PRINT "F";
NEXT
556
DO
kbd$ = INKEY$
SELECT CASE kbd$
    CASE IS = CHR$(0) + "<":
    c1 = 7: c2 = 7:
    CLOSE #1
    CLOSE #2
    GOTO 18
    CASE IS = CHR$(0) + ">":
    CLOSE #1
    CLOSE #2
    LlistFa CUANDO$
    GOTO 118
    CASE IS = CHR$(0) + "=":
    CLOSE #1
    CLOSE #2
    CALL Resumen
    GOTO 118
    CASE IS = CHR$(0) + "G"
    CASE IS = CHR$(0) + "O"
    CASE IS = CHR$(0) + "Q"
    CASE IS = CHR$(0) + "I"
    CASE IS = CHR$(0) + "P"
        GOSUB FaLstBck: GOTO 230
    CASE IS = CHR$(0) + "H"
        CLOSE #1: OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #1
        CLOSE #2: OPEN file$ FOR INPUT AS #2: GOSUB FaLstBck: GOTO 230
    CASE IS = CHR$(27):
    CLOSE #1
    CLOSE #2
    EXIT SUB
END SELECT
LOOP
EXIT SUB
FaLstBck:
FOR y% = 5 TO 24
    textocf 1, y%, 7, 7, STRING$(80, " ")
NEXT
FOR y% = 5 TO 24:
    textocf 16, y%, 1, 7, STRING$(1, 179)
    textocf 30, y%, 1, 7, STRING$(1, 179)
    textocf 57, y%, 1, 7, STRING$(1, 179)
    textocf 62, y%, 1, 7, STRING$(1, 179)
    textocf 76, y%, 1, 7, STRING$(1, 179)
NEXT
RETURN

END SUB

SUB LlistFa (CUANDO$)
WIDTH LPRINT 131
pag = 1
Espere 0
kk$ = "#############"
kf$ = "\                       \"
kd$ = "\   \"
GOSUB headlfa
f$ = RIGHT$(FECCHA$, 5)
file$ = "AMVHIS\FB" + f$ + ".DAT"
OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #1
OPEN file$ FOR INPUT AS #2
FOR I& = 1 TO LOF(1)
17
    IF A = 0 THEN
       GOSUB final
       IF I& < LOF(1) THEN GOSUB headlfa: GOTO 17
    END IF
    IF EOF(1) THEN CLOSE #1: EXIT FOR
        INPUT #1, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
        IF myfecha$ <> TRIM$(CUANDO$) THEN GOTO 17
    LPRINT USING kk$; VAL(myserial$);
    LPRINT USING kk$; VAL(mycodigo$);
    LPRINT "     ";
    LPRINT USING kf$; mynombre$;
    LPRINT USING kd$; mycantidad$;
    LPRINT "     ";
    LPRINT USING AS$; mysubtotal&;
    LPRINT STRING$(36, 32);
    LPRINT mytime$; "    ";
    LPRINT myclave$
    mottl& = mottl& + mysubtotal&
A = A - 1
NEXT
Z& = I&
FOR Z& = I& TO LOF(2)
21
    IF A = 0 THEN
       GOSUB final
        IF I& < LOF(2) THEN GOSUB headlfa: GOTO 21
    END IF
        IF EOF(2) THEN GOSUB final: CLOSE #2: EXIT FOR
        INPUT #2, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            IF myfecha$ <> TRIM$(CUANDO$) THEN GOTO 21
    LPRINT USING kk$; VAL(myserial$);
    LPRINT USING kk$; VAL(mycodigo$);
    LPRINT "     ";
    LPRINT USING kf$; mynombre$;
    LPRINT USING kd$; mycantidad$;
    LPRINT "     ";
    LPRINT USING AS$; mysubtotal&;
    LPRINT STRING$(36, 32);
    LPRINT mytime$; "    ";
    LPRINT myclave$
    mottl& = mottl& + mysubtotal&
A = A - 1
NEXT
EXIT SUB

headlfa:
    A = 45
    LPRINT CHR$(14); TRIM$(nombre$)
    LPRINT CHR$(18); Daylet$; " "; MFECHA$
    LPRINT STRING$(19, 32); CHR$(14); CHR$(15); "MOVIMIENTO DE InventS AL "; CUANDO$
    LPRINT CHR$(18); TIME$; "                                                      Pag. #"; pag
    LPRINT CHR$(15); STRING$(130, 196)
    LPRINT "     PACIENTE       CODIGO     DESCRIPCION          CANT.               MONTO                                    HORA        OP.";
    LPRINT STRING$(130, 196)
RETURN

final:
    LPRINT STRING$(130, 196)
    pag% = pag% + 1
    LPRINT "     Saldo: "; : LPRINT USING kk$; mottl&;
    FOR lines = 1 TO A
        LPRINT
    NEXT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT CHR$(18):
RETURN
END SUB

SUB Newlista (yy%, recupera%)
OPEN "AMVDAT\AMVPD.DAT" FOR RANDOM ACCESS READ AS #2 LEN = LEN(Pet)
IF LOF(2) = 0 THEN CLOSE #2: CALL NOHAY: EXIT SUB
CLOSE #2
' --- AUTOMATIZACION AQUI  ---
CALL sort
' ----------------------------
VIEW PRINT 1 TO 25: WIDTH 80
4454
Lineas
que$ = "JUAN"
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "ESC: Salir  ³ F4: Imprimir"
textocf 57, 25, 0, 3, "F5: Listar por Paciente"
IF yy% = 1 THEN
textocf 2, 25, 0, 3, "ESC: Salir  ³ ENTER: Seleccionar"
END IF
textocf 1, 2, 3, 3, STRING$(80, " ")
textocf 27, 2, 14, 3, "LISTADO DE PACIENTES POR DUE¥O"
textocf 1, 4, 3, 3, STRING$(80, " ")
textocf 4, 4, 0, 3, "REGISTRO": textocf 22, 4, 0, 3, "DUE¥O": textocf 38, 4, 0, 3, " NOMBRE"
textocf 69, 4, 0, 3, "SALDO": textocf 54, 4, 0, 3, "RAZA": textocf 78, 4, 0, 3, "OP."
GOSUB PaLstBck:
IF recupera% = 0 THEN file$ = "AMVDAT\AMVPD.DAT"
IF recupera% = 1 THEN file$ = "AMVHIS\AMVPA.HIS"
OPEN file$ FOR RANDOM ACCESS READ AS #2 LEN = LEN(Pet)
IF LOF(2) = 0 THEN
    CLOSE #2: CALL NOHAY: EXIT SUB
END IF
446
GOSUB FOND
IF LOF(2) <> 0 THEN final& = LOF(2) / LEN(Pet)
LOCATE 3, 2: COLOR 7, 1: PRINT "Total Pacientes: "; final&
FOR l& = 1 TO 20
        GET #2, l&, PetShow:
        scrollupall                 '186-201
        textocf 1, 24, 7, 7, STRING$(80, " ")
        q = 24: GOSUB coloca
        IF l& = 20 THEN M& = 1: EXIT FOR
NEXT l&
l& = 20
IF final& < 20 THEN final& = 20
6
DO
    kbd$ = INKEY$
        SELECT CASE kbd$
                CASE IS = CHR$(0) + "D"
                CASE IS = CHR$(0) + "O"
                    IF l& = final& THEN SOUND 58, 7: GOTO 6
                    IF final& > 20 THEN
                    l& = final& - 20: M& = l& - 19
                    GOSUB FOND
                    FOR r = 1 TO 21
                        l& = l& + 1: M& = M& + 1
                        IF l& > final& THEN
                            l& = final&: M& = l& - 19: GOTO 6
                        END IF
                        GET #2, l&, PetShow: q = 4 + r: GOSUB coloca
                    NEXT r
                    END IF
                CASE IS = CHR$(0) + "G"
                        IF l& = 20 THEN SOUND 58, 7: GOTO 6
                        GOSUB FOND
                        FOR l& = 1 TO 20
                            GET #2, l&, PetShow: scrollupall:
                            q = 24: GOSUB coloca
                            IF l& = 20 THEN
                            M& = 1: EXIT FOR
                            END IF
                        NEXT l&

                CASE IS = CHR$(0) + "I"
                    FOR r = 1 TO 20
                        l& = l& - 1: M& = M& - 1
                        IF l& < 20 OR M& < 1 THEN
                              l& = 20: M& = 1: SOUND 58, 7: GOTO 6
                        END IF
                        CALL scrolldownall
                        LOCATE 5, 1: COLOR 7, 7: PRINT STRING$(80, " ");
                        GET #2, M&, PetShow: q = 5:  GOSUB coloca
                    NEXT r
                CASE IS = CHR$(0) + "Q"
                    FOR r = 1 TO 20
                        l& = l& + 1: M& = M& + 1
                        IF l& > final& THEN
                           l& = final&: M& = l& - 19: SOUND 58, 7: GOTO 6
                        END IF
                        GET #2, l&, PetShow: CALL scrollupall
                        LOCATE 24, 1: COLOR 7, 7: PRINT STRING$(80, " ");
                        q = 24: GOSUB coloca
                        NEXT r
                CASE IS = CHR$(0) + "P"
                        l& = l& + 1: M& = M& + 1
                        IF l& > final& THEN
                                l& = final&: M& = l& - 19: SOUND 58, 7: GOTO 6
                        END IF
                        CALL scrollupall:
                        LOCATE 24, 1: COLOR 7, 7: PRINT STRING$(80, " ");
                        GET #2, l&, PetShow: q = 24: GOSUB coloca
                CASE IS = CHR$(0) + "H"
                        l& = l& - 1: M& = M& - 1
                        IF l& < 20 OR M& < 1 THEN
                              l& = 20: M& = 1: SOUND 58, 7: GOTO 6
                        END IF
                        CALL scrolldownall:
                        LOCATE 5, 1: COLOR 7, 7: PRINT STRING$(80, " ");
                        GET #2, M&, PetShow: q = 5: GOSUB coloca
                CASE IS = CHR$(27): CLOSE #2: REGNUMERO& = 0: REGNOMBRE$ = "": salir% = 1: EXIT SUB
                CASE IS = CHR$(0) + ">":
                IF yy% <> 1 THEN CLOSE #2: LlistPa: GOTO 4454
                CASE IS = CHR$(0) + "?": CLOSE #2: salir% = 0: EXIT SUB
                CASE IS = CHR$(13):
                IF yy% = 1 THEN
                    CLOSE #2: yy% = 0
                    textocf 1, 25, 3, 3, STRING$(80, " ")
                    textocf 2, 25, 0, 3, "ESC: Salir  ³  Ingrese el Nombre o el N£mero del Paciente a Mostrar"
                    CALL Question(1, "Pa", 1)
                    salir% = 1
                    EXIT SUB
                END IF
        END SELECT
LOOP
EXIT SUB

PaLstBck:
FOR y% = 5 TO 24
    textocf 1, y%, 7, 7, STRING$(80, " ")
NEXT
RETURN

coloca:
IF PetShow.id <> "             " THEN
    LOCATE q, 1: COLOR 7, 7: PRINT STRING$(80, " ");
    textocf 15, q, 1, 7, STRING$(1, 179)
    textocf 31, q, 1, 7, STRING$(1, 179)
    textocf 47, q, 1, 7, STRING$(1, 179)
    textocf 59, q, 1, 7, STRING$(1, 179)
    textocf 77, q, 1, 7, STRING$(1, 179)
    d = 4
    IF PetShow.monto < 0 THEN d = 1
    LOCATE q, 2:  COLOR c, 7: PRINT PetShow.id; : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 16: COLOR c, 7: PRINT LEFT$(PetShow.dueno, 17); : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 34: COLOR c, 7: PRINT PetShow.nombr; : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 50: COLOR c, 7: PRINT PetShow.raza; : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 66: COLOR d, 7: PRINT USING AS$; PetShow.monto;
    IF PetShow.monto = 0 THEN textocf 66, q, 1, 7, "***********"
    LOCATE q, 77: COLOR 1: PRINT STRING$(1, 179); : LOCATE q, 78: COLOR c, 7: PRINT PetShow.Clave; : COLOR 7, 7: PRINT STRING$(1, " ")
END IF
RETURN

FOND:
FOR y% = 5 TO 24:
    textocf 1, y%, 7, 7, STRING$(80, " ")
    textocf 15, y%, 1, 7, STRING$(1, 179)
    textocf 31, y%, 1, 7, STRING$(1, 179)
    textocf 47, y%, 1, 7, STRING$(1, 179)
    textocf 59, y%, 1, 7, STRING$(1, 179)
    textocf 77, y%, 1, 7, STRING$(1, 179)
NEXT
RETURN

END SUB

DEFSNG A-Z
SUB PintarVar
LOCATE 3, 12: COLOR 15, 1: PRINT PetShow.id;
textocf 76, 2, 1, 3, Iniciales$
Cargo.Clave = Iniciales$
COLOR 7, 1: LOCATE 4, 12: PRINT TRIM$(PetShow.dueno);
Cargo.dueno = PetShow.dueno
COLOR 7, 1: LOCATE 4, 49: PRINT TRIM$(PetShow.nombr);
Cargo.nombr = PetShow.nombr
COLOR 7, 1: LOCATE 5, 49: PRINT TRIM$(PetShow.checkin);
Cargo.checkin = PetShow.checkin
COLOR 7, 1: LOCATE 5, 15: PRINT TRIM$(PetShow.cedula);
Cargo.cedula = PetShow.cedula
COLOR 7, 1: LOCATE 6, 20: PRINT TRIM$(PetShow.direcc);
Cargo.direcc = PetShow.direcc
Cargo.fecha = TRIM$(FECCHA$)
Cargo.id = TRIM$(PetShow.id)
END SUB

SUB PRINTGASTO (file$, CUANDO$)
WIDTH LPRINT 131
pag = 1
Espere 0
kk$ = "#############"
kf$ = "\                       \"
kd$ = "\   \"
GOSUB headlGa
OPEN file$ FOR INPUT AS #1
FOR I& = 1 TO LOF(1)
29
    IF A = 0 THEN
        GOSUB Gfinal
        IF I& < LOF(1) THEN GOSUB headlGa: GOTO 29
    END IF
        IF EOF(1) THEN GOSUB Gfinal: CLOSE #1: EXIT FOR
        INPUT #1, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            IF myfecha$ <> TRIM$(CUANDO$) THEN GOTO 29
    myserial$ = ""
    LPRINT USING kk$; VAL(mycodigo$);
    LPRINT "     ";
    LPRINT USING kf$; mynombre$;
    LPRINT USING kd$; mycantidad$;
    LPRINT "     ";
    LPRINT USING AS$; mysubtotal&;
    LPRINT STRING$(49, 32);
    LPRINT mytime$; "    ";
    LPRINT myclave$
    mottl& = mottl& + mysubtotal&
A = A - 1
NEXT
EXIT SUB

headlGa:
    A = 45
    LPRINT CHR$(14); nombre$
    LPRINT CHR$(18); Daylet$; " "; MFECHA$
    LPRINT STRING$(19, 32); CHR$(14); CHR$(15); "MOVIMIENTO DE GASTOS AL "; CUANDO$
    LPRINT CHR$(18); TIME$; "                                                      Pag. #;"; pag
    LPRINT CHR$(15); STRING$(130, 196)
    LPRINT "       CODIGO     DESCRIPCION          CANT.               MONTO                                                 HORA        OP.";
    LPRINT STRING$(130, 196)
RETURN

Gfinal:
    LPRINT STRING$(130, 196)
    pag% = pag% + 1
    LPRINT "Saldo: "; : LPRINT USING kk$; mottl&;
    FOR lines = 1 TO A: LPRINT : NEXT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT CHR$(18):
RETURN

END SUB

DEFINT A-Z
SUB Resumen
DIM file(3) AS STRING * 18
GOSUB cuadro
Espere 5
FE$ = RIGHT$(FECCHA$, 5)
file(1) = "AMVDAT\AMVCA.DAT"
file(2) = "AMVHIS\GA" + FE$ + ".DAT"
file(3) = "AMVHIS\FB" + FE$ + ".DAT"
FOR I = 1 TO 3
    OPEN file(I) FOR APPEND AS #1: CLOSE #1
NEXT
FOR archi = 1 TO 3
    OPEN file(archi) FOR INPUT AS #1:
    FOR I& = 1 TO LOF(1)
21199
        IF EOF(1) THEN EXIT FOR
        INPUT #1, serial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
        IF myfecha$ = FECCHA$ AND serial$ <> "" THEN GOSUB variables
        IF myfecha$ = FECCHA$ AND serial$ = "" THEN ga& = ga& + mysubtotal&
        IF myfecha$ <> FECCHA$ AND serial$ <> "" THEN GOSUB variables2
        IF myfecha$ <> FECCHA$ AND serial$ = "" THEN bga& = bga& + mysubtotal&
    NEXT
    CLOSE #1
NEXT archi
CLOSE #1
textocf 26, 9, 3, 3, "                               "
textocf 26, 10, 3, 3, "                              "
textocf 26, 11, 3, 3, "                              "
textocf 26, 12, 3, 3, "                              "
textocf 26, 13, 0, 3, "------------------------------"
GOSUB vervariables
DO
kbd$ = INKEY$
SELECT CASE kbd$
    CASE IS = CHR$(27): EXIT SUB
    CASE IS = CHR$(0) + ">":
    GOSUB imprimir:
    EXIT SUB
    CASE ELSE
END SELECT
LOOP
EXIT SUB

cuadro:
FOR T = 5 TO 19: textocf 4, T, 3, 3, STRING$(75, " "): NEXT
textocf 3, 4, 0, 3, "É"
textocf 4, 4, 0, 3, STRING$(74, 205)
textocf 78, 4, 0, 3, "»"
textocf 78, 20, 0, 3, "¼"
textocf 3, 20, 0, 3, "È"
textocf 4, 20, 0, 3, STRING$(74, 205)
FOR T = 5 TO 19: textocf 3, T, 0, 3, "º": NEXT
FOR T = 5 TO 19: textocf 78, T, 0, 3, "º": NEXT
textocf 20, 4, 14, 3, " RESUMEN DEL MOVIMIENTO AL " + FECCHA$ + " "
textocf 7, 20, 8, 3, " F4: Imprimir ": textocf 63, 20, 8, 3, " ESC: Salir "
textocf 2, 25, 3, 3, STRING$(70, " ")
    me$ = "\          \"
    Mex$ = MMES$
    LOCATE 5, 4: COLOR 11, 3: PRINT "                                 Hoy      Acumulado          Total"
    LOCATE 6, 5: COLOR 1, 3: PRINT "InventS:";
    LOCATE 7, 5: COLOR 8, 3: PRINT "AJUSTES:";
    LOCATE 8, 5: COLOR 0, 3: PRINT "-------------------------------------------------------------------";
    LOCATE 9, 5: COLOR 0, 3: PRINT "SUB TOTAL:";
    LOCATE 11, 5: COLOR 4, 3: PRINT "EFECTIVO Y/O CHEQUES:   ";
    LOCATE 12, 5: COLOR 6, 3: PRINT "CUENTAS POR COBRAR:     ";
    LOCATE 13, 5: COLOR 0, 3: PRINT "-------------------------------------------------------------------";
    LOCATE 14, 5: COLOR 0, 3: PRINT "SUB TOTAL:              ";
    LOCATE 16, 5: COLOR 0, 3: PRINT "-------------------------------------------------------------------";
    LOCATE 17, 5: COLOR 0, 3: PRINT "SALDO PENDIENTE:        ";
    LOCATE 18, 5: COLOR 0, 3: PRINT "GASTOS:                 ";
RETURN

vervariables:
   
    LOCATE 6, 29: COLOR 1, 3: PRINT USING AS$; cf&;
    LOCATE 7, 29: COLOR 8, 3: PRINT USING AS$; af&;
    LOCATE 9, 29: COLOR 0, 3: PRINT USING AS$; cf& + af&;
    LOCATE 11, 29: COLOR 4, 3: PRINT USING AS$; ef&;
    LOCATE 12, 29: COLOR 6, 3: PRINT USING AS$; cc&;
    LOCATE 14, 29: COLOR 0, 3: PRINT USING AS$; ef& + cc&;
    LOCATE 17, 29: COLOR 0, 3: PRINT USING AS$; ABS(cf& + af&) + (ef& + cc&);
    LOCATE 18, 29: COLOR 0, 3: PRINT USING AS$; ga&;
   
    LOCATE 6, 44: COLOR 1, 3: PRINT USING AS$; bcf&;
    LOCATE 7, 44: COLOR 8, 3: PRINT USING AS$; baf&;
    LOCATE 9, 44: COLOR 0, 3: PRINT USING AS$; bcf& + baf&;
    LOCATE 11, 44: COLOR 4, 3: PRINT USING AS$; bef&;
    LOCATE 12, 44: COLOR 6, 3: PRINT USING AS$; bcc&;
    LOCATE 14, 44: COLOR 0, 3: PRINT USING AS$; bef& + bcc&;
    LOCATE 17, 44: COLOR 0, 3: PRINT USING AS$; ABS(bcf& + baf&) + (bef& + bcc&);
    LOCATE 18, 44: COLOR 0, 3: PRINT USING AS$; bga&;

    LOCATE 6, 59: COLOR 1, 3: PRINT USING AS$; cf& + bcf&;
    LOCATE 7, 59: COLOR 8, 3: PRINT USING AS$; af& + baf&;
    LOCATE 9, 59: COLOR 0, 3: PRINT USING AS$; cf& + af& + bcf& + baf&;
    LOCATE 11, 59: COLOR 4, 3: PRINT USING AS$; ef& + bef&;
    LOCATE 12, 59: COLOR 6, 3: PRINT USING AS$; cc& + bcc&;
    LOCATE 14, 59: COLOR 0, 3: PRINT USING AS$; ef& + cc& + bef& + bcc&;
    LOCATE 17, 59: COLOR 0, 3: PRINT USING AS$; ABS(cf& + af&) + (ef& + cc&) + ABS(bcf& + baf&) + (bef& + bcc&);
    LOCATE 18, 59: COLOR 0, 3: PRINT USING AS$; ga& + bga&;
RETURN

imprimir:
    LPRINT CHR$(14); TRIM$(nombre$)
    LPRINT CHR$(18); DAYELET$; " "; MFECHA$
    LPRINT STRING$(17, 32); CHR$(14); CHR$(15); "RESUMEN DEL MOVIMIENTO AL"; FECCHA$
    LPRINT CHR$(18); TIME$
    LPRINT STRING$(78, 196)
    LPRINT "                                      Hoy        Acumulado            Total"
    LPRINT STRING$(78, 196)
    LPRINT "  InventS:                     "; : LPRINT USING AS$; cf&; : LPRINT "      "; : LPRINT USING AS$; bcf&; : LPRINT "      "; : LPRINT USING AS$; cf& + bcf&
    LPRINT "  AJUSTES:                    "; : LPRINT USING AS$; af&; : LPRINT "      "; : LPRINT USING AS$; baf&; : LPRINT "      "; : LPRINT USING AS$; af& + baf&
    LPRINT "----------------------------------------------------------------------------";
    LPRINT "  SUB TOTAL:                  "; : LPRINT USING AS$; cf& + af&; : LPRINT "      "; : LPRINT USING AS$; bcf& + baf&; : LPRINT "      "; : LPRINT USING AS$; cf& + af& + bcf& + baf&;
    LPRINT
    LPRINT "  EFECTIVO Y/O CHEQUES:       "; : LPRINT USING AS$; ef&; : LPRINT "      "; : LPRINT USING AS$; bef&; : LPRINT "      "; : LPRINT USING AS$; ef& + bef&;
    LPRINT "  CUENTAS POR COBRAR:         "; : LPRINT USING AS$; cc&; : LPRINT "      "; : LPRINT USING AS$; bcc&; : LPRINT "      "; : LPRINT USING AS$; cc& + bcc&;
    LPRINT "----------------------------------------------------------------------------";
    LPRINT "  SUB TOTAL:                  "; : LPRINT USING AS$; ef& + cc&; : LPRINT "      "; : LPRINT USING AS$; bef& + bcc&; : LPRINT "      "; : LPRINT USING AS$; ef& + cc& + bef& + bcc&;
    LPRINT
    LPRINT "----------------------------------------------------------------------------";
    LPRINT "  SALDO SIN CANCELAR:         "; : LPRINT USING AS$; ABS(cf& + af&) + (ef& + cc&); : LPRINT "      "; : LPRINT USING AS$; ABS(bcf& + baf&) + (bef& + bcc&); : LPRINT "      "; : LPRINT USING AS$; ABS(cf& + af&) + (ef& + cc&) + ABS(bcf& +  _
baf&) + (bef& + bcc&);
    LPRINT "  GASTOS:                     "; : LPRINT USING AS$; ga&; : LPRINT "      "; : LPRINT USING AS$; bga&; : LPRINT "      "; : LPRINT USING AS$; ga& + bga&;
    LPRINT CHR$(12)
RETURN

variables:
        IF VAL(LEFT$(mycodigo$, 5)) > 49 THEN
            cf& = mysubtotal& + cf&
        END IF
        IF VAL(LEFT$(mycodigo$, 5)) = 1 OR VAL(LEFT$(mycodigo$, 5)) = 2 THEN
            ef& = mysubtotal& + ef&
        END IF
        IF VAL(LEFT$(mycodigo$, 5)) = 3 THEN
            af& = mysubtotal& + af&
        END IF
        IF VAL(LEFT$(mycodigo$, 5)) >= 4 AND VAL(LEFT$(mycodigo$, 5)) < 50 THEN
            cc& = mysubtotal& + cc&
        END IF
RETURN
variables2:
        IF VAL(LEFT$(mycodigo$, 5)) > 49 THEN
            bcf& = mysubtotal& + bcf&
        END IF
        IF VAL(LEFT$(mycodigo$, 5)) = 1 OR VAL(LEFT$(mycodigo$, 5)) = 2 THEN
            bef& = mysubtotal& + bef&
        END IF
        IF VAL(LEFT$(mycodigo$, 5)) = 3 THEN
            baf& = mysubtotal& + baf&
        END IF
        IF VAL(LEFT$(mycodigo$, 5)) >= 4 AND VAL(LEFT$(mycodigo$, 5)) < 50 THEN
            bcc& = mysubtotal& + bcc&
        END IF
RETURN
END SUB

SUB ShowFac
2559
CALL Lineas: GOSUB showInventsfondo
aquicf$ = "": REGNUMERO& = 0
GOSUB faccompara: REGNOMBRE$ = TRIM(Cargo.fac)
IF Cargo.fac = "             " THEN
    GOSUB errorshowCargo: GOTO 2559
END IF
myserial$ = TRIM$(Cargo.fac)
21777
    Z = 1
    OPEN filefb$ FOR INPUT AS #4:
    IF LOF(4) = 0 THEN
        CLOSE #4: KILL filefb$
        Cargo.fac = "             "
        GOSUB errorshowCargo: GOTO 2559
    END IF
    GOSUB ShowmVAR: GOSUB showInventsfondo: LARGO = 9
    CALL Invents(Cargo.fac)
CLOSE #1
   
    DO
    kbd$ = INKEY$
        SELECT CASE kbd$
        CASE IS = CHR$(0) + ">":
        Cargo.fac = "             ": CLOSE #4: GOTO 2559
            CASE IS = CHR$(27):  CLOSE #4: EXIT SUB
            CASE IS = CHR$(0) + ";"
                Cargo.fac = REGNOMBRE$
                CLOSE #4:
                Espere 0
                EmitirFac filefb$
                GOTO 2559
           CASE IS = CHR$(0) + "H"
                CLOSE #4:
                GOSUB showInventsfondo
                OPEN filefb$ FOR INPUT AS #4:
                CALL Invents(Cargo.fac)
           CASE IS = CHR$(0) + "P"
                 GOSUB showInventsfondo
                CALL Invents(Cargo.fac)
        END SELECT
    LOOP
    CLOSE #4
    GOTO 21777
EXIT SUB

errorshowCargo:
Lineas
GOSUB showInventsfondo
    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1
    CLOSE #1
RETURN


showInventsfondo:
textocf 29, 2, 14, 3, "HISTORIAL DE Cargo"
textocf 3, 4, 14, 1, "CLIENTE:"
textocf 40, 4, 14, 1, "MASCOTA:"
textocf 40, 3, 14, 1, "PACIENTE:"
textocf 40, 5, 14, 1, "ENTRADA:"
textocf 63, 5, 14, 1, "SALIDA:"
textocf 3, 5, 14, 1, "RIF-NIT-CI:"
textocf 3, 6, 14, 1, "DIRECCION/TELEF:"
textocf 1, 8, 3, 3, STRING$(80, " ")
textocf 3, 8, 14, 3, "FECHA"
textocf 12, 8, 14, 3, "OP."
textocf 18, 8, 14, 3, "CODIGO"
textocf 32, 8, 14, 3, "CONCEPTO"
textocf 53, 8, 14, 3, "CANT."
textocf 65, 8, 14, 3, "MONTO"
FOR y% = 9 TO 24
    textocf 1, y%, 7, 7, STRING$(80, " ")
    textocf 11, y%, 1, 7, STRING$(1, 179)
    textocf 15, y%, 1, 7, STRING$(1, 179)
    textocf 25, y%, 1, 7, STRING$(1, 179)
    textocf 52, y%, 1, 7, STRING$(1, 179)
    textocf 58, y%, 1, 7, STRING$(1, 179)
NEXT
textocf 2, 25, 0, 3, "ESC: Salir  ³ F1: Imprimir  ³ F4: Nueva Busqueda"
RETURN

ShowmVAR:
CALL Lineas
LOCATE 2, 2: COLOR 0, 3: PRINT "FACTURA #", Cargo.fac;
textocf 76, 2, 1, 3, Cargo.Clave
textocf 50, 3, 7, 1, Cargo.id
COLOR 7, 1: LOCATE 4, 12: PRINT (TRIM$(Cargo.dueno));
COLOR 7, 1: LOCATE 4, 49: PRINT (TRIM$(Cargo.nombr));
COLOR 7, 1: LOCATE 5, 15: PRINT (TRIM$(Cargo.cedula));
COLOR 7, 1: LOCATE 5, 71: PRINT (TRIM$(Cargo.fecha));
COLOR 7, 1: LOCATE 5, 49: PRINT (TRIM$(Cargo.checkin));
COLOR 7, 1: LOCATE 6, 20: PRINT (TRIM$(Cargo.direcc));
RETURN

faccompara:
factura.fac = 0
filefh$ = "": filefb$ = ""
Alert 1, 3, 23, 7, 33, 11
textocf 34, 6, 7, 4, "Buscar Factura."
textocf 25, 8, 14, 3, "Ingrese el N£mero, el mes y el"
textocf 25, 9, 14, 3, "a¤o de la Factura a consultar:"
textocf 28, 10, 1, 3, "                   "
textocf 45, 10, 0, 2, "mm-aa"
textocf 2, 25, 0, 3, "ESC: Salir                                      ":
kbd$ = LLENARCF$(29, 10, 0, 2, "", f$, 13, 13)
IF kbd$ = CHR$(27) THEN EXIT SUB
kbd$ = LLENARCF$(45, 10, 0, 2, "mm", M$, 2, 2)
kbd$ = LLENARCF$(48, 10, 0, 2, "aa", A$, 2, 2)
M% = VAL(M$): A% = VAL(A$)
IF M% < 10 THEN M$ = "0" + TRIM$(STR$(M%))
IF A% < 10 THEN A$ = "0" + TRIM$(STR$(A%))
A$ = M$ + "-" + A$
filefb$ = "AMVHIS\FB" + A$ + ".DAT"
filefh$ = "AMVHIS\FH" + A$ + ".DAT"
REGNOMBRE$ = TRIM$(UCASE$(f$)):  REGNUMERO& = VAL(LEFT$(REGNOMBRE$, 5)):
        IF REGNUMERO& <= 0 AND REGNOMBRE$ = "" THEN GOTO 2559
        GOSUB mmonent
        FOR REG& = 1 TO LOF(1) / LEN(Cargo) + 1
            GOSUB zalio
            IF REG& = LOF(1) / LEN(Cargo) + 1 THEN
                Cargo.fac = "             ":
                EXIT FOR
            END IF
            GET #1, REG&, Cargo
            IF REGNOMBRE$ = TRIM$(Cargo.fac) THEN EXIT FOR
            IF REGNOMBRE$ = LEFT$((TRIM$(Cargo.nombr)), LEN(REGNOMBRE$)) THEN EXIT FOR
        NEXT
        CLOSE #1
RETURN

mmonent:
IF B% = 1 THEN
END IF
        Espere 0
        OPEN filefh$ FOR RANDOM ACCESS READ AS #1 LEN = LEN(Cargo)
         IF LOF(1) = 0 THEN
             CLOSE #1: KILL filefh$
         GOSUB errorshowCargo: GOTO 2559
         END IF
        textocf 1, 25, 3, 3, STRING$(80, " ")
        textocf 51, 25, 0, 3, "ESC: Detener B£squeda y Salir"
RETURN
zalio:
         
            IF INKEY$ = CHR$(27) THEN
                Alert 1, 3, 26, 5, 26, 7
                textocf 28, 6, 0, 3, "Busqueda detenida..."
                SLEEP 1
                CLOSE #1: EXIT SUB
            END IF
RETURN
END SUB

