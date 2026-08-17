DECLARE SUB sort ()
DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE SUB PrintFicha ()
DECLARE SUB PrintHistory ()
DECLARE SUB AgregarHist ()
DECLARE SUB EDDIT (kbd$)
DECLARE SUB Newlista (yy%, recupera%)
DECLARE SUB NOHAY ()
DECLARE SUB ActualPa (yy%, file%)
DECLARE SUB ActualPr (yy%, file%)
DECLARE SUB ListPr (yy%, limite%, recupera%)
DECLARE SUB LlistPa ()
DECLARE SUB LOGIN (ZZ%)
DECLARE SUB Lineas ()
DECLARE SUB Question (u%, Fo$, B%)
DECLARE SUB comppa ()
DECLARE SUB PABACK ()
DECLARE SUB Invents (WHAT$)
DECLARE SUB SHOWPA ()
DECLARE SUB ShowInvents ()
DECLARE SUB ShowVacunas ()
DECLARE SUB listPa (yy%, recupera%)
DECLARE SUB CompSaldo ()
DECLARE SUB EditHead ()
DECLARE SUB EditAll (l%)
DECLARE SUB QuickInvent ()
DECLARE SUB ShowHead ()
DECLARE SUB ShowBlood ()
DECLARE FUNCTION SHOWVAC$ (who$, x%, y%)
DECLARE SUB ShowOrina ()
DECLARE SUB ShowHeces ()
DECLARE SUB ShowNotas ()
DECLARE SUB Espere (y%)
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'

DEFINT A-Z
SUB Invents (WHAT$)
FOR I = 1 TO LEN(WHAT$)
A$ = RIGHT$(LEFT$(WHAT$, I), 1)
IF A$ = CHR$(0) THEN MID$(WHAT$, I, LEN(WHAT$)) = CHR$(32)
NEXT
WHAT$ = TRIM$(WHAT$)
GOSUB Inventfondo
FOR I% = LARGO TO 24
99
    IF EOF(4) THEN
        textocf 2, I%, 1, 7, "********"
        textocf 12, I%, 1, 7, "**"
        textocf 16, I%, 1, 7, "*******"
        textocf 26, I%, 1, 7, "*************************"
        textocf 53, I%, 1, 7, "****"
        textocf 59, I%, 1, 7, "******************"
        EXIT SUB
    END IF
    INPUT #4, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
    IF TRIM$(myserial$) <> WHAT$ THEN GOTO 99
    D = 0: c = 4
    IF VAL(mycodigo$) < 50 THEN D = 1: c = 1
    textocf 2, I%, 0, 7, myfecha$
    textocf 12, I%, 0, 7, myclave$
    textocf 16, I%, 0, 7, mycodigo$
    textocf 26, I%, D, 7, mynombre$
    textocf 53, I%, 0, 7, mycantidad$
    LOCATE I%, 68: COLOR c, 7: PRINT USING AS$; mysubtotal&;
NEXT
EXIT SUB

Inventfondo:
textocf 1, LARGO - 1, 3, 3, STRING$(80, " ")
textocf 3, LARGO - 1, 14, 3, "FECHA"
textocf 12, LARGO - 1, 14, 3, "OP."
textocf 18, LARGO - 1, 14, 3, "CODIGO"
textocf 32, LARGO - 1, 14, 3, "CONCEPTO"
textocf 53, LARGO - 1, 14, 3, "CANT."
textocf 65, LARGO - 1, 14, 3, "MONTO"
FOR y% = LARGO TO 24
    IF y% = 11 THEN S% = 14
    textocf 1, y%, 7, 7, STRING$(80, " ")
    textocf 11, y%, 1, 7, STRING$(1, 179)
    textocf 15, y%, 1, 7, STRING$(1, 179)
    textocf 25, y%, 1, 7, STRING$(1, 179)
    textocf 52, y%, 1, 7, STRING$(1, 179)
    textocf 58, y%, 1, 7, STRING$(1, 179)
NEXT

RETURN
END SUB

SUB listPa (yy%, recupera%)
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #2 LEN = LEN(Pet)
IF LOF(2) = 0 THEN CLOSE #2: CALL NOHAY: EXIT SUB
CLOSE #2
' --- AUTOMATIZACIÃ“N AQUÃ ---
' Genera y ordena los archivos .IDX antes de que el usuario vea la pantalla
CALL sort
' ----------------------------
z = 0
VIEW PRINT 1 TO 25: WIDTH 80
4454
Lineas
que$ = "JUAN"
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "ESC: Salir   ³ F4: Imprimir  "
textocf 57, 25, 0, 3, "F5: Listar por Due¤o"
IF yy% = 1 THEN textocf 2, 25, 0, 3, "ESC: Salir   ³ ENTER: Seleccionar"
textocf 1, 2, 3, 3, STRING$(80, " ")
textocf 27, 2, 14, 3, "LISTADO DE PACIENTES POR NOMBRE"
textocf 1, 4, 3, 3, STRING$(80, " ")
textocf 4, 4, 0, 3, "REGISTRO": textocf 20, 4, 0, 3, "NOMBRE": textocf 36, 4, 0, 3, " RAZA  "
textocf 51, 4, 0, 3, "SALDO": textocf 64, 4, 0, 3, "DUE¥O": textocf 78, 4, 0, 3, "OP."
GOSUB PaLstBck:
IF recupera% = 0 THEN file$ = "AMVDAT\AMVPA.DAT"
IF recupera% = 1 THEN file$ = "AMVHIS\AMVPA.HIS"
OPEN file$ FOR RANDOM ACCESS READ AS #2 LEN = LEN(Pet)
IF LOF(2) = 0 THEN CLOSE #2: CALL NOHAY: EXIT SUB
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
'GOSUB printear
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
                CASE IS = CHR$(27): CLOSE #2: REGNUMERO& = 0: REGNOMBRE$ = "":
                salir% = 1: EXIT SUB
                CASE IS = CHR$(0) + "?": CLOSE #2:
                Newlista yy%, recupera%
                IF salir% = 1 THEN EXIT SUB
                GOTO 4454
                CASE IS = CHR$(0) + ">":
                IF yy% <> 1 THEN CLOSE #2: LlistPa: GOTO 4454
                'GOSUB primir: GOTO 4454
                CASE IS = CHR$(13):
                IF yy% = 1 THEN
                    CLOSE #2
                    yy% = 0
                    textocf 1, 25, 3, 3, STRING$(80, " ")
                    textocf 2, 25, 0, 3, "ESC: Salir   ³  Ingrese el Nombre o el N£mero del Pet a Mostrar"
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
    D = 4
    IF PetShow.monto < 0 THEN D = 1
    LOCATE q, 2:  COLOR c, 7: PRINT PetShow.id; : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 16: COLOR c, 7: PRINT PetShow.nombr; : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 32: COLOR c, 7: PRINT PetShow.raza; : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 48: COLOR D, 7: PRINT USING AS$; PetShow.monto; : COLOR 1: PRINT STRING$(1, 179)
    IF PetShow.monto = 0 THEN textocf 48, q, 1, 7, "***********"
    LOCATE q, 60: COLOR c, 7: PRINT LEFT$(PetShow.dueno, 17); : COLOR 1: PRINT STRING$(1, 179)
    LOCATE q, 78: COLOR c, 7: PRINT PetShow.Clave; : COLOR 7, 7: PRINT STRING$(1, " ")
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

primir:
Alert 1, 3, 27, 7, 26, 11
textocf 38, 6, 7, 4, "Imprimir"
z = 1
666
IF z = 1 THEN aaa = 7: aaaa = 0: bbb = 1: bbbb = 3: ccc = 1: cccc = 3
IF z = 2 THEN aaa = 1: aaaa = 3: bbb = 7: bbbb = 0: ccc = 1: cccc = 3
IF z = 3 THEN aaa = 1: aaaa = 3: bbb = 1: bbbb = 3: ccc = 7: cccc = 0
LOCATE 8, 29:  COLOR aaa, aaaa: PRINT " LISTADO GENERAL    "
LOCATE 9, 29: COLOR bbb, bbbb: PRINT " FICHA DE PACIENTES "
LOCATE 10, 29: COLOR ccc, cccc: PRINT " TODAS LAS FICHAS   "
CLOSE #2
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = CHR$(0) + "H": z = z - 1
        IF z < 1 THEN z = 3
        GOTO 666
        CASE IS = CHR$(0) + "P": z = z + 1
        IF z > 3 THEN z = 1
        GOTO 666
        CASE IS = CHR$(27): yy% = 0: z = 0: EXIT DO
        CASE IS = CHR$(13): yy% = 0: EXIT DO
        CASE ELSE
    END SELECT
LOOP

RETURN
printear:
        IF z = 1 THEN CLOSE #2: LlistPa
        IF z = 2 THEN
                jl% = 0
                    textocf 1, 25, 3, 3, STRING$(80, " ")
                    textocf 2, 25, 0, 3, "ESC: Salir   ³  Ingrese el Nombre o el N£mero del Paciente a Mostrar"
                    CALL Question(1, "Pa", 1)
                    comppa
                    PrintFicha
                    'CLOSE #2
                    END IF
        IF z = 3 THEN jl% = 1: CLOSE #2: PrintFicha: jl% = 0
        z = 0
RETURN
END SUB

SUB PABACK
CALL Lineas
textocf 3, 4, 14, 1, "PACIENTE: "
textocf 31, 4, 14, 1, "RAZA/GENERO: "
textocf 60, 4, 14, 1, "ENTRADA: "
textocf 3, 5, 14, 1, "NACIO: "
textocf 19, 5, 14, 1, "EDAD: "
textocf 38, 5, 14, 1, "SEXO: "
textocf 46, 5, 14, 1, "COLOR: "
textocf 62, 5, 14, 1, "TALLA: "
textocf 3, 6, 14, 1, "DUE¥O: "
textocf 35, 6, 14, 1, "RIF-NIT-CI: "
textocf 3, 7, 14, 1, "DIRECCION/TELF: "
textocf 1, 9, 7, 7, STRING$(80, " ")
textocf 2, 25, 0, 3, "ESC: Salir   ³ F4: Listado"
END SUB

SUB Pac
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #2 LEN = LEN(Pet)
IF LOF(2) = 0 THEN CLOSE #2: CALL NOHAY: EXIT SUB
CLOSE #2
salir% = 0
559
PABACK
textocf 32, 2, 14, 3, "MOSTRAR PACIENTE"
Question 0, "Pa", 1
IF salir% = 1 THEN salir% = 0: EXIT SUB
IF REGNOMBRE$ = "" AND REGNUMERO& = 0 THEN GOTO 559
l = 5
777
PABACK
textocf 32, 2, 14, 3, "MOSTRAR PACIENTE"
Espere 0
comppa
IF PetShow.id = "             " THEN
GOSUB mmalo: GOTO 559
END IF
778
rgero$ = TRIM$(PetShow.id)
PABACK
textocf 32, 2, 14, 3, "MOSTRAR PACIENTE"
779
SHOWPA
IF PetShow.monto > 0 THEN
    textocf 53, 8, 12, 0, " Saldo Deudor:   "
    LOCATE 8, 69: COLOR 12, 0: PRINT USING AS$; PetShow.monto;
END IF
IF PetShow.monto < 0 THEN
    textocf 53, 8, 3, 0, " Saldo a Favor:   "
    LOCATE 8, 69: COLOR 3, 0: PRINT USING AS$; PetShow.monto;
END IF
DO
kbd$ = INKEY$
800
SELECT CASE kbd$
        CASE IS = CHR$(0) + "B": IF l < 4 THEN AgregarHist: GOTO 778
        IF l = 5 THEN PrintHistory: GOTO 778
        CASE IS = CHR$(0) + ";":
            CALL LOGIN(0)
            IF EnGtCla% = 1 THEN
                EnGtCla% = 0
                Pet.Clave = Iniciales$
                PetShow.Clave = Iniciales$
                CALL PABACK: CALL SHOWPA
                textocf 2, 25, 0, 3, "ESC: Salir   ³" + STRING$(60, 32)
                CALL EditHead: REGNOMBRE$ = rgero$: GOTO 777
            END IF
            REGNOMBRE$ = rgero$: GOTO 778
        CASE IS = CHR$(0) + "<":
43
                IF l <> 7 THEN
                KL = 1
                    CALL LOGIN(0)
                    IF EnGtCla% = 1 THEN
                        EnGtCla% = 0
                        Pet.Clave = Iniciales$
                        PetShow.Clave = Iniciales$
                        CALL PABACK: CALL SHOWPA: jl = 0
                        textocf 2, 25, 0, 3, "ESC: Salir   ³" + STRING$(65, 32)
                        CALL EditAll(l)
                        REGNOMBRE$ = rgero$: GOTO 777
                    END IF
                    REGNOMBRE$ = rgero$: GOTO 778
                END IF
        CASE IS = CHR$(0) + ">"
44
           CLOSE #1:
           CALL listPa(1, 0)
                salir% = 0
                IF REGNOMBRE$ = "" AND REGNUMERO& = 0 THEN REGNOMBRE$ = rgero$: REGNUMERO& = 1: mttl& = 0: GOTO 777
                PABACK
                textocf 32, 2, 14, 3, "MOSTRAR PACIENTE"
                Espere 0
                comppa
                IF PetShow.id = "             " THEN GOSUB mmalo: REGNOMBRE$ = rgero$: REGNUMERO& = 1: mttl& = 0: GOTO 777
                mttl& = 0: GOTO 778
           CASE IS = CHR$(0) + "M": l = l + 1
                IF l = 8 THEN l = 1
                CALL SHOWPA
                IF salir% = 1 THEN EXIT SUB
        CASE IS = CHR$(0) + "K": l = l - 1
                IF l < 1 THEN l = 7
                CALL SHOWPA
                IF salir% = 1 THEN EXIT SUB
        CASE IS = CHR$(0) + "P":
                IF l = 4 THEN
                textocf 1, 24, 1, 1, STRING$(80, " "):
                CALL EDDIT(k$)
                IF k$ = CHR$(0) + "M" THEN l = 5: CALL SHOWPA
                IF k$ = CHR$(0) + "K" THEN l = 3: CALL SHOWPA
                IF k$ = CHR$(0) + "<" THEN GOTO 43
                IF k$ = CHR$(0) + ">" THEN GOTO 44
                GOTO 778
                END IF
        CASE IS = CHR$(13):
            IF l = 7 THEN EXIT DO
        CASE ELSE
    END SELECT
LOOP
CLOSE #1
EXIT SUB
mmalo:
    PABACK
    textocf 32, 2, 14, 3, "MOSTRAR PACIENTE"
    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1: CLOSE #1
RETURN
END SUB

SUB RecuperaPa
OPEN "AMVHIS\AMVPA.HIS" FOR APPEND AS #1:
IF LOF(1) = 0 THEN
    CLOSE #1: CALL NOHAY: EXIT SUB
END IF
CLOSE #1
CALL LOGIN(1)
IF EnGtCla% = 0 THEN EXIT SUB
3
REGNUMERO& = 0 AND REGNOMBRE$ = ""
listPa 1, 1
salir% = 0
IF REGNUMERO& = 0 AND REGNOMBRE$ = "" THEN EXIT SUB
OPEN "AMVHIS\AMVPA.HIS" FOR RANDOM AS #1 LEN = LEN(Pet)
FOR I& = 1 TO LOF(1) / LEN(Pet) + 1
    Pet.id = ""
    GET #1, I&, Pet
    IF I& = LOF(1) / LEN(Pet) + 1 THEN GOSUB Ferrado
    IF REGNOMBRE$ = TRIM$(Pet.id) THEN
        GOSUB Quadro
        OPEN "AMVDAT\AMVPD.DAT" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(Pet)
        IF LOF(2) <> 0 THEN z& = LOF(2) / LEN(Pet) + 1
        IF LOF(2) = 0 THEN z& = 1
        PUT #2, z&, Pet:
        CLOSE #2
        OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(Pet)
        IF LOF(2) <> 0 THEN z& = LOF(2) / LEN(Pet) + 1
        IF LOF(2) = 0 THEN z& = 1
        PUT #2, z&, Pet:
        CLOSE #2
        Pet.id = "             "
        PUT #1, I&, Pet:
        CLOSE #1: EXIT FOR

    END IF
NEXT
ActualPa 1, 0
ActualPa 1, 1
EXIT SUB
Ferrado:
    CLOSE #1
    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1: GOTO 3
RETURN
Quadro:
Alert 1, 3, 19, 12, 40, 15
textocf 30, 11, 7, 4, "RECUPERAR PACIENTES "
textocf 24, 12, 1, 3, " REGISTRO "
textocf 43, 12, 1, 3, " NOMBRE "
textocf 23, 13, 7, 0, Pet.id
textocf 40, 13, 7, 0, Pet.nombr
textocf 27, 14, 8, 3, "¨Continuar?"
textocf 45, 14, 6, 3, "(S/N)"
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27):
        textocf 48, 14, 15, 3, "  (N)"
        SLEEP 1
        EXIT SUB
    END SELECT
LOOP
textocf 45, 14, 15, 3, "(S)  ": SLEEP 1
textocf 27, 14, 16, 3, "Recuperando Registro...   "
textocf 23, 13, 0, 7, Pet.id
textocf 40, 13, 0, 7, Pet.nombr
RETURN

END SUB

SUB RecuperaPr
OPEN "AMVHIS\AMVPR.HIS" FOR APPEND AS #1:
IF LOF(1) = 0 THEN
    CLOSE #1: KILL "AMVHIS\AMVPR.HIS"
    CALL NOHAY: EXIT SUB
END IF
CLOSE #1
CALL LOGIN(1)
IF EnGtCla% = 0 THEN EXIT SUB
5
REGNUMERO& = 0 AND REGNOMBRE$ = ""
CALL ListPr(1, 0, 1)
IF REGNUMERO& = 0 AND REGNOMBRE$ = "" THEN EXIT SUB
OPEN "AMVHIS\AMVPR.HIS" FOR RANDOM AS #1 LEN = LEN(Invent)
FOR I& = 1 TO LOF(1) / LEN(Invent) + 1
    Invent.id = ""
    GET #1, I&, Invent
    IF I& = LOF(1) / LEN(Invent) + 1 THEN GOSUB rerrado
    IF REGNOMBRE$ = TRIM$(Invent.id) THEN
        GOSUB ruadro
        OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(Invent)
        IF LOF(2) <> 0 THEN z& = LOF(2) / LEN(Invent) + 1
        IF LOF(2) = 0 THEN z& = 1
        PUT #2, z&, Invent:
        CLOSE #2
        Invent.id = ""
        PUT #1, I&, Invent:
        CLOSE #1: EXIT FOR
    END IF
NEXT
ActualPr 1, 0
ActualPr 1, 1
EXIT SUB
rerrado:
    CLOSE #1
    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1: GOTO 5
RETURN
ruadro:
Alert 1, 3, 19, 12, 40, 15
textocf 30, 11, 7, 4, " RECUPERAR ARTICULO "
textocf 22, 12, 1, 3, " CODIGO "
textocf 37, 12, 1, 3, " DESCRIPCION "
textocf 20, 13, 7, 0, Invent.id
textocf 34, 13, 7, 0, Invent.produ
textocf 27, 14, 8, 3, "¨Continuar?"
textocf 45, 14, 6, 3, "(S/N)"
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27):
        textocf 45, 14, 15, 3, "(N)  "
        SLEEP 1
        EXIT SUB
    END SELECT
LOOP
textocf 45, 14, 15, 3, "  (S)": SLEEP 1
textocf 27, 14, 16, 3, "Recuperando Registro...   "
textocf 20, 13, 0, 7, Invent.id
textocf 34, 13, 0, 7, Invent.produ
RETURN


END SUB

SUB ShowInvents
LARGO = 11
    OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #4:
    CALL Invents(PetShow.id)
    DO
    kbd$ = INKEY$
        SELECT CASE kbd$
        CASE IS = CHR$(0) + ";":
            CALL LOGIN(0): CLOSE #4:
            IF EnGtCla% = 1 THEN
                EnGtCla% = 0: Pet.Clave = Iniciales$
                PetShow.Clave = Iniciales$: l = 8: kbd$ = ""
                CALL PABACK: SHOWPA:
                OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #4:
                CALL Invents(PetShow.id): CLOSE #4:
                textocf 2, 25, 0, 3, "ESC: Salir   ³" + STRING$(60, 32)
                CALL EditHead: l = 6
                jl = 1: EXIT SUB
            END IF
                jl = 2: EXIT SUB
            CASE IS = CHR$(0) + ">": CLOSE #4:
                rgero$ = TRIM$(PetShow.id): listPa 1, 0: salir% = 0
                IF REGNOMBRE$ = "" AND REGNUMERO& = 0 OR REGNOMBRE$ = "0" THEN REGNOMBRE$ = rgero$
                IF PetShow.id = "             " THEN
                    Alert 1, 3, 26, 5, 26, 7
                    textocf 30, 4, 7, 4, "Error."
                    textocf 28, 6, 0, 3, "Registro Invalido..."
                    textocf 1, 25, 3, 3, STRING$(80, 179)
                    BEEP: SLEEP 1: REGNOMBRE$ = rgero$
                END IF
                jl = 1: EXIT SUB
            CASE IS = CHR$(0) + "=": CLOSE #4:
                CALL LOGIN(0)
                IF EnGtCla% = 1 THEN EnGtCla% = 0: CALL QuickInvent
                jl = 2: EXIT SUB
           CASE IS = CHR$(0) + "H": CLOSE #4:
                OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #4:
                CALL Invents(PetShow.id)
            CASE IS = CHR$(0) + "P"
                CALL Invents(PetShow.id)
            CASE IS = CHR$(0) + "K":
                l = 5: CLOSE #4: SHOWPA: EXIT SUB
            CASE IS = CHR$(0) + "M":
                l = 7: CLOSE #4: SHOWPA: EXIT SUB
            CASE ELSE
        END SELECT
    LOOP
EXIT SUB

InventFON:
textocf 1, 10, 3, 3, STRING$(80, " ")
textocf 3, 10, 14, 3, "FECHA"
textocf 12, 10, 14, 3, "OP."
textocf 18, 10, 14, 3, "CODIGO"
textocf 32, 10, 14, 3, "CONCEPTO"
textocf 48, 10, 14, 3, "CANT."
textocf 58, 10, 14, 3, "MONTO"
FOR y% = 11 TO 24
    IF y% = 11 THEN
        S% = 14
    END IF
    textocf 1, y%, 7, 7, STRING$(80, " ")
    textocf 11, y%, 1, 7, STRING$(1, 179)
    textocf 15, y%, 1, 7, STRING$(1, 179)
    textocf 25, y%, 1, 7, STRING$(1, 179)
    textocf 51, y%, 1, 7, STRING$(1, 179)
    textocf 47, y%, 1, 7, STRING$(1, 179)
NEXT
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "F3: Cargar   ³ F4: Listado"
RETURN


END SUB

SUB SHOWPA
100
ShowHead
IF mttl& > 0 THEN
    textocf 53, 8, 12, 0, " Saldo Deudor:   "
LOCATE 8, 69: COLOR 12, 0: PRINT USING AS$; mttl&;
END IF
IF mttl& < 0 THEN
    textocf 53, 8, 3, 0, " Saldo a Favor:   "
LOCATE 8, 69: COLOR 3, 0: PRINT USING AS$; mttl&;
END IF
SELECT CASE l
CASE IS = 1
    A = 7: aa = 0: B = 0: bb = 7: c = 0: cc = 7: D = 0: DD = 7: E = 0: EE = 7: f = 0: FF = 7: G = 0: GG = 7
    GOSUB Ebarra
    textocf 2, 25, 0, 3, "F1: Edit. Datos  ³ F2: Edit. Valores  ³ F4: Listado  ³ F8: Anexar"
FOR T = 10 TO 24: textocf 1, T, 14, 1, SPACE$(80): NEXT
    textocf 1, 10, 3, 3, SPACE$(80): textocf 3, 10, 1, 3, "FECHA DEL EXAMEN:"
    CALL ShowBlood
    FOR y% = 11 TO 24: textocf 35, y%, 7, 1, STRING$(1, 179): NEXT
CASE IS = 2
    A = 0: aa = 7: B = 7: bb = 0: c = 0: cc = 7: D = 0: DD = 7: E = 0: EE = 7: f = 0: FF = 7: G = 0: GG = 7
    GOSUB Ebarra
    textocf 2, 25, 0, 3, "F1: Edit. Datos  ³ F2: Edit. Valores  ³ F4: Listado  ³ F8: Anexar"
FOR T = 10 TO 24: textocf 1, T, 14, 1, SPACE$(80): NEXT
        textocf 1, 10, 3, 3, SPACE$(80): textocf 3, 10, 1, 3, "FECHA DEL EXAMEN:"
FOR y% = 11 TO 24: textocf 35, y%, 7, 1, STRING$(1, 179): NEXT
    CALL ShowOrina
CASE IS = 3
    A = 0: aa = 7: B = 0: bb = 7: c = 7: cc = 0: D = 0: DD = 7: E = 0: EE = 7: f = 0: FF = 7: G = 0: GG = 7
    GOSUB Ebarra
    textocf 2, 25, 0, 3, "F1: Edit. Datos  ³ F2: Edit. Valores  ³ F4: Listado  ³ F8: Anexar"
FOR T = 10 TO 24: textocf 1, T, 14, 1, SPACE$(80): NEXT
        textocf 1, 10, 3, 3, SPACE$(80): textocf 3, 10, 1, 3, "FECHA DEL EXAMEN:"
FOR y% = 11 TO 24: textocf 35, y%, 7, 1, STRING$(1, 179): NEXT
    CALL ShowHeces
CASE IS = 4
    A = 0: aa = 7: B = 0: bb = 7: c = 0: cc = 7: D = 7: DD = 0: E = 0: EE = 7: f = 0: FF = 7: G = 0: GG = 7
    GOSUB Ebarra
FOR T = 11 TO 24: textocf 1, T, 14, 1, SPACE$(80): NEXT
    CALL ShowVacunas:
    FOR S = 11 TO 23: textocf 79, S, 14, 1, " ": NEXT
CASE IS = 5
    A = 0: aa = 7: B = 0: bb = 7: c = 0: cc = 7: D = 0: DD = 7: E = 7: EE = 0: f = 0: FF = 7: G = 0: GG = 7
    GOSUB Ebarra
    textocf 2, 25, 0, 3, "F1: Edit. Datos  ³ F2: Edit. Historia ³ F4: Listado  ³ F8: Imprimir"
FOR T = 10 TO 24: textocf 1, T, 14, 1, SPACE$(80): NEXT
CALL ShowNotas
CASE IS = 6, 8
    A = 0: aa = 7: B = 0: bb = 7: c = 0: cc = 7: D = 0: DD = 7: E = 0: EE = 7: f = 7: FF = 0: G = 0: GG = 7
    GOSUB Ebarra
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "F1: Editar Datos  ³ F3: Cargar   ³ F4: Listado"
IF l < 8 THEN ShowInvents
IF salir% = 1 THEN EXIT SUB
IF jl = 1 THEN
    jl = 0: CALL PABACK: Espere 0: CALL comppa:
    IF PetShow.id = "             " OR REGNUMERO& < 1 THEN GOSUB rmalo: REGNOMBRE$ = rgero$: REGNUMERO& = 1: mttl& = 0: comppa
    CALL PABACK: GOTO 100
END IF
IF jl = 2 THEN
    jl = 0: CALL PABACK: GOTO 100
END IF
CASE IS = 7
    A = 0: aa = 7: B = 0: bb = 7: c = 0: cc = 7: D = 0: DD = 7: E = 0: EE = 7: f = 0: FF = 7: G = 15: GG = 4
    GOSUB Ebarra
    FOR T = 10 TO 24: textocf 1, T, 14, 1, SPACE$(80): NEXT
    textocf 1, 25, 3, 3, STRING$(80, " ")
    textocf 2, 25, 0, 3, "F1: Editar Datos  ³ ENTER: Salir   ³ F4: Listado"
END SELECT
EXIT SUB

Ebarra:
textocf 2, 9, A, aa, " HEMATOLOGIA "
textocf 17, 9, B, bb, " ORINA "
textocf 26, 9, c, cc, " HECES "
textocf 35, 9, D, DD, " VACUNAS "
textocf 46, 9, E, EE, " HISTORIA "
textocf 58, 9, f, FF, " CARGOS "
textocf 73, 9, G, GG, " SALIR "
textocf 2, 25, 0, 3, "F1: Edit. Datos  ³ F2: Edit. Valores  ³ F4: Listado               "
RETURN
rmalo:
    PABACK
    textocf 32, 2, 14, 3, "MOSTRAR PACIENTE"
    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1: CLOSE #1
RETURN

END SUB

SUB ShowVacunas
    FOR y% = 11 TO 24
        textocf 24, y%, 7, 1, STRING$(1, 179)
        textocf 33, y%, 7, 1, STRING$(1, 179)
        textocf 50, y%, 7, 1, STRING$(1, 179)
        textocf 67, y%, 7, 1, STRING$(1, 179)
    NEXT
    textocf 1, 10, 3, 3, STRING$(80, " "):
    textocf 26, 10, 1, 3, "DOSIS:"
    textocf 37, 10, 1, 3, "APLICACION:"
    textocf 53, 10, 1, 3, "VENCIMIENTO:"
    textocf 70, 10, 1, 3, "ESTADO:"
    textocf 38, 11, 7, 1, (TRIM$(PetShow.examen(43)))
    textocf 55, 11, 7, 1, (TRIM$(PetShow.examen(44)))
    kbd$ = SHOWVAC$(PetShow.examen(44), 70, 11)
    textocf 2, 11, 14, 1, "PARVOVIRUS:"
    textocf 25, 11, 7, 1, PetShow.examen(63)

    textocf 38, 12, 7, 1, (TRIM$(PetShow.examen(45)))
    textocf 55, 12, 7, 1, (TRIM$(PetShow.examen(46)))
    kbd$ = SHOWVAC$(PetShow.examen(46), 70, 12)
    textocf 2, 12, 14, 1, "HEPATITIS:"
    textocf 25, 12, 7, 1, PetShow.examen(64)

    textocf 38, 13, 7, 1, (TRIM$(PetShow.examen(47)))
    textocf 55, 13, 7, 1, (TRIM$(PetShow.examen(48)))
    kbd$ = SHOWVAC$(PetShow.examen(48), 70, 13)
    textocf 2, 13, 14, 1, "LEPTOSPIROSIS:"
    textocf 25, 13, 7, 1, PetShow.examen(65)

    textocf 38, 14, 7, 1, (TRIM$(PetShow.examen(49)))
    textocf 55, 14, 7, 1, (TRIM$(PetShow.examen(50)))
    kbd$ = SHOWVAC$(PetShow.examen(50), 70, 14)
    textocf 2, 14, 14, 1, "DISTEMPER:"
    textocf 25, 14, 7, 1, PetShow.examen(66)

    textocf 38, 15, 7, 1, (TRIM$(PetShow.examen(51)))
    textocf 55, 15, 7, 1, (TRIM$(PetShow.examen(52)))
    kbd$ = SHOWVAC$(PetShow.examen(52), 70, 15)
    textocf 2, 15, 14, 1, "PARAINFLUENZA:"
    textocf 25, 15, 7, 1, PetShow.examen(67)

    textocf 38, 16, 7, 1, (TRIM$(PetShow.examen(53)))
    textocf 55, 16, 7, 1, (TRIM$(PetShow.examen(54)))
    kbd$ = SHOWVAC$(PetShow.examen(54), 70, 16)
    textocf 2, 16, 14, 1, "CORONAVIRUS:"
    textocf 25, 16, 7, 1, PetShow.examen(68)

    textocf 38, 17, 7, 1, (TRIM$(PetShow.examen(55)))
    textocf 55, 17, 7, 1, (TRIM$(PetShow.examen(56)))
    kbd$ = SHOWVAC$(PetShow.examen(56), 70, 17)
    textocf 2, 17, 14, 1, "CALICIVIRUS:"
    textocf 25, 17, 7, 1, PetShow.examen(69)

    textocf 38, 18, 7, 1, (TRIM$(PetShow.examen(57)))
    textocf 55, 18, 7, 1, (TRIM$(PetShow.examen(58)))
    kbd$ = SHOWVAC$(PetShow.examen(58), 70, 18)
    textocf 2, 18, 14, 1, "PANEUCOPEMIA FELINA:"
    textocf 25, 18, 7, 1, PetShow.examen(70)

    textocf 38, 19, 7, 1, (TRIM$(PetShow.examen(59)))
    textocf 55, 19, 7, 1, (TRIM$(PetShow.examen(60)))
    kbd$ = SHOWVAC$(PetShow.examen(60), 70, 19)
    textocf 2, 19, 14, 1, "RINOTRAQUEITIS:"
    textocf 25, 19, 7, 1, PetShow.examen(71)

    textocf 38, 20, 7, 1, (TRIM$(PetShow.examen(61)))
    textocf 55, 20, 7, 1, (TRIM$(PetShow.examen(62)))
    kbd$ = SHOWVAC$(PetShow.examen(62), 70, 20)
    textocf 2, 20, 14, 1, "RABIA:"
    textocf 25, 20, 7, 1, PetShow.examen(72)
END SUB

