'BIENVENIDO A MI HUMILDE LUGAR FELIZ.
'UN AMIGO ME DIJO QUE PROGRAMAR EN HTML ERA
'FACIL, BUSQUE INFORMACION Y ME CONSEGUI UN
'TUTORIAL DE HTML Y OTRO DE BASIC,
'CON UNA 386 HICE ESTE CODIGO SIN SABER
'MUCHO DE PROGRAMAR, BUSQUE EN LOS INICIOS DE
'INTERNET, GUARDABA EN FLOPPYS MUCHA INFORMACION
'Y TRATE DE TEJER ESTE PROGRAMA QUE LLEGUE
'A VENDER A 4 VETERINARIAS. ESO FUE EN EL 2002
'24 ANOS DESPUES ME SORPRENDE LO QUE HICE,
'NO ES PERFECTO PERO FUNCIONAL Y VISUALMENTE
'INTERESANTE.

DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE SUB Newlista (yy%, recupera%)
DECLARE SUB SAVECLINICA ()
DECLARE SUB CONFIGURAR ()
DECLARE SUB LOGIN (ZZ%)
DECLARE SUB EliminarPa ()
DECLARE SUB EliminarPr ()
DECLARE SUB Lineas ()
DECLARE SUB MENU2 ()
DECLARE SUB MENUSELECT ()
DECLARE SUB SHOWAYUDA ()
DECLARE SUB SPLASH ()
DECLARE SUB RecuperaPa ()
DECLARE SUB RecuperaPr ()
DECLARE SUB NOHAY ()
DECLARE SUB MENUSPLASH (hh%)
DECLARE SUB ActualPa (yy%, FILE%)
DECLARE SUB ActualPr (yy%, FILE%)
DECLARE SUB IndexPr ()
DECLARE SUB IndexPa ()
DECLARE SUB RegPa ()
DECLARE SUB RESPALDO ()
DECLARE SUB Pac ()
DECLARE SUB PABACK ()
DECLARE SUB listPa (yy%, recupera%)
DECLARE SUB ListPr (yy%, limite%, recupera%)
DECLARE SUB Pro ()
DECLARE SUB PrBack ()
DECLARE SUB RegPr ()
DECLARE SUB Checkout ()
DECLARE SUB Gastos ()
DECLARE SUB ListFa ()
DECLARE SUB ShowFac ()
DECLARE FUNCTION LLENARCE$ (x%, y%, letras%, Fondo%, START$, END$, vis%, MAX%)
DECLARE FUNCTION SHOWVAC$ (who$, x%, y%)
REM $DINAMIC
'$INCLUDE: 'C:\JLV_CODE\amv\AMV1.BI'
DIM SHARED Eleccion%, SubEleccion%, MnLim%, SMLIM%, S%
DIM SHARED A%(6), B%(6)
DIM SHARED SA%(6), SB%(6)
linea$ = COMMAND$
IF UCASE$(linea$) = "XYZ" OR UCASE$(linea$) = "RWLX" THEN CALL CONFIGURAR
CLEAR
ON ERROR GOTO ErrorTrap
RESPALDO
CLEAR : LOCATE , , 0
ON ERROR GOTO ErrorTrap
DEFINT A-Z
GOSUB dibujo: CALL SPLASH
DO: LOOP WHILE INKEY$ = ""
VIEW PRINT 1 TO 25: WIDTH 80
UnScroll 2, 1, 25, 79
SCREEN 0: LOCATE , , 0
224
'$INCLUDE: 'C:\JLV_CODE\amv\AMV2.BI'
ON ERROR GOTO ErrorTrap
VIEW PRINT 1 TO 25: WIDTH 80
AS$ = "###,###,###": Eleccion = 1: CALL Lineas
textocf 14, 25, 7, 3, CHR$(200 + 19) + CHR$(120 + 99) + CHR$(156 + 22) + CHR$(111 + 66) + CHR$(171 + 5) + "                                           " + CHR$(161 + 15) + CHR$(101 + 76) + CHR$(154 + 24) + CHR$(110 + 109) + CHR$(201 + 18)
kjh$ = CHR$(63 + 2) + CHR$(35 + 11) + CHR$(61 + 16)
kjh$ = kjh$ + CHR$(36 + 10) + CHR$(70 + 16) + CHR$(33 + 13) + CHR$(22 + 10)
kjh$ = kjh$ + CHR$(65 - 15) + CHR$(25 + 21) + CHR$(65 - 16) + CHR$(45 + 3)
kjh$ = kjh$ + CHR$(20 + 12) + CHR$(46 - 1) + CHR$(1 + 31) + CHR$(65 + 9)
kjh$ = kjh$ + CHR$(65 + 20) + CHR$(64 + 1) + CHR$(65 + 13) + CHR$(33 - 1)
kjh$ = kjh$ + CHR$(65 + 11) + CHR$(15 + 70) + CHR$(73 - 0) + CHR$(65 + 18)
kjh$ = kjh$ + CHR$(30 + 2) + CHR$(46 + 40) + CHR$(10 + 63) + CHR$(75 + 1)
kjh$ = kjh$ + CHR$(65 + 11) + CHR$(25 + 40) + CHR$(35 + 32) + CHR$(10 + 69)
kjh$ = kjh$ + CHR$(11 + 71) + CHR$(86 - 2) + CHR$(63 + 2) + CHR$(36 - 4)
kjh$ = kjh$ + CHR$(25 + 20) + CHR$(65 - 33) + CHR$(65 - 15) + CHR$(47 + 1)
kjh$ = kjh$ + CHR$(40 + 8) + CHR$(15 + 35)
textocf 20, 25, 8, 3, kjh$
textocf 2, 25, 0, 3, "F1: Ayuda": textocf 70, 25, 0, 3, "F10: Salir"
FOR I = 1 TO 3: textocf ITEMX%(I), 2, 0, 3, ITEM$(I)
textocf ITEMX%(I) + 1, 2, 15, 3, letra$(I): NEXT
h = 0
ON TIMER(1) GOSUB RELOj
CALL MENUSPLASH(h)
IF h = 1 THEN GOTO 4
IF h = 2 THEN CLEAR : END
ON TIMER(1) GOSUB RELOj
ON ERROR GOTO ErrorTrap
CALL MENU2
4
CLEAR : GOSUB dibujo: GOTO 224

dibujo:
    OPEN "AMVSYS\AMV21.SYS" FOR RANDOM ACCESS READ AS #1 LEN = LEN(clinica)
    GET #1, 1, clinica: CLOSE #1:
    propie$ = "Z7O79;OPK^\W9]\U9OXUU\;59Z7X7"
FOR B = 1 TO LEN(propie$)
        n% = ASC(RIGHT$(propie$, B)) XOR 25
        IF n% < 32 THEN n% = 0
        f$ = CHR$(n%) + f$
NEXT
propie$ = TRIM$(f$)
prop$ = clinica.nombre
FOR I = 1 TO LEN(prop$)
A$ = RIGHT$(LEFT$(prop$, I), 1)
IF A$ = CHR$(0) THEN MID$(prop$, I, LEN(prop$)) = CHR$(32): EXIT FOR
NEXT
    nombre$ = TRIM$(propie$)
    DECC TRIM$(clinica.direcc), direcc$: DECC TRIM$(clinica.telefonos), telefonos$
    DECC TRIM$(clinica.rif), rif$: DECC TRIM$(clinica.nit), nit$: DECC TRIM$(clinica.email), email$
    cha$ = SPACE$(35 - LEN(Daylet$ + MFECHA$ + " ")) + RIGHT$((Daylet$ + " " + MFECHA$), 35)

RETURN

report:
OPEN "AMVSYS\AMV21ERR.LOG" FOR APPEND AS #33
PRINT #33, DATE$; "/"; TIME$; "<"; ERR; ">"; Iniciales$; ">": CLOSE #33
RETURN

RELOj:
    mttl& = mttl& + 1
RETURN

ErrorTrap:
   SELECT CASE ERR
        CASE 27
            Alert 1, 3, 26, 5, 26, 7: textocf 28, 6, 20, 3, "IMPRESORA SIN PAPEL"
            BEEP: textocf 2, 24, 3, 3, STRING$(80, " ")
            LOCATE 25, 2: COLOR 0, 3: PRINT "IMPRESORA NO RESPONDE... PRESIONE 'ENTER' PARA CONTINUAR...";
            WHILE INKEY$ <> CHR$(13): WEND
            RESUME NEXT
        CASE 24, 25, 71, 57, 68, 73, 51
            Alert 1, 3, 26, 5, 26, 7: textocf 28, 6, 20, 3, "ERROR EN IMPRESORA..."
            BEEP: textocf 2, 24, 3, 3, STRING$(80, " ")
            LOCATE 25, 2: COLOR 0, 3: PRINT "IMPRESORA NO RESPONDE... PRESIONE 'ENTER' PARA CONTINUAR...";
            WHILE INKEY$ <> CHR$(13): WEND
            RESUME NEXT
        CASE ELSE
   Alert 1, 3, 26, 5, 26, 7: textocf 28, 6, 20, 3, "#" + STR$(ERR)
            BEEP: SLEEP 2: GOSUB report: CLS : LOCATE , , 1: COLOR 7, 0: CLS : END
    END SELECT
RESUME NEXT

REM $DYNAMIC
SUB CONFIGURAR
OPEN "AMVSYS\AMV21.SYS" FOR APPEND AS #1: CLOSE #1
IF COMMAND$ = "RWLX" THEN GOSUB rwlx
489
COLOR 0, 7: CLS :
LOCATE , , 0
FOR I = 4 TO 16: LOCATE I, 10: COLOR 1, 1: PRINT STRING$(60, 32); : NEXT
FOR I = 4 TO 16: LOCATE I, 37: COLOR 15, 1: PRINT STRING$(1, 179); : NEXT
BOX 15, 1, 10, 4, 60, 16, 0
kjh$ = CHR$(63 + 2) + CHR$(35 + 11) + CHR$(61 + 16)
kjh$ = kjh$ + CHR$(36 + 10) + CHR$(70 + 16) + CHR$(33 + 13)
COLOR 11, 1: LOCATE 4, 28: PRINT " "; kjh$; " CONFIGURACION "
COLOR 12, 1: LOCATE 16, 57: PRINT " ESC: SaLiR "
cont = 1
490
COLOR 0, 1: LOCATE 14, 15: PRINT "                      ";
A = 14: aa = 1: B = 14: bb = 1: c = 14: cc = 1: D = 14: DD = 1
SELECT CASE cont
CASE IS = 1: A = 1: aa = 3
        textocf 40, 6, 7, 1, " REGISTRA CLAVES DE LOS "
        textocf 40, 8, 7, 1, "OPERADORES, DE FORMA    "
        textocf 40, 10, 7, 1, "INDIVIDUAL              "
        textocf 40, 12, 7, 1, "                        "
        textocf 40, 14, 7, 1, "                        "
CASE IS = 2: B = 1: bb = 3
        textocf 40, 6, 7, 1, " ELIMINA LAS CLAVES DE  "
        textocf 40, 8, 7, 1, "LOS OPERADORES, DE FORMA"
        textocf 40, 10, 7, 1, "INDIVIDUAL              "
        textocf 40, 12, 7, 1, "                        "
        textocf 40, 14, 7, 1, "                        "
CASE IS = 3: c = 1: cc = 3
        textocf 40, 6, 7, 1, "CAMBIAR LOS NOMBRES DE  "
        textocf 40, 8, 7, 1, "ARTICULOS (DEL 1 AL 99) "
        textocf 40, 10, 7, 1, "                       "
        textocf 40, 12, 7, 1, "                        "
        textocf 40, 14, 7, 1, "                        "
CASE IS = 4: D = 1: DD = 3
        textocf 40, 6, 7, 1, "INGRESAR O CAMBIAR LOS  "
        textocf 40, 8, 7, 1, "DATOS DE LA EMPRESA     "
        textocf 40, 10, 7, 1, "                        "
        textocf 40, 12, 7, 1, "                        "
        textocf 40, 14, 7, 1, "                        "

END SELECT
COLOR A, aa: LOCATE 6, 12: PRINT "1.- AGREGAR CLAVE   "
COLOR B, bb: LOCATE 8, 12: PRINT "2.- ELIMINAR CLAVE  "
COLOR c, cc: LOCATE 10, 12: PRINT "3.- EDITAR CUENTAS  "
COLOR D, DD: LOCATE 12, 12: PRINT "4.- DATOS EMPRESA   "
DO
PRESS$ = INKEY$
SELECT CASE PRESS$
    CASE IS = CHR$(0) + "P"
        cont = cont + 1
        IF cont > 4 THEN cont = 1
        GOTO 490
    CASE IS = CHR$(0) + "H"
        cont = cont - 1
        IF cont < 1 THEN cont = 4:
        GOTO 490
    CASE IS = CHR$(27): CLOSE #1: EXIT SUB
    CASE IS = CHR$(13):
            IF cont = 4 THEN
            CALL SAVECLINICA: GOTO 489
            END IF
            IF cont = 1 THEN
                COLOR 15, 1: LOCATE 14, 15: PRINT "NUEVA: "
                COLOR 7, 1: LOCATE 15, 15: PRINT "INGRESE INICIALES... "
124
                KBD$ = LLENARCE$(22, 14, 4, 7, "", OLDCL$, 2, 2)
                COLOR 7, 1: LOCATE 15, 15: PRINT "INGRESE CLAVE SECRETA"
                KBD$ = LLENARCE$(25, 14, 0, 7, "", OLDCLAVE$, 8, 8)
                cl.clax = OLDCL$ + OLDCLAVE$
                IF RTRIM$(cl.clax) = "" THEN GOTO 489
                OPEN "AMVSYS\AMV21.SYS" FOR RANDOM ACCESS WRITE AS #1 LEN = LEN(cl)
                z& = (LOF(1) / LEN(cl)) + 1
                PUT #1, z&, cl: CLOSE #1: DECC TRIM$(cl.clax), nc$
                COLOR 1, 1: LOCATE 14, 21: PRINT "            ";
                COLOR 7, 1: LOCATE 15, 15: PRINT "                     "

                COLOR 14, 7: LOCATE 14, 15: PRINT "LISTO: "; nc$
                SLEEP 1: GOTO 489
            END IF
            IF cont = 2 THEN
                COLOR 15, 1: LOCATE 14, 15: PRINT "CLAVE:"
                COLOR 7, 1: LOCATE 15, 15: PRINT "INGRESE INICIALES... "
                KBD$ = LLENARCF(22, 14, 4, 7, "", OLDCL$, 2, 2)
                COLOR 7, 1: LOCATE 15, 15: PRINT "INGRESE CLAVE SECRETA"
                KBD$ = LLENARCF(25, 14, 0, 7, "", OLDCLAVE$, 8, 8)
                OLDCLAVE$ = OLDCL$ + OLDCLAVE$
                IF RTRIM$(OLDCLAVE$) = "" THEN GOTO 489
                IF RTRIM$(OLDCLAVE$) <> "" THEN
                        FOR I = 1 TO LEN(OLDCLAVE$)
                                OLD$ = CHR$(ASC(RIGHT$(OLDCLAVE$, I)) * 2) + OLD$
                                COLOR 14, 7: LOCATE 14, 15: PRINT "ELIMINANDO...     "
                                COLOR 7, 1: LOCATE 15, 15: PRINT "                     "
                        NEXT
                        OPEN "AMVSYS\AMV21.SYS" FOR RANDOM ACCESS READ AS #1 LEN = LEN(cl)
                        FOR I& = 1 TO (LOF(1) / LEN(cl))
                                GET #1, I&, cl
                                IF OLD$ <> TRIM$(cl.clax) THEN
                                        OPEN "AMVSYS\AMV.TMP" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(cl)
                                        PUT #2, I&, cl: CLOSE #2
                                END IF
                        NEXT
                        CLOSE #1: KILL "AMVSYS\AMV21.SYS"
                        NAME "AMVSYS\AMV.TMP" AS "AMVSYS\AMV21.SYS": GOTO 489
                END IF
            END IF
            IF cont = 3 THEN
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM AS #5 LEN = LEN(Invent)
2
COLOR 0, 1: LOCATE 10, 22: PRINT "                                        €€"
COLOR 0, 3: LOCATE 5, 21: PRINT "⁄ C¢digo ¬ƒƒƒƒƒƒƒ Concepto ƒƒƒƒƒƒƒƒƒƒƒƒƒø";
COLOR 0, 3: LOCATE 6, 21: PRINT "≥        ≥                              ≥€€";
COLOR 0, 3: LOCATE 7, 21: PRINT "√ƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥€€";
COLOR 0, 3: LOCATE 8, 21: PRINT "≥ ENTER: Aceptar         ≥  ESC: Salir  ≥€€";
COLOR 0, 3: LOCATE 9, 21: PRINT "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ€€";
1
A = 0: BOX 0, 3, 21, 10, 40, 22, 0
2445
FOR I = 1 TO 11
        IF A + I > 99 THEN EXIT FOR
        GET #5, A + I, InventShow
        IF VAL(LEFT$(InventShow.id, 5)) > 0 THEN
            LOCATE 10 + I, 22: COLOR 0, 3: PRINT InventShow.id; CHR$(179); InventShow.produ;
        END IF
NEXT
2556
    DO
    PRESS$ = INKEY$
        SELECT CASE PRESS$
         CASE IS = CHR$(13): GOSUB awaps: GOTO 1
         CASE IS = CHR$(27): CLOSE #5: GOTO 489
         CASE IS = CHR$(0) + "H"
                IF A + I < 1 THEN GOTO 2556
                A = A - 11
                IF A < 1 THEN A = 0
                GOTO 2445
         CASE IS = CHR$(0) + "P"
                IF A + I >= 99 THEN GOTO 2556
                A = A + 11: GOTO 2445
         END SELECT
    LOOP
END IF
END SELECT
LOOP

awaps:
        KBD$ = LLENARCF$(22, 6, 0, 7, START$, aqui$, 8, 8)
        REGNOMBRE$ = TRIM$(aqui$): START$ = ""
    IF KBD$ = CHR$(27) THEN CLOSE #5: GOTO 489
    IF REGNOMBRE$ = "" THEN GOTO 1
    LOCATE 6, 22: COLOR 0, 3: PRINT "        ";
    LOCATE 6, 22: COLOR 0, 7: PRINT LTRIM$(REGNOMBRE$);
    FOR f& = 1 TO 99
        GET #5, f&, InventShow:
        IF REGNOMBRE$ = TRIM$(InventShow.id) THEN
            START$ = TRIM$(InventShow.produ)
            KBD$ = LLENARCF$(31, 6, 0, 7, START$, aqui$, 20, 20)
            InventShow.produ = TRIM$(UCASE$(aqui$)): START$ = ""
            PUT #5, f&, InventShow: GOTO 2
        END IF
    NEXT
LOCATE 6, 22: COLOR 20, 7: PRINT "-ERROR- ";
SLEEP 1: LOCATE 6, 22: COLOR 0, 3: PRINT "        "; : GOTO 1
RETURN
rwlx:
COLOR 1, 7: CLS :
LOCATE , , 0
FOR T = 4 TO 16: COLOR 1, 1: LOCATE T, 10: PRINT STRING$(60, 32); : NEXT
BOX 15, 1, 10, 4, 60, 16, 0
textocf 12, 8, 7, 1, "  OPERADORES PARA PODER ACCEDER AL SISTEMA A.M.V., ASI"
kjh$ = CHR$(63 + 2) + CHR$(35 + 11) + CHR$(61 + 16)
kjh$ = kjh$ + CHR$(36 + 10) + CHR$(70 + 16) + CHR$(33 + 13)
textocf 12, 12, 7, 1, "  LA EMPRESA (RIF, NIT, TELEFONO, ETC)."
COLOR 12, 1: LOCATE 15, 18: PRINT "< Presione Cualquier Tecla para Continuar... >"
textocf 12, 6, 7, 1, "    A CONTINUACION, DEBERA REGISTRAR LAS CLAVES DE LOS"
textocf 12, 10, 7, 1, "  COMO TAMBIEN, LAS ESPECIFICACIONES DE LOS DATOS DE"
COLOR 15, 1: LOCATE 4, 28: PRINT " BIENVENIDO A: "; : COLOR 11, 1: PRINT kjh$; " ";
DO: LOOP WHILE INKEY$ = ""
RETURN
END SUB

SUB Lineas
LOCATE 1, 1: COLOR 7, 7: PRINT STRING$(80, " ");
LOCATE 1, 2: COLOR 1, 7: PRINT TRIM$(nombre$);
LOCATE 1, 45: COLOR 1, 7: PRINT cha$;
LOCATE 2, 1: COLOR 3, 3: PRINT STRING$(80, " ");
LOCATE 25, 1: COLOR 3, 3: PRINT STRING$(80, " ");
FOR y = 3 TO 24: COLOR 1, 1: LOCATE y, 1: PRINT STRING$(80, " "): NEXT


END SUB

SUB LOGIN (ZZ%)
Y1 = 6
IF ZZ = 1 THEN Y1 = 10
Y2 = Y1 + 3: EnGtCla% = 0: ZZ = 0
Alert 1, 3, 28, Y1, 23, Y2
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 39, Y1 - 1, 7, 4, "Contrase§a   "
textocf 28, Y1 - 1, 15, 0, " - "
textocf 31, Y1, 1, 3, " Ingrese su Clave "
textocf 30, Y1 + 1, 14, 3, "Iniciales: "
textocf 30, Y1 + 2, 14, 3, "    Clave: "
textocf 41, Y1 + 1, 7, 3, "€€"
textocf 41, Y1 + 2, 7, 3, "€€€€€€€€"
textocf 2, 25, 0, 3, "TECLEE SUS INICIALES...    "
KBD$ = LLENARCF$(41, Y1 + 1, 0, 2, "", aquicf$, 2, 2)
IF KBD$ = CHR$(27) THEN EXIT SUB
Clave$ = TRIM$(UCASE$(aquicf$))
textocf 2, 25, 0, 3, "TECLEE SU CODIGO SECRETO..."
KBD$ = Password$(41, Y1 + 2, 0, 2, "", aquicf$, 8, 8)
IF KBD$ = CHR$(27) THEN EXIT SUB
Clave$ = Clave$ + TRIM$(UCASE$(aquicf$))
IF TRIM$(Clave$) = "" THEN GOSUB no
OPEN "AMVSYS\AMV21.SYS" FOR RANDOM ACCESS READ AS #10 LEN = LEN(cl)
FOR I& = 20 TO (LOF(10) / LEN(cl))
clavef$ = ""
    GET #10, I&, cl: DECC TRIM$(cl.clax), clavef$
        IF Clave$ = clavef$ THEN
            IF clavef$ = CHR$(74) + CHR$(76) + TRIM$(STR$(123456789)) THEN juan% = 1
            Iniciales$ = LEFT$(clavef$, 2)
            EnGtCla% = 1: CLOSE #10: EXIT SUB
        END IF

NEXT

no:
IF Clave$ <> clavef$ OR TRIM$(Clave$) = "" THEN
        Alert 1, 3, 28, Y1, 23, Y2
        textocf 31, Y1 + 1, 0, 3, " Clave Incorrecta"
        BEEP: CLOSE #10: SLEEP 1: EnGtCla% = 0: EXIT SUB
END IF
RETURN

END SUB

SUB MENU2
SubEleccion% = 1: Lineas
100
textocf 70, 25, 0, 3, "F10: Salir "
SELECT CASE Eleccion%
    CASE IS = 1:
        Whee% = 7:
        SMLIM% = 5: A%(1) = 7: A%(2) = 0: A%(3) = 0: B%(1) = 0: B%(2) = 3: B%(3) = 3
    CASE IS = 2:
        Whee% = 5
        SMLIM% = 4: A%(1) = 0: A%(2) = 7: A%(3) = 0: B%(1) = 3: B%(2) = 0: B%(3) = 3
    CASE IS = 3:
        Whee% = 7
        SMLIM% = 5: A%(1) = 0: A%(2) = 0: A%(3) = 7: B%(1) = 3: B%(2) = 3: B%(3) = 0
END SELECT
    FOR I = 1 TO 3:
        textocf ITEMX%(I), 2, A%(I), B%(I), RIGHT$(ITEM$(I), LEN(ITEM$(I))):
        textocf ITEMX%(I), 2, 15, B%(I), LEFT$(ITEM$(I), 2)
    NEXT
SELECT CASE SubEleccion%
    CASE IS = 1:
        hlp = 7: SA%(1) = 7: SA%(2) = 0: SA%(3) = 0: SA%(4) = 0: SA%(5) = 0:   SA%(6) = 0: SB%(1) = 0: SB%(2) = 3: SB%(3) = 3: SB%(4) = 3: SB%(5) = 3: SB%(6) = 3
    CASE IS = 2:
        hlp = 8: SA%(1) = 0: SA%(2) = 7: SA%(3) = 0: SA%(4) = 0: SA%(5) = 0:    SA%(6) = 0: SB%(1) = 3: SB%(2) = 0: SB%(3) = 3: SB%(4) = 3: SB%(5) = 3: SB%(6) = 3
    CASE IS = 3:
        hlp = 9: SA%(1) = 0: SA%(2) = 0: SA%(3) = 7: SA%(4) = 0: SA%(5) = 0:    SA%(6) = 0: SB%(1) = 3: SB%(2) = 3: SB%(3) = 0: SB%(4) = 3: SB%(5) = 3: SB%(6) = 3
    CASE IS = 4:
        hlp = 10: SA%(1) = 0: SA%(2) = 0: SA%(3) = 0: SA%(4) = 7: SA%(5) = 0:  SA%(6) = 0: SB%(1) = 3: SB%(2) = 3: SB%(3) = 3: SB%(4) = 0: SB%(5) = 3: SB%(6) = 3
    CASE IS = 5:
        hlp = 11: SA%(1) = 0: SA%(2) = 0: SA%(3) = 0: SA%(4) = 0: SA%(5) = 7:   SA%(6) = 0: SB%(1) = 3: SB%(2) = 3: SB%(3) = 3: SB%(4) = 3: SB%(5) = 0: SB%(6) = 3
    CASE IS = 6:
        hlp = 12: SA%(1) = 0: SA%(2) = 0: SA%(3) = 0: SA%(4) = 0: SA%(5) = 0:   SA%(6) = 7: SB%(1) = 3: SB%(2) = 3: SB%(3) = 3: SB%(4) = 3: SB%(5) = 3: SB%(6) = 0
    END SELECT
 MM% = 0
 IF Whee% > 0 THEN MM% = 1
       LOCATE 2, ITEMX%(Eleccion%): COLOR 7, 0: PRINT LEFT$(ITEM$(Eleccion%), 2);
       BOX 0, 3, ITEMX%(Eleccion%), 3, LEN(MNITEM$(1, Eleccion%)) + 1, MNITEMY%(SMLIM%) + 1 + (MM%), Whee%
       FOR I = 1 TO SMLIM%:
        IF I >= Whee% - 2 THEN SMM% = 1
        LOCATE MNITEMY%(I) + SMM%, ITEMX%(Eleccion%) + 1: COLOR SA%(I), SB%(I): PRINT RIGHT$(MNITEM$(I, Eleccion%), 11);
        LOCATE MNITEMY%(I) + SMM%, ITEMX%(Eleccion%) + 1: COLOR 15, SB%(I): PRINT LEFT$(MNITEM$(I, Eleccion%), 2);
       NEXT
       Whee% = 0: SMM% = 0
       textocf 2, 25, 0, 3, MNITEM$(hlp, Eleccion%)
DO
PRESS$ = INKEY$
SELECT CASE PRESS$
    CASE IS = CHR$(0) + ";": SHOWAYUDA: EXIT SUB
    CASE IS = CHR$(0) + "D": SPLASH: SCREENCLEAR: COLOR 7, 0: LOCATE 1, 1, 1: END
    CASE IS = CHR$(27): EXIT SUB
    CASE IS = "M", "m":
    IF Eleccion% = 1 THEN Pac: EXIT SUB
    IF Eleccion% = 3 THEN Pro: EXIT SUB
    CASE IS = "L", "l":
    IF Eleccion% = 1 THEN listPa 0, 0: salir% = 0: EXIT SUB
    IF Eleccion% = 3 THEN ListPr 0, 99, 0: EXIT SUB
    CASE IS = "R", "r":
    IF Eleccion% = 1 THEN
    CALL LOGIN(1)
        IF EnGtCla% = 1 THEN RegPa
        EXIT SUB
    END IF
    IF Eleccion% = 2 THEN
      ListFa
      EXIT SUB
    END IF
    IF Eleccion% = 3 THEN
    CALL LOGIN(1)
        IF EnGtCla% = 1 THEN RegPr
        EXIT SUB
    END IF
    CASE IS = "F", "f":
    IF Eleccion% = 2 THEN CALL LOGIN(1)
        IF EnGtCla% = 1 THEN
            Checkout
        END IF
        EXIT SUB
    IF Eleccion% <> 2 THEN Eleccion% = 2: GOTO 100
    CASE IS = "R", "r":
    IF Eleccion% = 1 THEN RecuperaPa: EXIT SUB
    IF Eleccion% = 3 THEN RecuperaPr: EXIT SUB
    CASE IS = "H", "h":
    IF Eleccion% = 2 THEN ShowFac: EXIT SUB
    CASE IS = "G", "g": IF Eleccion% = 2 THEN Gastos: EXIT SUB
    CASE IS = CHR$(13): CALL MENUSELECT: EXIT SUB
    CASE IS = "P", "p": IF Eleccion% <> 1 THEN Eleccion% = 1:  GOTO 100
    CASE IS = "E", "e":
    IF Eleccion% = 1 THEN
        CALL EliminarPa: EXIT SUB
    END IF
    IF Eleccion% = 3 THEN
        CALL EliminarPr: EXIT SUB
    END IF
    CASE IS = "I", "i":
    IF Eleccion% = 1 THEN
        CALL IndexPa: EXIT SUB
    END IF
    IF Eleccion% = 3 THEN
        CALL IndexPr: EXIT SUB
    END IF
    IF Eleccion% <> 3 THEN Eleccion% = 3:  GOTO 100
    CASE IS = CHR$(0) + "M": Eleccion% = Eleccion% + 1
        SubEleccion% = 1
        IF Eleccion% > 3 THEN Eleccion% = 1
        CALL Lineas: GOTO 100
    CASE IS = CHR$(0) + "K": Eleccion% = Eleccion% - 1
    SubEleccion% = 1
    IF Eleccion% = 0 THEN Eleccion% = 3
    CALL Lineas: GOTO 100
    CASE IS = CHR$(0) + "P": SubEleccion% = SubEleccion% + 1
    IF SubEleccion% > SMLIM% THEN SubEleccion% = 1
    GOTO 100
    CASE IS = CHR$(0) + "H": SubEleccion% = SubEleccion% - 1
    IF SubEleccion% < 1 THEN SubEleccion% = SMLIM%
    GOTO 100
    CASE IS = "":
    CASE ELSE: BEEP
END SELECT
LOOP
END SUB

SUB MENUSELECT
IF SubEleccion% = 1 AND Eleccion% = 1 THEN CALL Pac
IF SubEleccion% = 2 AND Eleccion% = 1 THEN listPa 0, 0: salir% = 0
IF SubEleccion% = 3 AND Eleccion% = 1 THEN CALL LOGIN(1): IF EnGtCla% = 1 THEN CALL RegPa
IF SubEleccion% = 4 AND Eleccion% = 1 THEN CALL IndexPa
IF SubEleccion% = 5 AND Eleccion% = 1 THEN CALL EliminarPa
IF SubEleccion% = 6 AND Eleccion% = 1 THEN CALL RecuperaPa

IF SubEleccion% = 1 AND Eleccion% = 2 THEN CALL Checkout
IF SubEleccion% = 2 AND Eleccion% = 2 THEN CALL LOGIN(1): IF EnGtCla% = 1 THEN CALL ShowFac
IF SubEleccion% = 3 AND Eleccion% = 2 THEN CALL ListFa
IF SubEleccion% = 4 AND Eleccion% = 2 THEN CALL Gastos

IF SubEleccion% = 1 AND Eleccion% = 3 THEN CALL Pro
IF SubEleccion% = 2 AND Eleccion% = 3 THEN CALL ListPr(0, 99, 0)
IF SubEleccion% = 3 AND Eleccion% = 3 THEN CALL LOGIN(1): IF EnGtCla% = 1 THEN CALL RegPr
IF SubEleccion% = 4 AND Eleccion% = 3 THEN CALL IndexPr
IF SubEleccion% = 5 AND Eleccion% = 3 THEN CALL EliminarPr
IF SubEleccion% = 6 AND Eleccion% = 3 THEN CALL RecuperaPr
END SUB

SUB MENUSPLASH (hh%)
TIMER ON
DO
    KBD$ = INKEY$
    SELECT CASE KBD$
    CASE IS = CHR$(13): Eleccion% = 1: EXIT DO
    CASE IS = CHR$(0) + "M": EXIT DO
    CASE IS = CHR$(0) + "K": EXIT DO
    CASE IS = CHR$(0) + "P": EXIT DO
    CASE IS = CHR$(0) + "H": EXIT DO
    CASE IS = CHR$(0) + ";":
    FOR I = 1 TO 3: textocf ITEMX%(I) + 1, 2, 0, 3, letra$(I): NEXT
    SHOWAYUDA
    hh% = 1: EXIT DO
    CASE IS = CHR$(0) + "D": SPLASH: SCREENCLEAR: COLOR 7, 0: LOCATE 1, 1, 1:
    hh% = 2: EXIT DO
    CASE IS = "P", "p": Eleccion% = 1: EXIT DO
    CASE IS = "F", "f": Eleccion% = 2: EXIT DO
    CASE IS = "I", "i": Eleccion% = 3: EXIT DO
    CASE ELSE
END SELECT
IF mttl& = 120 THEN
mttl& = 0: SCREENCLEAR: LOCATE , , 0: SPLASH:
WHILE INKEY$ = "": WEND: hh = 1: EXIT DO
END IF
LOOP
TIMER OFF
END SUB

SUB NOHAY
    Alert 1, 3, 26, 11, 26, 13
    textocf 30, 10, 7, 4, "Error."
    textocf 28, 12, 0, 3, "No Existen Registros..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1:
END SUB

SUB Question (u%, Fo$, B%)
DD = 1: B = 7: ba = 24: bb = 4: bc = 30: bd = 8: tax = 26: tay = 5: tcx = 33: tcy = 7:
IF u = 1 THEN
DD = 7
B = 7: ba = 41: bb = 18: bc = 30: bd = 22: tax = 43: tay = 19: tcx = 50: tcy = 21:
END IF
IF Fo$ = "Pr" THEN B = 1
Alert 1, 3, ba, bb, bc, bd
textocf tax + 8, tay - 2, 7, 4, " Buscar Registro."
textocf tax, tay, 14, 3, "Ingrese Nombre o N£mero del"
textocf tax + 4, tay + 1, 14, 3, "C¢digo o Registro: "
textocf tax + 6, tay + 2, 1, 3, "             "
KBD$ = LLENARCF$(tcx, tcy, 0, 2, "", aquicf$, 13, 13)
REGNOMBRE$ = TRIM$(UCASE$(aquicf$))
REGNUMERO& = VAL(LEFT$(REGNOMBRE$, 5))
FOR I = 1 TO LEN(REGNOMBRE$)
A$ = RIGHT$(LEFT$(REGNOMBRE$, I), 1)
IF A$ = CHR$(164) THEN MID$(REGNOMBRE$, I, LEN(REGNOMBRE$)) = CHR$(165): EXIT FOR
NEXT
IF KBD$ = CHR$(27) THEN salir = 1: EXIT SUB
IF KBD$ = CHR$(13) AND u = 7 OR u = 1 THEN EXIT SUB
IF KBD$ = CHR$(0) + ">" THEN
    IF Fo$ = "Pa" THEN listPa 1, 0: salir = 0: PABACK
    IF Fo$ = "Pr" THEN ListPr 1, 99, 0: salir = 0: PrBack
END IF
END SUB

SUB Recipes
DIM notas(1 TO 13) AS STRING * 57
FOR I = 1 TO 13: notas(I) = "": NEXT
3
LOCATE 25, 1: COLOR 3, 3: PRINT STRING$(80, " ");
FOR O = 8 TO 22: LOCATE O, 11: COLOR 7, 4: PRINT STRING$(58, 32); : NEXT
BOX 7, 4, 10, 8, 59, 22, 0
FOR O = 9 TO 21: LOCATE O, 11: COLOR 7, 4: PRINT ">"; : NEXT
textocf 30, 8, 14, 4, " EMISION DE RECIPES ": textocf 13, 22, 15, 4, " F4: Imprimir "
textocf 55, 22, 15, 4, " ESC: Salir "
D = 1
DO
GOSUB presentar
KBD$ = fillhand$(12, 8 + D, 0, 7, RTRIM$(notas(D)), notas(D), 57, 57)
SELECT CASE KBD$
        CASE IS = CHR$(0) + "H", CHR$(0) + "K", CHR$(8), CHR$(0) + CHR$(15)
                D = D - 1: IF D < 1 THEN D = 1
        CASE IS = CHR$(0) + "P", CHR$(0) + "M", CHR$(8), CHR$(13)
                D = D + 1: IF D > 13 THEN D = 13
        CASE IS = CHR$(0) + ">"
                GOSUB empremir
        CASE IS = CHR$(27)
                EXIT DO
END SELECT
LOOP
EXIT SUB

empremir:
        BOX 1, 3, 27, 11, 26, 14, 0
        textocf 27, 10, 15, 0, " - "
        kjh$ = CHR$(63 + 2) + CHR$(35 + 11) + CHR$(61 + 16)
        kjh$ = kjh$ + CHR$(36 + 10) + CHR$(70 + 16) + CHR$(33 + 13)
        textocf 30, 10, 7, 6, " " + kjh$ + "                 "
        textocf 54, 11, 0, 0, "  "
        textocf 28, 12, 0, 3, " ®Imprimir con Membrete? "
        textocf 28, 13, 8, 3, "          (S/N)          "
        LOCATE 25, 1: COLOR 0, 3: PRINT " SI NO TIENE FORMATOS DE RECIPES DISPONIBLES, PUEDE IMPRIMIR CON MEMBRETE...";
cabeza = 0
DO
PRESS$ = INKEY$
SELECT CASE PRESS$
CASE IS = "S", "s": textocf 28, 13, 15, 3, "          (S)   ": cabeza = 1: EXIT DO
CASE IS = "N", "n": textocf 28, 13, 15, 3, "            (N)": cabeza = 0: EXIT DO
END SELECT
LOOP
WIDTH LPRINT 131
textocf 13, 22, 30, 4, " IMPRIMIENDO  "
IF cabeza = 1 THEN
        LPRINT CHR$(14); TRIM$(nombre$)
        LPRINT CHR$(18); Daylet$; " "; MFECHA$
        'LPRINT STRING$(27, 32); CHR$(14); CHR$(15); " RECIPE MEDICO "
        LPRINT CHR$(18); TIME$; "                                                      Pag. #"; pag%
        LPRINT CHR$(15); STRING$(130, 196); : LPRINT CHR$(18)
END IF
FOR I = 1 TO 13: LPRINT notas(I): NEXT
IF cabeza = 1 THEN LPRINT CHR$(15); STRING$(130, 196): cabeza = 0
EXIT SUB
RETURN

presentar:
FOR I = 1 TO 13: textocf 12, 8 + I, 7, 4, notas(I): NEXT
RETURN

END SUB

SUB SAVECLINICA
DIM SER$
OPEN "AMVSYS\AMV21.SYS" FOR APPEND AS #1: CLOSE #1
FOR I = 16 TO 21: COLOR 0, 0: LOCATE I, 5: PRINT STRING$(50, 32): NEXT
BOX 14, 0, 5, 15, 49, 21, 0
COLOR 7
OPEN "AMVSYS\AMV21.SYS" FOR RANDOM ACCESS READ AS #1 LEN = LEN(clinica)
GET #1, 1, clinica: CLOSE #1
'ser$ = clinica.serial
'prop$ = clinica.nombre
LOCATE 16, 6: PRINT "DIRECC.:";
LOCATE 17, 6: PRINT "TELEFO.:";
LOCATE 18, 6: PRINT "RIF    :";
LOCATE 19, 6: PRINT "NIT    :";
LOCATE 20, 6: PRINT "E-MAIL.:";
'KBD$ = LLENARCE$(14, 16, 14, 0, "", clinica.direcc, 40, 40)
'KBD$ = LLENARCE$(14, 17, 14, 0, "", clinica.telefonos, 31, 31)
'KBD$ = LLENARCE$(14, 18, 14, 0, "", clinica.rif, 14, 14)
'KBD$ = LLENARCE$(14, 19, 14, 0, "", clinica.nit, 14, 14)
'KBD$ = LLENARCE$(14, 20, 14, 0, "", clinica.email, 25, 25)
OPEN "AMVSYS\AMV21.SYS" FOR RANDOM ACCESS WRITE AS #1 LEN = LEN(clinica)
clinica.serial = SER$
clinica.nombre = prop$
PUT #1, 1, clinica: CLOSE #1
SLEEP 1
END SUB

SUB SHOWAYUDA
GOSUB Fon: GOSUB Fondo
FILE$ = "AMVSYS\AMV21.HLP"
OPEN FILE$ FOR RANDOM ACCESS READ AS #2 LEN = LEN(vuestra.linea)
FINAL& = LOF(2) / 76: hh& = FINAL& / 22
FOR l& = 1 TO 21
        GET #2, l&, vuestra.linea: scrollup
        klin$ = LEFT$(LTRIM$(vuestra.linea), 1)
        GOSUB colorea
        LOCATE 23, 3: COLOR clor, 1: PRINT vuestra.linea
        linea$ = ""
        IF l& = 21 THEN m& = 1: EXIT FOR
NEXT l&
        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
l& = 21
6
f& = INT(l& / hh&)
IF f& < 3 THEN f& = 2
IF f& > 23 THEN f& = 23
GOSUB dere
DO
    KBD$ = INKEY$
        SELECT CASE KBD$
                CASE IS = CHR$(0) + "O"
                    IF l& = FINAL& THEN SOUND 58, 7: GOTO 6
                    l& = FINAL& - 21: m& = l& - 20
                    GOSUB Fon
                    FOR R = 1 TO 21
                        l& = l& + 1: m& = m& + 1
                        IF l& > FINAL& THEN
                                l& = FINAL&: m& = l& - 20: GOTO 6
                        END IF
                        GET #2, l&, vuestra.linea
                        GOSUB colorea
                        LOCATE R + 2, 3: COLOR clor, 1: PRINT vuestra.linea
                        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                        linea$ = "": GOSUB dere: f& = INT(l& / hh&)
                        IF f& < 3 THEN f& = 2
                    NEXT R
                    LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                CASE IS = CHR$(0) + "G"
                        IF l& = 21 THEN SOUND 58, 7: GOTO 6
                        GOSUB Fon
                        FOR l& = 1 TO 21
                            GET #2, l&, vuestra.linea: scrollup
                            GOSUB colorea
                            LOCATE 23, 3: COLOR clor, 1: PRINT vuestra.linea
                            linea$ = ""
                            IF l& = 21 THEN
                                m& = 1: EXIT FOR
                            END IF
                        NEXT l&
                        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                        f& = 2: GOSUB dere
                CASE IS = CHR$(0) + "I"
                    FOR R = 1 TO 20
                        l& = l& - 1: m& = m& - 1
                        IF l& < 21 THEN
                              l& = 21: m& = 1: SOUND 58, 7: GOTO 6
                        END IF
                        CALL scrolldown
                        GET #2, m&, vuestra.linea:
                        GOSUB colorea
                        LOCATE 3, 3: COLOR clor, 1: PRINT vuestra.linea
                        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                        linea$ = "": GOSUB dere
                        f& = INT(l& / hh&)
                        IF f& < 3 THEN f& = 2
                        NEXT R
                CASE IS = CHR$(0) + "Q"
                    FOR R = 1 TO 20
                        l& = l& + 1: m& = m& + 1
                        IF l& > FINAL& THEN
                                l& = FINAL&: m& = l& - 20: SOUND 58, 7: GOTO 6
                        END IF
                        GET #2, l&, vuestra.linea
                        CALL scrollup
                        GOSUB colorea
                        LOCATE 23, 3: COLOR clor, 1: PRINT vuestra.linea
                        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                        linea$ = "": GOSUB dere: f& = INT(l& / hh&)
                        IF f& < 3 THEN f& = 2
                        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                        NEXT R
                CASE IS = CHR$(0) + "P"
                        l& = l& + 1: m& = m& + 1
                        IF l& > FINAL& THEN
                                l& = FINAL&: m& = l& - 20: SOUND 58, 7: GOTO 6
                        END IF
                        GET #2, l&, vuestra.linea
                        CALL scrollup
                        GOSUB colorea
                        LOCATE 23, 3: COLOR clor, 1: PRINT vuestra.linea
                        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                        linea$ = "": GOSUB dere:
                CASE IS = CHR$(0) + "H"
                        l& = l& - 1: m& = m& - 1
                        IF l& < 21 THEN
                              l& = 21: m& = 1: SOUND 58, 7: GOTO 6
                        END IF
                        CALL scrolldown:
                        GET #2, m&, vuestra.linea:
                        GOSUB colorea
                        LOCATE 3, 3: COLOR clor, 1: PRINT vuestra.linea
                        LOCATE 25, 70: COLOR 0, 3: PRINT (l&);
                        linea$ = "": GOSUB dere
                CASE IS = CHR$(27)
                  EXIT SUB
        END SELECT
f& = INT(l& / hh&)
IF f& < 2 THEN f& = 2
IF f& > 23 THEN f& = 23
LOOP
Fon:
FOR S = 3 TO 23: LOCATE S, 3: COLOR 1, 1: PRINT STRING$(76, " "); : NEXT
RETURN
Fondo:
LOCATE 25, 1: COLOR 3, 3: PRINT STRING$(80, " ");
FOR k = 3 TO 23
        LOCATE k, 1: COLOR 3, 0: PRINT CHR$(176);
        LOCATE k, 2: COLOR 3, 0: PRINT CHR$(179);
        LOCATE k, 79: COLOR 3, 0: PRINT CHR$(179);
        LOCATE k, 80: COLOR 3, 0: PRINT CHR$(176);
        LOCATE 2, 1: COLOR 3, 0: PRINT CHR$(176);
        LOCATE 24, 1: COLOR 3, 0: PRINT CHR$(176);
        LOCATE 2, 80: COLOR 3, 0: PRINT "";
        LOCATE 24, 80: COLOR 3, 0: PRINT "";
NEXT
FOR k = 3 TO 78
        LOCATE 2, k: COLOR 3, 0: PRINT CHR$(196);
        LOCATE 24, k: COLOR 3, 0: PRINT CHR$(196);
NEXT
        LOCATE 2, 79: COLOR 3, 0: PRINT CHR$(191);
        LOCATE 24, 2: COLOR 3, 0: PRINT CHR$(192);
        LOCATE 2, 2: COLOR 3, 0: PRINT CHR$(218);
        LOCATE 24, 79: COLOR 3, 0: PRINT CHR$(217);
kjh$ = CHR$(63 + 2) + CHR$(35 + 11) + CHR$(61 + 16)
kjh$ = kjh$ + CHR$(36 + 10) + CHR$(70 + 16) + CHR$(33 + 13)
LOCATE 2, 4: COLOR 3, 0: PRINT " "; kjh$;
LOCATE 2, 36: COLOR 14, 0: PRINT " AYUDA ";
LOCATE 25, 2: COLOR 0, 3: PRINT "ESC: Salir";
LOCATE 25, 64: COLOR 0, 3: PRINT "L°nea:";
RETURN

dere:
FOR k = 3 TO 23
        LOCATE k, 80: COLOR 3, 0: PRINT CHR$(176);
        LOCATE f& + 1, 80: COLOR 7, 7: PRINT " ";
NEXT
RETURN

colorea:
clor = 7: klin$ = LEFT$(LTRIM$(vuestra.linea), 1)
IF klin$ = CHR$(196) THEN clor = 15
IF klin$ = CHR$(200) OR klin$ = CHR$(201) OR klin$ = CHR$(186) THEN clor = 11
RETURN
END SUB

SUB SPLASH
COLOR 3, 3
FOR fin = 1 TO 25:
textocf 1, fin, 3, 0, "€€€€€€€€€€€€€"
textocf 14, fin, 7, 0, CHR$(100 + 119) + CHR$(20 + 199) + CHR$(106 + 72) + CHR$(11 + 166) + CHR$(71 + 105) + "                                           " + CHR$(160 + 16) + CHR$(110 + 67) + CHR$(101 + 77) + CHR$(110 + 109) + CHR$(203 + 16)
textocf 67, fin, 3, 0, "€€€€€€€€€€€€€€"
NEXT
size% = (80 - (LEN(RTRIM$(nombre$)))) / 2

textocf size%, 3, 9, 0, RTRIM$(nombre$)

textocf ((80 - (LEN(TRIM$(cha$)))) / 2), 5, 7, 0, TRIM(cha$)
kjh$ = kjh$ + CHR$(1 + 31) + CHR$(65 + 9)
kjh$ = kjh$ + CHR$(65 + 20) + CHR$(64 + 1) + CHR$(65 + 13) + CHR$(33 - 1)
kjh$ = kjh$ + CHR$(65 + 11) + CHR$(15 + 70) + CHR$(73 - 0) + CHR$(65 + 18)
kjh$ = kjh$ + CHR$(30 + 2) + CHR$(46 + 40) + CHR$(10 + 63) + CHR$(75 + 1)
kjh$ = kjh$ + CHR$(65 + 11) + CHR$(25 + 40) + CHR$(35 + 32) + CHR$(10 + 69)
kjh$ = kjh$ + CHR$(11 + 71) + CHR$(86 - 2) + CHR$(63 + 2)
LOCATE 9, 22: COLOR 15: PRINT "∫"; : COLOR 15: PRINT STRING$(34, 32); "∫";
LOCATE 17, 30: COLOR 4: PRINT kjh$
textocf 37, 19, 8, 0, STR$(4532 - 2530)
LOCATE 8, 22: COLOR 15: PRINT "…"; STRING$(34, "Õ"); "ª";
LOCATE 10, 22: PRINT "∫"; STRING$(34, 32); ; "∫";
LOCATE 11, 22: COLOR 15: PRINT "∫"; STRING$(34, 32); "∫";
LOCATE 11, 23: COLOR 12: PRINT " ∫ »ÕÕº ∫  ∫ ∫ ∫ ∫ ∫ ∫  ∫ ∫   ∫ ∫ ";
LOCATE 12, 22: COLOR 15: PRINT "∫"; STRING$(34, 32); "∫";
LOCATE 10, 23: COLOR 4: PRINT " ∫ …ÕÕª ∫  ∫ …Õª …Õª ∫ »ª …º »ª …º";
textocf 20, 25, 7, 0, "Presione Cualquier Tecla para Continuar..."
LOCATE 13, 22: COLOR 15: PRINT "∫"; STRING$(34, 32); ; "∫";
LOCATE 14, 22: COLOR 15: PRINT "∫"; STRING$(34, 32); ; "∫";
LOCATE 14, 23: COLOR 4: PRINT "»ÕÕº  »ÕÕº»ÕÕÕº   »ÕÕÕº  »ÕÕÕÕÕº ";
LOCATE 12, 23: COLOR 4: PRINT "…º …ÕÕª »ª…º ∫ »Õº ∫ »ª ∫ »ª …º ∫ ";
LOCATE 15, 22: COLOR 15: PRINT "»"; STRING$(34, "Õ"); "º";
LOCATE 13, 23: COLOR 4: PRINT "∫  ∫  ∫  ∫∫  »ª   …º  ∫ »ª »Õº …º ";
LOCATE 15, 43: COLOR 7: PRINT " Version 2.1 ";
LOCATE 9, 23: COLOR 4: PRINT " …ÕÕÕÕÕÕª  …ÕÕÕÕÕÕÕÕÕª …ÕÕÕª …ÕÕÕª";

END SUB

