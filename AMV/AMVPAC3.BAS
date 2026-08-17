DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE SUB PrintFicha ()
DECLARE SUB SHOWPA ()
DECLARE SUB siestavacio (file$)
DECLARE SUB SaveVacunas (que$, DOSIS$, Cual$, Cualfin$)
DECLARE SUB CompPr ()
DECLARE SUB GastosInvents ()
DECLARE SUB SavePac ()
DECLARE SUB Espere (y%)
DECLARE FUNCTION SHOWVAC$ (who$, x%, y%)
DECLARE SUB PABACK ()
DECLARE SUB Invents (what$)
DECLARE SUB CompSaldo ()
DECLARE SUB Ajustes ()
DECLARE SUB InventBox ()
DECLARE SUB Compfecha ()
DECLARE SUB QuickInvent2 ()
DECLARE SUB ShowHead ()
DECLARE SUB ListPr (yy%, limite%, recupera%)
DECLARE SUB GastosFondo ()
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'
DATA "HEPA","LEPT","DIST","PARA","CORO","CALI","PANE","RINO","RABI","PARV",

DEFINT A-Z
SUB AgregarHist
DIM notas(10) AS STRING * 76
DIM notas AS STRING * 76
BEEP
IF l = 1 THEN pg$ = "  de HEMATOLOGIA a la"
IF l = 2 THEN pg$ = "     de ORINA a la   "
IF l = 3 THEN pg$ = "     de HECES a la   "
Alert 1, 3, 27, 7, 26, 13
textocf 31, 6, 7, 4, "ANEXAR EXAMEN     "
LOCATE 8, 29: COLOR 0, 3: PRINT "Se Anexar  este ex men"
LOCATE 9, 29: COLOR 0, 3: PRINT pg$
LOCATE 10, 29: COLOR 0, 3: PRINT " Historia del Paciente"
LOCATE 11, 29: COLOR 8, 3: PRINT "   ¨Continuar? "
LOCATE 12, 38: COLOR 6, 3: PRINT "(S/N)"
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27): textocf 38, 12, 15, 3, "  (N)": SLEEP 1: EXIT SUB
    END SELECT
LOOP
textocf 38, 12, 15, 3, "(S)  ": SLEEP 1
textocf 29, 13, 1, 1, "                                "
textocf 29, 14, 1, 1, "                                "
bb$ = PetShow.id
FOR II = 1 TO LEN(bb$)
        a$ = RIGHT$(LEFT$(bb$, II), 1)
        IF ASC(a$) < 32 THEN MID$(bb$, II, LEN(bb$)) = " "
NEXT
FILEZ$ = "AMVDAT\HISTOR~1\" + TRIM$(bb$) + ".DAT"
bb$ = ""
OPEN FILEZ$ FOR APPEND AS #35
IF LOF(35) / 76 < 13 THEN GOSUB siestavacio
CLOSE #35
IF l = 1 THEN

    textocf 3, 10, AW, AWW, "FECHA DEL EXAMEN:"
    textocf 2, 11, BW, BWW, "GLOBULOS BLANCOS:"
    textocf 2, 12, CW, CWW, "GLOBULOS ROJOS:"
    textocf 2, 13, DW, DWW, "HEMOGLOBINA:"
    textocf 2, 14, EW, EWW, "HEMATOCRITOS:"
    textocf 2, 15, FW, FWW, "HEMATIES:"
    textocf 2, 16, GW, GWW, "NEUTROFILOS:"
    textocf 2, 17, HW, HWW, "MONOCITOS:"
    textocf 2, 18, IW, IWW, "EOSINOFILOS:"


   notas(1) = " <" + PetShow.examen(1) + ">: " + STRING$(15, "-") + " EXAMEN HEMATOLOGICO: " + STRING$(27, "-"): a = 10
   notas(2) = " GLOBULOS BLANCOS: " + PetShow.examen(2) + "    PLAQUETAS:        " + RTRIM$(PetShow.examen(10))
   notas(3) = " GLOBULOS ROJOS:   " + PetShow.examen(3) + "    FILARIA:          " + RTRIM$(PetShow.examen(11))
   notas(4) = " HEMOGLOBINA:      " + PetShow.examen(4) + "    ERLICHIOSIS:      " + RTRIM$(PetShow.examen(12))
   notas(5) = " HEMATOCRITOS:     " + PetShow.examen(5) + "    BABESIA:          " + RTRIM$(PetShow.examen(13))
   notas(6) = " NEUTROFILOS:      " + PetShow.examen(6) + "    ANAPLASMA:        " + RTRIM$(PetShow.examen(14))
   notas(7) = " LINFOCITOS:       " + PetShow.examen(7) + "    HEPATOZOOM CANIS: " + RTRIM$(PetShow.examen(15))
   notas(8) = " MONOCITOS:        " + PetShow.examen(8) + "    TRIPANOSOMA:      " + RTRIM$(PetShow.examen(16))
   notas(9) = " EOSINOFILOS:      " + PetShow.examen(9) + "    OTROS:            " + RTRIM$(PetShow.examen(17))
   notas(10) = STRING$(76, "-")
END IF
IF l = 2 THEN
   notas(1) = " <" + PetShow.examen(20) + ">: " + STRING$(15, "-") + " EXAMEN DE ORINA: ----" + STRING$(27, "-"): a = 10
   notas(2) = " COLOR:             " + PetShow.examen(21) + "    HEMOGLOBINA:         " + RTRIM$(PetShow.examen(28))
   notas(3) = " ASPECTO:           " + PetShow.examen(22) + "    NITRITOS:            " + RTRIM$(PetShow.examen(29))
   notas(4) = " DENSIDAD:          " + PetShow.examen(23) + "    UROBILINOGENO:       " + RTRIM$(PetShow.examen(30))
   notas(5) = " PH:                " + PetShow.examen(24) + "    LEUCOSITOS:          " + RTRIM$(PetShow.examen(31))
   notas(6) = " PROTEINAS:         " + PetShow.examen(25) + "    HEMATIES:            " + RTRIM$(PetShow.examen(32))
   notas(7) = " GLUCOSA:           " + PetShow.examen(26) + "    BACTERIAS:           " + RTRIM$(PetShow.examen(33))
   notas(8) = " CUERPOS CETONICOS: " + PetShow.examen(27) + "    CELULAS EPITELIALES: " + RTRIM$(PetShow.examen(34))
   notas(9) = " BILIRRUBINA:       " + RTRIM$(PetShow.examen(27))
   notas(10) = STRING$(76, "-")
END IF
IF l = 3 THEN
   notas(1) = " <" + PetShow.examen(34) + ">: " + STRING$(15, "-") + " EXAMEN DE HECES: ----" + STRING$(27, "-"): a = 6
   notas(2) = " ASPECTO:      " + PetShow.examen(35) + "    MOCO:                " + RTRIM$(PetShow.examen(39))
   notas(3) = " COLOR:        " + PetShow.examen(36) + "    REACCION:            " + RTRIM$(PetShow.examen(40))
   notas(4) = " CONSISTENCIA: " + PetShow.examen(37) + "    PARASITOS:           " + RTRIM$(PetShow.examen(41))
   notas(5) = " SANGRE:       " + PetShow.examen(38) + "    RESTOS ALIMENTICIOS: " + RTRIM$(PetShow.examen(42))
   notas(6) = STRING$(76, "-")
END IF
OPEN FILEZ$ FOR RANDOM AS #35 LEN = 76
OPEN "AMVDAT\HISTOR~1\AMVNOTAS.TMP" FOR RANDOM ACCESS WRITE AS #3 LEN = 76
cant& = LOF(35) / 76
FOR I& = cant& TO 1 STEP -1
        notas = STRING$(76, 32)
        GET #35, I&, notas
        IF LTRIM$(notas) <> "" THEN EXIT FOR
        IF LTRIM$(notas) = "" THEN notas = STRING$(76, 254): PUT #35, I&, notas
NEXT
FOR I& = 1 TO cant&
        notas = STRING$(76, 32)
        GET #35, I&, notas
        IF notas <> STRING$(76, 254) THEN II& = II& + 1: PUT #3, II&, notas
NEXT
CLOSE #35: CLOSE #3
KILL FILEZ$
NAME "AMVDAT\HISTOR~1\AMVNOTAS.TMP" AS FILEZ$
OPEN FILEZ$ FOR RANDOM AS #35 LEN = 76
cant& = LOF(35) / 76
FOR I& = 1 TO a
        PUT #35, cant& + I&, notas(I&)
NEXT
cant& = cant& + a
IF cant& < 13 THEN
        notas(10) = STRING$(76, 32)
        FOR I = cant& + 1 TO 13
                PUT #35, I, notas(10): B = B + 1
        NEXT
        cant& = cant& + B
END IF
FOR I = 1 TO 13
GET #35, cant& - I - 13, PetShow.notas(I)
NEXT
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS WRITE AS #22 LEN = LEN(Pet)
PUT #22, REG&, PetShow: CLOSE #22: CLOSE #35
EXIT SUB

siestavacio:
CLOSE #35
OPEN FILEZ$ FOR RANDOM ACCESS WRITE AS #35 LEN = 76
FOR j& = 1 TO 13
        notas(1) = "": PUT #35, j&, notas(1)
NEXT
RETURN

END SUB

DEFSNG A-Z
'mycantidad$, mysubtotal$, mynombre$
SUB Ajustes
    C% = 1
    IF VAL(LEFT$(InventShow.id, 2)) > 49 THEN C% = 4
    textocf 9, 8, 0, 3, "Ingrese o Modifique la Descripci¢n    "
    kbd$ = LLENARCF$(23, 6, 0, 7, TRIM$(InventShow.produ), aquicf$, 20, 20)
    InventShow.produ = LTRIM$(aquicf$)
    textocf 23, 6, C%, 3, InventShow.produ
    IF kbd$ = CHR$(27) THEN Pv% = 1: MERO$ = "": EXIT SUB
    mynombre$ = TRIM$(aquicf$): mycantidad$ = "1"
    textocf 50, 6, 3, 3, "   ": textocf 52, 6, C%, 3, mycantidad$
    textocf 9, 8, 0, 3, " Ingrese el Monto                     "
    kbd$ = LLENARCF$(55, 6, 0, 7, "", aquicf$, 7, 7)
    textocf 55, 6, 3, 3, "       ":
    textocf 55, 6, 1, 3, aquicf$
    IF kbd$ = CHR$(27) THEN Pv% = 1: EXIT SUB
    mysubtotal& = VAL(TRIM$(aquicf$))
    PetShowinv$ = TRIM$(InventShow.id)
    IF VAL(LEFT$(PetShowinv$, 8)) <= 49 THEN
       mysubtotal& = mysubtotal& * -1
    END IF
    IF VAL(LEFT$(PetShowinv$, 8)) >= 50 THEN
        mysubtotal& = mysubtotal& * 1
    END IF
END SUB

SUB Compfecha
dia% = VAL(LEFT$(TRIM$(InventShow.venci), 2))
mes% = VAL(LEFT$(RIGHT$(InventShow.venci, 5), 2))
ano% = VAL(RIGHT$(TRIM$(InventShow.venci), 2))
IF ano% < VAL(RIGHT$((TRIM$(FECCHA$)), 2)) THEN
                textocf 11, 8, 20, 3, " -Invent VENCIDO-             "
                SLEEP 1: ZZ = 1
END IF
IF ano% = VAL(RIGHT$((TRIM$(FECCHA$)), 2)) THEN
    IF mes% < VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
                textocf 11, 8, 20, 3, " -Invent VENCIDO-             "
                SLEEP 1: ZZ = 1
    END IF
    IF mes% = VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
        IF dia% <= VAL(LEFT$((TRIM$(FECCHA$)), 2)) THEN
                textocf 11, 8, 20, 3, " -Invent VENCIDO-             "
                SLEEP 1: ZZ = 1
        END IF
    END IF
END IF
END SUB

DEFINT A-Z
SUB EDDIT (kbd$)
DIM notas AS STRING * 76
LOCATE 10, 70: COLOR 0, 3: PRINT "       ";
FOR S = 11 TO 23: textocf 2, S, 14, 1, STRING$(76, 32): NEXT
FOR S = 11 TO 23: textocf 2, S, 14, 1, ">": textocf 79, S, 14, 1, "<": NEXT
textocf 1, 25, 3, 3, STRING$(80, 32)
bb$ = PetShow.id
FOR II = 1 TO LEN(bb$)
a$ = RIGHT$(LEFT$(bb$, II), 1)
IF ASC(a$) < 32 THEN MID$(bb$, II, LEN(bb$)) = ""
NEXT
FILEZ$ = "AMVDAT\VACUNAS\" + TRIM$(bb$) + ".DAT"
bb$ = ""
CLOSE #35
OPEN FILEZ$ FOR APPEND AS #35
IF LOF(35) < 988 THEN
CLOSE #35
CALL siestavacio(FILEZ$)
ELSE
CLOSE #35
END IF
OPEN FILEZ$ FOR RANDOM AS #35 LEN = LEN(notas)
cant& = LOF(35) / LEN(notas)
FOR I& = 1 TO 13
GOSUB tomar
HU = 7
IF ASC(RIGHT$(LEFT$(notas, 2), 1)) > 32 THEN HU = 14
textocf 3, 10 + I&, HU, 1, notas
NEXT
I& = 1: d = 13
13
GOSUB tomar
12
GOSUB presentar
LOCATE 24 - d, 79: COLOR 14, 1: PRINT "<";
LOCATE 25, 2: COLOR 0, 3: PRINT "ESC: Volver  ³ F2: Editar Valores ³ F4: Listado ³ L¡nea:                    ";
LOCATE 25, 60: COLOR 0, 3: PRINT TRIM$(STR$(I&)); "/"; TRIM$(STR$(cant&));
GOSUB presentar
DO
kbd$ = INKEY$
SELECT CASE kbd$
        CASE IS = CHR$(0) + "P", CHR$(9), CHR$(13): kbd$ = "":
                IF I& = cant& THEN GOTO 12
                I& = I& + 1: d = d - 1
                IF d < 1 THEN d = 1: FOR f = 1 TO 76: textocf 2 + f, 24 - d, 7, 1, CHR$(SCREEN(24 - d, 2 + f)): NEXT: scrolluptxt
                GOTO 13
        CASE IS = CHR$(0) + "H", CHR$(0) + CHR$(15): kbd$ = "":
                IF I& = 1 THEN GOTO 12
                I& = I& - 1: d = d + 1
                IF d > 13 THEN d = 13: FOR f = 1 TO 76: textocf 2 + f, 24 - d, 7, 1, CHR$(SCREEN(24 - d, 2 + f)): NEXT: scrolldowntxt
                GOTO 13
        CASE IS = CHR$(27), CHR$(0) + "M", CHR$(0) + "K", CHR$(0) + "<", CHR$(0) + ">"
                 CLOSE #35: EXIT DO:
END SELECT
LOOP
EXIT SUB

tomar:
GET #35, I& + R, notas
FOR II = 1 TO LEN(notas)
a$ = RIGHT$(LEFT$(notas, II), 1)
IF ASC(a$) < 32 THEN MID$(notas, II, LEN(notas)) = ""
NEXT
RETURN

presentar:
HY = 7
IF ASC(CHR$(SCREEN(23 - d, 4))) > 32 THEN HY = 14
IF d <> 13 THEN FOR f = 1 TO 76: textocf 2 + f, 23 - d, HY, 1, CHR$(SCREEN(23 - d, 2 + f)): NEXT
HU = 0
IF LTRIM$(notas) <> "" THEN IF ASC(LEFT$(LTRIM$(notas), 1)) > 32 THEN HU = 1
textocf 3, 24 - d, HU, 7, notas
HY = 7
IF ASC(CHR$(SCREEN(25 - d, 4))) > 32 THEN HY = 14
FOR f = 1 TO 76: textocf 2 + f, 25 - d, HY, 1, CHR$(SCREEN(25 - d, 2 + f)): NEXT
RETURN
END SUB

SUB EditVacun
DIM notas AS STRING * 76
bb$ = PetShow.id
FOR II = 1 TO LEN(bb$)
a$ = RIGHT$(LEFT$(bb$, II), 1)
IF ASC(a$) < 32 THEN MID$(bb$, II, LEN(bb$)) = ""
NEXT
FILEZ$ = "AMVDAT\VACUNAS\" + TRIM$(bb$) + ".DAT"
bb$ = ""
OPEN FILEZ$ FOR APPEND AS #35
IF LOF(35) < 988 THEN
CLOSE #35
CALL siestavacio(FILEZ$)
ELSE
CLOSE #35
END IF
u = 1
9
IF u = 1 THEN
    AW = 0: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 2 THEN
    AW = 14: AWW = 1: BW = 0: BWW = 3: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 3 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 0: CWW = 3: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 4 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 0: DWW = 3: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 5 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 0: EWW = 3: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 6 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 0: FWW = 3: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 7 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 0: GWW = 3: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 8 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 0: HWW = 3: IW = 14: IWW = 1: JW = 14: JWW = 1
END IF
IF u = 9 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 0: IWW = 3: JW = 14: JWW = 1
END IF
IF u = 10 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 0: JWW = 3
END IF
    textocf 2, 11, AW, AWW, "PARVOVIRUS:"
    textocf 2, 12, BW, BWW, "HEPATITIS:"
    textocf 2, 13, CW, CWW, "LEPTOSPIROSIS:"
    textocf 2, 14, DW, DWW, "DISTEMPER:"
    textocf 2, 15, EW, EWW, "PARAINFLUENZA:"
    textocf 2, 16, FW, FWW, "CORONAVIRUS:"
    textocf 2, 17, GW, GWW, "CALICIVIRUS:"
    textocf 2, 18, HW, HWW, "PANEUCOPEMIA FELINA:"
    textocf 2, 19, IW, IWW, "RINOTRAQUEITIS:"
    textocf 2, 20, JW, JWW, "RABIA:"
DO
kbd$ = INKEY$
    SELECT CASE kbd$
        CASE IS = CHR$(27): jl = 1: EXIT SUB
        CASE IS = CHR$(0) + "P": u = u + 1
            IF u = 11 THEN
                u = 1
            END IF
            GOTO 9
        CASE IS = CHR$(0) + "H": u = u - 1
            IF u < 1 THEN
                u = 10
            END IF
            GOTO 9
        CASE IS = CHR$(13):   EXIT DO
    END SELECT
LOOP
OPEN "AMVDAT\AMVVAC.TMP" FOR APPEND AS #36: CLOSE #36
OPEN "AMVDAT\AMVVAC.TMP" FOR RANDOM ACCESS WRITE AS #36 LEN = LEN(PetShow)
k& = 1
PUT #36, k&, PetShow: CLOSE #36
10
IF u = 1 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 11, 0, 7, VAL(PetShow.examen(63)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(63)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(63) = TRIM$(STR$(ACA&))
    textocf 25, 11, 7, 1, PetShow.examen(63)
    kbd$ = VERDATE(PetShow.examen(43), 38, 11)
    textocf 38, 11, 7, 1, PetShow.examen(43)
    kbd$ = VERDATE(PetShow.examen(44), 55, 11)
    textocf 55, 11, 7, 1, PetShow.examen(44)
    kbd$ = SHOWVAC$(PetShow.examen(44), 70, 11)
END IF
IF u = 2 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 12, 0, 7, VAL(PetShow.examen(64)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(64)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(64) = TRIM$(STR$(ACA&))
    textocf 25, 12, 7, 1, PetShow.examen(64)
    kbd$ = VERDATE(PetShow.examen(45), 38, 12)
    textocf 38, 12, 7, 1, PetShow.examen(45)
    kbd$ = VERDATE(PetShow.examen(46), 55, 12)
    textocf 55, 12, 7, 1, PetShow.examen(46)
    kbd$ = SHOWVAC$(PetShow.examen(46), 70, 12)
END IF
IF u = 3 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 13, 0, 7, VAL(PetShow.examen(65)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(65)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(65) = TRIM$(STR$(ACA&))
    textocf 25, 13, 7, 1, PetShow.examen(65)
    kbd$ = VERDATE(PetShow.examen(47), 38, 13)
    textocf 38, 13, 7, 1, PetShow.examen(47)
    kbd$ = VERDATE(PetShow.examen(48), 55, 13)
    textocf 55, 13, 7, 1, PetShow.examen(48)
    kbd$ = SHOWVAC$(PetShow.examen(48), 70, 13)
END IF
IF u = 4 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 14, 0, 7, VAL(PetShow.examen(66)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(66)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(66) = TRIM$(STR$(ACA&))
    textocf 25, 14, 7, 1, PetShow.examen(66)
    kbd$ = VERDATE(PetShow.examen(49), 38, 14)
    textocf 38, 14, 7, 1, PetShow.examen(49)
    kbd$ = VERDATE(PetShow.examen(50), 55, 14)
    textocf 55, 14, 7, 1, PetShow.examen(50)
    kbd$ = SHOWVAC$(PetShow.examen(50), 70, 14)
END IF
IF u = 5 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 15, 0, 7, VAL(PetShow.examen(67)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(67)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(67) = TRIM$(STR$(ACA&))
    textocf 25, 15, 7, 1, PetShow.examen(67)
    kbd$ = VERDATE(PetShow.examen(51), 38, 15)
    textocf 38, 15, 7, 1, PetShow.examen(51)
    kbd$ = VERDATE(PetShow.examen(52), 55, 15)
    textocf 55, 15, 7, 1, PetShow.examen(52)
    kbd$ = SHOWVAC$(PetShow.examen(52), 70, 15)
END IF
IF u = 6 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 16, 0, 7, VAL(PetShow.examen(68)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(68)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(68) = TRIM$(STR$(ACA&))
    textocf 25, 16, 7, 1, PetShow.examen(68)
    kbd$ = VERDATE(PetShow.examen(53), 38, 16)
    textocf 38, 16, 7, 1, PetShow.examen(53)
    kbd$ = VERDATE(PetShow.examen(54), 55, 16)
    textocf 55, 16, 7, 1, PetShow.examen(54)
    kbd$ = SHOWVAC$(PetShow.examen(54), 70, 16)
END IF
IF u = 7 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 17, 0, 7, VAL(PetShow.examen(69)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(69)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(69) = TRIM$(STR$(ACA&))
    textocf 25, 17, 7, 1, PetShow.examen(69)
    kbd$ = VERDATE(PetShow.examen(55), 38, 17)
    textocf 38, 17, 7, 1, PetShow.examen(55)
    kbd$ = VERDATE(PetShow.examen(56), 55, 17)
    textocf 55, 17, 7, 1, PetShow.examen(56)
    kbd$ = SHOWVAC$(PetShow.examen(56), 70, 17)
END IF
IF u = 8 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 18, 0, 7, VAL(PetShow.examen(70)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(70)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(70) = TRIM$(STR$(ACA&))
    textocf 25, 18, 7, 1, PetShow.examen(70)
    kbd$ = VERDATE(PetShow.examen(57), 38, 18)
    textocf 38, 18, 7, 1, PetShow.examen(57)
    kbd$ = VERDATE(PetShow.examen(58), 55, 18)
    textocf 55, 18, 7, 1, PetShow.examen(58)
    kbd$ = SHOWVAC$(PetShow.examen(58), 70, 18)
END IF
IF u = 9 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 19, 0, 7, VAL(PetShow.examen(71)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(71)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(71) = TRIM$(STR$(ACA&))
    textocf 25, 19, 7, 1, PetShow.examen(71)
    kbd$ = VERDATE(PetShow.examen(59), 38, 19)
    textocf 38, 19, 7, 1, PetShow.examen(59)
    kbd$ = VERDATE(PetShow.examen(60), 55, 19)
    textocf 55, 19, 7, 1, PetShow.examen(60)
    kbd$ = SHOWVAC$(PetShow.examen(60), 70, 19)
END IF
IF u = 10 THEN
    kbd$ = STR$(LLENARCFNUM%(25, 20, 0, 7, VAL(PetShow.examen(76)), ACA&, 8, 8))
    IF ACA& = 0 OR ACA& > VAL(PetShow.examen(76)) + 1 THEN BEEP: GOTO 10
    PetShow.examen(76) = TRIM$(STR$(ACA&))
    textocf 25, 20, 7, 1, PetShow.examen(76)
    kbd$ = VERDATE(PetShow.examen(61), 38, 20)
    textocf 38, 20, 7, 1, PetShow.examen(61)
    kbd$ = VERDATE(PetShow.examen(62), 55, 20)
    textocf 55, 20, 7, 1, PetShow.examen(62)
    kbd$ = SHOWVAC$(PetShow.examen(62), 70, 20)
END IF
jl = 1: SavePac
END SUB

SUB InventBox
textocf 8, 5, 0, 3, "ÚÄÄÄ CODIGO ÄÄÂÄÄÄÄÄÄÄÄÄ Concepto ÄÄÄÄÄÄÄÂCantÂÄPrecioÄÂSubTotal¿"
textocf 8, 6, 0, 3, "³             ³                          ³    ³        ³        ³"
textocf 8, 7, 0, 3, "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÅÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄ´"
textocf 8, 8, 0, 3, "³                                             ³Total:           ³"
textocf 8, 9, 0, 3, "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
FOR I = 10 TO 73
textocf I, 10, 8, 0, CHR$(SCREEN(10, I))
NEXT
FOR I = 6 TO 10
textocf 73, I, 8, 0, CHR$(SCREEN(I, 73)) + CHR$(SCREEN(I, 74))
NEXT

END SUB

SUB LlistPa
WIDTH LPRINT 131
pag% = 1: Espere 0
KM$ = "\             \"
KK$ = "\           \"
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #5 LEN = LEN(Pet)
GOSUB headlistpa
FOR I& = 1 TO (LOF(5) / LEN(Pet))
30
    a = a - 1
    IF a = 0 THEN EXIT FOR
    GET #5, I&, Pet
        LPRINT USING KK$; Pet.id;              '13
        LPRINT "   ";                                 '3
        LPRINT USING KM$; Pet.nombr;             '15
        LPRINT "     ";                               '5
        LPRINT USING KM$; Pet.raza;              '15
        LPRINT "     ";                               '5
        LPRINT USING AS$; Pet.monto;            '13
        LPRINT "       ";                             '7
        LPRINT Pet.dueno;                        '20
        LPRINT STRING$(30, 32);                       '30
        LPRINT Pet.Clave                         '2
NEXT

LPRINT STRING$(130, 196)
pag% = pag% + 1
tot& = (LOF(5) / LEN(Pet)):
LPRINT "   Total Registros: ";
LPRINT USING AS$; tot&;
FOR lines = 1 TO a: LPRINT : NEXT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT CHR$(18):
IF I& < LOF(5) / LEN(Pet) THEN GOSUB headlistpa: GOTO 30
CLOSE #5: EXIT SUB
EXIT SUB

headlistpa:
a = 45
LPRINT CHR$(14); TRIM$(nombre$)
LPRINT CHR$(18); Daylet$; " "; MFECHA$
LPRINT STRING$(27, 32); CHR$(14); CHR$(15); "LISTADO DE PACIENTES"
LPRINT CHR$(18); TIME$; "                                                           Pag. #"; pag%
LPRINT CHR$(15); STRING$(130, 196)
LPRINT "   REGISTRO     NOMBRE              RAZA                      SALDO       DUE¥O                                              OP.";
LPRINT STRING$(130, 196)
RETURN

END SUB

DEFSNG A-Z
SUB PrintFicha
IF jl% = 0 THEN GOSUB heat
IF jl% = 1 THEN
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(Pet)
FOR I& = 1 TO (LOF(1) / LEN(Pet))
GET #1, I&, PetShow
GOSUB heat
SLEEP 1
NEXT
CLOSE #1
END IF
EXIT SUB
heat:
LPRINT CHR$(14); TRIM$(nombre$)
LPRINT CHR$(14); CHR$(15);
LPRINT "REGISTRO #"; : LPRINT LEFT$("±±±±±±±±±±±±±", 13 - LEN(TRIM$(PetShow.id))) + TRIM$(PetShow.id); : LPRINT STRING$(32, 32); : LPRINT "OPER: "; : LPRINT PetShow.Clave
LPRINT CHR$(18);
LPRINT "PACIENTE: "; :  LPRINT PetShow.nombr; : LPRINT "  RAZA/GENERO: "; : LPRINT PetShow.raza; : LPRINT " ENTRADA: "; : LPRINT PetShow.checkin
LPRINT "NACIO: "; : LPRINT PetShow.fecha; : LPRINT "  SEXO: "; : LPRINT PetShow.sexos; : LPRINT "  COLOR: "; : LPRINT PetShow.clor; : LPRINT "TALLA: "; : LPRINT PetShow.talla
LPRINT "DUE¥O: "; : LPRINT PetShow.dueno; : LPRINT " RIF-NIT-CI: "; : LPRINT PetShow.cedula
LPRINT "DIRECCION/TELEF: "; : LPRINT PetShow.direcc
RETURN
END SUB

DEFINT A-Z
SUB PrintHistory
DIM notas AS STRING * 76
bb$ = PetShow.id
FOR II = 1 TO LEN(bb$)
a$ = RIGHT$(LEFT$(bb$, II), 1)
IF ASC(a$) < 32 THEN MID$(bb$, II, LEN(bb$)) = " "
NEXT
FILEZ$ = "AMVDAT\HISTOR~1\" + TRIM$(bb$) + ".DAT"
bb$ = ""
OPEN FILEZ$ FOR APPEND AS #35
IF (LOF(35) / 76) < 13 THEN
        CLOSE #35: EXIT SUB
ELSE
cant& = (LOF(35) / 76)
CLOSE #35
END IF
BEEP
z = 1
Alert 1, 3, 27, 7, 26, 10
textocf 38, 6, 7, 4, "Imprimir"
666
IF z = 2 THEN aaa = 7: aaaa = 0: bbb = 1: bbbb = 3
IF z = 1 THEN aaa = 1: aaaa = 3: bbb = 7: bbbb = 0
LOCATE 9, 29: COLOR aaa, aaaa: PRINT "  FICHA DEL PACIENTE   "
LOCATE 8, 29: COLOR bbb, bbbb: PRINT " HISTORIA DEL PACIENTE "
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = CHR$(0) + "H": z = z - 1
        IF z < 1 THEN z = 2
        GOTO 666
        CASE IS = CHR$(0) + "P": z = z + 1
        IF z > 2 THEN z = 1
        GOTO 666
        CASE IS = CHR$(27): EXIT SUB
        CASE IS = CHR$(13):
        IF z = 1 THEN EXIT DO
        IF z = 2 THEN PABACK: SHOWPA: PrintFicha: EXIT SUB
        CASE ELSE
    END SELECT
LOOP
'LOCATE 9, 29: COLOR 3, 3: PRINT "                       "
'LOCATE 11, 29: COLOR 3, 3: PRINT "                       "
'PABACK
'SHOWPA
Alert 1, 3, 27, 7, 26, 13
textocf 31, 6, 7, 4, "Imprimir Historia "
LOCATE 8, 29: COLOR 0, 3: PRINT " Verifique la impresora"
cad& = cant& / 40
IF cad& < cant& / 31 THEN cad& = cad& + 1
IF cad& > 1 THEN FF$ = "s": ee$ = "n"
pg$ = LTRIM$(STR$(cad&)) + " P gina" + FF$
LOCATE 9, 29 + ((24 - LEN(pg$)) / 2): COLOR 0, 3: PRINT pg$
LOCATE 10, 29: COLOR 0, 3: PRINT "    ser " + ee$ + " impresa" + FF$ + "."
LOCATE 11, 29: COLOR 8, 3: PRINT "   ¨Continuar?"
LOCATE 12, 38: COLOR 6, 3: PRINT "(S/N)"
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27): textocf 38, 12, 15, 3, "  (N)": SLEEP 1: EXIT SUB
    END SELECT
LOOP
textocf 38, 12, 15, 3, "(S)  ": SLEEP 1
textocf 29, 13, 1, 1, "                                "
textocf 29, 14, 1, 1, "                                "
PABACK
SHOWPA
GOSUB head
OPEN FILEZ$ FOR RANDOM ACCESS READ AS #35 LEN = 76
cant& = (LOF(35) / 76)
FOR I& = 1 TO cant&
21
GET #35, I&, notas
SLEEP 1
h& = h& + 1
IF h& = 40 THEN h& = 0: GOSUB Pausar: GOSUB head: GOTO 21
LPRINT notas
NEXT
FOR j& = 1 TO h&
LPRINT : NEXT
GOSUB Pausar
CLOSE #35
EXIT SUB

Pausar:
LPRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
pag& = pag& + 1
LPRINT " P gina #"; : LPRINT pag&; : LPRINT " de"; : LPRINT cad&
LPRINT : LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT : LPRINT
RETURN
head:
PrintFicha
LPRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
LPRINT CHR$(14); CHR$(15); " HISTORIA DEL PACIENTE                                   FECHA: "; : LPRINT FECCHA$
LPRINT CHR$(18); "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
RETURN
END SUB

SUB QuickInvent
mttl& = 0: MERO$ = "": Pv% = 0
101
CALL InventBox: I% = 8
    DO
1010
        IF Pet.id <> "             " AND PetShow.id <> "             " THEN CompSaldo
        textocf 50, 6, 3, 3, "    ": textocf 61, 8, 3, 3, "        "
        LOCATE 8, 61: COLOR 14, 3: PRINT USING AS$; mttl&;
        IF mttl& = 0 THEN textocf 61, 8, 1, 3, "***********"
        textocf 23, 6, 3, 3, "                         ":
        textocf 9, 8, 0, 3, " ESC: Salir  -  F4: Lista de Invents       "
        IF MERO$ = "" THEN
            kbd$ = ""
            kbd$ = LLENARCF$(9, 6, 0, 7, "", saqui$, 13, 13)
            MERO$ = UCASE$(TRIM$(saqui$)): START$ = ""
            IF kbd$ = CHR$(27) THEN EXIT SUB
            IF kbd$ = CHR$(0) + ">" THEN
                CALL ListPr(1, 0, 0)
                salir% = 0: MERO$ = REGNOMBRE$: l = 6
                IF Pet.id <> "             " AND PetShow.id <> "             " THEN
                    IF REGNUMERO& >= 0 THEN
                        PABACK
                        ShowHead
                        OPEN "AMVDAT\AMVCA.DAT" FOR INPUT AS #4:
                        CALL Invents(PetShow.id): CLOSE #4
                    END IF
                END IF
                IF Pet.id = "             " AND PetShow.id = "             " THEN
                    CALL GastosFondo
                    CALL GastosInvents
                END IF
                GOTO 101
             END IF
        END IF
        textocf 9, 6, 3, 3, "             "
        textocf 9, 6, 0, 7, TRIM$(MERO$)
        IF MERO$ = "" OR MERO$ = "0" THEN MERO$ = "": GOTO 1010
        REGNOMBRE$ = MERO$
        CompPr
            IF InventShow.id = "             " THEN
                textocf 9, 6, 20, 7, "-ERROR- "
                BEEP: SLEEP 1: textocf 9, 6, 3, 3, "             "
                CLOSE #13: MERO$ = "": GOTO 1010
            END IF
        'NEXT
        'C% = 0
        'IF VAL(LEFT$(PetShowinv$, 2)) > 49 THEN C% = 4
        textocf 9, 6, 1, 3, InventShow.id
        mycodigo$ = (TRIM$(InventShow.id))
        mynombre$ = TRIM$(InventShow.produ)
        textocf 23, 6, 1, 3, mynombre$
        IF VAL(LEFT$(InventShow.id, 4)) >= 1 AND VAL(LEFT$(InventShow.id, 4)) <= 99 THEN
            CALL Ajustes:
            IF Pv% = 1 THEN Pv% = 0: GOTO 1010
            GOTO 3
        END IF

        IF InventShow.exist = 0 AND InventShow.clase <> "S" THEN
                textocf 9, 8, 20, 3, " -AGOTADA LA EXISTENCIA-            "
                BEEP: SLEEP 1: MERO$ = "": GOTO 1010
        END IF
        textocf 9, 8, 0, 3, " Ingrese la Cantidad                  "
        IF InventShow.clase <> "S" THEN textocf 29, 8, 0, 3, ", existen" + STR$(InventShow.exist) + " Unds."
        kbd$ = LLENARCF$(50, 6, 0, 7, "", aquicf$, 3, 3)
        textocf 50, 6, 3, 3, "   ": textocf 50, 6, 1, 3, aquicf$
        IF kbd$ = CHR$(27) THEN MERO$ = "": GOTO 1010
        IF InventShow.clase <> "S" THEN CALL Compfecha
        IF aquicf$ <> "0" THEN
            IF VAL(aquicf$) = 0 THEN GOTO 1010
        END IF
        IF MERO$ = "" THEN GOTO 1010
        IF ZZ = 1 THEN ZZ = 0: GOTO 1010
        mycantidad$ = TRIM$(aquicf$)
            IF InventShow.exist < VAL(mycantidad$) AND InventShow.clase <> "S" THEN
                textocf 9, 8, 3, 7, "                                    "
                LOCATE 8, 9: COLOR 4, 3: PRINT " SOLO EXISTEN: "; InventShow.exist
                SLEEP 1: MERO& = 0: GOTO 1010
            END IF
        IF InventShow.clase = "P" THEN InventShow.exist = InventShow.exist - VAL(mycantidad$)
        IF InventShow.exist < 0 THEN InventShow.exist = 0
        textocf 50, 6, 1, 3, TRIM$(mycantidad$)
        mysubtotal& = InventShow.preci
        OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(Invent)
        PUT #3, REG&, InventShow: CLOSE #3
3
        textocf 55, 6, 1, 3, TRIM$(STR$(mysubtotal&))
    QuickInvent2
    LOOP
END SUB

DEFSNG A-Z
SUB QuickInvent2
        mysubtotal& = VAL(mycantidad$) * mysubtotal&:
        textocf 64, 6, 1, 3, STR$(mysubtotal&)
        IF mysubtotal& <> 0 THEN
            myserial$ = TRIM$(PetShow.id)
            IF myserial$ <> "" THEN file$ = "AMVDAT\AMVCA.DAT"
            IF myserial$ = "" THEN
                a$ = RIGHT$(FECCHA$, 5)
                file$ = "AMVHIS\GA" + a$ + ".DAT"
            END IF
            mytime$ = TIME$: myfecha$ = FECCHA$
            myserial$ = TRIM$(PetShow.id): myclave$ = Iniciales$:
            OPEN file$ FOR APPEND AS #14:
                WRITE #14, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            CLOSE #14
        END IF
        MERO$ = "": myserial$ = "": myfecha$ = "": myclave$ = "": mycodigo$ = "": mynombre$ = "": mycantidad$ = "": mysubtotal& = 0: mytime$ = "":
        CompSaldo
        textocf 61, 8, 3, 7, "        ": LOCATE 8, 61: COLOR 4, 3: PRINT USING AS$; mttl&;
        SLEEP 1
        textocf 8, 6, 0, 3, "³             ³                          ³    ³        ³        ³"
        textocf 9, 8, 11, 3, " Listo...                                    "
        SLEEP 1
END SUB

SUB SaveVacunas (que$, DOSIS$, Cual$, Cualfin$)
DIM notax AS STRING * 76
DIM DOSIZ AS STRING * 13
DIM notas AS STRING * 76
DOSIZ = DOSIS$
FOR II = 1 TO LEN(DOSIZ)
a$ = RIGHT$(LEFT$(DOSIZ, II), 1)
IF ASC(a$) < 32 THEN MID$(DOSIZ, II, LEN(DOSIZ)) = ""
NEXT
bb$ = PetShow.id
FOR II = 1 TO LEN(bb$)
a$ = RIGHT$(LEFT$(bb$, II), 1)
IF ASC(a$) < 32 THEN MID$(bb$, II, LEN(bb$)) = ""
NEXT
FILEZ$ = "AMVDAT\VACUNAS\" + TRIM$(bb$) + ".DAT"
OPEN FILEZ$ FOR APPEND AS #35: CLOSE #35
OPEN FILEZ$ FOR RANDOM AS #35 LEN = LEN(notas)
FOR j& = (LOF(35) / LEN(notas)) TO 1 STEP -1
IF j& - 1 <> 0 THEN
        GET #35, j&, notas
        GET #35, j& - 1, notax
        PUT #35, j&, notas
        PUT #35, j& - 1, notax
        IF LEFT$(LTRIM$(notas), 13) = DOSIZ$ THEN
        notas = STRING$(18, 32) + DOSIZ + "     " + Cual$ + "   --    " + Cualfin$
        PUT #35, j&, notas
        CLOSE #35: EXIT SUB
        END IF
END IF
NEXT
FOR j& = (LOF(35) / LEN(notas)) TO 1 STEP -1
IF j& - 1 <> 0 THEN
        GET #35, j&, notas
        GET #35, j& - 1, notax
        PUT #35, j& + 1, notas
        PUT #35, j&, notax
        IF LEFT$(LTRIM$(notax), 5) = que$ THEN
        notas = STRING$(18, 32) + DOSIZ + "     " + Cual$ + "   --    " + Cualfin$
        PUT #35, j&, notas
        CLOSE #35: EXIT SUB
        END IF
END IF
NEXT
CLOSE #35
END SUB

SUB siestavacio (file$)
DIM notas AS STRING * 76
OPEN file$ FOR RANDOM ACCESS WRITE AS #35 LEN = LEN(notas)
notas = " PARVOVIROSIS:" + STRING$(74 - LEN("PARVOVIROSIS:"), "-")
PUT #35, 1, notas
notas = " HEPATITIS:" + STRING$(74 - LEN("HEPATITIS:"), "-")
PUT #35, 2, notas
notas = " LEPTOSPIROSIS:" + STRING$(74 - LEN("LEPTOSPIROSIS:"), "-")
PUT #35, 3, notas
notas = " DISTEMPER:" + STRING$(74 - LEN("DISTEMPER:"), "-")
PUT #35, 4, notas
notas = " PARAINFLUENZA:" + STRING$(74 - LEN("PARAINFLUENZA:"), "-")
PUT #35, 5, notas
notas = " CORONAVIRUS:" + STRING$(74 - LEN("CORONAVIRUS:"), "-")
PUT #35, 6, notas
notas = " CALICIVIRUS" + STRING$(74 - LEN("CALICIVIRUS"), "-")
PUT #35, 7, notas
notas = " PANEUCOPEMIA FELINA:" + STRING$(74 - LEN("PANEUCOPEMIA FELINA:"), "-")
PUT #35, 8, notas
notas = " RINOTRAQUEITIS:" + STRING$(74 - LEN("RINOTRAQUEITIS:"), "-")
PUT #35, 9, notas
notas = " RABIA:" + STRING$(74 - LEN("RABIA:"), "-")
PUT #35, 10, notas
notas = ""
FOR j& = 11 TO 12
PUT #35, j&, notas
NEXT
notas = STRING$(74, "-")
PUT #35, 13, notas
CLOSE #35
                        CALL SaveVacunas("PARVO", "PARV/" + PetShow.examen(63), PetShow.examen(43), PetShow.examen(44))
                        CALL SaveVacunas("HEPAT", "HEPA/" + PetShow.examen(64), PetShow.examen(45), PetShow.examen(46))
                        CALL SaveVacunas("LEPTO", "LEPT/" + PetShow.examen(65), PetShow.examen(47), PetShow.examen(48))
                        CALL SaveVacunas("DISTE", "DIST/" + PetShow.examen(66), PetShow.examen(49), PetShow.examen(50))
                        CALL SaveVacunas("PARAI", "PARA/" + PetShow.examen(67), PetShow.examen(51), PetShow.examen(52))
                        CALL SaveVacunas("CORON", "CORO/" + PetShow.examen(68), PetShow.examen(53), PetShow.examen(54))
                        CALL SaveVacunas("CALIC", "CALI/" + PetShow.examen(69), PetShow.examen(55), PetShow.examen(56))
                        CALL SaveVacunas("PANEU", "PANE/" + PetShow.examen(70), PetShow.examen(57), PetShow.examen(58))
                        CALL SaveVacunas("RINOT", "RINO/" + PetShow.examen(71), PetShow.examen(59), PetShow.examen(60))
                        CALL SaveVacunas("RABIA", "RABI/" + PetShow.examen(76), PetShow.examen(61), PetShow.examen(62))

END SUB

