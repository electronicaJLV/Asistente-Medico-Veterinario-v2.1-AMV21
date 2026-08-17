DECLARE SUB SortInventario ()
DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE FUNCTION LLENARCE$ (x%, y%, letras%, Fondo%, START$, END$, vis%, MAX%)
DECLARE SUB LOGIN (ZZ%)
DECLARE SUB NOHAY ()
DECLARE SUB Lineas ()
DECLARE SUB Question (u%, Fo$, B%)
DECLARE SUB ActualPr (yy%, file%)
DECLARE SUB ListPr (yy%, limite%, recupera%)
DECLARE SUB PrBack ()
DECLARE SUB Espere (y%)
DECLARE SUB CompPr ()
DECLARE SUB EditNotasPr (Actualix%, salimos%, GUARDAR%)
DECLARE SUB ShowNotasPr ()
DECLARE SUB SaveProd (Actualizar%)
DECLARE SUB EditPr ()
DECLARE SUB LlistPr ()
DECLARE SUB PrLstComp ()
DECLARE SUB ShowPr ()
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'

SUB CompPr
        OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #16 LEN = LEN(InventShow)
        FOR REG& = 1 TO (LOF(16) / LEN(Invent))
            GET #16, REG&, InventShow
            REGhamBRE$ = TRIM$(REGNOMBRE$)
            IF REGhamBRE$ = LEFT$(TRIM$(InventShow.produ), LEN(REGNOMBRE$)) THEN
                CLOSE #16: EXIT SUB
            END IF
                FOR i = 1 TO LEN(InventShow.id)
                A$ = RIGHT$(LEFT$(InventShow.id, i), 1)
                IF A$ = CHR$(0) THEN MID$(InventShow.id, i, LEN(InventShow.id)) = CHR$(32)
                NEXT
            IF REGhamBRE$ = TRIM$(InventShow.id) THEN
                CLOSE #16: EXIT SUB
            END IF
        NEXT
CLOSE #16
120
InventShow.id = "             "
END SUB

DEFINT A-Z
SUB EditNotasPr (Actualix%, salimos%, GUARDAR%)
18
ShowNotasPr
kbd$ = fillhand$(3, 12, 0, 7, RTRIM$(InventShow.notas(1)), fdd$, 76, 76)
InventShow.notas(1) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 18
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
19
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 13, 0, 7, RTRIM$(InventShow.notas(2)), fdd$, 76, 76)
START$ = ""
InventShow.notas(2) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 18
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
20
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 14, 0, 7, RTRIM$(InventShow.notas(3)), fdd$, 76, 76)
InventShow.notas(3) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 19
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
121
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 15, 0, 7, RTRIM$(InventShow.notas(4)), fdd$, 76, 76)
InventShow.notas(4) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 20
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
22
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 16, 0, 7, RTRIM$(InventShow.notas(5)), fdd$, 76, 76)
PetShow.notas(5) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 121
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
23
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 17, 0, 7, RTRIM$(InventShow.notas(6)), fdd$, 76, 76)
InventShow.notas(6) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 22
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
24
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 18, 0, 7, RTRIM$(InventShow.notas(7)), fdd$, 76, 76)
InventShow.notas(7) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 23
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
25
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 19, 0, 7, RTRIM$(InventShow.notas(8)), fdd$, 76, 76)
InventShow.notas(8) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 24
IF kbd$ = CHR$(27) THEN GOTO 31
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
26
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 20, 0, 7, RTRIM$(InventShow.notas(9)), fdd$, 76, 76)
InventShow.notas(9) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 25
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
27
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 21, 0, 7, RTRIM$(InventShow.notas(10)), fdd$, 76, 76)
InventShow.notas(10) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 26
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
28
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 22, 0, 7, RTRIM$(InventShow.notas(11)), fdd$, 76, 76)
InventShow.notas(11) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 27
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
130
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 23, 0, 7, RTRIM$(InventShow.notas(12)), fdd$, 76, 76)
InventShow.notas(12) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 28
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
ShowNotasPr
fdd$ = ""
kbd$ = fillhand$(3, 24, 0, 7, RTRIM$(InventShow.notas(13)), fdd$, 76, 76)
InventShow.notas(13) = RTRIM$(fdd$)
IF kbd$ = CHR$(0) + "H" OR kbd$ = CHR$(0) + "K" OR kbd$ = CHR$(8) THEN GOTO 130
IF kbd$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT SUB
IF kbd$ = CHR$(27) THEN GOTO 31
fdd$ = ""



EXIT SUB
31
salimos% = 1
IF GUARDAR% = 0 THEN CALL SaveProd(Actualix)
END SUB

SUB EditPr
textocf 76, 2, 3, 3, "  "
textocf 76, 2, 16, 3, Iniciales$
u = 1
21
IF u = 1 THEN
    AW = 0: AWW = 2: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 2 THEN
    AW = 14: AWW = 1: BW = 0: BWW = 2: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 3 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 0: CWW = 2: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 4 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 0: DWW = 2: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 5 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 0: EWW = 2: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 6 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 0: FWW = 2: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 7 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 0: GWW = 2: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 8 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 0: HWW = 2: IW = 14: IWW = 1: JW = 1: JWW = 7
END IF
IF u = 9 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 0: IWW = 2: JW = 1: JWW = 7
END IF
IF u = 10 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 0: JWW = 2
END IF

    textocf 2, 4, AW, AWW, "CLASE (P/S):"
    textocf 2, 5, BW, BWW, "DESCRIPCION:"
    textocf 45, 5, CW, CWW, "FECHA DE VENCIMIENTO:"
    textocf 2, 6, DW, DWW, "PRECIO DE VENTA:"
    textocf 45, 6, EW, EWW, "COSTO:"
    textocf 2, 7, FW, FWW, "EXISTENCIA ACTUAL:"
    textocf 45, 7, GW, GWW, "EXISTENCIA MINIMA:"
    textocf 2, 8, HW, HWW, "DISTRIBUIDOR:"
    textocf 45, 8, IW, IWW, "FECHA DE COMPRA:"
    textocf 10, 10, JW, JWW, "OBSERVACIONES"
DO
kbd$ = INKEY$
    SELECT CASE kbd$
        CASE IS = CHR$(27): CALL SaveProd(Actual): EXIT SUB
        CASE IS = CHR$(0) + "P": u = u + 1
            IF u > 10 THEN
                u = 1
            END IF
            GOTO 21
        CASE IS = CHR$(0) + "H": u = u - 1
            IF u < 1 THEN
                u = 10
            END IF
            GOTO 21
        CASE IS = CHR$(0) + "M": u = u + 1
            IF u > 10 THEN
                u = 1
            END IF
            GOTO 21
        CASE IS = CHR$(0) + "K": u = u - 1
            IF u < 1 THEN
                u = 10
            END IF
            GOTO 21
        CASE IS = CHR$(13):   EXIT DO
    END SELECT
LOOP
IF u = 1 THEN
46
    IF juan% = 1 THEN
    kbd$ = LLENARCF$(10, 2, 0, 7, InventShow.id, aquicf$, 15, 15)
    InventShow.id = TRIM$(aquicf$)
    textocf 10, 2, 0, 3, InventShow.id + "  "
    END IF

    textocf 15, 4, 14, 1, "        "
    kbd$ = LLENARCF$(15, 4, 0, 7, TRIM$(InventShow.clase), aquicf$, 1, 1)
    IF UCASE$(aquicf$) <> "P" AND UCASE$(aquicf$) = "S" OR UCASE$(aquicf$) = "P" AND UCASE$(aquicf$) <> "S" THEN
    ELSE
        BEEP: GOTO 46
    END IF
    InventShow.clase = UCASE$(aquicf$)
    IF InventShow.clase = "P" THEN textocf 15, 4, 7, 1, "Invent"
    IF InventShow.clase = "S" THEN textocf 15, 4, 7, 1, "SERVICIO"
END IF
IF u = 2 THEN
667
    nombre$ = InventShow.produ
    kbd$ = LLENARCF$(15, 5, 0, 7, InventShow.produ, aquicf$, 25, 25)
        IF TRIM$(aquicf$) <> "" THEN
                zx = ASC(UCASE$(LEFT$(aquicf$, 1)))
                IF zx < 65 AND zx <> 60 OR zx > 90 AND zx <> 60 THEN
                        textocf 15, 5, 4, 7, "   -ERROR-    ": BEEP: SLEEP 1: GOTO 667
                END IF
        END IF
        IF TRIM$(aquicf$) = "" THEN
                aquicf$ = "<* SIN NOMBRE*>"
        END IF

    InventShow.produ = TRIM$(UCASE$(aquicf$))
    IF InventShow.produ <> nombre$ THEN
       Actual = 1
    END IF
textocf 15, 5, 7, 1, InventShow.produ
END IF
IF u = 3 THEN
2234
    textocf 67, 5, 0, 7, InventShow.venci
    kbd$ = VERDATE$(InventShow.venci, 67, 5)
    InventShow.venci = InventShow.venci
    textocf 67, 5, 7, 1, InventShow.venci
    GOSUB comprobar
END IF
IF u = 4 THEN
    kbd$ = STR$(LLENARCFNUM%(22, 6, 0, 7, InventShow.preci, InventShow.preci, 7, 7))
    textocf 22, 6, 7, 1, "        "
    textocf 22, 6, 7, 1, LTRIM$(STR$(InventShow.preci))
END IF
IF u = 5 THEN
    kbd$ = STR$(LLENARCFNUM%(67, 6, 0, 7, InventShow.costo, InventShow.costo, 7, 7))
    textocf 67, 6, 7, 1, "        "
    textocf 67, 6, 7, 1, LTRIM$(STR$(InventShow.costo))
END IF
IF u = 6 THEN
    kbd$ = STR$(LLENARCFNUM%(22, 7, 0, 7, InventShow.exist, InventShow.exist, 7, 7))
    textocf 22, 7, 7, 1, "        "
    textocf 22, 7, 7, 1, LTRIM$(STR$(InventShow.exist))
    GOSUB comprobar
END IF
IF u = 7 THEN
    kbd$ = STR$(LLENARCFNUM%(67, 7, 0, 7, InventShow.minima, InventShow.minima, 7, 7))
    textocf 67, 7, 7, 1, "        "
    textocf 67, 7, 7, 1, LTRIM$(STR$(InventShow.minima))
    GOSUB comprobar
END IF
IF u = 8 THEN
    kbd$ = LLENARCF$(22, 8, 0, 7, TRIM$(InventShow.distr), aquicf$, 20, 20)
    InventShow.distr = TRIM$(UCASE$(aquicf$))
    textocf 22, 8, 7, 1, InventShow.distr
END IF
IF u = 9 THEN
    textocf 69, 8, 0, 7, CHR$(45): textocf 72, 8, 0, 7, CHR$(45)
    kbd$ = VERDATE$(InventShow.compra, 67, 8)
    InventShow.compra = InventShow.compra
    textocf 67, 8, 7, 1, InventShow.compra
END IF
IF u = 10 THEN
    EditNotasPr Actual, sal, 0
    IF sal = 1 THEN sal = 0: EXIT SUB
    textocf 3, 24, 7, 1, InventShow.notas(12)
    u = 1
END IF
InventShow.id = InventShow.id
InventShow.Clave = Iniciales$
GOTO 21
EXIT SUB

comprobar:
IF InventShow.clase = "P" THEN
Pv = 7
dia% = VAL(LEFT$((TRIM$(InventShow.venci)), 2))
mes% = VAL(LEFT$(RIGHT$(InventShow.venci, 5), 2))
ano% = VAL(RIGHT$((TRIM$(InventShow.venci)), 2))
IF ano% < VAL(RIGHT$((TRIM$(FECCHA$)), 2)) THEN
    textocf 42, 5, 12, 1, "   ERROR EN FECHA DE VENCIMIENTO     "
    BEEP: SLEEP 1
    textocf 42, 5, 15, 1, "NO DEBE SER IGUAL O MAYOR A LA ACTUAL"
    SLEEP 2
    textocf 42, 5, 14, 1, "   FECHA DE VENCIMIENTO:             "
    GOTO 2234
END IF
IF ano% > VAL(RIGHT$((TRIM$(FECCHA$)), 2)) THEN
    textocf 45, 5, 14, 1, "FECHA DE VENCIMIENTO:"
END IF
IF ano% = VAL(RIGHT$((TRIM$(FECCHA$)), 2)) THEN
    IF mes% < VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
        textocf 42, 5, 12, 1, "   ERROR EN FECHA DE VENCIMIENTO     "
        BEEP: SLEEP 1
        textocf 42, 5, 15, 1, "NO DEBE SER IGUAL O MAYOR A LA ACTUAL"
        SLEEP 2
        textocf 42, 5, 14, 1, "   FECHA DE VENCIMIENTO:             "
        GOTO 2234
    END IF
    IF mes% > VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
        textocf 45, 5, 14, 1, "FECHA DE VENCIMIENTO:"
    END IF
    IF mes% = VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
        IF dia% > VAL(LEFT$((TRIM$(FECCHA$)), 2)) THEN
            textocf 45, 5, 14, 1, "FECHA DE VENCIMIENTO:"
        END IF
        IF dia% <= VAL(LEFT$((TRIM$(FECCHA$)), 2)) THEN
            textocf 42, 5, 12, 1, "   ERROR EN FECHA DE VENCIMIENTO     "
            BEEP: SLEEP 1
            textocf 42, 5, 15, 1, "NO DEBE SER IGUAL O MAYOR A LA ACTUAL"
            SLEEP 2
            textocf 42, 5, 14, 1, "   FECHA DE VENCIMIENTO:             "
            GOTO 2234
        END IF
    END IF
END IF
IF InventShow.minima >= InventShow.exist OR InventShow.minima >= InventShow.exist THEN
    textocf 2, 7, 31, 1, "Invent AGOTADO:  "
    textocf 45, 7, 31, 1, "EXISTENCIA MINIMA:"
END IF
IF InventShow.minima < InventShow.exist OR InventShow.minima < InventShow.exist THEN
    textocf 2, 7, 14, 1, "EXISTENCIA ACTUAL:"
    textocf 45, 7, 14, 1, "EXISTENCIA MINIMA:"
END IF
END IF
RETURN

END SUB

DEFSNG A-Z
SUB Espere (y%)
Alert 1, 3, 26, 5 + y%, 26, 7 + y%
textocf 28, 6 + y%, 16, 3, "Un momento por favor..."
END SUB

DEFINT A-Z
SUB ListPr (yy%, limite%, recupera%)
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #2 LEN = LEN(Invent)
IF LOF(2) < 107020 THEN CLOSE #2: CALL NOHAY: EXIT SUB
CLOSE #2

' Genera y ordena los archivos .IDX antes de que el usuario vea la pantalla
CALL SortInventario
' ----------------------------

VIEW PRINT 1 TO 25: WIDTH 80
8
CALL Lineas
textocf 2, 25, 0, 3, "ESC: Salir  ³ F4: Imprimir"
IF yy% = 1 THEN textocf 2, 25, 0, 3, "ESC: Salir  ³ ENTER: Seleccionar"
textocf 1, 2, 3, 3, STRING$(80, " ")
textocf 31, 2, 14, 3, "INVENTARIO GENERAL"
textocf 1, 4, 3, 3, STRING$(80, " ")
textocf 5, 4, 0, 3, "CODIGO"
textocf 22, 4, 0, 3, "DESCRIPCION"
textocf 42, 4, 0, 3, "PRECIO"
textocf 50, 4, 0, 3, "EXIST."
textocf 61, 4, 0, 3, "DISTRIBUIDOR"
textocf 78, 4, 0, 3, "OP."
FOR y% = 5 TO 24
    textocf 1, y%, 7, 7, STRING$(80, " ")
NEXT
z = 1
IF recupera% = 1 THEN file$ = "AMVHIS\AMVPR.HIS"
IF recupera% = 0 THEN file$ = "AMVDAT\AMVPR.DAT"
OPEN file$ FOR RANDOM ACCESS READ AS #2 LEN = LEN(Invent)
IF (LOF(2) / LEN(Invent)) < 100 AND file$ = "AMVDAT\AMVPR.DAT" THEN
    CLOSE #2: CALL NOHAY: EXIT SUB
END IF
IF (LOF(2) / LEN(Invent)) < 0 THEN
    CLOSE #2: CALL NOHAY: EXIT SUB
END IF

IF LOF(2) <> 0 THEN final& = (LOF(2) / (LEN(Invent)) - limite%)
LOCATE 3, 2: COLOR 7, 1: PRINT "Total Registros: "; final&
GOSUB FOND
FOR l& = 1 TO 20
        GET #2, l& + limite%, InventShow: scrollupall
        textocf 1, 24, 7, 7, STRING$(80, " ")
        li = 24: GOSUB PrLstBck
        IF l& = 20 THEN
            M& = 1
            EXIT FOR
        END IF
NEXT l&
IF final& < 20 THEN final& = 20
l& = 20
6
DO
    kbd$ = INKEY$
        SELECT CASE kbd$
                CASE IS = CHR$(0) + "O"
                    IF l& = final& THEN SOUND 58, 7: GOTO 6
                    l& = final& - 20: M& = l& - 20
                    GOSUB FOND
                    FOR r = 1 TO 20
                        l& = l& + 1: M& = M& + 1
                        IF l& > final& THEN
                                l& = final&: M& = l& - 19: SOUND 58, 7: GOTO 6
                        END IF
                        GET #2, l& + limite%, InventShow: li = 4 + r: GOSUB PrLstBck
                    NEXT r
                CASE IS = CHR$(0) + "G"
                        IF l& = 20 THEN SOUND 58, 7: GOTO 6
                        GOSUB FOND
                        FOR l& = 1 TO 20
                            GET #2, l& + limite%, InventShow: scrollupall:
                            li = 24: GOSUB PrLstBck
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
                        GET #2, M& + limite%, InventShow: li = 5: GOSUB PrLstBck
                    NEXT r
                CASE IS = CHR$(0) + "Q"
                    FOR r = 1 TO 20
                        l& = l& + 1: M& = M& + 1
                        IF l& > final& THEN
                                l& = final&: M& = l& - 19: SOUND 58, 7: GOTO 6
                        END IF
                        GET #2, l& + limite%, InventShow: CALL scrollupall
                        li = 24: GOSUB PrLstBck
                        NEXT r
                CASE IS = CHR$(0) + "P"
                        l& = l& + 1: M& = M& + 1
                        IF l& > final& THEN
                                l& = final&: M& = l& - 19: SOUND 58, 7: GOTO 6
                        END IF
                        CALL scrollupall
                        GET #2, l& + limite%, InventShow: li = 24: GOSUB PrLstBck
                CASE IS = CHR$(0) + "H"
                        l& = l& - 1: M& = M& - 1
                        IF l& < 20 OR M& < 1 THEN
                              l& = 20: M& = 1: SOUND 58, 7: GOTO 6
                        END IF
                        CALL scrolldownall
                        GET #2, M& + limite%, InventShow: li = 5: GOSUB PrLstBck
                CASE IS = CHR$(27): CLOSE #2: REGNUMERO& = 0: REGNOMBRE$ = "": EXIT SUB
                CASE IS = CHR$(0) + ">":
                IF yy% <> 1 THEN CLOSE #2: LlistPr: GOTO 8
                CASE IS = CHR$(13):
                        IF yy% = 1 THEN
                            textocf 1, 25, 3, 3, STRING$(80, " ")
                            textocf 2, 25, 0, 3, "ESC: Salir  ³  Ingrese el Nombre o el CODIGO del Art¡culo a Mostrar"
                            CALL Question(1, "", 1)
                            CLOSE #2: y% = 0: EXIT SUB
                END IF
END SELECT
LOOP

PrLstBck:
textocf 1, li, 7, 7, STRING$(80, " ")
IF TRIM$(InventShow.id) <> "" THEN
    LOCATE li, 2: COLOR c, 7: PRINT (LTRIM$(InventShow.id));
    LOCATE li, 15: COLOR 1, 7: PRINT CHR$(179);
    LOCATE li, 16: COLOR c, 7: PRINT InventShow.produ; : COLOR 1, 7: PRINT CHR$(179)
    LOCATE li, 42: COLOR c, 7: PRINT (LTRIM$(STR$(InventShow.preci)));
    textocf 49, li, 1, 7, CHR$(179)
    LOCATE li, 50: COLOR c, 7: PRINT (LTRIM$(STR$(InventShow.exist)));
    textocf 57, li, 1, 7, CHR$(179)
    LOCATE li, 58: COLOR c, 7: PRINT LEFT$(InventShow.distr, 19);
    textocf 77, li, 1, 7, CHR$(179)
    CALL PrLstComp
    LOCATE li, 78: COLOR c, 7: PRINT InventShow.Clave;
END IF
RETURN
FOND:
FOR y% = 5 TO 24:
    textocf 1, y%, 7, 7, STRING$(80, " ")
    textocf 15, y%, 1, 7, STRING$(1, 179)
    textocf 41, y%, 1, 7, STRING$(1, 179)
    textocf 49, y%, 1, 7, STRING$(1, 179)
    textocf 57, y%, 1, 7, STRING$(1, 179)
    textocf 77, y%, 1, 7, STRING$(1, 179)
NEXT
RETURN
END SUB

FUNCTION LLENARCE$ (x%, y%, letras%, Fondo%, START$, END$, vis%, MAX%)
CONST TRUE = -1
CONST FALSE = NOT TRUE
COLOR letras%, Fondo%
    curr$ = LTRIM$(LEFT$(START$, MAX))
    IF curr$ = CHR$(8) THEN curr$ = ""

    LOCATE , , 1, 3, 5
    finished = FALSE
    DO
        GOSUB LLENARCEShowText
        GOSUB LLENARCEGetKey

        IF LEN(kbd$) > 2 THEN
            finished = TRUE
            LLENARCE$ = kbd$
        ELSE
            SELECT CASE kbd$
                CASE CHR$(13), CHR$(27), CHR$(9) ' CHR$(0) + "P", CHR$(0) + "H"
                COLOR 0, 0
               
                    finished = TRUE
                    LLENARCE$ = kbd$
           
                CASE CHR$(8)
                    IF curr$ <> "" THEN
                        curr$ = LEFT$(curr$, LEN(curr$) - 1)
                        G$ = LEFT$(G$, LEN(G$) - 1)
                    END IF
                CASE CHR$(0) + "K"
                    IF curr$ <> "" THEN
                        curr$ = LEFT$(curr$, LEN(curr$) - 1)
                        G$ = LEFT$(G$, LEN(G$) - 1)
                    END IF
                CASE CHR$(0) + "M"
                    IF LEN(curr$) < MAX THEN
                        curr$ = curr$ + " "
                        G$ = G$ + " "
                    END IF
                CASE ":":
                curr$ = curr$ + CHR$(164)
                G$ = G$ + CHR$(164)
                CASE ";":
                curr$ = curr$ + CHR$(164)
                G$ = G$ + CHR$(164)
                CASE "<":
                curr$ = curr$ + ";"
                G$ = G$ + ";"
                CASE ">":
                curr$ = curr$ + ":"
                G$ = G$ + ":"
                CASE " " TO "}"
                    IF LEN(G$) < MAX THEN
                  G$ = G$ + kbd$
                    END IF
                  k% = (ASC(UCASE$(kbd$)) * 2)
                  kbd$ = CHR$(k%)
                    IF LEN(curr$) < MAX THEN
                        curr$ = curr$ + kbd$
                    ELSE
                        BEEP
                    END IF

                CASE ELSE
                    BEEP
            END SELECT
        END IF

    LOOP UNTIL finished

    END$ = curr$
    LOCATE , , 0
    EXIT FUNCTION


LLENARCEShowText:

    LOCATE y%, x%
    IF LEN(G$) > vis THEN
        PRINT RIGHT$(G$, vis);
    ELSE
        PRINT G$; SPACE$(vis - LEN(G$));
        LOCATE y%, x% + LEN(G$)
        END IF
    RETURN

LLENARCEGetKey:
    kbd$ = ""
    WHILE kbd$ = ""
        kbd$ = INKEY$
    WEND
    RETURN

END FUNCTION

DEFSNG A-Z
SUB LlistPr
WIDTH LPRINT 131
pag% = 1
Espere 0
KM$ = "\             \"
KK$ = "#############"
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #5 LEN = LEN(Invent)
GOSUB headlistpa
FOR i& = 100 TO LOF(5) / LEN(Invent)
30
    IF A = 0 THEN EXIT FOR
    GET #5, i&, Invent
    A = A - 1
        LPRINT USING KM$; Invent.id;        '13
        LPRINT "   ";                       '3
        LPRINT Invent.produ;                  '25
        LPRINT "     ";                       '5
        LPRINT USING AS$; Invent.preci;       '13
        LPRINT "     ";                       '5
        LPRINT USING KK$; Invent.exist;       '13
        LPRINT "     ";                       '5
        LPRINT Invent.distr;                  '20
        LPRINT STRING$(25, 32);                 '30
        LPRINT TRIM$(Invent.Clave)            '2
NEXT
LPRINT CHR$(15); STRING$(130, 196)
pag% = pag% + 1
tot& = (LOF(5) / LEN(Invent)) - 99
LPRINT "Total Registros: ";
LPRINT USING KK$; tot&;
FOR lines = 1 TO A: LPRINT : NEXT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT : LPRINT
LPRINT : LPRINT : LPRINT CHR$(18):
IF i& < LOF(5) / LEN(Invent) THEN GOSUB headlistpa: GOTO 30
CLOSE #5: EXIT SUB
EXIT SUB

headlistpa:
A = 45
LPRINT CHR$(14); TRIM$(nombre$)
LPRINT CHR$(18); Daylet$; " "; MFECHA$
LPRINT STRING$(27, 32); CHR$(14); CHR$(15); "INVENTARIO GENERAL"
LPRINT CHR$(18); TIME$; "                                                      Pag. #"; pag%
LPRINT CHR$(15); STRING$(130, 196)
LPRINT "   CODIGO       DESCRIPCION                        PRECIO            EXIST.    DISTRIBUIDOR                                  OP.";
LPRINT CHR$(15); STRING$(130, 196)
RETURN

END SUB

DEFINT A-Z
SUB PrBack
    CALL Lineas
    textocf 1, 10, 7, 7, STRING$(80, " ")
    textocf 2, 4, 14, 1, "CLASE (P/S):"
    textocf 2, 5, 14, 1, "DESCRIPCION:"
    textocf 45, 5, 14, 1, "FECHA DE VENCIMIENTO:"
    textocf 2, 6, 14, 1, "PRECIO DE VENTA:"
    textocf 45, 6, 14, 1, "COSTO:"
    textocf 2, 7, 14, 1, "EXISTENCIA ACTUAL:"
    textocf 45, 7, 14, 1, "EXISTENCIA MINIMA:"
    textocf 2, 8, 14, 1, "DISTRIBUIDOR:"
    textocf 45, 8, 14, 1, "FECHA DE COMPRA:"
    textocf 10, 10, 1, 7, "OBSERVACIONES"
FOR S = 12 TO 24
textocf 2, S, 14, 1, ">"
textocf 79, S, 14, 1, "<"
NEXT
END SUB

SUB PrLstComp
IF InventShow.clase = "P" THEN
dia% = VAL(LEFT$(TRIM$(InventShow.venci), 2))
mes% = VAL(LEFT$(RIGHT$(InventShow.venci, 5), 2))
ano% = VAL(RIGHT$(TRIM$(InventShow.venci), 2))
IF ano% < VAL(RIGHT$(TRIM$(FECCHA$), 2)) THEN textocf 42, li, 4, 7, "VENCIDO"
IF ano% = VAL(RIGHT$(TRIM$(FECCHA$), 2)) THEN
    IF mes% < VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN textocf 42, li, 4, 7, "VENCIDO"
    IF mes% = VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
        IF dia% <= VAL(LEFT$((TRIM$(FECCHA$)), 2)) THEN textocf 42, li, 4, 7, "VENCIDO"
    END IF
END IF
IF InventShow.minima >= InventShow.exist THEN textocf 50, li, 5, 7, "AGOTADO"
END IF
IF InventShow.clase = "S" THEN
    textocf 50, li, 1, 7, "-------"
    textocf 42, li, 1, 7, "-------"
END IF
END SUB

SUB Pro
salir% = 0: Pv = 7
560
CALL PrBack
textocf 28, 2, 14, 3, "INVENTARIO"
textocf 2, 25, 0, 3, "ESC: Salir  ³ F4: Listado"
Question 0, "Pr", 1
IF salir% = 1 THEN salir% = 0: EXIT SUB
IF REGNUMERO& = 0 AND REGNOMBRE$ = "" THEN GOTO 560
422
PrBack
textocf 28, 2, 14, 3, "INVENTARIO"
IF REGNUMERO& > 0 AND REGNUMERO& < 100 THEN GOSUB pmalo: GOTO 560
Espere 0
CompPr
IF InventShow.id = "             " THEN GOSUB pmalo: GOTO 560
880
ShowPr
RGMERO$ = InventShow.id
DO
kbd$ = INKEY$
SELECT CASE kbd$
    CASE IS = CHR$(27): EXIT SUB
    CASE IS = CHR$(0) + ";":
        CALL LOGIN(0)
            IF EnGtCla% = 1 THEN
                EnGtCla% = 0: ShowPr: textocf 2, 25, 0, 3, "ESC: Salir  ³" + STRING$(60, 32)
                CALL EditPr: REGNOMBRE$ = TRIM$(InventShow.id): GOTO 422
            END IF
            GOTO 880
            CASE IS = CHR$(0) + ">"
                ListPr 1, 99, 0: salir% = 0
                IF REGNUMERO& = 0 AND REGNOMBRE$ = "" THEN REGNOMBRE$ = RGMERO$
                textocf 28, 2, 14, 3, "INVENTARIO GENERAL": PrBack
                IF REGNUMERO& > 0 AND REGNUMERO& < 100 THEN GOSUB pmalo: REGNOMBRE$ = RGMERO$: REGNUMERO& = 100
                CompPr
                IF InventShow.id = "             " THEN REGNOMBRE$ = RGMERO$: REGNUMERO& = 100: GOSUB pmalo
                GOTO 422
END SELECT
LOOP

EXIT SUB

pmalo:
Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1
RETURN
END SUB

DEFSNG A-Z
SUB RegPr
220
CALL PrBack
textocf 29, 2, 14, 3, "REGISTRO DE INVENTARIO"
textocf 2, 25, 0, 3, "ESC: Salir  ³ S¢lo puede realizar hasta 10 Registros a la vez"
Alert 1, 3, 20, 7, 38, 10
 aquicf$ = ""
textocf 22, 8, 14, 3, "¨Cu ntos Invents a Registrar?:"
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #15 LEN = LEN(Invent)
LOCATE 9, 25: COLOR 15, 3:
PRINT "Total Registrados: "; (LOF(15) / LEN(Invent)) - 99; : CLOSE #15
kbd$ = LLENARCF$(55, 8, 0, 2, START$, aquicf$, 2, 2)
n = VAL(aquicf$)
IF n = 0 THEN EXIT SUB
IF n > 10 THEN
    CALL PrBack
    textocf 29, 2, 14, 3, " REGISTRO DE INVENTARIO"
    Alert 1, 3, 25, 7, 31, 10
    textocf 29, 6, 7, 4, "Error "
    textocf 26, 8, 14, 3, "   S¢lo se pueden Registrar"
    textocf 26, 9, 14, 3, "  Diez Art¡culos como M ximo"
    BEEP: SLEEP 2
    GOTO 220
END IF

FOR i = 1 TO n
Invent.clase = "": Invent.produ = "": Invent.venci = "": Invent.preci = 0
Invent.costo = 0: Invent.exist = 0: Invent.minima = 0
Invent.distr = "": Invent.venci = "dd-mm-aa"

CALL PrBack:
    textocf 29, 2, 14, 3, "REGISTRO"
    textocf 76, 2, 1, 3, Iniciales$
    Invent.Clave = Iniciales$
    OPEN "AMVSYS\AMVPR.SYS" FOR INPUT AS #2
    INPUT #2, REG&: CLOSE #2
    IF REG& = 99999999 THEN REG& = 0
    LOCATE 2, 2: COLOR 0, 3: PRINT "CODIGO #",
    REG& = REG& + 1
    Invent.id = TRIM$(STR$(REG&))
    LOCATE 2, 10: COLOR 0, 3: PRINT Invent.id

47
    textocf 15, 4, 7, 1, "            "
    kbd$ = LLENARCF$(15, 4, 0, 7, Invent.clase, aquicf$, 1, 1)
    IF UCASE$(aquicf$) <> "P" AND UCASE$(aquicf$) = "S" OR UCASE$(aquicf$) = "P" AND UCASE$(aquicf$) <> "S" THEN
    ELSE
        BEEP: GOTO 47
    END IF
    Invent.clase = UCASE$(aquicf$)
    IF Invent.clase = "P" THEN textocf 15, 4, 7, 1, "Invent"
    IF Invent.clase = "S" THEN textocf 15, 4, 7, 1, "SERVICIO"
    GOSUB Sargo
952
    kbd$ = LLENARCF$(15, 5, 0, 7, Invent.produ, aquicf$, 25, 25)
    IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 47
    GOSUB Sargo
        IF TRIM$(aquicf$) <> "" THEN
                zx = ASC(UCASE$(LEFT$(aquicf$, 1)))
                IF zx < 65 AND zx <> 60 OR zx > 90 AND zx <> 60 THEN
                        textocf 15, 5, 4, 7, "   -ERROR-    ": BEEP: SLEEP 1
                        textocf 15, 5, 0, 7, "              ": GOTO 952
                END IF
        END IF
        IF TRIM$(aquicf$) = "" THEN
                Invent.produ = "<* SIN NOMBRE*>": GOTO 952
        END IF
    Invent.produ = TRIM$(UCASE$(aquicf$))
    START$ = ""
1234
kbd$ = VERDATE$(Invent.venci, 67, 5)
 IF jl% = 1 THEN jl% = 0: GOTO 952
 GOSUB Sargo
 GOSUB regcomprobar
10
    kbd$ = STR$(LLENARCFNUM%(22, 6, 0, 7, Invent.preci, Naquicf&, 7, 7))
    GOSUB Sargo
    Invent.preci = Naquicf&
    IF jl% = 1 THEN jl% = 0: GOTO 1234
14
    kbd$ = STR$(LLENARCFNUM%(67, 6, 0, 7, Invent.costo, Naquicf&, 7, 7))
    GOSUB Sargo
    Invent.costo = Naquicf&
    IF jl% = 1 THEN jl% = 0: GOTO 10
11
    kbd$ = STR$(LLENARCFNUM%(22, 7, 0, 7, Invent.exist, Naquicf&, 7, 7))
    GOSUB Sargo
    Invent.exist = Naquicf&
    IF jl% = 1 THEN jl% = 0: GOTO 14
12
    kbd$ = STR$(LLENARCFNUM%(67, 7, 0, 7, Invent.minima, Naquicf&, 7, 7))
    GOSUB Sargo
    Invent.minima = Naquicf&
    GOSUB regcomprobar
    IF jl% = 1 THEN jl% = 0: GOTO 11
13
    kbd$ = LLENARCF$(22, 8, 0, 7, Invent.distr, aquicf$, 20, 20)
    GOSUB Sargo
    Invent.distr = (TRIM$(UCASE$(aquicf$)))
    textocf 67, 8, 0, 7, FECCHA$
    Invent.compra = (TRIM$(UCASE$(FECCHA$)))
    IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 12
    START$ = ""
    FOR i% = 1 TO 13
        InventShow.notas(i%) = "":
    NEXT
    EditNotasPr 0, 0, 1
    IF jl% = 1 THEN jl% = 0: GOTO 13
    FOR i% = 1 TO 13
        Invent.notas(i%) = InventShow.notas(i%):
    NEXT
   
    OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS WRITE AS #8 LEN = LEN(Invent)
    x& = LOF(8) / LEN(Invent) + 1
    PUT #8, x&, Invent: CLOSE #8
    COLOR 0, 3: LOCATE 25, 2: PRINT "Invent ALMACENADO EN EL REGISTRO"; x&; ". PRESIONE '" + CHR$(26) + "' PARA CONTINUAR";
    OPEN "AMVSYS\AMVPR.SYS" FOR OUTPUT AS #14: WRITE #14, REG&: CLOSE #14

    DO: LOOP UNTIL INKEY$ = CHR$(0) + "M"
NEXT i
Espere 0
ActualPr 2, 0
EXIT SUB

regcomprobar:
IF Invent.clase = "P" THEN
Pv = 7: pa = 7
dia% = VAL(LEFT$((TRIM$(Invent.venci)), 2))
mes% = VAL(LEFT$(RIGHT$(Invent.venci, 5), 2))
ano% = VAL(RIGHT$((TRIM$(Invent.venci)), 2))
IF ano% < VAL(RIGHT$(TRIM$(FECCHA$), 2)) THEN
    Pv = 15
    textocf 42, 5, 12, 1, "   ERROR EN FECHA DE VENCIMIENTO     ": BEEP: SLEEP 1
    textocf 42, 5, 15, 1, "NO DEBE SER IGUAL O MAYOR A LA ACTUAL": SLEEP 2
    textocf 42, 5, 14, 1, "   FECHA DE VENCIMIENTO:             ": GOTO 1234
END IF
IF ano% > VAL(RIGHT$((TRIM$(FECCHA$)), 2)) THEN textocf 45, 5, 14, 1, "FECHA DE VENCIMIENTO:"
IF ano% = VAL(RIGHT$((TRIM$(FECCHA$)), 2)) THEN
    IF mes% > VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
        textocf 45, 5, 14, 1, "FECHA DE VENCIMIENTO:"
    END IF
    IF mes% < VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
    Pv = 15
    textocf 42, 5, 12, 1, "   ERROR EN FECHA DE VENCIMIENTO     ": BEEP: SLEEP 1
    textocf 42, 5, 15, 1, "NO DEBE SER IGUAL O MAYOR A LA ACTUAL": SLEEP 2
    textocf 42, 5, 14, 1, "   FECHA DE VENCIMIENTO:             ": GOTO 1234
    END IF
    IF mes% = VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
    IF dia% > VAL(LEFT$((TRIM$(FECCHA$)), 2)) THEN
        textocf 45, 5, 14, 1, "FECHA DE VENCIMIENTO:"
    END IF
    IF dia% <= VAL(LEFT$((TRIM$(FECCHA$)), 2)) THEN
    Pv = 15
    BEEP: textocf 42, 5, 12, 1, "   ERROR EN FECHA DE VENCIMIENTO     ": SLEEP 1
    textocf 42, 5, 15, 1, "NO DEBE SER IGUAL O MAYOR A LA ACTUAL": SLEEP 2
    textocf 42, 5, 14, 1, "   FECHA DE VENCIMIENTO:             ": GOTO 1234
    END IF
END IF
END IF
    IF Invent.minima >= Invent.exist THEN
        aquicf$ = "":
        textocf 2, 7, 31, 1, "EXISTENCIA ACTUAL:"
        textocf 45, 7, 31, 1, "EXISTENCIA MINIMA:"
    END IF
    END IF
    IF Invent.minima = 0 AND Invent.exist = 0 OR Invent.minima < Invent.exist THEN
        pa = 7
        textocf 2, 7, 14, 1, "EXISTENCIA ACTUAL:"
        textocf 45, 7, 14, 1, "EXISTENCIA MINIMA:"
    END IF
RETURN

Sargo:
IF kbd$ = CHR$(27) OR INKEY$ = CHR$(27) THEN
     Alert 1, 3, 26, 5, 26, 7
     textocf 27, 6, 16, 3, "Registro ha sido abortado"
     SLEEP 1
     EXIT SUB
END IF
RETURN

END SUB

DEFINT A-Z
SUB SaveProd (Actualizar%)
Alert 1, 3, 30, 5, 20, 8
textocf 32, 6, 0, 3, "¨Guardar Edici¢n?"
textocf 32, 7, 8, 3, "      (S/N)"
DO
kbd$ = INKEY$
REGNOMBRE$ = TRIM$(InventShow.id)
    SELECT CASE kbd$
        CASE IS = "S", "s":
                textocf 32, 7, 15, 3, "      (S)  ": SLEEP 1
                Espere 0
                OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS WRITE AS #22 LEN = LEN(InventShow)
                PUT #22, REG&, InventShow: CLOSE #22
                IF Actualizar = 1 THEN
                Actualizar = 0
                CALL ActualPr(2, 0)
                END IF
                REGNUMERO& = RGMERO&: EXIT SUB
        CASE IS = "n", "N": textocf 32, 7, 15, 3, "        (N)": SLEEP 1
            modifica = 0: EXIT SUB
    END SELECT
LOOP
END SUB

SUB ShowNotasPr
FOR S = 12 TO 24
textocf 2, S, 14, 1, ">"
textocf 79, S, 14, 1, "<"
NEXT
textocf 3, 12, 7, 1, InventShow.notas(1)
textocf 3, 13, 7, 1, InventShow.notas(2)
textocf 3, 14, 7, 1, InventShow.notas(3)
textocf 3, 15, 7, 1, InventShow.notas(4)
textocf 3, 16, 7, 1, InventShow.notas(5)
textocf 3, 17, 7, 1, InventShow.notas(6)
textocf 3, 18, 7, 1, InventShow.notas(7)
textocf 3, 19, 7, 1, InventShow.notas(8)
textocf 3, 20, 7, 1, InventShow.notas(9)
textocf 3, 21, 7, 1, InventShow.notas(10)
textocf 3, 22, 7, 1, InventShow.notas(11)
textocf 3, 23, 7, 1, InventShow.notas(12)
textocf 3, 24, 7, 1, InventShow.notas(13)

END SUB

SUB ShowPr
CALL PrBack
textocf 2, 25, 0, 3, "ESC: Salir  ³ F1: Editar  ³ F4: Listado"
textocf 28, 2, 14, 3, "INVENTARIO"
LOCATE 2, 2: COLOR 0, 3: PRINT "CODIGO #"; InventShow.id
IF InventShow.clase = "P" THEN
    IF InventShow.minima >= InventShow.exist THEN
        textocf 2, 7, 31, 1, "ESTA AGOTADO:     "
        textocf 45, 7, 31, 1, "EXISTENCIA MINIMA:"
    END IF
END IF
LOCATE 2, 76: COLOR 1, 3: PRINT InventShow.Clave
IF InventShow.clase = "P" THEN textocf 15, 4, 7, 1, "ARTICULO"
IF InventShow.clase = "S" THEN textocf 15, 4, 7, 1, "SERVICIO"
textocf 15, 5, 7, 1, TRIM$(UCASE$(InventShow.produ))
textocf 67, 5, Pv%, 1, TRIM$(UCASE$(InventShow.venci))
textocf 22, 6, 7, 1, TRIM$(STR$(InventShow.preci))
textocf 67, 6, 7, 1, TRIM$(STR$(InventShow.costo))
textocf 22, 7, 7, 1, TRIM$(STR$(InventShow.exist))
textocf 67, 7, 7, 1, TRIM$(STR$(InventShow.minima))
textocf 22, 8, 7, 1, TRIM$(UCASE$(InventShow.distr))
textocf 67, 8, 7, 1, TRIM$(UCASE$(InventShow.compra))
FOR i% = 1 TO 13
        textocf 3, 11 + i%, 7, 1, InventShow.notas(i%)
NEXT
END SUB

