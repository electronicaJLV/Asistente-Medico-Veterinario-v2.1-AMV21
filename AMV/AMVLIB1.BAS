DECLARE SUB Alert (cl1%, cl2%, X1%, Y1%, X2%, Y2%)
DECLARE SUB BOX (cl1%, cl2%, X1%, Y1%, X2%, Y2%, WHERE%)
DECLARE FUNCTION CODEDATE$ ()
DECLARE FUNCTION Daylet$ ()
DECLARE SUB DECC (I$, f$)
DECLARE SUB Drawbox (UpRow%, LtCol%, LoRow%, RtCol%)
DECLARE SUB Drawbox2 (UpRow%, LtCol%, LoRow%, RtCol%)
DECLARE FUNCTION FECCHA$ ()
DECLARE FUNCTION fillhand$ (x%, y%, letras%, Fondo%, START$, END$, vis%, MAX%)
DECLARE FUNCTION LLENARCF$ (x%, y%, letras%, Fondo%, START$, END$, vis%, MAX%)
DECLARE FUNCTION LLENARCFNUM% (x%, y%, letras%, Fondo%, Nstart&, Nend&, vis%, MAX%)
DECLARE FUNCTION MFECHA$ ()
DECLARE FUNCTION MMES$ ()
DECLARE SUB Pause (ticks%)
DECLARE FUNCTION Password$ (x%, y%, letras%, Fondo%, START$, END$, vis%, MAX%)
DECLARE SUB SCREENCLEAR ()
DECLARE SUB scrollup ()
DECLARE SUB scrollupall ()
DECLARE SUB scrolldown ()
DECLARE SUB scrolldownall ()
DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE FUNCTION TRIM$ (vari$)
DECLARE SUB UnScroll (UpRow%, LtCol%, LoRow%, RtCol%)
DECLARE FUNCTION VERDATE$ (who$, x%, y%)
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'

DEFINT A-Z
SUB Alert (cl1%, cl2%, X1%, Y1%, X2%, Y2%) STATIC
textocf X1, Y1 - 1, 15, 0, " - "
textocf X1 + 3, Y1 - 1, 7, 4, " A.M.V." + STRING$(X2 - 9, " ")
textocf X1, Y1, cl1, cl2, "Ú" + STRING$(X2 - 1, "Ä") + "¿"
FOR A = Y1 + 1 TO Y2
textocf X1, A, cl1, cl2, "³" + SPACE$(X2 - 1) + "³": NEXT A

textocf X1, Y2, cl1, cl2, "À" + STRING$(X2 - 1, "Ä") + "Ù"

FOR A = X1 + 2 TO (X1 + X2)
    LOCATE Y2 + 1, A: COLOR 8, 0: PRINT CHR$(SCREEN(Y2 + 1, A));
NEXT

FOR A = Y1 TO Y2 + 1
    LOCATE A, X1 + X2 + 1: COLOR 8, 0: PRINT CHR$(SCREEN(A, X1 + X2 + 1));
    PRINT CHR$(SCREEN(A, X1 + X2 + 2))
NEXT

END SUB

SUB BOX (cl1%, cl2%, X1%, Y1%, X2%, Y2%, WHERE%)
LOCATE Y1, X1: COLOR cl1, cl2: PRINT "Ú" + STRING$(X2 - 1, "Ä") + "¿";
FOR A = Y1 + 1 TO Y2 - 1
    LOCATE A, X1: COLOR cl1, cl2: PRINT "³"; : LOCATE A, X1 + X2: COLOR cl1, cl2: PRINT "³";
    IF WHERE% > 0 THEN LOCATE WHERE% + 1, X1: COLOR cl1, cl2: PRINT CHR$(195) + STRING$(X2 - 1, 196) + CHR$(180);
NEXT A
LOCATE Y2, X1: COLOR cl1, cl2: PRINT "À" + STRING$(X2 - 1, "Ä") + "Ù";

FOR A = X1 + 2 TO (X1 + X2)
    LOCATE Y2 + 1, A: COLOR 8, 0: PRINT CHR$(SCREEN(Y2 + 1, A));
NEXT

FOR A = Y1 + 1 TO Y2 + 1
    LOCATE A, X1 + X2 + 1: COLOR 8, 0: PRINT CHR$(SCREEN(A, X1 + X2 + 1));
    PRINT CHR$(SCREEN(A, X1 + X2 + 2))
NEXT
END SUB

FUNCTION CODEDATE$
    ano$ = RIGHT$(DATE$, 2)
    dia$ = LEFT$(RIGHT$(DATE$, 7), 2)
    mes$ = LEFT$(DATE$, 2)
    CODEDATE$ = dia$ + mes$ + ano$
END FUNCTION

FUNCTION Daylet$
DIM Year(0 TO 27, 1 TO 12)
ano$ = RIGHT$(DATE$, 4)
dia$ = LEFT$(RIGHT$(DATE$, 7), 2)
mes$ = LEFT$(DATE$, 2)
Year(0, 1) = 5
Year(0, 2) = 1
Year(0, 3) = 2
Year(0, 4) = 5
Year(0, 5) = 0
Year(0, 6) = 3
Year(0, 7) = 5
Year(0, 8) = 1
Year(0, 9) = 4
Year(0, 10) = 6
Year(0, 11) = 2
Year(0, 12) = 4

Year(1, 1) = 0
Year(1, 2) = 3
Year(1, 3) = 3
Year(1, 4) = 6
Year(1, 5) = 1
Year(1, 6) = 4
Year(1, 7) = 6
Year(1, 8) = 2
Year(1, 9) = 5
Year(1, 10) = 0
Year(1, 11) = 3
Year(1, 12) = 5

Year(2, 1) = 1
Year(2, 2) = 4
Year(2, 3) = 4
Year(2, 4) = 0
Year(2, 5) = 2
Year(2, 6) = 5
Year(2, 7) = 0
Year(2, 8) = 3
Year(2, 9) = 6
Year(2, 10) = 1
Year(2, 11) = 4
Year(2, 12) = 6

Year(3, 1) = 2
Year(3, 2) = 5
Year(3, 3) = 5
Year(3, 4) = 1
Year(3, 5) = 3
Year(3, 6) = 6
Year(3, 7) = 1
Year(3, 8) = 4
Year(3, 9) = 0
Year(3, 10) = 2
Year(3, 11) = 5
Year(3, 12) = 0

Year(4, 1) = 3
Year(4, 2) = 6
Year(4, 3) = 0
Year(4, 4) = 3
Year(4, 5) = 5
Year(4, 6) = 1
Year(4, 7) = 3
Year(4, 8) = 6
Year(4, 9) = 2
Year(4, 10) = 4
Year(4, 11) = 0
Year(4, 12) = 2

Year(5, 1) = 5
Year(5, 2) = 1
Year(5, 3) = 1
Year(5, 4) = 4
Year(5, 5) = 6
Year(5, 6) = 2
Year(5, 7) = 4
Year(5, 8) = 0
Year(5, 9) = 3
Year(5, 10) = 5
Year(5, 11) = 1
Year(5, 12) = 3

Year(6, 1) = 6
Year(6, 2) = 2
Year(6, 3) = 2
Year(6, 4) = 5
Year(6, 5) = 0
Year(6, 6) = 3
Year(6, 7) = 5
Year(6, 8) = 1
Year(6, 9) = 4
Year(6, 10) = 6
Year(6, 11) = 2
Year(6, 12) = 4

Year(7, 1) = 0
Year(7, 2) = 3
Year(7, 3) = 3
Year(7, 4) = 6
Year(7, 5) = 1
Year(7, 6) = 4
Year(7, 7) = 6
Year(7, 8) = 2
Year(7, 9) = 5
Year(7, 10) = 0
Year(7, 11) = 3
Year(7, 12) = 5

Year(8, 1) = 1
Year(8, 2) = 4
Year(8, 3) = 5
Year(8, 4) = 1
Year(8, 5) = 3
Year(8, 6) = 6
Year(8, 7) = 1
Year(8, 8) = 4
Year(8, 9) = 0
Year(8, 10) = 2
Year(8, 11) = 5
Year(8, 12) = 0

Year(9, 1) = 3
Year(9, 2) = 6
Year(9, 3) = 6
Year(9, 4) = 2
Year(9, 5) = 4
Year(9, 6) = 0
Year(9, 7) = 2
Year(9, 8) = 5
Year(9, 9) = 1
Year(9, 10) = 3
Year(9, 11) = 6
Year(9, 12) = 1

Year(10, 1) = 4
Year(10, 2) = 0
Year(10, 3) = 0
Year(10, 4) = 3
Year(10, 5) = 5
Year(10, 6) = 1
Year(10, 7) = 3
Year(10, 8) = 6
Year(10, 9) = 2
Year(10, 10) = 4
Year(10, 11) = 0
Year(10, 12) = 2

Year(11, 1) = 5
Year(11, 2) = 1
Year(11, 3) = 1
Year(11, 4) = 4
Year(11, 5) = 6
Year(11, 6) = 2
Year(11, 7) = 4
Year(11, 8) = 0
Year(11, 9) = 3
Year(11, 10) = 5
Year(11, 11) = 1
Year(11, 12) = 3

Year(12, 1) = 6
Year(12, 2) = 2
Year(12, 3) = 3
Year(12, 4) = 6
Year(12, 5) = 1
Year(12, 6) = 4
Year(12, 7) = 6
Year(12, 8) = 2
Year(12, 9) = 5
Year(12, 10) = 0
Year(12, 11) = 3
Year(12, 12) = 5

Year(13, 1) = 1
Year(13, 2) = 4
Year(13, 3) = 4
Year(13, 4) = 0
Year(13, 5) = 2
Year(13, 6) = 5
Year(13, 7) = 0
Year(13, 8) = 3
Year(13, 9) = 6
Year(13, 10) = 1
Year(13, 11) = 4
Year(13, 12) = 6

Year(14, 1) = 2
Year(14, 2) = 5
Year(14, 3) = 5
Year(14, 4) = 1
Year(14, 5) = 3
Year(14, 6) = 6
Year(14, 7) = 1
Year(14, 8) = 4
Year(14, 9) = 0
Year(14, 10) = 2
Year(14, 11) = 5
Year(14, 12) = 0

Year(15, 1) = 3
Year(15, 2) = 6
Year(15, 3) = 6
Year(15, 4) = 2
Year(15, 5) = 4
Year(15, 6) = 0
Year(15, 7) = 2
Year(15, 8) = 5
Year(15, 9) = 1
Year(15, 10) = 3
Year(15, 11) = 6
Year(15, 12) = 1

Year(16, 1) = 4
Year(16, 2) = 0
Year(16, 3) = 1
Year(16, 4) = 4
Year(16, 5) = 6
Year(16, 6) = 2
Year(16, 7) = 4
Year(16, 8) = 0
Year(16, 9) = 3
Year(16, 10) = 5
Year(16, 11) = 1
Year(16, 12) = 3

Year(17, 1) = 6
Year(17, 2) = 2
Year(17, 3) = 2
Year(17, 4) = 5
Year(17, 5) = 0
Year(17, 6) = 3
Year(17, 7) = 5
Year(17, 8) = 1
Year(17, 9) = 4
Year(17, 10) = 6
Year(17, 11) = 2
Year(17, 12) = 4

Year(18, 1) = 0
Year(18, 2) = 3
Year(18, 3) = 3
Year(18, 4) = 6
Year(18, 5) = 1
Year(18, 6) = 4
Year(18, 7) = 6
Year(18, 8) = 2
Year(18, 9) = 5
Year(18, 10) = 0
Year(18, 11) = 3
Year(18, 12) = 5

Year(19, 1) = 1
Year(19, 2) = 4
Year(19, 3) = 4
Year(19, 4) = 0
Year(19, 5) = 2
Year(19, 6) = 5
Year(19, 7) = 0
Year(19, 8) = 3
Year(19, 9) = 6
Year(19, 10) = 1
Year(19, 11) = 4
Year(19, 12) = 6

Year(20, 1) = 2
Year(20, 2) = 5
Year(20, 3) = 6
Year(20, 4) = 2
Year(20, 5) = 4
Year(20, 6) = 0
Year(20, 7) = 2
Year(20, 8) = 5
Year(20, 9) = 1
Year(20, 10) = 3
Year(20, 11) = 6
Year(20, 12) = 1

Year(21, 1) = 4
Year(21, 2) = 0
Year(21, 3) = 0
Year(21, 4) = 3
Year(21, 5) = 5
Year(21, 6) = 1
Year(21, 7) = 3
Year(21, 8) = 6
Year(21, 9) = 2
Year(21, 10) = 4
Year(21, 11) = 0
Year(21, 12) = 2

Year(22, 1) = 5
Year(22, 2) = 1
Year(22, 3) = 1
Year(22, 4) = 4
Year(22, 5) = 6
Year(22, 6) = 2
Year(22, 7) = 4
Year(22, 8) = 0
Year(22, 9) = 3
Year(22, 10) = 5
Year(22, 11) = 1
Year(22, 12) = 3

Year(23, 1) = 6
Year(23, 2) = 2
Year(23, 3) = 2
Year(23, 4) = 5
Year(23, 5) = 0
Year(23, 6) = 3
Year(23, 7) = 5
Year(23, 8) = 1
Year(23, 9) = 4
Year(23, 10) = 6
Year(23, 11) = 2
Year(23, 12) = 4

Year(24, 1) = 0
Year(24, 2) = 3
Year(24, 3) = 4
Year(24, 4) = 0
Year(24, 5) = 2
Year(24, 6) = 5
Year(24, 7) = 0
Year(24, 8) = 3
Year(24, 9) = 6
Year(24, 10) = 1
Year(24, 11) = 4
Year(24, 12) = 6

Year(25, 1) = 2
Year(25, 2) = 5
Year(25, 3) = 5
Year(25, 4) = 1
Year(25, 5) = 3
Year(25, 6) = 6
Year(25, 7) = 1
Year(25, 8) = 4
Year(25, 9) = 0
Year(25, 10) = 2
Year(25, 11) = 5
Year(25, 12) = 0

Year(26, 1) = 3
Year(26, 2) = 6
Year(26, 3) = 6
Year(26, 4) = 2
Year(26, 5) = 4
Year(26, 6) = 0
Year(26, 7) = 2
Year(26, 8) = 5
Year(26, 9) = 1
Year(26, 10) = 3
Year(26, 11) = 6
Year(26, 12) = 1

Year(27, 1) = 4
Year(27, 2) = 0
Year(27, 3) = 0
Year(27, 4) = 3
Year(27, 5) = 5
Year(27, 6) = 1
Year(27, 7) = 3
Year(27, 8) = 6
Year(27, 9) = 2
Year(27, 10) = 4
Year(27, 11) = 0
Year(27, 12) = 2

y = VAL(ano$) - (INT(VAL(ano$) / 28) * 28)
D = VAL(dia$) + Year(y, VAL(mes$))

DO
IF D < 8 THEN EXIT DO
D = D - 7
LOOP
IF D = 1 THEN Daylet$ = "Domingo"
IF D = 2 THEN Daylet$ = "Lunes"
IF D = 3 THEN Daylet$ = "Martes"
IF D = 4 THEN Daylet$ = "Mi‚rcoles"
IF D = 5 THEN Daylet$ = "Jueves"
IF D = 6 THEN Daylet$ = "Viernes"
IF D = 7 THEN Daylet$ = "S bado"
END FUNCTION

SUB DECC (I$, f$)
f$ = ""
FOR B = 1 TO LEN(I$)
    n% = ASC(RIGHT$(I$, B)) / 2
        IF n% = 17 THEN
            n% = 0
        END IF
    f$ = CHR$(n%) + f$
NEXT
f$ = TRIM$(f$)
END SUB

 SUB Drawbox (UpRow%, LtCol%, LoRow%, RtCol%) STATIC
    Wide% = (RtCol% - LtCol%) - 1
    LOCATE UpRow%, LtCol%
    PRINT CHR$(201); STRING$(Wide%, CHR$(205)); CHR$(187);
    FOR I% = UpRow% + 1 TO LoRow% - 1
        LOCATE I, LtCol%: COLOR 1, 1: PRINT CHR$(186); SPACE$(Wide%); CHR$(186);
    NEXT I%
    LOCATE LoRow%, LtCol%
    PRINT CHR$(200); STRING$(Wide%, CHR$(205)); CHR$(188);
 END SUB

SUB Drawbox2 (UpRow%, LtCol%, LoRow%, RtCol%) STATIC
    Wide% = (RtCol% - LtCol%) - 1
    LOCATE UpRow%, LtCol%
    PRINT CHR$(201); STRING$(Wide%, CHR$(205)); CHR$(187);
        LOCATE 1, LtCol%: COLOR 7, 7: PRINT CHR$(186); SPACE$(Wide%); CHR$(186);
        LOCATE 2, LtCol%: COLOR 3, 3: PRINT CHR$(186); SPACE$(Wide%); CHR$(186);
    FOR I% = UpRow% + 1 TO LoRow% - 1
        LOCATE I, LtCol%: COLOR 1, 1: PRINT CHR$(186); SPACE$(Wide%); CHR$(186);
    NEXT I%
        LOCATE 25, LtCol%: COLOR 3, 3: PRINT CHR$(186); SPACE$(Wide%); CHR$(186);
  
    LOCATE LoRow%, LtCol%
    PRINT CHR$(200); STRING$(Wide%, CHR$(205)); CHR$(188);

END SUB

FUNCTION FECCHA$
    ano$ = RIGHT$(DATE$, 2)
    dia$ = LEFT$(RIGHT$(DATE$, 7), 2)
    mes$ = LEFT$(DATE$, 2)
    FECCHA$ = dia$ + CHR$(45) + mes$ + CHR$(45) + ano$
END FUNCTION

FUNCTION fillhand$ (x, y, letras, Fondo, START$, END$, vis, MAX)
CURR$ = "":
END$ = ""
CONST TRUE = -1
CONST FALSE = NOT TRUE
    CURR$ = RTRIM$(ST$ + START$)
    IF CURR$ = CHR$(8) THEN CURR$ = ""
    COLOR letras, Fondo
    g = LEN(RTRIM$(START$))
    LOCATE , , 1, 3, 15
    IF alba% = 0 THEN
       LOCATE , , 1, 3, 5
    END IF
    finished = FALSE
    DO
        GOSUB fillhandShowText
        GOSUB fillhandGetKey

        IF LEN(KBD$) > 2 THEN
            finished = TRUE
            fillhand$ = KBD$
        ELSE
            SELECT CASE KBD$
                CASE CHR$(0) + ";":
                CURR$ = FECCHA$ + ":" + CURR$
                CASE CHR$(0) + "S":
                        CURR$ = MID$(CURR$, 1, g) + RIGHT$(CURR$, ABS(LEN(CURR$) - LEN(MID$(CURR$, 1, g + 1))))
                CASE CHR$(13), CHR$(27), CHR$(0) + CHR$(15)
                    finished = TRUE
                    fillhand$ = KBD$
                CASE CHR$(0) + ">":
                    finished = TRUE
                    fillhand$ = KBD$
                CASE CHR$(0) + "H", CHR$(0) + "P", CHR$(0) + "I", CHR$(0) + "Q"
                    finished = TRUE
                    fillhand$ = KBD$
                CASE CHR$(8)
                    IF CURR$ <> "" THEN
                    IF g <> 0 THEN
                      g = g - 1
                         IF g < 1 THEN
                                g = 0
                         END IF
                         CURR$ = MID$(CURR$, 1, g) + RIGHT$(CURR$, ABS(LEN(CURR$) - LEN(MID$(CURR$, 1, g + 1))))
                      END IF
                    END IF
                CASE CHR$(0) + "G"
                    g = x - 3
                    IF g > vis THEN
                        finished = TRUE
                        fillhand$ = KBD$
                   END IF
                    IF g > LEN(CURR$) THEN CURR$ = CURR$ + " "
                CASE CHR$(0) + "O"
                    g = LEN(CURR$)
                    IF g > vis THEN
                        finished = TRUE
                        fillhand$ = KBD$
                   END IF
                    IF g > LEN(CURR$) THEN CURR$ = CURR$ + " "

                CASE CHR$(0) + "M"
                    g = g + 1
                    IF g > vis THEN
                        finished = TRUE
                        fillhand$ = KBD$
                   END IF
                    IF g > LEN(CURR$) THEN CURR$ = CURR$ + " "
                CASE CHR$(9)
                    g = g + 5
                    IF g > vis THEN
                        finished = TRUE
                        fillhand$ = KBD$
                   END IF
                    IF g > LEN(CURR$) THEN CURR$ = CURR$ + "     "
                CASE CHR$(0) + "K"
                    g = g - 1
                    IF g < 0 THEN
                        finished = TRUE
                        fillhand$ = KBD$
                        g = 0
                    END IF
                CASE CHR$(0) + "R"
                alba% = alba% + 1
                LOCATE , , 1, 1, 15
                IF alba% > 1 THEN
                        alba% = 0
                        LOCATE , , 1, 3, 5
                END IF
                CASE CHR$(32) TO CHR$(173)
                            lr = LEN(CURR$) - g - alba%
                            IF lr < 1 THEN
                               lr = 0
                            END IF
                            CURR$ = LEFT$(CURR$, g) + KBD$ + RIGHT$(CURR$, lr)
                            g = g + 1
                            IF g > vis THEN
                                g = vis
                            END IF
                            IF vis = LEN(CURR$) - 1 THEN
                              CURR$ = LEFT$(CURR$, vis)
                            END IF
                CASE ELSE
            END SELECT
        END IF
    LOOP UNTIL finished
    END$ = CURR$
    LOCATE , , 0
    g = 0:
    lr = 0
START$ = ""
EXIT FUNCTION


fillhandShowText:
     LOCATE y, x
     IF LEN(CURR$) > vis THEN
            finished = TRUE
            fillhand$ = KBD$
     ELSE
        PRINT CURR$; SPACE$(vis - LEN(CURR$));
     END IF
        LOCATE y, x + g
    RETURN

fillhandGetKey:
    KBD$ = ""
    WHILE KBD$ = ""
        KBD$ = INKEY$
        IF KBD$ <> "" THEN
        END IF
 WEND
RETURN
END FUNCTION

FUNCTION LLENARCF$ (x, y, letras, Fondo, START$, END$, vis, MAX) STATIC
CURR$ = "": g = 0
END$ = ""
CONST TRUE = -1
CONST FALSE = NOT TRUE
    CURR$ = RTRIM$(START$)
    IF CURR$ = CHR$(8) THEN CURR$ = ""
    COLOR letras, Fondo
    LOCATE , , 1, 1, 15
    IF alba% = 0 THEN LOCATE , , 1, 3, 5
    finished = FALSE
    DO
        GOSUB LLENARCFShowText: GOSUB LLENARCFGetKey
        IF LEN(KBD$) > 2 THEN
            finished = TRUE: LLENARCF$ = KBD$
        ELSE
            SELECT CASE KBD$
                CASE CHR$(0) + "G"
                    IF LEN(CURR$) <> 0 THEN
                        g = 0
                        IF g < 1 THEN g = 0
                    END IF
                    IF g > LEN(CURR$) THEN CURR$ = CURR$ + " "
                CASE CHR$(0) + "O"
                    g = LEN(CURR$)
                    IF g > vis THEN
                        finished = TRUE
                        LLENARCF$ = KBD$
                   END IF
                    IF g > LEN(CURR$) THEN CURR$ = CURR$ + " "
                CASE CHR$(0) + "S":
                      CURR$ = MID$(CURR$, 1, g) + RIGHT$(CURR$, ABS(LEN(CURR$) - LEN(MID$(CURR$, 1, g + 1))))
                CASE CHR$(13), CHR$(27), CHR$(9), CHR$(0) + ">", CHR$(0) + CHR$(15)
                    finished = TRUE: LLENARCF$ = KBD$
                CASE CHR$(8)
                    IF CURR$ <> "" THEN
                    IF g <> 0 THEN
                      g = g - 1
                         IF g < 1 THEN g = 0
                         CURR$ = MID$(CURR$, 1, g) + RIGHT$(CURR$, ABS(LEN(CURR$) - LEN(MID$(CURR$, 1, g + 1))))
                      END IF
                    END IF
                CASE CHR$(0) + "M"
                    g = g + 1
                    IF g > LEN(CURR$) THEN g = LEN(CURR$)
                CASE CHR$(0) + "K"
                    IF LEN(CURR$) <> 0 THEN
                        g = g - 1
                        IF g < 1 THEN g = 0
                    END IF
                CASE CHR$(0) + "R"
                alba% = alba% + 1: LOCATE , , 1, 1, 15
                IF alba% > 1 THEN
                        alba% = 0: LOCATE , , 1, 3, 5
                END IF
                CASE CHR$(32) TO CHR$(173)
                            lr = LEN(CURR$) - g - alba%
                            IF lr < 1 THEN lr = 0
                            CURR$ = LEFT$(CURR$, g) + KBD$ + RIGHT$(CURR$, lr)
                            g = g + 1
                            IF g > vis THEN g = vis
                            IF vis = LEN(CURR$) - 1 THEN CURR$ = LEFT$(CURR$, vis)
                CASE ELSE
            END SELECT
        END IF
    LOOP UNTIL finished
    END$ = CURR$: LOCATE , , 0
    g = 0: lr = 0
    EXIT FUNCTION
LLENARCFShowText:
     LOCATE y, x
     IF LEN(CURR$) > vis THEN
         PRINT RIGHT$(CURR$, vis);
     ELSE
        PRINT CURR$; SPACE$(vis - LEN(CURR$));
     END IF
    COLOR letras, Fondo
        LOCATE y, x + g
    RETURN

LLENARCFGetKey:
    KBD$ = ""
    WHILE KBD$ = ""
        KBD$ = INKEY$

    WEND
RETURN
END FUNCTION

FUNCTION LLENARCFNUM% (x%, y%, letras%, Fondo%, Nstart&, Nend&, vis%, MAX%)
CONST TRUE = -1
CONST FALSE = NOT TRUE
COLOR letras%, Fondo%
    CURR$ = LTRIM$(LEFT$(STR$(Nstart&), MAX))
    IF LEFT$(CURR$, 1) = "0" THEN CURR$ = RIGHT$(CURR$, LEN(CURR$) - 1)
    IF CURR$ = CHR$(0) THEN CURR$ = ""
    LOCATE , , 1, 3, 5: finished = FALSE
    DO
        GOSUB LLENARCFNUMShowText: GOSUB LLENARCFNUMGetKey
        IF LEN(KBD$) > 2 THEN
            finished = TRUE:  LLENARCFNUM% = VAL(KBD$)
        ELSE
            SELECT CASE KBD$
                CASE CHR$(13), CHR$(27), CHR$(9), "+", "-", "*", "/"
                        finished = TRUE: LLENARCFNUM% = VAL(KBD$)
                CASE CHR$(0) + CHR$(15): jl% = 1
                        finished = TRUE: LLENARCFNUM% = VAL(KBD$)
                CASE CHR$(8)
                    IF CURR$ <> "" THEN CURR$ = LEFT$(CURR$, LEN(CURR$) - 1)
                CASE CHR$(0) + "K"
                    IF CURR$ <> "" THEN CURR$ = LEFT$(CURR$, LEN(CURR$) - 1)
                CASE CHR$(0) + "M"
                    IF LEN(CURR$) < MAX THEN CURR$ = CURR$ + " "
                CASE CHR$(45): CURR$ = CHR$(45) + CURR$
                CASE "0" TO "9"
                IF LEN(CURR$) < MAX THEN
                        CURR$ = CURR$ + KBD$
                    ELSE
                        BEEP
                    END IF
                CASE ELSE
                    BEEP
            END SELECT
        END IF
    LOOP UNTIL finished
    Nend& = VAL(CURR$): LOCATE , , 0: EXIT FUNCTION

LLENARCFNUMShowText:
    LOCATE y%, x%
    IF LEN(CURR$) > vis THEN
        PRINT RIGHT$(CURR$, vis);
    ELSE
        PRINT CURR$; SPACE$(vis - LEN(CURR$));
        LOCATE y%, x% + LEN(CURR$)
    END IF
RETURN

LLENARCFNUMGetKey:
    KBD$ = ""
    WHILE KBD$ = ""
       KBD$ = INKEY$
   WEND
RETURN

END FUNCTION

FUNCTION MFECHA$
    ano$ = RIGHT$(DATE$, 4)
    dia$ = LEFT$(RIGHT$(DATE$, 7), 2)
    mes$ = LEFT$(DATE$, 2)
IF VAL(mes$) = 1 THEN
    mes$ = "Enero"
    ELSEIF VAL(mes$) = 2 THEN mes$ = "Febrero"
    ELSEIF VAL(mes$) = 3 THEN mes$ = "Marzo"
    ELSEIF VAL(mes$) = 4 THEN mes$ = "Abril"
    ELSEIF VAL(mes$) = 5 THEN mes$ = "Mayo"
    ELSEIF VAL(mes$) = 6 THEN mes$ = "Junio"
    ELSEIF VAL(mes$) = 7 THEN mes$ = "Julio"
    ELSEIF VAL(mes$) = 8 THEN mes$ = "Agosto"
    ELSEIF VAL(mes$) = 9 THEN mes$ = "Septiembre"
    ELSEIF VAL(mes$) = 10 THEN mes$ = "Octubre"
    ELSEIF VAL(mes$) = 11 THEN mes$ = "Noviembre"
    ELSEIF VAL(mes$) = 12 THEN mes$ = "Diciembre"
END IF
    MFECHA$ = dia$ + " de " + mes$ + " del " + ano$
END FUNCTION

DEFSNG A-Z
FUNCTION MMES$
    mes$ = LEFT$(DATE$, 2)
IF VAL(mes$) = 1 THEN
    mes$ = "Enero"
    ELSEIF VAL(mes$) = 2 THEN mes$ = "Febrero"
    ELSEIF VAL(mes$) = 3 THEN mes$ = "Marzo"
    ELSEIF VAL(mes$) = 4 THEN mes$ = "Abril"
    ELSEIF VAL(mes$) = 5 THEN mes$ = "Mayo"
    ELSEIF VAL(mes$) = 6 THEN mes$ = "Junio"
    ELSEIF VAL(mes$) = 7 THEN mes$ = "Julio"
    ELSEIF VAL(mes$) = 8 THEN mes$ = "Agosto"
    ELSEIF VAL(mes$) = 9 THEN mes$ = "Septiembre"
    ELSEIF VAL(mes$) = 10 THEN mes$ = "Octubre"
    ELSEIF VAL(mes$) = 11 THEN mes$ = "Nobiembre"
    ELSEIF VAL(mes$) = 12 THEN mes$ = "Diciembre"
END IF
    MMES$ = mes$
END FUNCTION

DEFINT A-Z
FUNCTION Password$ (x%, y%, letras%, Fondo%, START$, END$, vis%, MAX%) STATIC
JU$ = ""
CONST TRUE = -1
CONST FALSE = NOT TRUE
    CURR$ = LTRIM$(LEFT$(START$, MAX))
    IF CURR$ = CHR$(8) THEN CURR$ = ""
    COLOR letras, Fondo
    LOCATE , , 1, 3, 5
    finished = FALSE
    DO
        GOSUB PassShowText
        GOSUB PassGetKey

        IF LEN(KBD$) > 2 THEN
            finished = TRUE
            Password$ = KBD$
        ELSE
            SELECT CASE KBD$
                CASE CHR$(13), CHR$(27), CHR$(9)
                    finished = TRUE
                    Password$ = KBD$
                CASE CHR$(0) + ">":
                    finished = TRUE
                    Password$ = KBD$

                CASE CHR$(8)
                    IF CURR$ <> "" THEN
                      CURR$ = LEFT$(CURR$, LEN(CURR$) - 1)
                     JU$ = LEFT$(JU$, LEN(JU$) - 1)
                    END IF
              
                CASE CHR$(0) + "M"
                    IF LEN(CURR$) < MAX THEN
                    JU$ = JU$ + " "
                    CURR$ = CURR$ + " "
                    END IF
                CASE CHR$(0) + "K"
                    IF LEN(CURR$) <> 0 THEN
                    JU$ = LEFT$(JU$, LEN(JU$) - 1)
                    CURR$ = LEFT$(CURR$, LEN(CURR$) - 1)
                    END IF
              
                CASE ":": CURR$ = CURR$ + CHR$(164)
                CASE ";": CURR$ = CURR$ + CHR$(164)
                CASE "<": CURR$ = CURR$ + ";"
                CASE ">": CURR$ = CURR$ + ":"
                CASE " " TO "}"
                    IF LEN(CURR$) < MAX THEN
                        CURR$ = CURR$ + KBD$
                        JU$ = JU$ + "*"
                    ELSE
                        BEEP
                    END IF

                CASE ELSE
                    BEEP
            END SELECT
        END IF

    LOOP UNTIL finished

    END$ = CURR$
    LOCATE , , 0
    EXIT FUNCTION


PassShowText:
    COLOR 0
    LOCATE y, x
    IF LEN(JU$) > vis THEN
        PRINT RIGHT$(JU$, vis);
    ELSE
        PRINT JU$; SPACE$(vis - LEN(JU$));
        LOCATE y, x + LEN(CURR$)
        END IF
    RETURN

PassGetKey:
    KBD$ = ""
    WHILE KBD$ = ""
        KBD$ = INKEY$
        IF KBD$ <> "" THEN
        END IF

 WEND
RETURN

END FUNCTION

SUB Pause (ticks%)
    DEF SEG = 0
    DO UNTIL TestTick% > ticks%
        IF LEN(INKEY$) THEN EXIT SUB
        LastTick% = GetTick%: GetTick% = PEEK(&H46C)
        IF LastTick% <> GetTick% THEN
            TestTick% = TestTick% + 1
        END IF
    LOOP
    DEF SEG
END SUB

 SUB SCREENCLEAR
    LOCATE , , 0
    DIM Lines$(1 TO 23)
    Lines$(1) = STRING$(80, CHR$(196))
    Sp% = 2
    Length% = 76
    FOR I% = 2 TO 21
        Lines$(I%) = SPACE$(Sp%) + STRING$(Length%, CHR$(196)) + "  "
        Sp% = Sp% + 2
        Length% = Length% - 4
    NEXT I%
    Lines$(22) = SPACE$(39) + CHR$(179) + SPACE$(2)
    Lines$(23) = SPACE$(39) + "." + SPACE$(2)
    COLOR 0, 0
    x% = 1
    y% = 25
    FOR I% = 1 TO 12
        LOCATE y%, 1
        PRINT STRING$(80, CHR$(32));
        LOCATE x%, 1
        PRINT STRING$(80, CHR$(32));
        Pause 1
        x% = x% + 1
        y% = y% - 1
    NEXT I%
    COLOR 4, 0
    FOR I% = 1 TO 23
        LOCATE 13, 1
        PRINT Lines$(I%);
        Pause 1
    NEXT I%
    COLOR 7
    LOCATE , , 1, 6, 7
    CLS
 END SUB

SUB textocf (txx%, txy%, txcol%, txcolf%, que$) STATIC
LOCATE txy%, txx%: COLOR txcol%, txcolf%: PRINT que$;
END SUB

FUNCTION TRIM$ (vari$)
TRIM$ = LTRIM$(RTRIM$(vari$))
END FUNCTION

 SUB UnScroll (UpRow%, LtCol%, LoRow%, RtCol%)
    RowCenter% = ((LoRow% - UpRow%) \ 2) + UpRow%
    ColCenter% = ((RtCol% - LtCol%) \ 2) + LtCol%
    UprRow% = RowCenter%: LeftCol% = ColCenter%
    LwrRow% = RowCenter%: RghtCol% = ColCenter%
    DO
        UprRow% = UprRow% - 1
        LwrRow% = LwrRow% + 1
        IF UprRow% < UpRow% THEN UprRow% = UpRow%
        IF LwrRow% >= LoRow% THEN LwrRow% = LoRow%
        Drawbox UprRow%, LeftCol% - 1, LwrRow%, RghtCol% + 1
        Pause 1
        IF UprRow% = UpRow% AND LwrRow% = LoRow% THEN EXIT DO
    LOOP
    DO
        LeftCol% = LeftCol% - 1
        RghtCol% = RghtCol% + 1
        IF LeftCol% < LtCol% THEN LeftCol% = LtCol%
        IF RghtCol% > RtCol% THEN RghtCol% = RtCol%
        Drawbox2 UprRow%, LeftCol%, LwrRow%, RghtCol%
        Pause 1
        IF LeftCol% = LtCol% AND RghtCol% = RtCol% THEN EXIT DO
    LOOP
 END SUB

FUNCTION VERDATE$ (who$, x%, y%)
dia$ = TRIM$(LEFT$(TRIM$(LCASE$(who$)), 2))
mes$ = TRIM$(LEFT$(RIGHT$(TRIM$(who$), 5), 2))
ano$ = TRIM$(RIGHT$(TRIM$(who$), 2))
textocf x% + 2, y%, 0, 7, CHR$(45)
textocf x% + 5, y%, 0, 7, CHR$(45)
   IF TRIM$(who$) = "" THEN
    textocf x%, y%, 0, 7, "dd-mm-aa"
   END IF
3234
START$ = dia$
    KBD$ = LLENARCF$(x%, y%, 0, 7, START$, dia$, 2, 2)
    IF KBD$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT FUNCTION
    IF KBD$ = CHR$(13) AND dia$ = "  " OR dia$ = "" THEN
        dia$ = "dd"
    END IF
    IF VAL(dia$) > 31 THEN
        BEEP
        GOTO 3234
    END IF
     IF ASC(dia$) < 47 OR ASC(dia$) > 58 AND UCASE$(dia$) <> "DD" THEN
         BEEP
         GOTO 3234
     END IF
     IF ASC(dia$) = 48 AND ASC(RIGHT$(dia$, 1)) = 48 AND UCASE$(dia$) <> "DD" THEN
        BEEP
        GOTO 3234
    END IF
    IF ASC(RIGHT$(dia$, 1)) > 58 AND UCASE$(dia$) <> "DD" OR ASC(RIGHT$(dia$, 1)) < 47 AND UCASE$(dia$) <> "DD" THEN
        BEEP
        GOTO 3234
    END IF
    IF LEFT$(dia$, 1) <> "0" AND UCASE$(dia$) <> "DD" THEN
    IF VAL(dia$) < 10 THEN
        dia$ = "0" + (TRIM$(dia$))
    END IF
    END IF
    textocf x%, y%, 0, 7, dia$
    IF KBD$ = CHR$(0) + CHR$(15) THEN jl% = 1: EXIT FUNCTION
3235
START$ = mes$
    KBD$ = LLENARCF$(x% + 3, y%, 0, 7, START$, mes$, 2, 2)
    IF KBD$ = CHR$(13) AND mes$ = "  " OR mes$ = "" THEN
    mes$ = "mm"
    END IF
    IF VAL(mes$) > 12 OR ASC(mes$) + ASC(RIGHT$(mes$, 1)) = 96 AND UCASE$(mes$) <> "MM" OR ASC(mes$) > 58 AND UCASE$(mes$) <> "MM" OR ASC(mes$) < 47 AND UCASE$(mes$) <> "MM" THEN
        BEEP
        GOTO 3235
    END IF
    IF ASC(RIGHT$(mes$, 1)) > 58 AND UCASE$(mes$) <> "MM" OR ASC(RIGHT$(mes$, 1)) < 47 AND UCASE$(mes$) <> "MM" THEN
        BEEP
        GOTO 3235
    END IF
    IF LEFT$(mes$, 1) <> "0" AND UCASE$(mes$) <> "MM" THEN
    IF VAL(mes$) < 10 THEN
        mes$ = "0" + (TRIM$(mes$))
    END IF
    END IF
    textocf x% + 3, y%, 0, 7, mes$
    IF KBD$ = CHR$(0) + CHR$(15) THEN GOTO 3234
3236
START$ = ano$
   KBD$ = LLENARCF$(x% + 6, y%, 0, 7, START$, ano$, 2, 2)
    IF KBD$ = CHR$(13) AND ano$ = "  " OR ano$ = "" THEN
    ano$ = "aa"
    END IF
    IF ASC(ano$) > 58 AND UCASE$(ano$) <> "AA" OR ASC(ano$) < 47 AND UCASE$(mes$) <> "MM" THEN
    BEEP
        GOTO 3236
    END IF
    IF ASC(RIGHT$(ano$, 1)) > 58 AND UCASE$(ano$) <> "AA" OR ASC(RIGHT$(ano$, 1)) < 47 AND UCASE$(ano$) <> "AA" THEN
        BEEP
        GOTO 3236
    END IF
    IF LEFT$(ano$, 1) <> "0" AND UCASE$(ano$) <> "AA" THEN
    IF VAL(ano$) < 10 THEN
        ano$ = "0" + (TRIM$(ano$))
    END IF
    END IF
    textocf x% + 6, y%, 0, 7, ano$
    IF KBD$ = CHR$(0) + CHR$(15) THEN GOTO 3235
who$ = TRIM$(dia$) + CHR$(45) + TRIM$(mes$) + CHR$(45) + TRIM$(ano$)
who$ = LCASE$(who$)
START$ = ""
ano$ = "": mes$ = "": dia$ = ""
END FUNCTION

