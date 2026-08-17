DECLARE SUB OrdenarIDX ()
DECLARE SUB OrdenarIDXDueno ()
DECLARE SUB sort ()
DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE SUB LOGIN (ZZ%)
DECLARE SUB NOHAY ()
DECLARE SUB ActualPr (yy%, FILE%)
DECLARE SUB listPa (yy%, recupera%)
DECLARE SUB ListPr (yy%, limite%, recupera%)
DECLARE SUB ActualPa (yy%, FILE%)
DECLARE SUB RegPa2 ()
DECLARE SUB PABACK ()
DECLARE SUB CompSaldo ()
DECLARE SUB Espere (y%)
DEFINT A-Z
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'
DATA  60,62,42,168,63,32,65,133,160,66,67,68,69,144,130,138,70
DATA  71,72,73,141,161,74,75,76,77,78,165,164,79,149,80,81,82
DATA  83,84,85,163,151,86,87,88,89,90,49,50,51,52,53,54,55,56
DATA  57,64,91,92,93,94,95,33,34,35,36,37,38,39,40,41,43,44,45
DATA  46,47,48,58,59,61,96,123,124,125,126,133,162,166,167,171
DATA  172,173,246,247,248,249,250,251,252,253

SUB ActualPa (yy%, FILE%)
OPEN "AMVDAT\AMVPA.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVDAT\AMVPA.TMP"
OPEN "AMVDAT\AMVPD.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVDAT\AMVPD.TMP"
OPEN "AMVHIS\AMVPA.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVHIS\AMVPA.TMP"
OPEN "AMVHIS\AMVPD.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVHIS\AMVPD.TMP"
FILE$ = "AMVDAT\AMVPA.DAT": temporal$ = "AMVDAT\AMVPA.TMP"
FILD$ = "AMVDAT\AMVPD.DAT": temporad$ = "AMVDAT\AMVPD.TMP"
IF FILE% = 1 THEN
      FILE$ = "AMVHIS\AMVPA.HIS": temporal$ = "AMVHIS\AMVPA.TMP"
END IF
RESTORE
Alert 1, 3, 26, 5, 26, 9: textocf 29, 4, 7, 4, " ACTUALIZACION "
LOCATE 7, 27: COLOR 4, 3: PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "ACTUALIZACION DE DATOS, NO TOCAR EL SISTEMA..."
OPEN FILE$ FOR RANDOM ACCESS READ AS #19 LEN = LEN(pet)
lar& = (LOF(19) / LEN(pet))
x& = 0
    FOR i = 1 TO 96
            textocf 27, 6, 16, 3, " Leyendo datos...        "
            READ z: nombre$ = CHR$(z)
                IF x& = lar& THEN EXIT FOR
                FOR rg& = 1 TO lar&
                    paziente$ = "             "
                    GET #19, rg&, pet:
                    IF pet.id <> paziente$ THEN
                        IF nombre$ = LEFT$(pet.nombr, 1) THEN
                            x& = x& + 1:
                            LOCATE 7, 27: COLOR 14, 3: PRINT LEFT$("€€€€€€€€€€€€€€€€€€€€€€€€€", 1 + INT((24 * INT((x& * 100) / lar&)) / 100))
                            LOCATE 8, 46: COLOR 15, 3: PRINT STR$(INT((x& * 100) / lar&)) + "% "
                            LOCATE 6, 27: COLOR 0, 3: PRINT pet.nombr; CHR$(32); CHR$(32)
                            OPEN temporal$ FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(PetShow)
                                LOCATE 8, 27: COLOR 0, 3: PRINT LEFT$("00000000", 8 - LEN(LTRIM$(RTRIM$(STR$(x&))))) + LTRIM$(RTRIM$(STR$(x&))); ":"; LTRIM$(STR$(lar&))
                                PUT #3, x&, pet: CLOSE #3
                        END IF
                    END IF
                NEXT
        NEXT
CLOSE #19
KILL FILE$
OPEN temporal$ FOR APPEND AS #1: CLOSE #1
NAME temporal$ AS FILE$
RESTORE
OPEN FILD$ FOR RANDOM ACCESS READ AS #19 LEN = LEN(pet)
lar& = (LOF(19) / LEN(pet)): x& = 0
    FOR i = 1 TO 96
            textocf 27, 6, 16, 3, " Leyendo datos...        "
            READ z: nombre$ = CHR$(z)
                IF x& = lar& THEN EXIT FOR
                FOR rg& = 1 TO lar&
                    paziente$ = "             "
                    GET #19, rg&, pet:
                    IF pet.id <> paziente$ THEN
                        IF UCASE$(nombre$) = UCASE$(LEFT$(pet.dueno, 1)) THEN
                            x& = x& + 1:
                            LOCATE 6, 27: COLOR 0, 3: PRINT pet.dueno;
                            OPEN temporad$ FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(PetShow)
                            PUT #3, x&, pet: CLOSE #3
                        END IF
                    END IF
                NEXT
        NEXT
CLOSE #19
KILL FILD$:
OPEN temporad$ FOR APPEND AS #1: CLOSE #1
NAME temporad$ AS FILD$
END SUB

SUB ActualPr (yy%, FILE%)
OPEN "AMVDAT\AMVPR.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVDAT\AMVPR.TMP"
OPEN "AMVHIS\AMVPR.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVHIS\AMVPR.TMP"
FILE$ = "AMVDAT\AMVPR.DAT": temporal$ = "AMVDAT\AMVPR.TMP": donde& = 99: FF = 100
IF FILE% = 1 THEN
    FILE$ = "AMVHIS\AMVPR.HIS": temporal$ = "AMVHIS\AMVPR.TMP": FF = 1
END IF
Alert 1, 3, 26, 5, 26, 9: textocf 29, 4, 7, 4, " ACTUALIZACION "
LOCATE 7, 27: COLOR 4, 3: PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "ACTUALIZACION DE DATOS, NO TOCAR EL SISTEMA..."
OPEN FILE$ FOR RANDOM ACCESS READ AS #1 LEN = LEN(Invent)
lar& = LOF(1) / (LEN(Invent)) - 99
IF FILE% = 1 THEN donde& = lar& + 99
OPEN temporal$ FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(Invent)
textocf 27, 6, 16, 3, " Leyendo datos...        "
LOCATE 8, 27: COLOR 0, 3: PRINT " Analizando Registros..."
x& = 0
FOR j& = 1 TO donde&
    GET #1, j&, Invent
    IF Invent.id <> "             " THEN
        x& = x& + 1
        PUT #3, x&, Invent
    END IF
    LOCATE 7, 27: COLOR 14, 3: PRINT "€"
    LOCATE 7, 27 + FIX(x& / 4): COLOR 14, 3: PRINT "€"
NEXT
CLOSE #3:
LOCATE 7, 27: COLOR 4, 3: PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
LOCATE 8, 27: COLOR 0, 3: PRINT "                         "
x& = FF - 1: RESTORE
FOR i = 1 TO 96
    textocf 27, 6, 16, 3, " Leyendo datos...        "
    IF x& = lar& + 99 THEN EXIT FOR
    READ z: nombre$ = CHR$(z)
    FOR rg& = FF TO lar& + 99
                GET #1, rg&, Invent:
                IF Invent.id <> "             " THEN
                    IF nombre$ = LEFT$(TRIM$(Invent.produ), 1) THEN
                        x& = x& + 1
                        LOCATE 8, 46: COLOR 15, 3: PRINT STR$(INT(((x& - donde&) * 100) / lar&)) + "% "
                        LOCATE 7, 27: COLOR 14, 3: PRINT LEFT$("€€€€€€€€€€€€€€€€€€€€€€€€€", 1 + INT((24 * INT(((x& - donde&) * 100) / lar&)) / 100)):
                        LOCATE 6, 27: COLOR 0, 3: PRINT RTRIM$(Invent.produ); : COLOR 3, 3: PRINT STRING$(25 - LEN(RTRIM$(Invent.produ)), "€");
                        OPEN temporal$ FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(InventShow)
                            LOCATE 8, 27: COLOR 0, 3: PRINT LEFT$("00000000", 8 - LEN(LTRIM$(RTRIM$(STR$(x& - donde&))))) + LTRIM$(RTRIM$(STR$(x& - donde&))); ":"; LTRIM$(STR$(lar&))
                            PUT #3, x&, Invent: CLOSE #3
                    END IF
                END IF
    NEXT
NEXT
CLOSE #1
OPEN temporal$ FOR APPEND AS #1: CLOSE #1
KILL FILE$: NAME temporal$ AS FILE$
END SUB

SUB comppa
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(pet)
        FOR REG& = 1 TO LOF(1) / LEN(pet) + 1
            GET #1, REG&, PetShow: REGHAMBRE$ = UCASE$(LTRIM$(RTRIM$(REGNOMBRE$)))
            IF REGHAMBRE$ = LEFT$(UCASE$(TRIM$(PetShow.nombr)), LEN(REGNOMBRE$)) THEN
                CLOSE #1: CompSaldo: EXIT SUB
            END IF
                FOR i = 1 TO LEN(PetShow.id)
                a$ = RIGHT$(LEFT$(PetShow.id, i), 1)
                IF a$ = CHR$(0) THEN MID$(PetShow.id, i, LEN(PetShow.id)) = CHR$(32)
                NEXT
            IF REGHAMBRE$ = LTRIM$(RTRIM$(PetShow.id)) THEN
              CLOSE #1: CompSaldo: EXIT SUB
            END IF
        NEXT
PetShow.id = "             ":
CLOSE #1
END SUB

SUB EliminarPa
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #2 LEN = LEN(pet)
IF LOF(2) = 0 THEN CLOSE #2: CALL NOHAY: EXIT SUB
CLOSE #2
CALL LOGIN(1)
IF EnGtCla% = 0 THEN EXIT SUB
EnGtCla% = 0
1
REGNUMERO& = 0 AND REGNOMBRE$ = ""
listPa 1, 0
salir% = 0
IF REGNUMERO& = 0 AND REGNOMBRE$ = "" THEN EXIT SUB
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM AS #1 LEN = LEN(pet)
FOR i& = 1 TO (LOF(1) / LEN(pet))
    pet.id = ""
    GET #1, i&, pet
    qur$ = pet.id
    IF REGNOMBRE$ = LTRIM$(RTRIM$(qur$)) THEN
    pet.id = qur$
        GOSUB kuadro
        OPEN "AMVHIS\AMVPA.HIS" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(pet)
        z& = (LOF(2) / LEN(pet)) + 1
        PUT #2, z&, pet:
        CLOSE #2
        pet.id = ""
        PUT #1, i&, pet:
        CLOSE #1:
        EXIT FOR
    END IF
NEXT
CLOSE #1: CLOSE #2
OPEN "AMVDAT\AMVPD.DAT" FOR RANDOM AS #1 LEN = LEN(pet)
FOR i& = 1 TO (LOF(1) / LEN(pet))
    pet.id = ""
    qur$ = ""
    GET #1, i&, pet
    qur$ = pet.id
    IF REGNOMBRE$ = LTRIM$(RTRIM$(qur$)) THEN
        pet.id = ""
        PUT #1, i&, pet:
        CLOSE #1:
        ActualPa 1, 0
        ActualPa 1, 1
        EXIT SUB
    END IF
NEXT
CLOSE #1

    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1: GOTO 1
EXIT SUB
RETURN

kuadro:
Alert 1, 3, 19, 12, 40, 15
textocf 30, 11, 7, 4, "ELIMINAR PACIENTES "
textocf 24, 12, 1, 3, " REGISTRO "
textocf 43, 12, 1, 3, " NOMBRE "
textocf 23, 13, 0, 7, pet.id
textocf 40, 13, 0, 7, pet.nombr
textocf 27, 14, 8, 3, "®Eliminar?"
textocf 45, 14, 6, 3, "(S/N)"
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27):
        textocf 45, 14, 15, 3, "  (N)"
        SLEEP 1
        EXIT SUB
    END SELECT
LOOP
textocf 45, 14, 15, 3, "(S)  ": SLEEP 1
textocf 27, 14, 16, 3, "Eliminando Registro...   "
textocf 23, 13, 7, 0, pet.id
textocf 40, 13, 7, 0, pet.nombr
RETURN

END SUB

SUB EliminarPr
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #2 LEN = LEN(Invent)
IF LOF(2) < 107020 THEN CLOSE #2: CALL NOHAY: EXIT SUB
CLOSE #2
CALL LOGIN(1)
IF EnGtCla% = 0 THEN EXIT SUB
EnGtCla% = 0
2
REGNUMERO& = 0 AND REGNOMBRE$ = ""
ListPr 1, 99, 0
IF REGNUMERO& = 0 AND REGNOMBRE$ = "" THEN EXIT SUB
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM AS #1 LEN = LEN(Invent)
FOR i& = 100 TO (LOF(1) / LEN(Invent))
    Invent.id = ""
    GET #1, i&, Invent
    IF REGNOMBRE$ = TRIM$(Invent.id) THEN
        GOSUB cuadro
        OPEN "AMVHIS\AMVPR.HIS" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(InventShow)
        IF LOF(2) > 0 THEN z& = (LOF(2) / LEN(InventShow)) + 1
        IF LOF(2) = 0 THEN z& = 1
        PUT #2, z&, Invent: CLOSE #2

        Invent.id = "             ":
        PUT #1, i&, Invent: CLOSE #1
        CALL ActualPr(1, 0): CALL ActualPr(1, 1): EXIT SUB
    END IF
NEXT
    CLOSE #1
    Alert 1, 3, 26, 5, 26, 7
    textocf 30, 4, 7, 4, "Error."
    textocf 28, 6, 0, 3, "Registro Invalido..."
    textocf 1, 25, 3, 3, STRING$(80, 179)
    BEEP: SLEEP 1: GOTO 2
EXIT SUB

cuadro:
Alert 1, 3, 19, 12, 40, 15
textocf 30, 11, 7, 4, "ELIMINAR ARTICULO"
textocf 22, 12, 1, 3, " CODIGO "
textocf 37, 12, 1, 3, " DESCRIPCION "
textocf 20, 13, 0, 7, Invent.id
textocf 34, 13, 0, 7, Invent.produ
textocf 27, 14, 8, 3, "®Eliminar?"
textocf 45, 14, 6, 3, "(S/N)"
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27):
        textocf 45, 14, 15, 3, "  (N)"
        SLEEP 1
        EXIT SUB
    END SELECT
LOOP
textocf 45, 14, 15, 3, "(S)  ": SLEEP 1
textocf 27, 14, 16, 3, "Eliminando Registro...   "
textocf 20, 13, 7, 0, Invent.id
textocf 34, 13, 7, 0, Invent.produ
RETURN
END SUB

SUB IndexPa
DIM kd$, total&, i&

' 1. Verificar si hay datos
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(pet)
total& = LOF(1) / LEN(pet)
IF total& = 0 THEN CLOSE #1: CALL NOHAY: EXIT SUB
CLOSE #1

' 2. Pantalla de Advertencia Hist¢rica
Alert 1, 3, 26, 7, 24, 12
textocf 30, 6, 7, 4, "INDEXAR PACIENTES "
LOCATE 8, 27: COLOR 0, 3: PRINT "  Procesando °ndices al"
LOCATE 9, 27: COLOR 0, 3: PRINT "  100% en Memoria RAM  "
LOCATE 10, 27: COLOR 8, 3: PRINT "   ®Continuar?"
LOCATE 11, 36: COLOR 6, 3: PRINT "(S/N)"

DO
    kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27): textocf 36, 11, 15, 3, "  (N)": SLEEP 1: EXIT SUB
    END SELECT
LOOP

textocf 36, 11, 15, 3, "(S)  ": SLEEP 1

' 3. Pantalla de Progreso Visual
textocf 26, 12, 1, 1, "                                "
textocf 26, 13, 1, 1, "                                "
Alert 1, 3, 26, 7, 26, 12
textocf 30, 6, 7, 4, "INDEXAR PACIENTES "
LOCATE 9, 27: COLOR 4, 3: PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "INDEXANDO REGISTROS, ESPERE UN MOMENTO..."


FOR i& = 1 TO 25
    LOCATE 9, 26 + i&: COLOR 14, 3: PRINT "Í"
    LOCATE 10, 46: COLOR 15, 3: PRINT STR$(INT((i& * 100) / 25)) + "%"
    NEXT i&

CALL sort

SOUND 440, 3: SOUND 340, 3: SOUND 740, 5
LOCATE 8, 27: COLOR 2, 3: PRINT " ≠Indexado Concluido!  "
SLEEP 1
END SUB

SUB IndexPr
OPEN "AMVDAT\AMVPR.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVDAT\AMVPR.TMP"
OPEN "AMVHIS\AMVPR.TMP" FOR APPEND AS #1: CLOSE #1
KILL "AMVHIS\AMVPR.TMP"
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(Invent)
IF (LOF(1) / (LEN(Invent))) = 99 THEN
    CLOSE #1: CALL NOHAY: EXIT SUB
END IF
CLOSE #1
Alert 1, 3, 26, 7, 24, 12
textocf 30, 6, 7, 4, "INDEXAR INVENTARIO"
LOCATE 8, 27: COLOR 0, 3: PRINT "  Puede tomar horas en "
LOCATE 9, 27: COLOR 0, 3: PRINT " culminar este Proceso"
LOCATE 10, 27: COLOR 8, 3: PRINT "   ®Continuar?"
LOCATE 11, 36: COLOR 6, 3: PRINT "(S/N)"
DO
kd$ = INKEY$
    SELECT CASE kd$
        CASE IS = "s", "S", CHR$(13): EXIT DO
        CASE IS = "n", "N", CHR$(27): textocf 36, 11, 15, 3, "  (N)": SLEEP 1: EXIT SUB
    END SELECT
LOOP
textocf 36, 11, 15, 3, "(S)  ": SLEEP 1
textocf 26, 12, 1, 1, "                                "
textocf 26, 13, 1, 1, "                                "
Alert 1, 3, 26, 7, 26, 12
textocf 30, 6, 7, 4, "INDEXAR INVENTARIO"
LOCATE 9, 27: COLOR 4, 3: PRINT "∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞"
textocf 1, 25, 3, 3, STRING$(80, " ")
textocf 2, 25, 0, 3, "ACTUALIZACION DE DATOS, NO TOCAR EL SISTEMA..."
LOCATE , , 0: RESTORE
FOR l& = 1 TO 25
    TIMER ON: GOSUB timerer
    OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(Invent)
    OPEN "AMVDAT\AMVPR.TMP" FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(InventShow)
    x& = 0: textocf 27, 8, 16, 3, " Leyendo datos...        "
    FOR rg& = 1 TO 99
        TIMER ON: GOSUB timerer
        GET #1, rg&, Invent:
        x& = x& + 1: PUT #3, x&, Invent:
    NEXT
    CLOSE #3: CLOSE #1
OPEN "AMVDAT\AMVPR.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(Invent)
FOR i = 1 TO 97
        TIMER ON: GOSUB timerer
            textocf 27, 8, 16, 3, " Leyendo datos...        "
            IF x& = (LOF(1) / (LEN(Invent))) THEN EXIT FOR
            READ z: nombre$ = CHR$(z):
            FOR rg& = 100 TO (LOF(1) / (LEN(Invent)))
                TIMER ON: GOSUB timerer: GET #1, rg&, Invent:
                IF nombre$ = LEFT$(RIGHT$(Invent.produ, l&), 1) THEN
                        LOCATE 9, 27 + INT(l& / 1.08): COLOR 14, 3: PRINT "€":
                        LOCATE 10, 46: COLOR 15, 3: PRINT STR$(INT((l& * 100) / 25) - 1) + "%"
                        LOCATE 8, 27: COLOR 0, 3: PRINT Invent.produ;
                        OPEN "AMVDAT\AMVPR.TMP" FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(InventShow)
                        x& = x& + 1: LOCATE 10, 27: COLOR 0, 3: PRINT LEFT$("00000000", 8 - LEN(TRIM$(STR$(x&)))) + TRIM$(STR$(x&));
                        PUT #3, x&, Invent: CLOSE #3
                END IF
            NEXT
        NEXT: RESTORE: CLOSE #1: KILL "AMVDAT\AMVPR.DAT"
        OPEN "AMVDAT\AMVPR.TMP" FOR APPEND AS #1: CLOSE #1
        NAME "AMVDAT\AMVPR.TMP" AS "AMVDAT\AMVPR.DAT"
NEXT
TIMER OFF: SOUND 440, 3: SOUND 340, 3: SOUND 740, 5: SLEEP 1: mttl& = 0
EXIT SUB
timerer:
        IF mttl& = 60 THEN m = m + 1: mttl& = 0
        IF m = 60 THEN h = h + 1: m = 0
        IF h = 24 THEN h = 0
        LOCATE 11, 35: COLOR 8, 3: PRINT "Tiempo: "; LEFT$("00", 2 - LEN(TRIM$(STR$(h)))); TRIM$(STR$(h)); ":"; LEFT$("00", 2 - LEN(TRIM$(STR$(m)))); TRIM$(STR$(m)); ":"; LEFT$("00", 2 - LEN(TRIM$(STR$(mttl&)))); TRIM$(STR$(mttl&))
RETURN
END SUB

SUB OrdenarIDX
DIM lar&, i&, j&
DIM t AS tipoindice

' Abrimos el archivo IDX para leerlo completo a la memoria RAM
OPEN "AMVDAT\AMVPA.IDX" FOR RANDOM ACCESS READ AS #2 LEN = LEN(indice)
lar& = LOF(2) / LEN(indice)

' Si el archivo est† vac°o, no hay nada que ordenar
IF lar& <= 1 THEN CLOSE #2: EXIT SUB

' Creamos una matriz din†mica en la memoria RAM
REDIM listaRAM(1 TO lar&) AS tipoindice

' Pasamos todo el archivo IDX a la memoria RAM
FOR i& = 1 TO lar&
    GET #2, i&, listaRAM(i&)
NEXT i&
CLOSE #2

' --- ALGORITMO DE ORDENAMIENTO (Insertion Sort por Nombre Completo) ---
' Este mÇtodo eval£a todas las letras del nombre, no solo la primera.
FOR i& = 2 TO lar&
    t = listaRAM(i&)
    j& = i& - 1
    DO WHILE j& >= 1
        ' Comparamos los nombres alfabÇticamente
        IF listaRAM(j&).nombr > t.nombr THEN
            listaRAM(j& + 1) = listaRAM(j&)
            j& = j& - 1
        ELSE
            EXIT DO
        END IF
    LOOP
    listaRAM(j& + 1) = t
NEXT i&

' --- GUARDADO ALFABêTICO ---
' Volvemos a abrir el archivo IDX para sobreescribirlo ya ordenado
OPEN "AMVDAT\AMVPA.IDX" FOR OUTPUT AS #2: CLOSE #2
OPEN "AMVDAT\AMVPA.IDX" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(indice)

FOR i& = 1 TO lar&
    PUT #2, i&, listaRAM(i&)
NEXT i&
CLOSE #2

END SUB

SUB OrdenarIDXDueno
DIM lar&, i&, j&
DIM t AS tipoindice

' Abrimos el archivo IDX de due§os
OPEN "AMVDAT\AMVPD.IDX" FOR RANDOM ACCESS READ AS #2 LEN = LEN(indice)
lar& = LOF(2) / LEN(indice)

IF lar& <= 1 THEN CLOSE #2: EXIT SUB

REDIM listaRAM(1 TO lar&) AS tipoindice

FOR i& = 1 TO lar&
    GET #2, i&, listaRAM(i&)
NEXT i&
CLOSE #2

FOR i& = 2 TO lar&
    t = listaRAM(i&)
    j& = i& - 1
    DO WHILE j& >= 1
        IF listaRAM(j&).dueno > t.dueno THEN
            listaRAM(j& + 1) = listaRAM(j&)
            j& = j& - 1
        ELSE
            EXIT DO
        END IF
    LOOP
    listaRAM(j& + 1) = t
NEXT i&

OPEN "AMVDAT\AMVPD.IDX" FOR OUTPUT AS #2: CLOSE #2
OPEN "AMVDAT\AMVPD.IDX" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(indice)

FOR i& = 1 TO lar&
    PUT #2, i&, listaRAM(i&)
NEXT i&
CLOSE #2
END SUB

SUB RegPa
221
CALL PABACK
textocf 28, 2, 14, 3, "REGISTRO DE PACIENTES"
textocf 2, 25, 0, 3, "ESC: Salir  ≥ S¢lo puede realizar hasta 10 Registros a la vez"
Alert 1, 3, 20, 7, 38, 10: aquicf$ = ""
textocf 22, 8, 14, 3, "®Cu†ntos Pacientes a Registrar?:"
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #15 LEN = LEN(pet)
LOCATE 9, 25: COLOR 15, 3: PRINT "Total Registrados: "; (LOF(15) / LEN(pet)); : CLOSE #15
kbd$ = LLENARCF$(55, 8, 0, 2, "", aquicf$, 2, 2)
n = VAL(aquicf$)
IF n > 10 THEN
    CALL PABACK
    textocf 28, 2, 14, 3, "REGISTRO DE PACIENTES"
    Alert 1, 3, 24, 7, 29, 10:
    textocf 28, 6, 7, 4, "Error"
    textocf 26, 8, 14, 3, " S¢lo pueden Registrarse"
    textocf 26, 9, 14, 3, "Diez Pacientes como M†ximo":
    SLEEP 2: GOTO 221
END IF
IF n = 0 THEN EXIT SUB
FOR i = 1 TO n
     OPEN "AMVSYS\AMVPA.SYS" FOR INPUT AS #2
     DO WHILE NOT EOF(2)
        INPUT #2, id&: LOOP: CLOSE #2
        IF id& = 9999999 THEN id& = 0
        id& = id& + 1
        pet.id = LTRIM$(STR$(id&))
        CALL PABACK
        textocf 28, 2, 14, 3, " REGISTRO DE PACIENTES  "
        textocf 2, 9, 0, 7, " HEMATOLOGIA "
        textocf 17, 9, 0, 7, " ORINA "
        textocf 26, 9, 0, 7, " HECES "
        textocf 35, 9, 0, 7, " VACUNAS "
        textocf 46, 9, 0, 7, " HISTORIA "
        textocf 58, 9, 0, 7, " CARGOS "
        textocf 73, 9, 0, 7, " SALIR "
        textocf 2, 2, 0, 3, "PACIENTE #" + pet.id
        textocf 76, 2, 1, 3, Iniciales$
        pet.clave = Iniciales$
        LOCATE 4, 69: COLOR 7, 1: PRINT FECCHA$;
        pet.raza = STRING$(15, 32): pet.dueno = STRING$(20, 32)
        pet.nombr = STRING$(15, 32): pet.clor = STRING$(10, 32): pet.cedula = STRING$(15, 32)
        pet.talla = STRING$(10, 32): pet.sexos = STRING$(1, 32): pet.direcc = STRING$(50, 32): pet.fecha = "dd-mm-aa"
951
kbd$ = LLENARCF$(13, 4, 0, 7, pet.nombr, aquicf$, 15, 15)
GOSUB salgo
IF TRIM$(aquicf$) <> "" THEN
    zx = ASC(UCASE$(LEFT$(aquicf$, 1)))
    IF zx < 65 AND zx <> 60 OR zx > 90 AND zx <> 60 THEN
        textocf 13, 4, 4, 7, "   -ERROR-    ": BEEP: SLEEP 1
        textocf 13, 4, 0, 7, "              ": GOTO 951
    END IF
 END IF
IF TRIM$(aquicf$) = "" THEN
pet.nombr = "<* SIN NOMBRE*>": GOTO 951
END IF
pet.nombr = TRIM$(UCASE$(aquicf$)):
textocf 13, 4, 0, 7, pet.nombr
48
kbd$ = LLENARCF$(44, 4, 0, 7, pet.raza, aquicf$, 15, 15)
GOSUB salgo
pet.raza = TRIM$(UCASE$(aquicf$)):
textocf 44, 4, 0, 7, pet.raza
IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 951
47
LOCATE 25, 16: COLOR 0, 3: PRINT "SOLO SE CALCULARAN FECHAS ENTRE 1.970 Y EL PRESENTE A•O";
kbd$ = VERDATE$(pet.fecha, 10, 5)
IF jl% = 1 THEN jl% = 0: GOTO 48
46
LOCATE 25, 16: COLOR 0, 3: PRINT "                                                       ";

kbd$ = LLENARCF$(44, 5, 0, 7, pet.sexos, SE$, 1, 1)
IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 47
IF UCASE$(SE$) <> "M" AND UCASE$(SE$) = "F" OR UCASE$(SE$) = "M" AND UCASE$(SE$) <> "F" OR UCASE$(SE$) <> "M" AND UCASE$(SE$) = "H" OR UCASE$(SE$) = "M" AND UCASE$(SE$) <> "H" THEN
ELSE
 BEEP: GOTO 46
END IF
GOSUB salgo
pet.sexos = TRIM$(UCASE$(SE$)):
textocf 44, 5, 0, 7, pet.sexos
SE$ = ""
49
kbd$ = LLENARCF$(53, 5, 0, 7, pet.clor, aquicf$, 8, 8)
GOSUB salgo
pet.clor = TRIM$(UCASE$(aquicf$)):
textocf 53, 5, 0, 7, LEFT$(pet.clor, 8)
IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 46
50
kbd$ = LLENARCF$(69, 5, 0, 7, pet.talla, aquicf$, 8, 8)
GOSUB salgo
pet.talla = TRIM$(UCASE$(aquicf$)):
textocf 69, 5, 0, 7, LEFT$(pet.talla, 8)
IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 49
51
kbd$ = LLENARCF$(10, 6, 0, 7, pet.dueno, aquicf$, 20, 20)
GOSUB salgo
pet.dueno = TRIM$(UCASE$(aquicf$)):
textocf 10, 6, 0, 7, pet.dueno
IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 50
52
kbd$ = LLENARCF$(47, 6, 0, 7, pet.cedula, aquicf$, 15, 15)
GOSUB salgo
pet.cedula = TRIM$(UCASE$(aquicf$)):
textocf 47, 6, 0, 7, pet.cedula
IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 51
53
kbd$ = LLENARCF$(19, 7, 0, 7, pet.direcc, aquicf$, 50, 50)
GOSUB salgo
pet.direcc = TRIM$(aquicf$):
pet.checkin = FECCHA$
IF kbd$ = CHR$(0) + CHR$(15) THEN GOTO 52
CALL RegPa2
OPEN "AMVDAT\AMVPD.DAT" FOR RANDOM ACCESS WRITE AS #1 LEN = LEN(pet)
    z& = (LOF(1) / LEN(pet)) + 1
    PUT #1, z&, pet: CLOSE #1
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS WRITE AS #1 LEN = LEN(pet)
    z& = 0
    z& = (LOF(1) / LEN(pet)) + 1
    PUT #1, z&, pet: CLOSE #1
    DD = DD + 1
    LOCATE 25, 2: COLOR 0, 3: PRINT "PACIENTE REGISTRADO EN EL #"; LTRIM$(STR$(z&)); " ("; LTRIM$(STR$(DD)); "/"; LTRIM$(STR$(n)); "). PRESIONE '" + CHR$(26) + "' PARA CONTINUAR";
    OPEN "AMVSYS\AMVPA.SYS" FOR OUTPUT AS #3: WRITE #3, id&: CLOSE #3
    DO: LOOP UNTIL INKEY$ = CHR$(0) + "M"
NEXT
Espere 0
CALL ActualPa(0, 0)
EXIT SUB

salgo:
IF kbd$ = CHR$(27) THEN
     Alert 1, 3, 26, 5, 26, 7
     textocf 27, 6, 16, 3, "Registro ha sido abortado"
    SLEEP 1: EXIT SUB
END IF
RETURN

END SUB

DEFSNG A-Z
SUB RegPa2

FOR i = 43 TO 62
pet.examen(i) = "dd-mm-aa":
NEXT

FOR i = 63 TO 72
pet.examen(i) = "1       ":
NEXT


FOR i = 1 TO 13
pet.notas(i) = STRING$(76, 32):
NEXT
END SUB

DEFINT A-Z
SUB RESPALDO
OPEN "AMVBAKUP.BAT" FOR OUTPUT AS #1
PRINT #1, "@ECHO OFF"
PRINT #1, "CD AMVDAT"
PRINT #1, "COPY AMVPA.DAT ..\AMVHIS\AMVPA.BAK"
PRINT #1, "CLS"
PRINT #1, "COPY AMVPD.DAT ..\AMVHIS\AMVPD.BAK"
PRINT #1, "CLS"
PRINT #1, "COPY AMVPR.DAT ..\AMVHIS\AMVPR.BAK"
PRINT #1, "CLS"
PRINT #1, "COPY AMVCA.DAT ..\AMVHIS\AMVCA.BAK"
PRINT #1, "CLS"
PRINT #1, "CD.."
CLOSE #1
LOCATE , , 0
COLOR 0, 0
SHELL "AMVBAKUP.BAT"
KILL "AMVBAKUP.BAT"
END SUB

SUB sort
OPEN "AMVDAT\AMVPA.IDX" FOR OUTPUT AS #2: CLOSE #2
OPEN "AMVDAT\AMVPD.IDX" FOR OUTPUT AS #3: CLOSE #3

OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(pet)
OPEN "AMVDAT\AMVPA.IDX" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(indice)
OPEN "AMVDAT\AMVPD.IDX" FOR RANDOM ACCESS WRITE AS #3 LEN = LEN(indice)

DIM totalRegistros AS LONG, i&
totalRegistros = LOF(1) / LEN(pet)

FOR i& = 1 TO totalRegistros
    GET #1, i&, pet
    indice.id = pet.id
    indice.nombr = pet.nombr
    indice.dueno = pet.dueno
    indice.posic = i&

    PUT #2, i&, indice ' Espejo para Pacientes
    PUT #3, i&, indice ' Espejo para Due§os
NEXT i&

CLOSE #1: CLOSE #2: CLOSE #3

' Ejecuta los dos ordenamientos en RAM al instante
CALL OrdenarIDX
CALL OrdenarIDXDueno
END SUB

SUB SortInventario
' 1. Crear espejo del inventario
OPEN "AMVDAT\AMVINV.IDX" FOR OUTPUT AS #2: CLOSE #2
OPEN "AMVDAT\AMVINV.DAT" FOR RANDOM ACCESS READ AS #1 LEN = LEN(Invent)
OPEN "AMVDAT\AMVINV.IDX" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(indiceinv)

DIM totalProd AS LONG, i&
totalProd = LOF(1) / LEN(Invent)

FOR i& = 1 TO totalProd
    GET #1, i&, Invent
    indiceinv.id = Invent.id
    indiceinv.produ = Invent.produ
    indiceinv.posic = i&
    PUT #2, i&, indiceinv
NEXT i&
CLOSE #1: CLOSE #2

' 2. Ordenar el °ndice de inventario en RAM
OPEN "AMVDAT\AMVINV.IDX" FOR RANDOM ACCESS READ AS #2 LEN = LEN(indiceinv)
DIM lar&
lar& = LOF(2) / LEN(indiceinv)

IF lar& <= 1 THEN CLOSE #2: EXIT SUB

REDIM listaRAM(1 TO lar&) AS tipoindiceinv
FOR i& = 1 TO lar&
    GET #2, i&, listaRAM(i&)
NEXT i&
CLOSE #2

' Ordenamiento por inserci¢n para el nombre del producto (.produ)
DIM j&
DIM t AS tipoindiceinv
FOR i& = 2 TO lar&
    t = listaRAM(i&)
    j& = i& - 1
    DO WHILE j& >= 1
        IF listaRAM(j&).produ > t.produ THEN
            listaRAM(j& + 1) = listaRAM(j&)
            j& = j& - 1
        ELSE
            EXIT DO
        END IF
    LOOP
    listaRAM(j& + 1) = t
NEXT i&

' Guardar °ndice de inventario ordenado
OPEN "AMVDAT\AMVINV.IDX" FOR OUTPUT AS #2: CLOSE #2
OPEN "AMVDAT\AMVINV.BOTTOM" FOR RANDOM ACCESS WRITE AS #2 LEN = LEN(indiceinv)
FOR i& = 1 TO lar&
    PUT #2, i&, listaRAM(i&)
NEXT i&
CLOSE #2
END SUB

