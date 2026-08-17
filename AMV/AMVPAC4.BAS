DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE SUB SaveVacunas (que$, dosis$, Cual$, CualFin$)
DECLARE SUB ActualPa (yy%, file%)
DECLARE SUB Espere (y%)
DECLARE FUNCTION SHOWVAC$ (who$, x%, y%)
DECLARE FUNCTION SHOWVAC$ (who$, x%, y%)
DECLARE SUB Edad ()
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'

SUB Edad
dia1 = VAL(LEFT$(TRIM$(PetShow.fecha), 2))
mes1 = VAL(LEFT$(RIGHT$(PetShow.fecha, 5), 2))
ano1 = VAL(RIGHT$(TRIM$(PetShow.fecha), 2))
dia2 = VAL(LEFT$(TRIM$(FECCHA$), 2))
mes2 = VAL(LEFT$(RIGHT$(FECCHA$, 5), 2))
ano2 = VAL(RIGHT$(TRIM$(FECCHA$), 2))
IF ano1 = ano2 THEN
    ano3 = 0
    mes3 = (mes2 - mes1) - 1
    dia3 = dia2 + (30 - dia1)
END IF

IF ano1 < ano2 THEN
    ano3 = (ano2 - ano1) - 1
    mes3 = (mes2 + (12 - mes1)) - 1
    dia3 = dia2 + (30 - dia1)
END IF

IF ano1 > 70 THEN
    ano3 = (((ano2 + 2000) - (ano1 + 1900)) - 1)
    mes3 = (mes2 + (12 - mes1)) - 1
    dia3 = dia2 + (30 - dia1)
END IF
IF dia3 > 29 THEN
    mes3 = mes3 + 1
    dia3 = (dia3 - 30)
END IF

IF mes3 > 11 THEN
    ano3 = ano3 + 1
    mes3 = (mes3 - 12)
END IF
IF dia1 > dia2 AND mes1 > mes2 AND ano1 = ano2 THEN
ano3 = 0: mes3 = 0: dia3 = 0
END IF
v$ = "###"
LOCATE 5, 25:
COLOR 7: PRINT USING v$; ano3; : COLOR 3: PRINT "a";
COLOR 7: PRINT USING v$; mes3; : COLOR 3: PRINT "m";
COLOR 7: PRINT USING v$; dia3; : COLOR 3: PRINT "d";
END SUB

DEFINT A-Z
SUB SavePac
Alert 1, 3, 30, 5, 20, 8
textocf 32, 6, 0, 3, "¨Guardar Edici¢n?"
textocf 32, 7, 8, 3, "      (S/N)"
DO
KBD$ = INKEY$
REGNOMBRE$ = TRIM$(PetShow.id)
    SELECT CASE KBD$
        CASE IS = "S", "s":
                textocf 32, 7, 15, 3, "      (S)  ": SLEEP 1
                Espere 0
                IF jl = 1 THEN
                        jl = 0
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
                END IF

                OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS WRITE AS #22 LEN = LEN(Pet)
                OPEN "AMVDAT\AMVVAC.TMP" FOR APPEND AS #66
                IF LOF(66) <> 0 THEN
                CLOSE #66
                OPEN "AMVDAT\AMVVAC.TMP" FOR RANDOM AS #66 LEN = LEN(Pet)
                GET #66, 1, Pet:
                IF VAL(Pet.examen(63)) > VAL(PetShow.examen(63)) THEN PetShow.examen(63) = Pet.examen(63): PetShow.examen(43) = Pet.examen(43): PetShow.examen(44) = Pet.examen(44)
                IF VAL(Pet.examen(64)) > VAL(PetShow.examen(64)) THEN PetShow.examen(64) = Pet.examen(64): PetShow.examen(45) = Pet.examen(45): PetShow.examen(46) = Pet.examen(46)
                IF VAL(Pet.examen(65)) > VAL(PetShow.examen(65)) THEN PetShow.examen(65) = Pet.examen(65): PetShow.examen(47) = Pet.examen(47): PetShow.examen(48) = Pet.examen(48)
                IF VAL(Pet.examen(66)) > VAL(PetShow.examen(66)) THEN PetShow.examen(66) = Pet.examen(66): PetShow.examen(49) = Pet.examen(49): PetShow.examen(50) = Pet.examen(50)
                IF VAL(Pet.examen(67)) > VAL(PetShow.examen(67)) THEN PetShow.examen(67) = Pet.examen(67): PetShow.examen(51) = Pet.examen(51): PetShow.examen(52) = Pet.examen(52)
                IF VAL(Pet.examen(68)) > VAL(PetShow.examen(68)) THEN PetShow.examen(68) = Pet.examen(68): PetShow.examen(53) = Pet.examen(53): PetShow.examen(54) = Pet.examen(54)
                IF VAL(Pet.examen(69)) > VAL(PetShow.examen(69)) THEN PetShow.examen(69) = Pet.examen(69): PetShow.examen(55) = Pet.examen(55): PetShow.examen(56) = Pet.examen(56)
                IF VAL(Pet.examen(70)) > VAL(PetShow.examen(70)) THEN PetShow.examen(70) = Pet.examen(70): PetShow.examen(57) = Pet.examen(57): PetShow.examen(58) = Pet.examen(58)
                IF VAL(PetShow.examen(71)) > VAL(PetShow.examen(71)) THEN PetShow.examen(71) = PetShow.examen(71): PetShow.examen(59) = Pet.examen(59): PetShow.examen(60) = Pet.examen(60)
                IF VAL(Pet.examen(76)) > VAL(PetShow.examen(76)) THEN PetShow.examen(76) = Pet.examen(76): PetShow.examen(61) = Pet.examen(61): PetShow.examen(62) = Pet.examen(62)
                END IF
                CLOSE #66: KILL "AMVDAT\AMVVAC.TMP"
                PUT #22, REG&, PetShow
                CLOSE #22
                OPEN "AMVDAT\AMVPD.DAT" FOR RANDOM AS #22 LEN = LEN(Pet)
                FOR t& = 1 TO LOF(22) / LEN(Pet)
                        GET #22, t&, Pet
                        fuck$ = PetShow.id
                        feck$ = Pet.id
                        FOR I = 1 TO LEN(feck$)
                                A$ = RIGHT$(LEFT$(feck$, I), 1)
                                IF A$ = CHR$(0) THEN MID$(feck$, I, LEN(feck$)) = CHR$(32)
                        NEXT
                        FOR I = 1 TO LEN(fuck$)
                                A$ = RIGHT$(LEFT$(fuck$, I), 1)
                                IF A$ = CHR$(0) THEN MID$(fuck$, I, LEN(fuck$)) = CHR$(32)
                        NEXT
                        IF TRIM$(feck$) = TRIM$(fuck$) THEN
                                PUT #22, t&, PetShow:
                        EXIT FOR
                        END IF
                NEXT
                CLOSE #22
                EXIT SUB
        CASE IS = "n", "N": textocf 32, 7, 15, 3, "        (N)": SLEEP 1
            modifica = 0: EXIT SUB
    END SELECT
LOOP
END SUB

SUB SavePacHead (Actualizar)
Alert 1, 3, 30, 5, 20, 8
textocf 32, 6, 0, 3, "¨Guardar Edici¢n?"
textocf 32, 7, 8, 3, "      (S/N)"
DO
KBD$ = INKEY$
REGNOMBRE$ = TRIM$(PetShow.id)
    SELECT CASE KBD$
        CASE IS = "S", "s":
                textocf 32, 7, 15, 3, "      (S)  ": SLEEP 1
                Espere 0
                OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS WRITE AS #22 LEN = LEN(Pet)
                PUT #22, REG&, PetShow: CLOSE #22
                REGNOMBRE$ = TRIM$(PetShow.id):
                OPEN "AMVDAT\AMVPD.DAT" FOR RANDOM AS #22 LEN = LEN(Pet)
                FOR t& = 1 TO LOF(22) / LEN(Pet)
                        GET #22, t&, Pet
                        IF Pet.id = PetShow.id THEN
                                PUT #22, t&, PetShow: EXIT FOR
                        END IF
                NEXT
                CLOSE #22
                REGNOMBRE$ = TRIM$(PetShow.id)
                IF Actualizar = 1 THEN
                    Actualizar = 0
                    ActualPa 0, 0
                END IF
                EXIT SUB
        CASE IS = "n", "N": textocf 32, 7, 15, 3, "        (N)": SLEEP 1
            modifica = 0: EXIT SUB
    END SELECT
LOOP
END SUB

SUB ShowBlood
    COLOR 0, 3: LOCATE 10, 21: PRINT PetShow.examen(1)
    textocf 2, 11, 14, 1, "GLOBULOS BLANCOS:"
    COLOR 7, 1: LOCATE 11, 26: PRINT (TRIM$(PetShow.examen(2)))
    textocf 2, 12, 14, 1, "GLOBULOS ROJOS:"
    COLOR 7, 1: LOCATE 12, 26: PRINT (TRIM$(PetShow.examen(3)))
    textocf 2, 13, 14, 1, "HEMOGLOBINA:"
    COLOR 7, 1: LOCATE 13, 26: PRINT (TRIM$(PetShow.examen(4)))
    textocf 2, 14, 14, 1, "HEMATOCRITOS:"
    COLOR 7, 1: LOCATE 14, 26: PRINT (TRIM$(PetShow.examen(5)))
    textocf 2, 15, 14, 1, "HEMATIES:"
    COLOR 7, 1: LOCATE 15, 26: PRINT (TRIM$(PetShow.examen(6)))
    textocf 2, 16, 14, 1, "NEUTROFILOS:"
    COLOR 7, 1: LOCATE 16, 26: PRINT (TRIM$(PetShow.examen(7)))
    textocf 2, 17, 14, 1, "MONOCITOS:"
    COLOR 7, 1: LOCATE 17, 26: PRINT (TRIM$(PetShow.examen(8)))
    textocf 2, 18, 14, 1, "EOSINOFILOS:"
    COLOR 7, 1: LOCATE 18, 26: PRINT (TRIM$(PetShow.examen(9)))
    textocf 40, 11, 14, 1, "PLAQUETAS:"
    COLOR 7, 1: LOCATE 11, 65: PRINT (TRIM$(PetShow.examen(10)))

    textocf 40, 12, 14, 1, "FILARIA:"
    COLOR 7, 1: LOCATE 12, 65: PRINT (TRIM$(PetShow.examen(11)))
    textocf 40, 13, 14, 1, "ERLICHIOSIS:"
    COLOR 7, 1: LOCATE 13, 65: PRINT (TRIM$(PetShow.examen(11)))
    textocf 40, 14, 14, 1, "BABESIA:"
    COLOR 7, 1: LOCATE 14, 65: PRINT (TRIM$(PetShow.examen(13)))
    textocf 40, 15, 14, 1, "ANAPLASMA:"
    COLOR 7, 1: LOCATE 15, 65: PRINT (TRIM$(PetShow.examen(14)))
    textocf 40, 16, 14, 1, "HEPATOZOOM CANIS:"
    COLOR 7, 1: LOCATE 16, 65: PRINT (TRIM$(PetShow.examen(15)))
    textocf 40, 17, 14, 1, "TRIPANOSOMA:"
    COLOR 7, 1: LOCATE 17, 65: PRINT (TRIM$(PetShow.examen(16)))
    textocf 40, 18, 14, 1, "OTROS:"
    COLOR 7, 1: LOCATE 18, 65: PRINT (TRIM$(PetShow.examen(17)))
END SUB

SUB ShowHead
textocf 32, 2, 14, 3, "MOSTRAR PACIENTE"
LOCATE 2, 2: COLOR 0, 3: PRINT "PACIENTE #"; PetShow.id;
textocf 76, 2, 1, 3, PetShow.Clave
textocf 13, 4, 7, 1, TRIM$(PetShow.nombr)
textocf 44, 4, 7, 1, TRIM$(PetShow.raza)
textocf 69, 4, 7, 1, TRIM$(PetShow.checkin)
textocf 10, 5, 7, 1, TRIM$(PetShow.fecha)
Edad
textocf 44, 5, 7, 1, TRIM$(PetShow.sexos)
textocf 53, 5, 7, 1, TRIM$(PetShow.clor)
textocf 69, 5, 7, 1, TRIM$(PetShow.talla)
textocf 10, 6, 7, 1, TRIM$(PetShow.dueno)
textocf 47, 6, 7, 1, TRIM$(PetShow.cedula)
textocf 19, 7, 7, 1, TRIM$(PetShow.direcc)
EXIT SUB

END SUB

DEFSNG A-Z
SUB ShowHeces
    COLOR 0, 3: LOCATE 10, 21: PRINT PetShow.examen(34)
    textocf 2, 11, 14, 1, "ASPECTO:"
    COLOR 7, 1: LOCATE 11, 26: PRINT (TRIM$(PetShow.examen(35)))
    textocf 2, 13, 14, 1, "COLOR:"
    COLOR 7, 1: LOCATE 13, 26: PRINT (TRIM$(PetShow.examen(36)))
    textocf 2, 15, 14, 1, "CONSISTENCIA:"
    COLOR 7, 1: LOCATE 15, 26: PRINT (TRIM$(PetShow.examen(37)))
    textocf 2, 17, 14, 1, "SANGRE:"
    COLOR 7, 1: LOCATE 17, 26: PRINT (TRIM$(PetShow.examen(38)))
    textocf 40, 11, 14, 1, "MOCO:"
    COLOR 7, 1: LOCATE 11, 65: PRINT (TRIM$(PetShow.examen(39)))
    textocf 40, 13, 14, 1, "REACCION:"
    COLOR 7, 1: LOCATE 13, 65: PRINT (TRIM$(PetShow.examen(40)))
    textocf 40, 15, 14, 1, "PARASITOS:"
    COLOR 7, 1: LOCATE 15, 65: PRINT (TRIM$(PetShow.examen(41)))
    textocf 40, 17, 14, 1, "RESTOS ALIMENTICIOS:"
    COLOR 7, 1: LOCATE 17, 65: PRINT (TRIM$(PetShow.examen(42)))
END SUB

DEFINT A-Z
SUB ShowNotas
FOR S = 11 TO 23
textocf 2, S, 14, 1, ">"
textocf 79, S, 14, 1, "<"
NEXT
FOR I = 1 TO 13
textocf 3, 10 + I, 7, 1, (PetShow.notas(I))
NEXT
END SUB

DEFSNG A-Z
SUB ShowOrina
    COLOR 0, 3: LOCATE 10, 21: PRINT PetShow.examen(20)
    textocf 2, 11, 14, 1, "COLOR:"
    COLOR 7, 1: LOCATE 11, 26: PRINT (TRIM$(PetShow.examen(21)))
    textocf 2, 12, 14, 1, "ASPECTO:"
    COLOR 7, 1: LOCATE 12, 26: PRINT (TRIM$(PetShow.examen(22)))
    textocf 2, 13, 14, 1, "DENSIDAD:"
    COLOR 7, 1: LOCATE 13, 26: PRINT (TRIM$(PetShow.examen(23)))
    textocf 2, 14, 14, 1, "PH:"
    COLOR 7, 1: LOCATE 14, 26: PRINT (TRIM$(PetShow.examen(24)))
    textocf 2, 15, 14, 1, "PROTEINA:"
    COLOR 7, 1: LOCATE 15, 26: PRINT (TRIM$(PetShow.examen(25)))
    textocf 2, 16, 14, 1, "GLUCOSA:"
    COLOR 7, 1: LOCATE 16, 26: PRINT (TRIM$(PetShow.examen(26)))
    textocf 2, 17, 14, 1, "CUERPOS CETONICOS:"
    COLOR 7, 1: LOCATE 17, 26: PRINT (TRIM$(PetShow.examen(8)))
    textocf 2, 18, 14, 1, "BILIRRUBINA:"
    COLOR 7, 1: LOCATE 18, 26: PRINT (TRIM$(PetShow.examen(27)))
    textocf 40, 11, 14, 1, "HEMOGLOBINA:"
    COLOR 7, 1: LOCATE 11, 65: PRINT (TRIM$(PetShow.examen(28)))
    textocf 40, 12, 14, 1, "NITRITOS:"
    COLOR 7, 1: LOCATE 12, 65: PRINT (TRIM$(PetShow.examen(29)))
    textocf 40, 13, 14, 1, "UROBILINOGENO:"
    COLOR 7, 1: LOCATE 13, 65: PRINT (TRIM$(PetShow.examen(30)))
    textocf 40, 14, 14, 1, "LEUCOSITOS:"
    COLOR 7, 1: LOCATE 14, 65: PRINT (TRIM$(PetShow.examen(31)))
    textocf 40, 15, 14, 1, "HEMATIES:"
    COLOR 7, 1: LOCATE 15, 65: PRINT (TRIM$(PetShow.examen(6)))
    textocf 40, 16, 14, 1, "BACTERIAS:"
    COLOR 7, 1: LOCATE 16, 65: PRINT (TRIM$(PetShow.examen(32)))
    textocf 40, 17, 14, 1, "CELULAS EPITELIALES:"
    COLOR 7, 1: LOCATE 17, 65: PRINT (TRIM$(PetShow.examen(33)))
END SUB

DEFINT A-Z
FUNCTION SHOWVAC$ (who$, x%, y%)
DIA% = VAL(LEFT$(TRIM$(who$), 2))
MES% = VAL(LEFT$(RIGHT$(who$, 5), 2))
ANO% = VAL(RIGHT$(TRIM$(who$), 2))
textocf 70, y%, 15, 1, "OK...  "
IF who$ <> "dd-mm-aa" THEN
IF ANO% < VAL(RIGHT$(TRIM$(FECCHA$), 2)) THEN textocf x%, y%, 29, 1, "VENCIDA"
IF ANO% = VAL(RIGHT$(TRIM$(FECCHA$), 2)) THEN
    IF MES% < VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN textocf x%, y%, 29, 1, "VENCIDA"
    IF MES% = VAL(LEFT$(RIGHT$(FECCHA$, 5), 2)) THEN
        IF DIA% <= VAL(LEFT$(TRIM$(FECCHA$), 2)) THEN textocf x%, y%, 29, 1, "VENCIDA"
    END IF
END IF
ELSEIF who$ = "dd-mm-aa" THEN textocf 70, y%, 15, 1, "  ---  "
END IF
END FUNCTION

