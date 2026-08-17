DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE SUB RegPa2 ()
DECLARE SUB Lineas ()
DECLARE SUB Question (U%, Fo$, B%)
DECLARE SUB ActualPa (yy%, file%)
DECLARE SUB EditBlood ()
DECLARE SUB EditOrina ()
DECLARE SUB EditHeces ()
DECLARE SUB EditVacun ()
DECLARE SUB EditNotas ()
DECLARE SUB SavePac ()
DECLARE SUB SavePacHead (Actualizar%)
DECLARE SUB LlistPa ()
DECLARE SUB ShowNotas ()
DECLARE SUB Espere (y%)
DECLARE SUB Edad ()
DECLARE FUNCTION SHOWVAC$ (who$, x%, y%)
'$INCLUDE: 'C:\JLV_CODE\AMV\AMV1.BI'

SUB CompSaldo
mttl& = 0
IF PetShow.id <> "             " THEN file$ = "AMVDAT\AMVCA.DAT"
IF PetShow.id = "             " THEN
    A$ = RIGHT$(FECCHA$, 5): file$ = "AMVHIS\GA" + A$ + ".DAT"
END IF

OPEN file$ FOR INPUT AS #12:
    FOR d& = 1 TO LOF(12)
1299
        IF EOF(12) THEN EXIT FOR
        INPUT #12, serial$, myfecha$, myclave$, mycodigo$, mynombre$, mycantidad$, mysubtotal&, mytime$
            IF serial$ <> TRIM$(PetShow.id) THEN GOTO 1299
            IF PetShow.id = "             " THEN
                    IF mttl& >= neto& THEN neto& = 0
                    mttl& = mttl& + mysubtotal&
                    IF myfecha$ = FECCHA$ THEN saldo& = saldo& + neto& + mysubtotal&
            END IF
            IF serial$ = TRIM$(PetShow.id) AND serial$ <> "" THEN mttl& = mttl& + mysubtotal&
     NEXT
            IF PetShow.id = "             " THEN
              LOCATE 3, 52: COLOR 3, 1: PRINT USING AS$; mttl&;
              LOCATE 3, 9: COLOR 3, 1: PRINT USING AS$; saldo&;
            END IF
CLOSE #12: saldo& = 0: neto& = 0
REGNOMBRE$ = PetShow.id
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM AS #3 LEN = LEN(Pet)
FOR I& = 1 TO (LOF(3) / LEN(Pet))
GET #3, I&, Pet

IF REGNOMBRE$ = Pet.id THEN
    Pet.monto = mttl&
    PetShow.monto = mttl&
    PUT #3, I&, Pet
    EXIT FOR
END IF
NEXT
CLOSE #3
OPEN "AMVDAT\AMVPD.DAT" FOR RANDOM AS #3 LEN = LEN(Pet)
FOR I& = 1 TO (LOF(3) / LEN(Pet))
GET #3, I&, Pet
IF REGNOMBRE$ = Pet.id THEN
    Pet.monto = mttl&
    PetShow.monto = mttl&
    PUT #3, I&, Pet
    EXIT FOR
END IF
NEXT
CLOSE #3

END SUB

DEFINT A-Z
SUB EditAll (l)
textocf 76, 2, 3, 3, "  "
textocf 76, 2, 16, 3, PetShow.Clave$
RegPa2
SELECT CASE l
    CASE IS = 1
        CALL EditBlood
    CASE IS = 2
        CALL EditOrina
    CASE IS = 3
        CALL EditHeces
    CASE IS = 4
        CALL EditVacun
    CASE IS = 5
        CALL EditNotas
END SELECT
END SUB

SUB EditBlood
U = 1
12
IF U = 1 THEN
    AW = 0: AWW = 2: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW _
 = 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 2 THEN
    AW = 1: AWW = 3: BW = 0: BWW = 2: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 3 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 0: CWW = 2: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 4 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 0: DWW = 2: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 5 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 0: EWW = 2: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 6 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 0: FWW = 2: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 7 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 0: GWW = 2: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 8 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 0: HWW = 2: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 9 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 0: IWW = 2: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 10 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 0: JWW = 2: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 11 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 0: KWW = 2: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 12 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 0: LWW = 2: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 13 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 0: MWW = 2: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 14 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 0: NWW  _
= 2: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 15 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: _
                                                      NWW = 1: OW = 0: OWW = 2: PW = 14: PWW = 1: QW = 14: QWW = 1
END IF
IF U = 16 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: _
                                                      NWW = 1: OW = 14: OWW = 1: PW = 0: PWW = 2: QW = 14: QWW = 1
END IF
IF U = 17 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: _
                                                      NWW = 1: OW = 14: OWW = 1: PW = 14: PWW = 1: QW = 0: QWW = 2
END IF
    textocf 3, 10, AW, AWW, "FECHA DEL EXAMEN:"
    textocf 2, 11, BW, BWW, "GLOBULOS BLANCOS:"
    textocf 2, 12, CW, CWW, "GLOBULOS ROJOS:"
    textocf 2, 13, DW, DWW, "HEMOGLOBINA:"
    textocf 2, 14, EW, EWW, "HEMATOCRITOS:"
    textocf 2, 15, FW, FWW, "HEMATIES:"
    textocf 2, 16, GW, GWW, "NEUTROFILOS:"
    textocf 2, 17, HW, HWW, "MONOCITOS:"
    textocf 2, 18, IW, IWW, "EOSINOFILOS:"
    textocf 40, 11, JW, JWW, "PLAQUETAS:"
    textocf 40, 12, KW, KWW, "FILARIA:"
    textocf 40, 13, LW, LWW, "ERLICHIOSIS:"
    textocf 40, 14, MW, MWW, "BABESIA:"
    textocf 40, 15, NW, NWW, "ANAPLASMA:"
    textocf 40, 16, OW, OWW, "HEPATOZOOM CANIS:"
    textocf 40, 17, PW, PWW, "TRIPANOSOMA:"
    textocf 40, 18, QW, QWW, "OTROS:"
DO
kbd$ = INKEY$
    SELECT CASE kbd$
        CASE IS = CHR$(27): CALL SavePac:

        EXIT SUB
        CASE IS = CHR$(0) + "M":
            IF U = 1 THEN U = 2: GOTO 12
            IF U > 10 THEN GOTO 12
            U = U + 8
            IF U > 17 THEN U = U - 8
            GOTO 12
        CASE IS = CHR$(0) + "K":
            IF U < 10 THEN GOTO 12
            U = U - 8
            IF U < 1 THEN U = 17
            GOTO 12
        CASE IS = CHR$(0) + "P": U = U + 1
            IF U > 17 THEN U = 1
            GOTO 12
        CASE IS = CHR$(0) + "H": U = U - 1
            IF U < 1 THEN U = 17
            GOTO 12
        CASE IS = CHR$(13):   EXIT DO
    END SELECT
LOOP
IF U = 1 THEN
kbd$ = VERDATE$(PetShow.examen(1), 21, 10)
    COLOR 0, 3: LOCATE 10, 21: PRINT PetShow.examen(1)
END IF
IF U = 2 THEN
    kbd$ = LLENARCF$(26, 11, 0, 7, RTRIM$(PetShow.examen(2)), PetShow.examen(2), 8, 8)
    COLOR 7, 1: LOCATE 11, 26: PRINT PetShow.examen(2)
END IF
IF U = 3 THEN
    kbd$ = LLENARCF$(26, 12, 0, 7, RTRIM$(PetShow.examen(3)), PetShow.examen(3), 8, 8)
    COLOR 7, 1: LOCATE 12, 26: PRINT PetShow.examen(3)
END IF
IF U = 4 THEN
    kbd$ = LLENARCF$(26, 13, 0, 7, RTRIM$(PetShow.examen(4)), PetShow.examen(4), 8, 8)
    COLOR 7, 1: LOCATE 13, 26: PRINT PetShow.examen(4)
END IF
IF U = 5 THEN
    kbd$ = LLENARCF$(26, 14, 0, 7, RTRIM$(PetShow.examen(5)), PetShow.examen(5), 8, 8)
    COLOR 7, 1: LOCATE 14, 26: PRINT PetShow.examen(5)
END IF
IF U = 6 THEN
    kbd$ = LLENARCF$(26, 15, 0, 7, RTRIM$(PetShow.examen(6)), PetShow.examen(6), 8, 8)
    COLOR 7, 1: LOCATE 15, 26: PRINT PetShow.examen(6)
END IF
IF U = 7 THEN
    kbd$ = LLENARCF$(26, 16, 0, 7, RTRIM$(PetShow.examen(7)), PetShow.examen(7), 8, 8)
    COLOR 7, 1: LOCATE 16, 26: PRINT PetShow.examen(7)
END IF
IF U = 8 THEN
    kbd$ = LLENARCF$(26, 17, 0, 7, RTRIM$(PetShow.examen(8)), PetShow.examen(8), 8, 8)
    COLOR 7, 1: LOCATE 17, 26: PRINT PetShow.examen(8)
END IF
IF U = 9 THEN
    kbd$ = LLENARCF$(26, 18, 0, 7, RTRIM$(PetShow.examen(9)), PetShow.examen(9), 8, 8)
    COLOR 7, 1: LOCATE 18, 26: PRINT PetShow.examen(9)
END IF
IF U = 10 THEN
    kbd$ = LLENARCF$(65, 11, 0, 7, RTRIM$(PetShow.examen(10)), PetShow.examen(10), 8, 8)
    COLOR 7, 1: LOCATE 11, 65: PRINT PetShow.examen(10)
END IF
IF U = 11 THEN
    kbd$ = LLENARCF$(65, 12, 0, 7, RTRIM$(PetShow.examen(11)), PetShow.examen(11), 8, 8)
    COLOR 7, 1: LOCATE 12, 65: PRINT PetShow.examen(11)
END IF
IF U = 12 THEN
    kbd$ = LLENARCF$(65, 13, 0, 7, RTRIM$(PetShow.examen(12)), PetShow.examen(12), 8, 8)
    COLOR 7, 1: LOCATE 13, 65: PRINT PetShow.examen(12)
END IF
IF U = 13 THEN
    kbd$ = LLENARCF$(65, 14, 0, 7, RTRIM$(PetShow.examen(13)), PetShow.examen(13), 8, 8)
    COLOR 7, 1: LOCATE 14, 65: PRINT PetShow.examen(13)
END IF
IF U = 14 THEN
    kbd$ = LLENARCF$(65, 15, 0, 7, RTRIM$(PetShow.examen(14)), PetShow.examen(14), 8, 8)
    COLOR 7, 1: LOCATE 15, 65: PRINT PetShow.examen(14)
END IF
IF U = 15 THEN
    kbd$ = LLENARCF$(65, 16, 0, 7, RTRIM$(PetShow.examen(15)), PetShow.examen(15), 8, 8)
    COLOR 7, 1: LOCATE 16, 65: PRINT PetShow.examen(15)
END IF
IF U = 16 THEN
    kbd$ = LLENARCF$(65, 17, 0, 7, RTRIM$(PetShow.examen(16)), PetShow.examen(16), 8, 8)
    COLOR 7, 1: LOCATE 17, 65: PRINT PetShow.examen(16)
END IF
IF U = 17 THEN
    kbd$ = LLENARCF$(65, 18, 0, 7, RTRIM$(PetShow.examen(17)), PetShow.examen(17), 8, 8)
    COLOR 7, 1: LOCATE 18, 65: PRINT PetShow.examen(17)
END IF
GOTO 12
END SUB

SUB EditHead

ajuste = 0

textocf 76, 2 + ajuste, 3, 3, "  "
textocf 76, 2 + ajuste, 16, 3, PetShow.Clave
U = 1
13
IF U = 1 THEN
    AW = 0: AWW = 2: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 2 THEN
    AW = 14: AWW = 1: BW = 0: BWW = 2: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 3 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 0: CWW = 2: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 4 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 0: DWW = 2: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 5 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 0: EWW = 2: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 6 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 0: FWW = 2: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 7 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 0: GWW = 2: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 8 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 0: HWW = 2: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 9 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 0: IWW = 2: JW = 14: JWW = 1: KW = 14: KWW = 1
END IF
IF U = 10 THEN
    AW = 14: AWW = 1: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 0: JWW = 2: KW = 14: KWW = 1
END IF

textocf 3, 4 + ajuste, AW, AWW, "PACIENTE: "
textocf 31, 4 + ajuste, BW, BWW, "RAZA/GENERO: "
textocf 60, 4 + ajuste, CW, CWW, "ENTRADA: "
textocf 3, 5 + ajuste, DW, DWW, "NACIO: "
textocf 38, 5 + ajuste, EW, EWW, "SEXO: "
textocf 46, 5 + ajuste, FW, FWW, "COLOR: "
textocf 62, 5 + ajuste, GW, GWW, "TALLA: "
textocf 3, 6 + ajuste, HW, HWW, "DUE¥O: "
textocf 35, 6 + ajuste, IW, IWW, "RIF-NIT-CI: "
textocf 3, 7 + ajuste, JW, JWW, "DIRECCION/TELF: "
textocf 16, 25 + ajuste, 3, 3, STRING$(64, " ")
DO
kbd$ = INKEY$
    SELECT CASE kbd$
        CASE IS = CHR$(27): CALL SavePacHead(Actual): EXIT SUB
        CASE IS = CHR$(0) + "P": U = U + 1
            IF U > 10 THEN
                U = 1
            END IF
            GOTO 13
        CASE IS = CHR$(0) + "H": U = U - 1
            IF U < 1 THEN
                U = 10
            END IF
            GOTO 13
        CASE IS = CHR$(0) + "M": U = U + 1
            IF U > 10 THEN
                U = 1
            END IF
            GOTO 13
        CASE IS = CHR$(0) + "K": U = U - 1
            IF U < 1 THEN
                U = 10
            END IF
            GOTO 13
        CASE IS = CHR$(13):   EXIT DO
    END SELECT
LOOP
IF DW = 0 THEN LOCATE 25, 16: COLOR 0, 3: PRINT "SOLO SE CALCULARAN FECHAS ENTRE 1.970 Y EL PRESENTE A¥O";
IF U = 1 THEN
1
    nombre$ = PetShow.nombr
    IF juan% = 1 THEN
    kbd$ = LLENARCF$(12, 2 + ajuste, 0, 7, PetShow.id, aquicf$, 15, 15)
    PetShow.id = TRIM$(aquicf$)
    END IF
    kbd$ = LLENARCF$(13, 4 + ajuste, 0, 7, PetShow.nombr, aquicf$, 15, 15)
    IF TRIM$(aquicf$) <> "" THEN
    zx = ASC(UCASE$(LEFT$(aquicf$, 1)))
        IF zx < 65 AND zx <> 60 OR zx > 90 AND zx <> 60 THEN
                textocf 13, 4 + ajuste, 4, 7, "   -ERROR-    ": BEEP: SLEEP 1: GOTO 1
        END IF
    END IF
    IF TRIM$(aquicf$) = "" THEN aquicf$ = "<* SIN NOMBRE*>"
    PetShow.nombr = TRIM$(UCASE$(aquicf$))
    IF PetShow.nombr <> nombre$ THEN Actual = 1
textocf 13, 4 + ajuste, 7, 1, PetShow.nombr
END IF
IF U = 2 THEN
    kbd$ = LLENARCF$(44, 4 + ajuste, 0, 7, TRIM$(PetShow.raza), PetShow.raza, 15, 15)
    PetShow.raza = UCASE$(PetShow.raza)
    textocf 44, 4 + ajuste, 7, 1, PetShow.raza
END IF
IF U = 3 THEN
    kbd$ = VERDATE$(PetShow.checkin, 69, 4)
    textocf 69, 4 + ajuste, 7, 1, PetShow.checkin
END IF
IF U = 4 THEN
    kbd$ = VERDATE$(PetShow.fecha, 10, 5)
    textocf 10, 5 + ajuste, 7, 1, PetShow.fecha
    Edad
END IF
IF U = 5 THEN
46
    kbd$ = LLENARCF$(44, 5 + ajuste, 0, 7, TRIM$(PetShow.sexos), PetShow.sexos, 1, 1)
    IF UCASE$(PetShow.sexos) <> "M" AND UCASE$(PetShow.sexos) = "F" OR UCASE$(PetShow.sexos) = "M" AND UCASE$(PetShow.sexos) <> "F" OR UCASE$(PetShow.sexos) <> "M" AND UCASE$(PetShow.sexos) = "H" OR UCASE$(PetShow.sexos) = "M" AND UCASE$(PetShow. _
sexos) <> "H" THEN
    ELSE
        BEEP: GOTO 46
    END IF
    textocf 44, 5 + ajuste, 7, 1, UCASE$(PetShow.sexos)
    PetShow.sexos = UCASE$(PetShow.sexos)
END IF
IF U = 6 THEN
    kbd$ = LLENARCF$(53, 5 + ajuste, 0, 7, TRIM$(PetShow.clor), PetShow.clor, 8, 8)
    textocf 53, 5 + ajuste, 7, 1, PetShow.clor
END IF
IF U = 7 THEN
    kbd$ = LLENARCF$(69, 5 + ajuste, 0, 7, TRIM$(PetShow.talla), PetShow.talla, 8, 8)
    textocf 69, 5 + ajuste, 7, 1, PetShow.talla
END IF
IF U = 8 THEN
    nombre$ = UCASE$(PetShow.dueno)
    kbd$ = LLENARCF$(10, 6 + ajuste, 0, 7, TRIM$(PetShow.dueno), PetShow.dueno, 20, 20)
    textocf 10, 6 + ajuste, 7, 1, PetShow.dueno
    IF PetShow.dueno <> nombre$ THEN Actual = 1
    PetShow.dueno = UCASE$(PetShow.dueno)
END IF
IF U = 9 THEN
    kbd$ = LLENARCF$(47, 6 + ajuste, 0, 7, TRIM$(PetShow.cedula), PetShow.cedula, 15, 15)
    textocf 47, 6 + ajuste, 7, 1, PetShow.cedula
END IF
IF U = 10 THEN
    kbd$ = LLENARCF$(19, 7 + ajuste, 0, 7, TRIM$(PetShow.direcc), PetShow.direcc, 50, 50)
    textocf 19, 7 + ajuste, 7, 1, PetShow.direcc
END IF
COLOR 3, 3
GOTO 13
END SUB

SUB EditHeces
U = 1
11
IF U = 1 THEN
    AW = 0: AWW = 2: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1
END IF
IF U = 2 THEN
    AW = 1: AWW = 3: BW = 0: BWW = 2: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1
END IF
IF U = 3 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 0: CWW = 2: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1
END IF
IF U = 4 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 0: DWW = 2: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1
END IF
IF U = 5 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 0: EWW = 2: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1
END IF
IF U = 6 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 0: FWW = 2: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1
END IF
IF U = 7 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 0: GWW = 2: HW = 14: HWW = 1: IW = 14: IWW = 1
END IF
IF U = 8 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 0: HWW = 2: IW = 14: IWW = 1
END IF
IF U = 9 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 0: IWW = 2
END IF
    textocf 3, 10, AW, AWW, "FECHA DEL EXAMEN:"
    textocf 2, 11, BW, BWW, "ASPECTO:"
    textocf 2, 13, CW, CWW, "COLOR:"
    textocf 2, 15, DW, DWW, "CONSISTENCIA:"
    textocf 2, 17, EW, EWW, "SANGRE:"
    textocf 40, 11, FW, FWW, "MOCO:"
    textocf 40, 13, GW, GWW, "REACCION:"
    textocf 40, 15, HW, HWW, "PARASITOS:"
    textocf 40, 17, IW, IWW, "RESTOS ALIMENTICIOS:"
DO
kbd$ = INKEY$
    SELECT CASE kbd$
        CASE IS = CHR$(27): SavePac: EXIT SUB
        CASE IS = CHR$(0) + "M":
            IF U = 1 THEN GOTO 11
            IF U > 5 THEN GOTO 11
            U = U + 4
            IF U > 9 THEN U = 1
            GOTO 11
        CASE IS = CHR$(0) + "K":
            IF U < 6 THEN GOTO 11
            U = U - 4
            IF U < 1 THEN U = 9
            GOTO 11
        CASE IS = CHR$(0) + "P": U = U + 1
            IF U > 9 THEN U = 1
            GOTO 11
        CASE IS = CHR$(0) + "H": U = U - 1
            IF U < 1 THEN U = 9
            GOTO 11
        CASE IS = CHR$(13): EXIT DO
    END SELECT
LOOP
IF U = 1 THEN
    kbd$ = VERDATE$(PetShow.examen(34), 21, 10)
    COLOR 0, 3: LOCATE 10, 21: PRINT PetShow.examen(34)
END IF
IF U = 2 THEN
    kbd$ = LLENARCF$(26, 11, 0, 7, TRIM$(PetShow.examen(35)), PetShow.examen(35), 8, 8)
    textocf 26, 11, 7, 1, PetShow.examen(35)
END IF
IF U = 3 THEN
    kbd$ = LLENARCF$(26, 13, 0, 7, TRIM$(PetShow.examen(36)), PetShow.examen(36), 8, 8)
    textocf 26, 13, 7, 1, PetShow.examen(36)
END IF
IF U = 4 THEN
    kbd$ = LLENARCF$(26, 15, 0, 7, TRIM$(PetShow.examen(37)), PetShow.examen(37), 8, 8)
    textocf 26, 15, 7, 1, PetShow.examen(37)
END IF
IF U = 5 THEN
    kbd$ = LLENARCF$(26, 17, 0, 7, TRIM$(PetShow.examen(38)), PetShow.examen(38), 8, 8)
    textocf 26, 17, 7, 1, PetShow.examen(38)
END IF
IF U = 6 THEN
    kbd$ = LLENARCF$(65, 11, 0, 7, TRIM$(PetShow.examen(39)), PetShow.examen(39), 8, 8)
    textocf 65, 11, 7, 1, PetShow.examen(39)
END IF
IF U = 7 THEN
    kbd$ = LLENARCF$(65, 13, 0, 7, TRIM$(PetShow.examen(40)), PetShow.examen(40), 8, 8)
    textocf 65, 13, 7, 1, PetShow.examen(40)
END IF
IF U = 8 THEN
    kbd$ = LLENARCF$(65, 15, 0, 7, TRIM$(PetShow.examen(41)), PetShow.examen(41), 8, 8)
    textocf 65, 15, 7, 1, PetShow.examen(41)
END IF
IF U = 9 THEN
    kbd$ = LLENARCF$(65, 17, 0, 7, TRIM$(PetShow.examen(42)), PetShow.examen(42), 8, 8)
    textocf 65, 17, 7, 1, PetShow.examen(42)
END IF
GOTO 11
END SUB

DEFSNG A-Z
SUB EditNotas
DIM notas AS STRING * 76
bb$ = PetShow.id
FOR II = 1 TO LEN(bb$)
A$ = RIGHT$(LEFT$(bb$, II), 1)
IF ASC(A$) < 32 THEN MID$(bb$, II, LEN(bb$)) = " "
NEXT
FILEZ$ = "AMVDAT\HISTOR~1\" + TRIM$(bb$) + ".DAT"
bb$ = ""
OPEN FILEZ$ FOR APPEND AS #35
IF LOF(35) < 988 THEN
GOSUB siestavacio
ELSE
CLOSE #35
END IF
LOCATE 25, 16: COLOR 0, 3: PRINT "F1: Fecha  ³";
OPEN FILEZ$ FOR RANDOM AS #35 LEN = 76
cant& = LOF(35) / 76
FOR I = 0 TO 12
GOSUB tomar
textocf 3, 23 - I, 7, 1, notas
NEXT
I = 12
d = 13
3
GOSUB tomar
2
GOSUB presentar
LOCATE 24 - d, 79: COLOR 14, 1: PRINT "<";
LOCATE 25, 36: COLOR 0, 3: PRINT "                ";
LOCATE 25, 29: COLOR 0, 3: PRINT "L¡nea: "; TRIM$(STR$(cant& - I)); "/"; TRIM$(STR$(cant&));
kbd$ = fillhand$(3, 24 - d, 0, 7, RTRIM$(notas), notas, 76, 76)
PUT #35, cant& - I, notas
GOSUB presentar
DO
SELECT CASE kbd$
        CASE IS = CHR$(0) + "P", CHR$(0) + "M", CHR$(8), CHR$(13): kbd$ = "":
                I = I - 1: d = d - 1
                IF d < 1 THEN d = 1: FOR f = 1 TO 76: textocf 2 + f, 24 - d, 7, 1, CHR$(SCREEN(24 - d, 2 + f)): NEXT: scrolluptxt
                IF cant& - I > cant& THEN GOSUB maslineas
                GOTO 3
        CASE IS = CHR$(0) + "H", CHR$(0) + "K", CHR$(8), CHR$(0) + CHR$(15): kbd$ = "":
                IF I >= cant& - 1 THEN I = cant& - 1: SOUND 58, 7: GOTO 3
                I = I + 1: d = d + 1
                IF d > 13 THEN d = 13: FOR f = 1 TO 76: textocf 2 + f, 24 - d, 7, 1, CHR$(SCREEN(24 - d, 2 + f)): NEXT: scrolldowntxt
                GOTO 3
        CASE IS = CHR$(27)
           GOSUB REMATAR: EXIT DO
        CASE ELSE: GOTO 3
END SELECT
LOOP
EXIT SUB

maslineas:
notas = STRING$(76, 32)
PUT #35, cant& + 1, notas: CLOSE #35
OPEN FILEZ$ FOR RANDOM AS #35 LEN = LEN(notas)
I = I + 1
cant& = LOF(35) / LEN(notas)
GOTO 3
RETURN

tomar:
GET #35, cant& - I + R, notas
FOR II = 1 TO LEN(notas)
A$ = RIGHT$(LEFT$(notas, II), 1)
IF ASC(A$) < 32 THEN MID$(notas, II, LEN(notas)) = ""
NEXT
RETURN

presentar:
FOR f = 1 TO 76: textocf 2 + f, 23 - d, 7, 1, CHR$(SCREEN(23 - d, 2 + f)): NEXT
FOR f = 1 TO 76: textocf 2 + f, 25 - d, 7, 1, CHR$(SCREEN(25 - d, 2 + f)): NEXT
RETURN


siestavacio:
CLOSE #35
OPEN FILEZ$ FOR RANDOM ACCESS WRITE AS #35 LEN = LEN(notas)
FOR j& = 1 TO 13
notas = ""
PUT #35, j&, notas
NEXT
CLOSE #35
RETURN

REMATAR:
CLOSE #35
OPEN FILEZ$ FOR RANDOM AS #35 LEN = 76
OPEN "AMVDAT\HISTOR~1\AMVNOTAS.TMP" FOR RANDOM ACCESS WRITE AS #3 LEN = 76
FOR I& = 1 TO 13
notas = ""
GET #35, I&, notas: PUT #3, I&, notas
NEXT
IF cant& > 13 THEN
FOR I& = cant& TO 14 STEP -1
notas = STRING$(76, 32)
GET #35, I&, notas
IF LTRIM$(notas) <> "" THEN EXIT FOR
IF LTRIM$(notas) = "" THEN notas = STRING$(76, 254): PUT #35, I&, notas
NEXT
II& = 13
FOR I& = 14 TO cant&
notas = STRING$(76, 32)
GET #35, I&, notas
IF notas <> STRING$(76, 254) THEN II& = II& + 1: PUT #3, II&, notas
NEXT
END IF
CLOSE #35: CLOSE #3
KILL FILEZ$
NAME "AMVDAT\HISTOR~1\AMVNOTAS.TMP" AS FILEZ$
OPEN FILEZ$ FOR RANDOM AS #3 LEN = 76
cant& = LOF(3) / 76
FOR I = 1 TO 13
GET #3, cant& - I - 13, PetShow.notas(I)
NEXT
OPEN "AMVDAT\AMVPA.DAT" FOR RANDOM ACCESS WRITE AS #22 LEN = LEN(Pet)
PUT #22, REG&, PetShow: CLOSE #22: CLOSE #3
RETURN
END SUB

DEFINT A-Z
SUB EditOrina
U = 1
10
IF U = 1 THEN
    AW = 0: AWW = 2: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW _
 = 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 2 THEN
    AW = 1: AWW = 3: BW = 0: BWW = 2: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 3 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 0: CWW = 2: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 4 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 0: DWW = 2: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 5 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 0: EWW = 2: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 6 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 0: FWW = 2: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 7 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 0: GWW = 2: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 8 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 0: HWW = 2: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 9 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 0: IWW = 2: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 10 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 0: JWW = 2: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 11 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 0: KWW = 2: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 12 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 0: LWW = 2: MW = 14: MWW = 1: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 13 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 0: MWW = 2: NW = 14: NWW  _
= 1: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 14 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 0: NWW  _
= 2: OW = 14: OWW = 1: PW = 14: PWW = 1
END IF
IF U = 15 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: _
                                 NWW = 1: OW = 0: OWW = 2: PW = 14: PWW = 1
END IF
IF U = 16 THEN
    AW = 1: AWW = 3: BW = 14: BWW = 1: CW = 14: CWW = 1: DW = 14: DWW = 1: EW = 14: EWW = 1: FW = 14: FWW = 1: GW = 14: GWW = 1: HW = 14: HWW = 1: IW = 14: IWW = 1: JW = 14: JWW = 1: KW = 14: KWW = 1: LW = 14: LWW = 1: MW = 14: MWW = 1: NW = 14: _
                                 NWW = 1: OW = 14: OWW = 1: PW = 0: PWW = 2
END IF
    textocf 3, 10, AW, AWW, "FECHA DEL EXAMEN:"
    textocf 2, 11, BW, BWW, "COLOR:"
    textocf 2, 12, CW, CWW, "ASPECTO:"
    textocf 2, 13, DW, DWW, "DENSIDAD:"
    textocf 2, 14, EW, EWW, "PH:"
    textocf 2, 15, FW, FWW, "PROTEINA:"
    textocf 2, 16, GW, GWW, "GLUCOSA:"
    textocf 2, 17, HW, HWW, "CUERPOS CETONICOS:"
    textocf 2, 18, IW, IWW, "BILIRRUBINA:"
    textocf 40, 11, JW, JWW, "HEMOGLOBINA:"
    textocf 40, 12, KW, KWW, "NITRITOS:"
    textocf 40, 13, LW, LWW, "UROBILINOGENO:"
    textocf 40, 14, MW, MWW, "LEUCOSITOS:"
    textocf 40, 15, NW, NWW, "HEMATIES:"
    textocf 40, 16, OW, OWW, "BACTERIAS:"
    textocf 40, 17, PW, PWW, "CELULAS EPITELIALES:"
DO
kbd$ = INKEY$
    SELECT CASE kbd$
        CASE IS = CHR$(27): CALL SavePac: EXIT SUB
        CASE IS = CHR$(0) + "M":
            IF U = 9 THEN U = 10: GOTO 10
            IF U = 1 THEN GOTO 10
            IF U > 10 THEN GOTO 10
            U = U + 8
            IF U > 16 THEN U = U - 8
            GOTO 10
        CASE IS = CHR$(0) + "K":
            IF U = 1 THEN GOTO 10
            IF U < 11 THEN GOTO 10
            U = U - 8
            IF U < 1 THEN U = 16
            GOTO 10
        CASE IS = CHR$(0) + "P": U = U + 1
            IF U > 16 THEN U = 1
            GOTO 10
        CASE IS = CHR$(0) + "H": U = U - 1
            IF U < 1 THEN U = 16
            GOTO 10
        CASE IS = CHR$(13):   EXIT DO
    END SELECT
LOOP
IF U = 1 THEN
    kbd$ = VERDATE$(PetShow.examen(20), 21, 10)
    COLOR 0, 3: LOCATE 10, 21: PRINT PetShow.examen(20)
END IF
IF U = 2 THEN
    kbd$ = LLENARCF$(26, 11, 0, 7, TRIM$(PetShow.examen(21)), PetShow.examen(21), 8, 8)
    COLOR 7, 1: LOCATE 11, 26: PRINT PetShow.examen(21)
END IF
IF U = 3 THEN
    kbd$ = LLENARCF$(26, 12, 0, 7, TRIM$(PetShow.examen(22)), PetShow.examen(22), 8, 8)
    COLOR 7, 1: LOCATE 12, 26: PRINT PetShow.examen(22)
END IF
IF U = 4 THEN
    kbd$ = LLENARCF$(26, 13, 0, 7, TRIM$(PetShow.examen(23)), PetShow.examen(23), 8, 8)
    COLOR 7, 1: LOCATE 13, 26: PRINT PetShow.examen(23)
END IF
IF U = 5 THEN
    kbd$ = LLENARCF$(26, 14, 0, 7, TRIM$(PetShow.examen(24)), PetShow.examen(24), 8, 8)
    COLOR 7, 1: LOCATE 14, 26: PRINT PetShow.examen(24)
END IF
IF U = 6 THEN
    kbd$ = LLENARCF$(26, 15, 0, 7, TRIM$(PetShow.examen(25)), PetShow.examen(25), 8, 8)
    COLOR 7, 1: LOCATE 15, 26: PRINT PetShow.examen(25)
END IF
IF U = 7 THEN
    kbd$ = LLENARCF$(26, 16, 0, 7, TRIM$(PetShow.examen(26)), PetShow.examen(26), 8, 8)
    COLOR 7, 1: LOCATE 16, 26: PRINT PetShow.examen(26)
END IF
IF U = 8 THEN
    kbd$ = LLENARCF$(26, 17, 0, 7, TRIM$(PetShow.examen(8)), PetShow.examen(8), 8, 8)
    COLOR 7, 1: LOCATE 17, 26: PRINT PetShow.examen(8)
END IF
IF U = 9 THEN
    kbd$ = LLENARCF$(26, 18, 0, 7, TRIM$(PetShow.examen(27)), PetShow.examen(27), 8, 8)
    COLOR 7, 1: LOCATE 18, 26: PRINT PetShow.examen(27)
END IF
IF U = 10 THEN
    kbd$ = LLENARCF$(65, 11, 0, 7, TRIM$(PetShow.examen(28)), PetShow.examen(28), 8, 8)
    COLOR 7, 1: LOCATE 11, 65: PRINT PetShow.examen(28)
END IF
IF U = 11 THEN
    kbd$ = LLENARCF$(65, 12, 0, 7, TRIM$(PetShow.examen(29)), PetShow.examen(29), 8, 8)
    COLOR 7, 1: LOCATE 12, 65: PRINT PetShow.examen(29)
END IF
IF U = 12 THEN
    kbd$ = LLENARCF$(65, 13, 0, 7, TRIM$(PetShow.examen(30)), PetShow.examen(30), 8, 8)
    COLOR 7, 1: LOCATE 13, 65: PRINT PetShow.examen(30)
END IF
IF U = 13 THEN
    kbd$ = LLENARCF$(65, 14, 0, 7, TRIM$(PetShow.examen(31)), PetShow.examen(31), 8, 8)
    COLOR 7, 1: LOCATE 14, 65: PRINT PetShow.examen(31)
END IF
IF U = 14 THEN
    kbd$ = LLENARCF$(65, 15, 0, 7, TRIM$(PetShow.examen(6)), PetShow.examen(6), 8, 8)
    COLOR 7, 1: LOCATE 15, 65: PRINT PetShow.examen(6)
END IF
IF U = 15 THEN
    kbd$ = LLENARCF$(65, 16, 0, 7, TRIM$(PetShow.examen(32)), PetShow.examen(32), 8, 8)
    COLOR 7, 1: LOCATE 16, 65: PRINT PetShow.examen(32)
END IF
IF U = 16 THEN
    kbd$ = LLENARCF$(65, 17, 0, 7, TRIM$(PetShow.examen(33)), PetShow.examen(33), 8, 8)
    COLOR 7, 1: LOCATE 17, 65: PRINT PetShow.examen(33)
END IF
GOTO 10
END SUB

