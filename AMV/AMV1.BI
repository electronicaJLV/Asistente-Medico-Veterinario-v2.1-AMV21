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
DECLARE SUB scrolluptxt ()
DECLARE SUB scrolldown ()
DECLARE SUB scrolldownall ()
DECLARE SUB scrolldowntxt ()
DECLARE SUB textocf (txx%, txy%, txcol%, txcolf%, que$)
DECLARE FUNCTION TRIM$ (vari$)
DECLARE SUB UnScroll (UpRow%, LtCol%, LoRow%, RtCol%)
DECLARE FUNCTION VERDATE$ (who$, x%, y%)
TYPE cr
   clax AS STRING * 10
END TYPE

TYPE MU
        LST AS STRING * 1
        linea AS STRING * 76
END TYPE

TYPE ma
    serial AS STRING * 30
    nombre AS STRING * 35
    direcc AS STRING * 40
    telefonos AS STRING * 31
    rif AS STRING * 14
    nit AS STRING * 14
    email AS STRING * 31
END TYPE

TYPE TipoIndice
  id AS STRING * 13
  nombr AS STRING * 15
  dueno AS STRING * 20
  posic as long
END TYPE

TYPE TipoIndiceInv
    id    AS STRING * 13
    produ AS STRING * 25
    posic AS LONG
END TYPE
TYPE Registro
  Clave AS STRING * 2
  id AS STRING * 13
  nombr AS STRING * 15
  fecha AS STRING * 8
  sexos AS STRING * 1
  clor AS STRING * 10
  talla AS STRING * 10
  raza AS STRING * 15
  dueno AS STRING * 20
  cedula AS STRING * 15
  direcc AS STRING * 50
  checkin AS STRING * 8
  monto AS LONG
  notas(1 TO 13)  AS STRING * 76
  examen(1 TO 76) AS STRING * 8
END TYPE
TYPE Pregistro
  Clase AS STRING * 1
  Clave AS STRING * 2
  id AS STRING * 13
  produ AS STRING * 25
  preci AS LONG
  costo AS LONG
  exist AS LONG
  minima AS LONG
  compra AS STRING * 8
  venci AS STRING * 8
  distr AS STRING * 20
  notas(1 TO 13) AS STRING * 76
END TYPE
TYPE Fregistro
  Clave AS STRING * 2
  fac AS STRING * 13
  id AS STRING * 13
  dueno AS STRING * 20
  cedula AS STRING * 12
  direcc AS STRING * 50
  nombr AS STRING * 15
  fecha AS STRING * 8
  checkin AS STRING * 8
END TYPE

COMMON SHARED clinica AS ma
COMMON SHARED cl AS cr
COMMON SHARED vuestra AS MU

COMMON SHARED indice AS TipoIndice
COMMON SHARED indiceinv AS TipoIndiceInv

COMMON SHARED Pet     AS Registro
COMMON SHARED PetShow AS Registro

COMMON SHARED Invent     AS Pregistro
COMMON SHARED InventShow AS Pregistro

COMMON SHARED Cargo    AS Fregistro
COMMON SHARED FactShow AS Fregistro


COMMON SHARED REG&, REGNUMERO&, mysubtotal&, cant&, precio&, mttl&, rgero&, neto&
COMMON SHARED LARGO%, l%, EnGtCla%, jl%, zx%, li%, Pv%, salir%, alba%, f%, juan%
COMMON SHARED nombre$, direcc$, telefonos$, rif$, nit$, email$, Iniciales$, RGMERO$
COMMON SHARED MERO$, rgero$, AS$, FF$, REGNOMBRE$, myserial$, myfecha$, myclave$, mycodigo$, mynombre$, mytime$
COMMON SHARED mycantidad$, cha$
DIM SHARED ScrollUpAsm(1 TO 7)
DIM SHARED ScrollDownAsm(1 TO 7)

