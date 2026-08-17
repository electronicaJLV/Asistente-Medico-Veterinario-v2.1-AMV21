REM $DYNAMIC
DIM SHARED letra$(3)
    letra$(1) = "P"
    letra$(2) = "F"
    letra$(3) = "I"
DIM SHARED ITEM$(3)
    ITEM$(1) = " PACIENTES "
    ITEM$(2) = " FACTURACION "
    ITEM$(3) = " INVENTARIO "
DIM SHARED ITEMX%(3)
    ITEMX%(1) = 2
    ITEMX%(2) = LEN(ITEM$(1)) + 5
    ITEMX%(3) = LEN(ITEM$(1)) + ITEMX%(2) + 4

' NOTA: Ahora las columnas 1 y 3 (Pacientes e Inventario) tienen 5 opciones.
' La columna 2 (Facturaci¢n) se mantiene intacta con sus 4 opciones originales.
DIM SHARED MNITEM$(11, 3) 
    ' --- PACIENTES ---
    MNITEM$(1, 1) = " MOSTRAR   "
    MNITEM$(2, 1) = " LISTA     "
    MNITEM$(3, 1) = " REGISTRAR "
    MNITEM$(4, 1) = " ELIMINAR  "  ' <-- Subi¢ del puesto 5 al 4
    MNITEM$(5, 1) = " RECUPERAR "  ' <-- Subi¢ del puesto 6 al 5
   
    MNITEM$(6, 1) = " VER, EDITAR HISTORIAS, HACER CARGOS A PACIENTES "
    MNITEM$(7, 1) = " VER, IMPRIMIR LISTA GENERAL DE PACIENTES        "
    MNITEM$(8, 1) = " INGRESO DE PACIENTES                            "
    MNITEM$(9, 1) = " ELIMINAR INDIVIDUALMENTE REGISTROS DE PACIENTES  " ' <-- Subi¢ del 11 al 9
    MNITEM$(10, 1) = " VER, RECUPERAR REGISTROS DE PACIENTES ELIMINADOS" ' <-- Subi¢ del 12 al 10
   
    ' --- FACTURACIàN (Se mantiene igual) ---
    MNITEM$(1, 2) = " FACTURAR  "
    MNITEM$(2, 2) = " HISTORIAL "
    MNITEM$(3, 2) = " RESUMEN   "
    MNITEM$(4, 2) = " GASTOS    "
   
    MNITEM$(6, 2) = " EMISION DE FACTURACION                          "
    MNITEM$(7, 2) = " VER RECUPERAR DE FACTURACION                    "
    MNITEM$(8, 2) = " RESUMEN DEL MOVIMIENTO ACTUAL DE CAJA           "
    MNITEM$(9, 2) = " VER, REGISTRAR LOS GASTOS DE LA CLINICA         "
   
    ' --- INVENTARIO ---
    MNITEM$(1, 3) = " MOSTRAR   "
    MNITEM$(2, 3) = " LISTA     "
    MNITEM$(3, 3) = " REGISTRAR "
    MNITEM$(4, 3) = " ELIMINAR  "  ' <-- Subi¢ del puesto 5 al 4
    MNITEM$(5, 3) = " RECUPERAR "  ' <-- Subi¢ del puesto 6 al 5

    MNITEM$(6, 3) = " VER PRODUCTOS O SERVICIOS INDIVIDUALMENTE"
    MNITEM$(7, 3) = " VER, IMPRIMIR INVENTARIO GENERAL (PRODUCTOS O SERVICIOS)"
    MNITEM$(8, 3) = " REGISTRAR PRODUCTOS O SERVICIOS "
    MNITEM$(9, 3) = " ELIMINAR REGISTROS DEL INVENTARIO, INDIVIDUALMENTE " ' <-- Subi¢ del 11 al 9
    MNITEM$(10, 3) = " VER, RECUPERAR REGISTROS ELIMINADOS DEL INVENTARIO " ' <-- Subi¢ del 12 al 10

' NOTA: Reducimos la matriz de renglones en pantalla. Ahora el men£ de barras 
' desplegables solo pintar  5 filas verticales en lugar de 6.
DIM SHARED MNITEMY%(5)
    MNITEMY%(1) = 4
    MNITEMY%(2) = 5
    MNITEMY%(3) = 6
    MNITEMY%(4) = 7
    MNITEMY%(5) = 8
