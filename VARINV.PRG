FUNCTION INV0601
* OPC6[1]=[Cambio fecha]
MOD0601()
RETURN NIL

FUNCTION INV0602
* OPC6[3]=[Cambio operador]
MOD0603()
RETURN NIL

FUNCTION INV0603
* CONTADORES DE INVENTARIO
IF ABRIR('NUMODP',MXNUMODP,[E])
   XNUMODP=NUMERO
IF ABRIR('NUMNSA',MXNUMNSA,[E])
   XNUMNSA=NUMERO
IF ABRIR('NUMTRA',MXNUMTRA,[E])
   XNUMTRA=NUMERO
IF ABRIR('NUMGUI',MXNUMGUI,[E])
   XNUMGUI=NUMERO
   BOXSOM([ CONTADORES ],12,40,17,70)
   @ 13,41 SAY 'Transferencias...:' get XNUMTRA VALID !EMPTY(XNUMTRA)
   @ 14,41 SAY 'Nota de Salidas..:' get XNUMNSA VALID !EMPTY(XNUMNSA)
   @ 15,41 SAY 'Orden de Prod....:' get XNUMODP VALID !EMPTY(XNUMODP)
   @ 16,41 SAY 'Guias de Despacho:' get XNUMGUI VALID !EMPTY(XNUMGUI)
   CUR_ON()
   READ
   CUR_OFF()
   IF !ESCAPAR()
       AUDITORIA([Acceso a la opci¢n de contadores (Salvados)])
       ININUM('NUMTRA')
       ININUM('NUMNSA')
       ININUM('NUMODP')
       ININUM('NUMGUI')
   ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
RETURN NIL

FUNCTION INV0604
 XLIBODP=PAD(G_LIBODP,8)
 XLIBTRF=PAD(G_LIBTRF,8)
 XLIBNSA=PAD(G_LIBNSA,8)
 XLIBGUI=PAD(G_LIBGUI,8)

 BOXSOM(" CATALOGOS ",9,30,14,67,2)
 SETCOLOR(C_TEX2)
 @ 10,32 SAY [    Producci¢n:] GET XLIBODP
 @ 11,32 SAY [Transferencias:] GET XLIBTRF
 @ 12,32 SAY [Nota de salida:] GET XLIBNSA
 @ 13,32 SAY [Guia Despacho.:] GET XLIBGUI
 LEER()
 IF !ESCAPAR()
  G_LIBODP=ALLTRIM(XLIBODP)
  G_LIBTRF=ALLTRIM(XLIBTRF)
  G_LIBNSA=ALLTRIM(XLIBNSA)
  G_LIBGUI=ALLTRIM(XLIBGUI)
  SAVE TO LIBREP.MEM ALL LIKE G_LIB*
  AUDITORIA([Acceso a la opci¢n de catalogo (Salvados)])
 ENDIF
RETURN NIL

FUNCTION INV0605
Private PVFILE:=CE([MXPARINV.MTD]),xLI:=3,xCI:=1

IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF

BOXSOM(" Par metros generales ",xLI,xCI,xLI+17,xCI+54,2)
SETCOLOR(C_TEX2)
@ ++xLi,xCi+1 SAY [ ¨ Activar c¢digo contable en renglones?.(S/N):] GET inv_codcon PICT '!' VALID inv_codcon$'SN'
@ ++xLi,xCi+1 SAY [ ¨ Siempre Recalcular Costo ?............(S/N):] GET inv_recalc PICT '!' VALID inv_recalc$'SN'
@ ++xLi,xCi+1 SAY [ ¨ Utilizar Ultimo Costo para c lculos...(S/N):] GET inv_ultcos PICT '!' VALID inv_ultcos$'SN'

@ ++xLI,xCI+1 SAY [ ¨ Desglosa el I.V.A. al Facturar ?.....(S/N).:] GET inv_divaf  PICT '!'  VALID inv_divaf$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Desea Cambiar transf. x N/E Interna ?.(S/N):] GET inv_trfne  PICT '!'  VALID inv_trfne$'SN'
++xli
@ ++xLI,xCI+1 SAY [ OPCIONES DE REPRECIACION SEGUN MARGEN POR GRUPOS] COLOR c_tit2
@ ++xLI,xCI+1 SAY [ ¨ Activar Repreciaci¢n Automatica     ?.(S/N).:] GET inv_repr   PICT '!' VALID inv_repr$'SN'
@ ++xLI,xCI+1 SAY [ Segun CP=Costo Promedio, CA=Costo actual......:] GET inv_cualc  PICT '!!' WHEN inv_repr$'S' VALID inv_cualc$'CP.CA'
@ ++xLI,xCI+1 SAY [ ¨ Convertir cifras a Enteros          ?.(S/N).:] GET inv_entero PICT '!'  WHEN inv_repr$'S' VALID inv_entero$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Convertir Enteros a Multiplos de 10 ?.(S/N).:] GET inv_multi  PICT '!'  WHEN inv_repr$'S' VALID inv_multi$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Incluye I.V.A. al Precio de ventas  ?.(S/N).:] GET inv_civa   PICT '!'  WHEN inv_repr$'S' VALID inv_civa$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Filtrar Productos por Uso           ?.(S/N).:] GET inv_filpr  PICT '!'  VALID inv_filpr$'SN'
@ ++xLI,xCI+1 SAY [ Configurar Productos a usar en Ventas.........:] GET inv_usven   PICT '9999' WHEN  inv_filpr='S' VALID ValUsos(@inv_usven,17,48)
@ ++xLI,xCI+1 SAY [ Configurar Productos a usar en Compras........:] GET inv_uspag   PICT '9999' WHEN  inv_filpr='S' VALID ValUsos(@inv_uspag,18,48)
@ ++xLI,xCI+1 SAY [ ¨ Filtrar Productos desactivados      ?.(S/N).:] GET inv_prdes  PICT '!'  VALID inv_prdes$'SN'
leer()
IF !ESCAPAR()
 SAVE TO (PVFILE) ALL LIKE inv_*
 AUDITORIA([Acceso a la opci¢n de par metros generales (Salvados)])
 OPC3[ 7]=if(inv_trfne='N',[Transferencias],[N/E Interna])
ENDIF
RETURN NIL

FUNCTION INV0606
* OPC6[5]=[Cambio Ejercicio]
MOD0605()
RETURN NIL

FUNCTION INV0607
RETURN(ASIGNA_COLOR())


PROCEDURE INV0316
 Editor_Def()
RETURN

Static Function ValUsos(cUsos,xl,xc)
  local aOpc,uColor:=SetColor()
  IF LASTKEY()=5
     RETURN .T.
  ENDIF
  IF Len(Alltrim(cUsos))=4
     RETURN .T.
  ENDIF
  FOR x := 1 TO LEN(cUsos)
   IF !substr(cUsos,x,1)$'0123'
      ALERTA('DEBE ESCRIBIR: 0,1,2,3')
      RETURN .F.
   ENDIF
  NEXT
  aOpc:={"0 = Todo Uso             ",;
         "1 = Producto Terminado   ",;
         "2 = Materia Prima        ",;
         "3 = Materiales Indirectos"}
  nOpc:=la_opcion(aOpc,xl,xc,4,1,[ DIFERENTES USOS ])
  IF lastkey()=27 .OR. nOpc=0
      RETURN .T.
  ENDIF
  IF nOpc=1
     cUsos:=alltrim(cUsos)+'0'
  ELSEIF nOpc=2
     cUsos:=alltrim(cUsos)+'1'
  ELSEIF nOpc=3
     cUsos:=alltrim(cUsos)+'2'
  ELSEIF nOpc=4
     cUsos:=alltrim(cUsos)+'3'
  ENDIF
  cUsos:=Pad(cUsos,4)
  SetColor(uColor)
Return(.f.)






