FUNCTION CON0601
* OPC6[1]=[Cambio fecha]
MOD0601()
RETURN NIL

FUNCTION CON0602
* OPC6[3]=[Cambio operador]
MOD0603()
RETURN NIL

FUNCTION CON0603
PRIVATE XNUMASI,tnumasi
IF ABRIR('NUMASI',MXNUMASI,[E])
 XNUMASI=NUMERO
 tnumasi:=numero
 BOXSOM([ CONTADOR ],13,40,15,70)
 @ 14,42 SAY 'Asientos .......:' get XNUMasi VALID !EMPTY(XNUMasi)
 CUR_ON()
 READ
 CUR_OFF()
 IF !ESCAPAR()
  AUDITORIA([Accesando mantenimiento de Contadores],xnumasi,tnumasi,[MOD])
  ININUM('NUMASI')
 ENDIF
ENDIF
RETURN NIL

FUNCTION CON0604
* DEFINICIONES
PRIVATE pvfile:=CE([LREPCON.MTD]),xLin:=6,xCol:=4
Private con_mgral:=SPACE(12)
Private con_bcomp:=SPACE(12)
Private con_bgsde:=SPACE(12)
Private con_bgspl:=SPACE(12)
if file(pvfile)
   RESTORE FROM (pvfile) ADDITIVE
endif
BOXSOM(" DEFINICIONES (G.I) ",xLin,xCol,xLin+5,xCol+43,2)
SETCOLOR(C_TEX2)
@  ++xLin,xCol+1 SAY [Mayor general INF          :] GET CON_MGRAL
@  ++xLin,xCol+1 SAY [Balance de comprobacion INF:] GET CON_BCOMP
@  ++xLin,xCol+1 SAY [Balance general S/Defin.   :] GET CON_BGSDE
@  ++xLin,xCol+1 SAY [Balance general S/Plan.    :] GET CON_BGSPL
leer()
IF !ESCAPAR()
 SAVE TO (pvfile) ALL LIKE CON_*
 AUDITORIA([Accesando la opcion de Definiciones],,[MOD])
ENDIF
RETURN  NIL

PROCEDURE CON0605
* DEFINICIONES
DEFICON()
RETURN NIL

FUNCTION CON0606
* OPC6[5]=[Cambio Ejercicio]
MOD0605()
RETURN NIL

FUNCTION CON0607
RETURN(ASIGNA_COLOR())

FUNCTION CON0310
 EDITOR_DEF()
RETURN NIL


FUNCTION CON0305
 Init_Ver()
 Edit_Ventana('EDCCCCON')
RETURN



















