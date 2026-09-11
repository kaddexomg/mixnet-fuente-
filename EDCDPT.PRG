PROCEDURE DPT0000
 PARAMETERS P1,P2,P3
  edit_ventana('EDCDPT')
RETURN

FUNCTION CHdpt(EL_CMP, fil,col,x)
 LOCAL USEL,DEVO:=.T.
 IF LASTKEY()=5
  RETURN .T.
 ENDIF
 USEL =SELECT()
 DBSELECTAR('MXCTADPT')
 SET SOFTSEEK ON
 SEEK(EL_CMP)
 SET SOFTSEEK OFF
 IF !FOUND()
    IF EOF()
       DBGOTOP()
    ENDIF
    edit_ventana('EDCDPT')
    CUR_ON()
    DEVO=.F.
 ELSE
    IF VALTYPE(FIL)='N'
       @ fil,col SAY NOMCOD(EL_CMP,[MXCTADPT],[NOMDPT])
    ENDIF
    DEVO:=.T.
 ENDIF
 DBSELECTAR(USEL)
RETURN DEVO

FUNCTION _CHdpt(EL_ALM, fil,col,x)
 LOCAL USEL,DEVO:=.T.
 IF LASTKEY()=5
  RETURN .T.
 ENDIF
 USEL =SELECT()
 IF ABRIR([MXCTADPT],MXCTADPT,[S],MXCTDPX1)
  SEEK(EL_ALM)
  IF !FOUND()
   DPT0000()
   CUR_ON()
   DEVO=.F.
  ELSE
   IF VALTYPE(FIL)='N'
    @ fil,col SAY NOMCOD(EL_ALM,[MXCTADPT],[NOMDPT])
   ENDIF
   DEVO:=.T.
  ENDIF
  CERRAR()
 ENDIF
 SELE (USEL)
RETURN DEVO

************************************************************************************
*** Calcula e incluye Movimientos en inventario provenientes de notas de salidas ***
************************************************************************************
Function Act_Nsi(xxcodmovart,xxfecha_mov,xxhora,xxnumdoc,xxunidad,xxcantidad,xxalms,xxalme,xxcodcon,xxcoddpto)
 Private xcodmovart := xxcodmovart
 Private xfecha_mov := xxfecha_mov
 Private xhora      := xxhora
 Private xtipinv    := IF(cantidad<0,'AE','SA')
 Private xnumdoc    := xxnumdoc
 Private xunidad    := xxunidad
 Private xcantidad  := abs(xxcantidad)
 Private xcosto_tot := tot_costo
 Private xorigen    := 'NSI'
 Private xprov_cli  := xxalme
 Private xventa_tot := 0
 Private xcodven    := 'ASNSI'
 Private xalmacen   := xxalms
 Private sw_recal   := .f.
 Private xcodcon    := if(Empty(xxcodcon),' ',xxcodcon)
 Private xcoddpto   := if(Empty(xxcoddpto),' ',xxcoddpto)
 Private ult_sel    := Select()
 If !Abrir('MXTRAINV',mxtrainv,[S],mxtrinx1,mxtrinx2,mxtrinx3)
    pare('No se pudo abrir el archivo de inventario')
    DbSelectar(ult_sel)
    Return(.f.)
 endif
 Mensaj('Generando movimientos de salidas para el c¢digo: '+alltrim(xcodmovart))
 if xxalme=nil .and. xcantidad<0
    xCantidad:=abs(xcantidad)
 endif
 DbSelectar('mxtrainv')
 Add_Rec(0)
 Arc_Inv()
 if sw_recal
  RecalCos(xcodmovart)
 endif
 Cerrar('MXTRAINV')
 DbSelectar(ult_sel)
Return Nil

* Reversa la notas de salida interna
function Rev_Nsi(xxcodmovart,xxfecha_mov,xxnumdoc,xxunidad,x)
 Private xcodmovart := xxcodmovart
 Private xfecha_mov := xxfecha_mov
 Private xnumdoc    := xxnumdoc
 Private xunidad    := xxunidad
 Private xtipinv    := if(cantidad<0,'AE','SA')
 Private xorigen    := 'NSI'
 Private sw_recal   := .f.
 Private ult_sel    := Select()
 Private busca_esto := xnumdoc+xTipinv+xcodmovart+xOrigen
 If !Abrir('MXTRAINV',mxtrainv,[S],mxtrinx1,mxtrinx2,mxtrinx3)
    pare('No se pudo abrir el archivo de inventario')
    DbSelectar(ult_sel)
    Return(.f.)
 endif
 Mensaj('Reversando movimientos de salida para el c¢digo: '+alltrim(xcodmovart))
 DbSelectar('MXTRAINV')
 DbSetOrder(3)
 if DbSeek(busca_esto)
    DbSetOrder(1)
    rev_inv()
    DbSelectar('MXTRAINV')
    rec_lock(0)
    DbDelete()
    UNLOCK
    if sw_recal
       recalcos(xcodmovart)
    endif
 endif
 Cerrar('MXTRAINV')
 DbSelectar(ult_sel)
Return

Function Dev_Nsi()
   local uSel:=Select()
   DBSELECTAR("MXRENNSA")
   IF DbSeek(MXENCNSA->numtrf)
      HacerBuffer('MXENCNSA->NUMTRF=MXRENNSA->NUMTRF',buffer)
      IF abrir('BUFFER',buffer,'E')
         DbEval(Blk("BUFFER->cantidad:=cantidad*-1,BUFFER->tot_costo:=tot_costo*-1"),,Blk("!eof()"))
         Cerrar('BUFFER')
      ENDIF
   ENDIF
   DbSelectar(uSel)
RETURN




