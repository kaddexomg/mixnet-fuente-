PROCEDURE INV0314
if g_multi
   ALM0000()
else
   Pare(men15)
endif
RETURN

PROCEDURE ALM0000
 PARAMETERS P1,P2,P3
 edit_ventana('EDCALM')
RETURN

FUNCTION CHalm(EL_ALM, fil,col,x)
 LOCAL USEL,DEVO:=.T.
 IF LASTKEY()=5
  RETURN .T.
 ENDIF
 IF !G_MULTI
    RETURN .T.
 ENDIF
 USEL =SELECT()
 if pcount()<4
  if ustr_edoalm='N' .and. el_alm<>ustr_almuser
     Pare(men12)
     SELE (USEL)
     RETURN .f.
  endif
 endif
 if ABRIR([MXCTAALM],MXCTAALM,[S],MXCTALX1)
  SEEK(EL_ALM)
  IF !FOUND()
   ALM0000()
   CUR_ON()
   DEVO=.F.
  ELSE
   if blk_alm='S' .and. Empty(blk_fec)
      Pare('Almac‚n seleccionado se encuentra bloqueado, intente con otro')
      CERRAR('MXCTAALM')
      SELE (USEL)
      RETURN .f.
   endif
   IF VALTYPE(FIL)='N'
    @ fil,col SAY NOMCOD(EL_ALM,[MXCTAALM],[NOMALM])
   ENDIF
   DEVO:=.T.
  ENDIF
  CERRAR('MXCTAALM')
 ENDIF
 SELE (USEL)
RETURN DEVO




