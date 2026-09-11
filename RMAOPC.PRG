* EDITOR DE TABLA DE TECNICOS
FUNCTION RMA0302()
 EDIT_VENTANA('TECNICOS')
RETURN NIL

* EDITOR DE MARCA DE EQUIPOS
FUNCTION RMA0303()
 EDIT_VENTANA('RMAMAREQ')
RETURN NIL

* EDITOR DE SERIE DE EQUIPOS
FUNCTION RMA0304()
 EDIT_VENTANA('RMASERIE')
RETURN NIL

* EDITOR DE MODELO DE EQUIPOS
FUNCTION RMA0305()
 EDIT_VENTANA('RMAMODEQ')
RETURN NIL

* EDICION DE COTIZACIONES EXPRESS
PROCEDURE RMA0307
 Private ii_listasi:=''
 Private es_oferta:=.f.
 Edit_ventana('COTEX01')
RETURN

* EDICION DE R.M.A.
PROCEDURE RMA0308
 Private ii_listasi:=''
 Private es_oferta:=.f.
 Edit_ventana('RECEPRMA')
RETURN

* LLAMA A A LOS EDITORES
PROCEDURE RMA0310
 PARE('NO ESTA DISPONIBLE EN ESTE MODULO')
RETURN

FUNCTION RMA0601
 * OPC6[1]=[Cambio fecha]
 MOD0601()
RETURN NIL

FUNCTION RMA0602
 * OPC6[3]=[Cambio operador]
 MOD0603()
RETURN NIL


FUNCTION RMA0603
 * CONTADORES
 IF ABRIR('NUMRMA',g_camino+'RMANUMRE',[E])
    XNUMRMA=NUMERO
    BOXSOM([ CONTADORES ],7,39,9,70)
    @ 8 ,41 SAY 'Numero de R.M.A.:' get XNUMRMA VALID !EMPTY(XNUMRMA)
    leer()
    IF !ESCAPAR()
       AUDITORIA([Acceso a la opci¢n de contadores (Salvados)])
       ININUM([NUMRMA])
    ENDIF
 ENDIF
RETURN NIL

FUNCTION RMA0604
 * DEFINICIONES
 XLIB1=PAD(G_LIBRMA,8)
 BOXSOM("Catalogos",7,35,9,74,2)
 SETCOLOR(C_TEX2)
 @  8,37 SAY [Libreria de R.M.A..:] GET XLIB1
 leer()
 IF !ESCAPAR()
  G_LIBRMA=ALLTRIM(XLIB1)
  SAVE TO LIBREP.MEM ALL LIKE G_LIB*
  AUDITORIA([Acceso a la opci¢n de catalogos (Salvados)])
 ENDIF
RETURN  NIL

FUNCTION RMA0605()
RETURN(MOD0605())

FUNCTION RMA0606()
RETURN(ASIGNA_COLOR())

// Revision de equipo
function Revi(t5)
  Private aTIT:={Pad([  Fecha de Revisi¢n:],20),;
                 Pad([            Tecnico:],20),;
                 Pad([ ¨ Garantia (S/N) ?:],20),;
                 Pad([ Fecha fin garantia:],20)}
  Private aCMP:={ctod(g_fecha),RMARECEP->codtecr,RMARECEP->garantia,RMARECEP->fecvenceg}
  Private aMAS:={[],[],[!],[]}
  Private aVAL:={{'.f.','.t.','.t.','if(aCMP[3]$"S",.t.,.f.)'},{'.t.','if(RMATECNI->(dbseek(aCMP[2])),.t.,123->(edit_ventana([TECNICOS]),off_key(),.f.))','if(aCMP[3]$"SN",.t.,A->(pare("DEBE ESCRIBIR S/N"),.f.))','!EMPTY(aCMP[4])'}}
  if !empty(RMARECEP->codtecr) .AND. !g_id$t5
     pare('NO TIENE ACCESO A REALIZAR ESTA OPERACION')
     RETURN nil
  endif
  DTeclasRMA()
  mixget(aTIT,aCMP,aMAS,aVAL,9,20,14,52,'REVISION DE EQUIPO',1,.t.)
  if lastkey()<>27
     if decis([¨ Desea actualizar el registro ?])='S'.AND.RMARECEP->(rlock())
        AUDITORIA([Acceso a mantenimiento de R.M.A. (revisi¢n)],dtoc(aCmp[1])+' '+aCmp[2],dtoc(RMARECEP->fechrev)+' '+RMARECEP->codtecr,[MOD])
        RMARECEP->fechrev  :=aCMP[1]
        RMARECEP->codtecr  :=aCMP[2]
        RMARECEP->garantia :=aCMP[3]
        RMARECEP->fecvenceg:=aCMP[4]
        RMARECEP->(dbunlock())
        dbselectar('RMARECEP')
        w_detreg()
     endif
  endif
  ATeclasRMA(T4,T5)
Return nil


// Diagnosticar fallas
function Diag()
  Private aTIT:={Pad([Diagnostico de falla:],21),;
                 Pad([             Tecnico:],21),[],[]}
  Private aCMP:={ctod(g_fecha),RMARECEP->codtecd,RMARECEP->dfalla1,RMARECEP->dfalla2}
  Private aMAS:={[],[],[@S56],[@S56]}
  Private aVAL:={{'.f.','.t.','.t.','.t.'},{'.t.','if(RMATECNI->(dbseek(aCMP[2])),.t.,123->(edit_ventana([TECNICOS]),off_key(),.f.))','.t.','.t.'}}
  if !empty(RMARECEP->codtecd) .AND. !g_id$t5
     pare('NO TIENE ACCESO A REALIZAR ESTA OPERACION')
     RETURN nil
  endif
  DTeclasRMA()
  mixget(aTIT,aCMP,aMAS,aVAL,9,10,14,70,'DIAGNOSTICO DE EQUIPO',1,.t.)
  if lastkey()<>27
     if decis([¨ Desea actualizar el registro ?])='S'.AND.RMARECEP->(rlock())
        AUDITORIA([Acceso a mantenimiento de R.M.A. (diagnostico)],dtoc(aCmp[1])+' '+aCmp[2],dtoc(RMARECEP->fechdia)+' '+RMARECEP->codtecd,[MOD])
        RMARECEP->fechdia:=aCMP[1]
        RMARECEP->codtecd:=aCMP[2]
        RMARECEP->dfalla1:=aCMP[3]
        RMARECEP->dfalla2:=aCMP[4]
        RMARECEP->(dbunlock())
        w_detreg()
     endif
  endif
  ATeclasRMA(T4,T5)
Return nil

// Entregar mercancia
Function Entr()
 Private aTIT:={Pad([  Fecha de entrega:],20),;
                Pad([           Tecnico:],20)}
 Private aCMP:={ctod(g_fecha),RMARECEP->codtecent}
 Private aMAS:={[],[]}
 Private aVAL:={{'.f.','.t.'},{'.t.','if(RMATECNI->(dbseek(aCMP[2])),.t.,123->(edit_ventana([TECNICOS]),off_key(),.f.))'}}
 if !empty(RMARECEP->codtecent) .AND. !g_id$t5
    pare('NO TIENE ACCESO A REALIZAR ESTA OPERACION')
    RETURN nil
 endif
 DTeclasRMA()
 mixget(aTIT,aCMP,aMAS,aVAL,9,20,12,52,'ENTREGA DE EQUIPO',1,.t.)
 if lastkey()<>27
    if decis('¨ Cofirmar entrega ?')='S' .and. RMARECEP->(rlock())
       AUDITORIA([Acceso a mantenimiento de R.M.A. (entrega)],dtoc(aCmp[1])+' '+aCmp[2],dtoc(RMARECEP->fecentreg)+' '+RMARECEP->codtecent,[MOD])
       RMARECEP->fecentreg:=aCMP[1]
       RMARECEP->codtecent:=aCMP[2]
       RMARECEP->estatus  :='OK'
       RMARECEP->(dbunlock())
       w_detreg()
       off_key()
       eval(w_var6)
       on_key()
    endif
 endif
 ATeclasRMA(T4,T5)
Return nil

// Reparacion
Function Repa()
 private buffer:=g_camino+'BUFRMA.DBF',cont:=.f.
 Private cNumRma:=lnumrma:=numrma,cCodCli:=codcli
 Private aTIT:={Pad([Fecha de reparaci¢n:],20),;
                Pad([            Tecnico:],20)}
 Private aCMP:={ctod(g_fecha),RMARECEP->codtecrep}
 Private aMAS:={[],[]}
 Private aVAL:={{'.f.','.t.'},{'.t.','if(RMATECNI->(dbseek(aCMP[2])),.t.,123->(edit_ventana([TECNICOS]),off_key(),.f.))'}}
 if !empty(RMARECEP->codtecrep) .AND. !g_id$t5
    pare('NO TIENE ACCESO A REALIZAR ESTA OPERACION')
    RETURN nil
 endif
 DTeclasRMA()
 mixget(aTIT,aCMP,aMAS,aVAL,9,20,12,52,'REPARACION DE EQUIPO',1,.t.)
 if lastkey()<>27
    if RMARECEP->garantia='S'
       cont:=PedirPiezas()
       DbSelectar('RMARECEP')
    endif
    if decis('¨ Cofirmar reparaci¢n ?')='S' .and. RMARECEP->(rlock())
       AUDITORIA([Acceso a mantenimiento de R.M.A. (reparaci¢n)],dtoc(aCmp[1])+' '+aCmp[2],dtoc(RMARECEP->fecrepara)+' '+RMARECEP->codtecrep,[MOD])
       RMARECEP->fecrepara:=aCMP[1]
       RMARECEP->codtecrep:=aCMP[2]
       RMARECEP->(dbunlock())
       if cont .AND. file(buffer)
          if abrir('RMAGARAN',g_camino+'RMAGARAN.DBF','C',g_camino+'RMAGARX1.NTX')
             do while !flock()
             enddo
             SEEK(lnumrma)
             rev_invrma(lnumrma)
             SEEK(lnumrma)
             DELETE WHILE numrma = lnumrma
             APPEND FROM (buffer)
             gen_invrma()
             UNLOCK
             cerrar('RMAGARAN')
             DbSelectar('RMARECEP')
          endif
       endif
       w_detreg()
       off_key()
       eval(w_var7)
       on_key()
    endif
 endif
 ATeclasRMA(T4,T5)
Return nil

Function PedirPiezas()
 if abrir('RMAGARAN',g_camino+'RMAGARAN.DBF','C',g_camino+'RMAGARX1.NTX')
    SEEK(cNumrma)
    COPY TO (buffer) WHILE numrma = cNumrma
    if !file(buffer)
       COPY STRUC TO (buffer)
    endif
    cerrar('RMAGARAN')
 else
    pare('NO SE PUDO TENER ACCESO AL ARCHIVO DE GARANTIAS')
    return .f.
 endif
 if abrir('BUFFER',buffer,'C')
    edit_ventana('RMAGARAN',{cNumrma,cCodCli})
    cerrar('BUFFER')
 endif
Return .t.

Function ATeclasRma(aAcc,t5)
 if !g_hay_user
    setkey(-7,blk(w_var1))      //      F8 ACCESORIOS
    setkey(-37,blk(w_var2))     //  ALT+F8 OBSERVACIONES
    setkey(-8,blk("revi()"))    //      F9 REVISIONES
    setkey(-38,blk("Diag()"))   //  ALT+F9 DIAGNOSTICO
    setkey(-9,blk("Repa()"))    //     F10 REPARACION
    setkey(-39,blk("Entr()"))   // ALT+F10 ENTREGA EQUIPO
 else
    if g_id$aAcc[1]
       setkey(-7,blk(w_var1))      //      F8 ACCESORIOS
    endif
    if g_id$aAcc[2]
       setkey(-37,blk(w_var2))     //  ALT+F8 OBSERVACIONES
    endif
    if g_id$aAcc[3]
       setkey(-8,blk("revi(t5)"))    //      F9 REVISIONES
    endif
    if g_id$aAcc[4]
       setkey(-38,blk("Diag(t5)"))   //  ALT+F9 DIAGNOSTICO
    endif
    if g_id$aAcc[5]
       setkey(-9,blk("Repa(t5)"))    //     F10 REPARACION
    endif
    if g_id$aAcc[6]
       setkey(-39,blk("Entr(t5)"))   // ALT+F10 ENTREGA EQUIPO
    endif
 endif
return nil

Function DTeclasRMA()
 setkey(-7,'')               //      F8 ACCESORIOS
 setkey(-37,'')              //  ALT+F8 OBSERVACIONES
 setkey(-8,'')               //      F9 REVISIONES
 setkey(-38,'')              //  ALT+F9 DIAGNOSTICO
 setkey(-9,'')               //     F10 REPARACION
 setkey(-39,'')              // ALT+F10 ENTREGAR EQUIPO
Return nil

*************************************************************************
*************************************************************************
*************************************************************************
FUNCTION GEN_INVRMA()
PRIVATE XPALMACEN,XORIGEN,XCANTIDAD,XUNIDAD,XITEM ,ACT_PED, ;
        ACT_COT, YDESPACHO,XBULTO
PRIVATE SELANT:=SELECT()
PRIVATE PV_MUL:='N'
DECLARE ULT_REC[15],ULT_IND[15],ABIERTA[15]
AFILL(ABIERTA,.F.)
* GENERA MOVIMIENTOS DE INVENTARIO
IF ABRIR([MXCTAINV],MXCTAINV,[S],MXCTINX1)
 IF ABRIR([MXEXTINV],MXEXTINV,[S],MXEXINX1)
 IF ABRIR([MXTRAINV],MXTRAINV,[S],MXTRINX1,MXTRINX2,MXTRINX3)
   *****************************************
   SELECT RMAGARAN
   SEEK(cNUMRMA)
   PALMACEN:=''
   DO WHILE !EOF() .AND. NUMRMA=cNUMRMA
     XITEM=ITEM
     XCODMOVART = ITEM
     XFECHA_MOV = EMISION
     XTIPINV    = 'AS'
     XNUMDOC    = cNUMRMA
     XUNIDAD    = ''
     XBULTO     = 1
     XCANTIDAD  = CANTIDAD
     XCOSTO_TOT = 0
     XVENTA_TOT = 0
     XORIGEN    = [INV]
     XPROV_CLI  = cCodcli
     YNUMPED    = ' '
     YNUMCOT    = ' '
     xCODCON    = ' '
     xCODDPTO   = ' '
     XALMACEN   = ALMACEN
     SELECT MXCTAINV
     IF DBSEEK(XITEM)
      DO MENSAJ WITH [Generando movimiento en INVENTARIO ...]
      SELECT MXTRAINV
      ADD_REC(0)
      ARC_INV()
     ENDIF
     SELECT RMAGARAN
     SKIP
   ENDDO
   CERRAR([MXTRAINV])
 ENDIF
 CERRAR([MXEXTINV])
ENDIF
CERRAR([MXCTAINV])
ENDIF
DBSELECTAR(SELANT)
RETURN 0

FUNCTION REV_INVRMA(EL_NUM)
PRIVATE YCANTIDAD,ACT_PED, ACT_COT, YNUMPED,YNUMCOT, YBULTO, YTIPINV
PRIVATE SELANT:=SELECT()
* REVERSA MOVIMIENTOS DE INVENTARIO
DECLARE ULT_REC[15],ULT_IND[15],ABIERTA[15]
AFILL(ABIERTA,.F.)
IF ABRIR([MXCTAINV],MXCTAINV,[S],MXCTINX1)
  IF ABRIR([MXEXTINV],MXEXTINV,[S],MXEXINX1)
  IF ABRIR([MXTRAINV],MXTRAINV,[S],MXTRINX3)
   SELECT RMAGARAN
   SEEK(EL_NUM)
   DO WHILE !EOF() .AND. NUMRMA=EL_NUM
    XITEM=ITEM
    YCANTIDAD  = CANTIDAD
    YTIPINV    = 'AS'
    YBULTO     = 1
    YNUMPED    = ' '
    YNUMCOT    = ' '
    SELECT MXTRAINV
    IF DBSEEK(EL_NUM+YTIPINV+XITEM+"INV")
     Do while numdoc+tipinv+codmovart+origen=el_num+ytipinv+xitem+"INV" .AND. !eof()
       DO MENSAJ WITH [Reversando movimiento en INVENTARIO ARTICULO: ]+Alltrim(xitem)
       REV_INV()
       SELECT MXTRAINV
       REC_LOCK(0)
       DELETE
       UNLOCK
       DbSkip()
     Enddo
    ENDIF
    SELECT RMAGARAN
    SKIP
   ENDDO
   CERRAR([MXTRAINV])
  ENDIF
  CERRAR([MXEXTINV])
 ENDIF
 CERRAR([MXCTAINV])
ENDIF
DBSELECTAR(SELANT)
RETURN 0


FUNCTION RMA0501()
RETURN(PARE('OPCION NO IMPLEMENTADA'))

FUNCTION RMA0401()
RETURN(EMISOR_INF())
