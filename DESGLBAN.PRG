* PERMITE EL MANTENIMIENTO AL ARCHIVO DE DESGLOSE BANCARIO (CUENTAS CONTABLES)
* cOrig = es el origen de donde se elaboro el documento
FUNCTION DESGBAN(cOrig)
 Private cBusc1,nImpDb
 Private ult_sel:=select()
 Private buffer := g_camino+'BUFMBA'+estacion+'.DBF'
 Private bufntx := g_camino+'BUFMBA'+estacion+'.NTX'
 Private nImpor := xImporte
 cORIG  := if(PCOUNT()=0,[BAN],cORIG)
 cBusc1 := if(!modifica,xcodmovban+xtipban+xnumdoc,lcodmovban+ltipban+lnumdoc)
 if abrir('MXCTACON',mxctacon,'S',mxctcox1)
  SET FILTER TO Substr(Alltrim(Codigo),Len(Alltrim(Codigo)),1)<>"."
  if abrir('MXCMOBAN',g_camino+'MXCMOBAN.DBF','S',g_camino+'MXCMBAX1.NTX')
     SELECT MXCMOBAN
     SEEK(cBusc1)
     IF FOUND()
        COPY TO (BUFFER) WHILE CODBAN+TIPBAN+NUMDOC=cBusc1
     ELSE
        COPY STRUC TO (BUFFER)
     ENDIF
     IF !abrir('TEMMBA',BUFFER,'E')
        pare('NO SE PUDO TENER ACCESO AL ARCHIVO DE DESGLOSE BANCARIO')
        Return nil
     endif
     INDEX ON CODCON TO (BUFNTX)
     DBGOTOP()
     REPLACE ALL FECHA_MOV WITH XFECHA_MOV
     DO WHILE .T.
        DBGOTOP()
        edit_ventana('EDCVOUCH',{nImpor})
        REPLACE ALL CODBAN WITH XCODMOVBAN,TIPBAN WITH XTIPBAN,NUMDOC WITH XNUMDOC,ORIGEN WITH cORIG,FECHA_MOV WITH XFECHA_MOV
        PACK
        if LastRec()=0
           EXIT
        endif
        DBGOTOP()
        SUM IMPORTE TO nImpDb
        if !difcero(nImpor)
           EXIT
        endif
        if difcero(nImpDb-nImpor)
            PARE('DESGLOSE NO CUADRA, INF: Monto Cheque:'+alltrim(convstr(nImpor))+'-'+alltrim(convstr(nImpdb))+'='+alltrim(convstr(nImpor-nImpdb)))
            if decis('¨ Desea corregir los montos (S/N) ?')='S'
               LOOP
            else
               EXIT
            endif
        endif
        EXIT
     ENDDO
     cerrar('TEMMBA')
  ENDIF
 ENDIF
 cerrar('MXCTACON')
 cerrar('MXCMOBAN')
 select (ULT_SEL)
Return nil


* COPIA LOS REGISTROS DE CUENTAS CONTABLES DE UN MOV. DE BANCARIO
Function gdesgban()
 PRIVATE SELANT:=SELECT()
 PRIVATE BUFFER := G_CAMINO+'BUFMBA'+ESTACION+'.DBF'
 PRIVATE BUFNTX := G_CAMINO+'BUFMBA'+ESTACION+'.NTX'
 IF !FILE(BUFFER)
    RETURN nil
 ENDIF
 IF ABRIR('MXCMOBAN',G_CAMINO+'MXCMOBAN.DBF','S',G_CAMINO+'MXCMBAX1.NTX')
    do while !flock()
    enddo
    APPEND FROM (BUFFER)
    UNLOCK
 ENDIF
 CERRAR("MXCMOBAN")
 DELETE FILE (BUFFER)
 DELETE FILE (BUFNTX)
 SELECT(SELANT)
RETURN NIL

* BORRAR LOS REGISTROS DE CUENTAS CONTABLES DE UN MOV. BANCARIO
Function bdesgban(cBuscar)
 local selant:=select()
 IF ABRIR('MXCMOBAN',G_CAMINO+'MXCMOBAN.DBF','S',G_CAMINO+'MXCMBAX1.NTX')
  IF DBSEEK(cBuscar)
   WHILE !FLOCK()
   END
   DELETE WHILE CODBAN+TIPBAN+NUMDOC=cBuscar
   UNLOCK
  ENDIF
 ENDIF
 CERRAR('MXCMOBAN')
 SELECT(SELANT)
RETURN

* PERMITE MOSTRAR EL DESGLOSE BANCARIO SIN TENER QUE MODIFICAR EL REGISTRO
FUNCTION MOS_DESG()
 Private ult_sel:=select()
 Private buffer := g_camino+'BUFMBA'+estacion+'.DBF'
 Private bufntx := g_camino+'BUFMBA'+estacion+'.NTX'
 Private cBusc1 := codmovban+tipban+numdoc
 SET KEY -41 TO
 if abrir('MXCMOBAN',g_camino+'MXCMOBAN.DBF','S',g_camino+'MXCMBAX1.NTX')
  DO WHILE .T.
    SEEK(cBusc1)
    IF FOUND()
       COPY TO (BUFFER) WHILE CODBAN+TIPBAN+NUMDOC=cBusc1
       CERRAR('MXCMOBAN')
    ELSE
       pare('NO EXISTE DESGLOSE BANCARIO PARA ESTE DOCUMENTO')
       CERRAR('MXCMOBAN')
       EXIT
    ENDIF
    IF !abrir('TEMMBA',BUFFER,'E')
       pare('NO SE PUDO TENER ACCESO AL ARCHIVO DE DESGLOSE BANCARIO')
       EXIT
    endif
    IF LASTREC()=0
       pare('NO EXISTE DESGLOSE BANCARIO PARA ESTE DOCUMENTO')
       CERRAR('TEMMBA')
       EXIT
    ENDIF
    INDEX ON CODCON TO (BUFNTX)
    DBGOTOP()
    edit_ventana('VERVAUCH')
    cerrar('TEMMBA')
    EXIT
  ENDDO
 ENDIF
 IF FILE(BUFFER)
    FERASE(BUFFER)
 ENDIF
 SELECT(ULT_SEL)
 SET KEY -41 TO MOS_DESG
Return nil
