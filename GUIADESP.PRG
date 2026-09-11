Function INV0311
 nOpc:=La_Opcion({'Mantenimiento de Guias',;
                  'Actualizar Guias      '},4,2,2,1,;
                  [ GUIAS DE DESPACHO ])
 IF nOpc=0 .OR. LastKey()=27
    RETURN NIL
 endif
 IF nOpc=1
    INV0311A()
 ENDIF
 IF nOpc=2
    INV0311B()
 ENDIF
Return nil

** procesos para el control de las guias de despachos
PROC INV0311A()
   Private buffer:=g_camino+'BUFGUI'+estacion+'.DBF',xT1:=0,cHora,cNumGuia,cBuffer:='BUFGD'+ESTACION+'.DBF'
   lista_var:=''
   if ABRIR('MXENCGUI',MXENCGUI,'S',MXENGUX1,MXENGUX2,MXENGUX3) .AND. ABRIR('MXRENGUI',MXRENGUI,'S',MXREGUX1,MXREGUX2,MXREGUX3)
      __DBCOPYSTRUC(buffer)
      DBSELECTAR('MXENCGUI')
      DBGOBOTTOM()
      EDIT_VENTANA('GDCABEZA')
      DBUNLOCKALL()
      DBCLOSEALL()
      FERASE(Buffer)
      FERASE(cBuffer)
   ENDIF
RETURN NIL

** procesos para actualizar las guias de despachos
PROC INV0311B()
   Private cNumGuia
   Private aLisEnt:={}
   lista_var:=''
   if ABRIR('MXENCGUI',MXENCGUI,'S',MXENGUX1,MXENGUX2,MXENGUX3) .AND. ABRIR('MXRENGUI',MXRENGUI,'S',MXREGUX1,MXREGUX2,MXREGUX3)
      DBSELECTAR('MXENCGUI')
      FILTRARDBF('STATUS="PE"')
      DBGOTOP()
      EDIT_VENTANA('GDCAMBST')
      DBUNLOCKALL()
      DBCLOSEALL()
   ENDIF
RETURN NIL

// Elimina los renglones de una guia de despacho al eliminarla y modifica los documentos relacionados
Function w_eligdd(cNumguia)
   Local uSel:=Select()
   DbSelectar('MXRENGUI')
   Dbsetorder(1)
   IF DBSEEK(cNumguia)
      DO WHILE NUMGUIA = cNumguia .AND. !Eof()
         IF rlock()
            DELETE
            dbunlock()
         ENDIF
         DBSKIP()
      ENDDO
   ENDIF
   DbSelectar(uSel)
Return .T.

// SELECCIONA LAS ALBARANES Y LAS FACTURAS
Function CargarGuia(pNg)
   Local uSel:=Select(),lValue
   Private cNg:=pNg
   SELECT BUFFER
   COPY STRU TO (cBuffer)
   IF ABRIR('BUFGUIA',cBuffer,'E')            .AND. ;
      ABRIR('MXENCFAC',MXENCFAC,'S',MXENFAX1) .AND. ;
      ABRIR('MXENCALB',MXENCALB,'S',MXENALX1) .AND. ;
      ABRIR('MXCTACLI',CF('MXCTACLI.DBF'),'S',CF('MXCTCLX1.NTX'))
      dbselectar('MXENCALB')
      DBGOTOP()
      DBEVAL(BLK("grabarGuia(1,cNg)"),Blk("Empty(NUMGUIA) .AND. Empty(NUMFAC) .AND. EMPTY(ESTGUIA)"),blk("!eof()"))
      dbselectar('MXENCFAC')
      DBGOTOP()
      DBEVAL(BLK("grabarGuia(2,cNg)"),Blk("Empty(NUMGUIA) .AND. EMPTY(ESTGUIA)"),blk("!eof()"))
      CERRAR('MXENCFAC')
      CERRAR('MXENCALB')
      CERRAR('MXCTACLI')
      DBSELECTAR('BUFGUIA')
      DbGotop()
      Edit_Ventana('GDSELDOC')
      Cerrar('BUFGUIA')
      DBSELECTAR('BUFFER')
      APPEND FROM (cBuffer) FOR STATUS='SL'
      REPLACE ALL STATUS WITH 'PE'
      DBGOTOP()
   ENDIF
   DBSELECTAR(uSel)
Return lValue

// GRABA LA INFORMACION EN EL ARCHIVO TEMPORAL PARA SU POSTERIOR SELECCION
Function GrabarGuia(el_par,cNg)
   Local uSel:=Select()
   Private cAlias:=if(el_par=1,'MXENCALB->','MXENCFAC->')
   DBSELECTAR('BUFGUIA')
   DBAPPEND()
   REPLACE ;
   NUMGUIA  WITH cNg,;
   FECHADOC WITH &cAlias.EMISION,;
   CLIENTE  WITH &cAlias.CLIENTE,;
   CODVEN   WITH &cAlias.CODVEN,;
   CODZON   WITH NOMCOD(&cAlias.CLIENTE,'MXCTACLI','ZONA'),;
   ORIGEN   WITH if(el_par=1,'ALB','FAC')
   IF el_par=1
      REPLACE CONDPAG  WITH 1
      REPLACE FORMAPAG WITH 'NE'
      REPLACE NUMORG   WITH &cAlias.NUMALB
      REPLACE IMPORTE  WITH &cAlias.TOT_ALB
   ELSE
      REPLACE CONDPAG  WITH &cAlias.CONDPAG
      REPLACE FORMAPAG WITH &cAlias.FORMAPAG
      REPLACE NUMORG   WITH &cAlias.NUMFAC
      REPLACE IMPORTE  WITH &cAlias.TOT_NET
   ENDIF
   GMoneda(&cAlias.CAMBIO)
   DBSELECTAR(uSel)
Return nil

// ABRE LAS TABLAS ENCFAC Y ENCALB
STATIC FUNCTION ABALBFAC()
   IF ABRIR('MXENCFAC',MXENCFAC,'S',MXENFAX1) .AND. ABRIR('MXENCALB',MXENCALB,'S',MXENALX1)
      RETURN .T.
   ENDIF
RETURN .F.

// CIERRA LAS TABLAS ENCFAC Y ENCALB
STATIC FUNCTION CEALBFAC()
   CERRAR('MXENCFAC')
   CERRAR('MXENCALB')
RETURN .T.

// REVERSA LOS NUMEROS DE GUIAS EN LOS DOCUMENTOS ALB Y FAC AL ELIMINAR UNA GUIA
FUNCTION REVALBFAC(P1,P2)
   LOCAL uSel:=SELECT()
   DBSELECTAR(if(p2='ALB','MXENCALB','MXENCFAC'))
   IF DBSEEK(P1)
      REPLACE NUMGUIA WITH '', ESTGUIA WITH ''
   ENDIF
   DBSELECTAR(uSel)
RETURN .T.

Function ActGuias(cNg)
   Local uSel:=Select()
   IF ABALBFAC()
      DbSelectar('MXRENGUI')
      IF dbseek(cNg)
         DO WHILE NUMGUIA=cNg .AND. !EOF()
            Dbselectar(if(MXRENGUI->ORIGEN='ALB','MXENCALB','MXENCFAC'))
            if dbseek(MXRENGUI->NUMORG)  .AND. Rlock()
               REPLACE NUMGUIA WITH cNg
            endif
            dbselectar('MXRENGUI')
            dbskip()
         ENDDO
      ENDIF
      CEALBFAC()
   ENDIF
   DbSelectar(uSel)
Return nil

Function ActStGuias(cNg)
   Local uSel:=Select()
   IF ABALBFAC()
      DbSelectar('MXRENGUI')
      IF dbseek(cNg)
         DO WHILE NUMGUIA=cNg .AND. !EOF()
            Dbselectar(if(MXRENGUI->ORIGEN='ALB','MXENCALB','MXENCFAC'))
            if dbseek(MXRENGUI->NUMORG)  .AND. Rlock()
               REPLACE ESTGUIA WITH 'OK'
            endif
            dbselectar('MXRENGUI')
            dbskip()
         ENDDO
      ENDIF
      CEALBFAC()
   ENDIF
   DbSelectar(uSel)
Return nil

Function RevGuias(cNg)
   Local uSel:=Select()
   IF ABALBFAC()
      DbSelectar('MXRENGUI')
      IF dbseek(cNg)
         DO WHILE NUMGUIA=cNg .AND. !EOF()
            Dbselectar(if(MXRENGUI->ORIGEN='ALB','MXENCALB','MXENCFAC'))
            if dbseek(MXRENGUI->NUMORG)  .AND. Rlock()
               REPLACE NUMGUIA WITH '',ESTGUIA WITH ''
            endif
            dbselectar('MXRENGUI')
            dbskip()
         ENDDO
      ENDIF
      CEALBFAC()
   ENDIF
   DbSelectar(uSel)
Return nil

Function CambiaStatus()
   Local uSel:=Select(),cNvoSt:='OK'
   DbEval(blk("RLOCK(),MXRENGUI->STATUS:=IF(STATUS='SL','OK','PE'),DBUNLOCK()"),,blk("!eof()"))
   DbGotop()
   DO WHILE !Eof()
      IF STATUS='PE'
         cNvoSt:='PE'
         EXIT
      ENDIF
      Dbskip()
   ENDDO
   IF cNvoSt='OK'
      DBSELECTAR('MXENCGUI')
      ActStGuias(NUMGUIA)
      IF RLOCK()
         REPLACE STATUS WITH cNvoSt
         DBUNLOCK()
      ENDIF
   ENDIF
   DBSELECTAR(uSel)
Return nil
