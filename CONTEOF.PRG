Function INV0310
 opcCos:=La_Opcion({'Seg£n Costo Promedio  ',;
                    'Seg£n Ultimo Costo    '},4,2,2,1,;
                    [ COSTO DE ENTRADA ])
 if OpcCos=0 .OR. LastKey()=27
    RETURN NIL
 endif
 Edit_Ventana('EDCCONFI',{OpcCos})
Return nil

Function INV0505
 Actualizar()
Return nil

STATIC Function Actualizar()
 PRIVATE aGrupos,cGrupos,opcsel,yd,yh,xCosto_Nvo:=0,OpcCos

 Alerta('ESTE PROCESO GENERA NUEVOS MOVIMIENTO EN LAS TRANSACCIONES DE INVENTARIO A LA VEZ QUE ELIMINA EL CONTEO FISICO UNA VEZ PROCESADO Y RECALCULADO')
 IF decis('¨ Desea continuar con este proceso',2)='N'
    RETURN nil
 ENDIF

 DO WHILE .t.
    aGrupos :={}
    cGrupos :=''
    Init_Ver()

    opcsel:=La_Opcion({'Por Grupos              ',;
                       'Por Articulos           ',;
                       'Por Almac‚n             ',;
                       'Todos                   '},4,2,5,1,[ FORMAS DE SELECCION ],'dejar')
    if OpcSel=0
       EXIT
    endif

    /*
    opcCos:=La_Opcion({'Seg£n Costo Promedio  ',;
                       'Seg£n Ultimo Costo    '},11,2,2,1,;
                       [ COSTO DE ENTRADA ])
    if OpcCos=0
       LOOP
    endif
    */

    DO CASE
     CASE opcsel=1
      Edit_Ventana('SELGART')
     CASE opcsel=2
      Edit_Ventana('SELARTC')
     CASE opcsel=3
      Edit_Ventana('SELALMA')
    ENDCASE
    if OpcSel<>4
       if aLenG(aGrupos)>0
         for a=1 to aLenG(aGrupos)
            cGrupos:=cGrupos+Alltrim(aGrupos[a])+'/'
         next
       else
          Pare('No ha sido seleccionado ning£n registro')
          LOOP
       endif
    endif
    IF Abre_File()
       Act_Inv(cGrupos)
    ENDIF
 ENDDO
RETURN nil

Static Function Abre_File()
 if DbVacio(g_camino+'MXFISINV.DBF','S')=0
    Pare('No existen registros por actualizar')
    RETURN .f.
 endif
 if !abrir([MXCTAINV],mxctainv,[S],mxctinx1,mxctinx2,mxctinx3,mxctinx4,mxctinx5)
    Return .f.
 endif
 if !abrir([MXEXTINV],mxextinv,[S],mxexinx1)
    DbCloseAll()
    Return .f.
 endif
 if !abrir([MXTRAINV],mxtrainv,[S],mxtrinx1,mxtrinx2,mxtrinx3)
    DbCloseAll()
    Return .f.
 endif
 if !abrir([MXFISINV],g_camino+'MXFISINV.DBF',[S],g_camino+'MXFIINX1.NTX')
    DbCloseAll()
    Return .f.
 endif
Return .t.

Static Function Act_Inv(xGrupos)
 Local Crit,xcuantos:=0
 Private dFecha:=Ctod(g_fecha),la_hora:=StrTran(Time(),':'),borrar:=.f.
  crit:=if(OpcSel=1,"Alltrim(grupo)$cGrupos",if(OpcSel=2,"Alltrim(codmovart)$cGrupos",if(OpcSel=3,"Alltrim(almacen)$cGrupos",;
       if(OpcSel=4,"!Empty(codmovart)",''))))
 DbSelectar('MXFISINV')
 DbGotop()
 xcuantos:=0
 borrar:=.t.
 Do while !eof()
    if &crit
       auditoria([Acceso a la opci¢n de actualizar conteo fisico (Actualizando)],codmovart,,[INC])
       Mensaj('Actualizando c¢digo: '+Alltrim(codmovart))
       GrabConteo(MXFISINV->CODMOVART,MXFISINV->ALMACEN,MXFISINV->NVO_COSTO)
       DbSelectar('MXFISINV')
       Rec_Lock(0)
       DELETE
       UNLOCK
       ++xcuantos
    else
       Mensaj('Revisando conteo c¢digo: '+Alltrim(codmovart))
    endif
    DbSelectar('MXFISINV')
    DbSkip()
 Enddo
 cerrar('MXFISINV')
 Indlista('MXFISINV')
 Mensaj([])
 Pare('Fuer¢n Actualizados: '+Alltrim(Str(xcuantos))+' Art¡culos de Inventario')
Return nil

Static Function GrabConteo(el_cod,el_alma,el_costo)
 Local nUltC:=0,el_numdoc,nNvoCosto
 el_numdoc:='CF'+StrZero(Day(dFecha),2)+StrZero(Month(dFecha),2)+Substr(StrZero(Year(dFecha),4),3,2)
 nNvoCosto:=el_costo
 DbSelectar('MXCTAINV')
 IF !DbSeek(el_cod)
    Pare('No se pudo encontrar el C¢digo del Art¡culo en el Maestro')
    Return nil
 ENDIF
 Private XCODMOVART := el_cod
 Private XFECHA_MOV := dFecha
 Private XTIPINV    := [AS]
 Private XNUMDOC    := el_numdoc
 Private XUNIDAD    := unidad
 Private XORIGEN    := 'INV'
 Private XALMACEN   := el_alma
 Private XCANTIDAD  := Exist(dFecha,xAlmacen)
 Private XCOSTO_TOT := CalCos(dFecha,3)*xCantidad
 Private XCODCON    := Space(20)
 Private XCODDPTO   := Space(10)
 Private XHORA      := la_hora
 Private xult_costo := ult_costo

 ** GENERA LA SALIDA DE AJUSTE PARA EL CONTEO FISICO
 if xcantidad<>0
    DbSelectar('MXTRAINV')
    DbAppend()
    Arc_Inv()
    RecalCos(xCodmovart)
 endif

 ** GENERA LA ENTRADA PARA EL AJUSTE DE CONTEO FISICO
 XTIPINV    := [EN]
 XCANTIDAD  := MXFISINV->con_fis
 XCOSTO_TOT := if(nNvoCosto>0,nNvoCosto,CalCos(dFecha,3))*xCantidad
 IF nNvoCosto>0
    xcosto_tot := (nNvoCosto*xCantidad)
    xult_costo := nNvoCosto
 ENDIF
 if xcantidad>0
    DbSelectar('MXTRAINV')
    DbAppend()
    Arc_Inv()
    RecalCos(xCodmovart)
 endif
 DbSelectar('MXCTAINV')
 REC_LOCK(0)
 REPLACE ult_costo WITH xult_costo
 UNLOCK
Return nil












