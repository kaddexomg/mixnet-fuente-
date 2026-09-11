********** SELECCION DE INVENTARIO
PROCEDURE SELINFINV
LOCAL UCTL3,_pv_pvp
PARAMETERS P1,P2,P3,ES_TPV

PRIVATE OKK:=.T.,XCODALT:='N'

IF VALTYPE(VTAS_CODALT)='C'
   XCODALT:=VTAS_CODALT
ENDIF

IF VALTYPE(ES_TPV)='C'
   XCODALT:=PV_CALT
ENDIF

PRIVATE ULT_ALIAS
DECLARE ULT_REC[15],ULT_IND[15],ABIERTA[15],RCAMPO[IF(XCODALT='S',4,4)],RSUBTIT[IF(XCODALT='S',4,4)]
AFILL(ABIERTA,.F.)

PRIVATE XCODART,XNOMART,XNOMALT,XGRUPO,XALTERNO,XIVA,XFECHA_CREA,XCODCON
PRIVATE XPRECIO_A,XPRECIO_B,XPRECIO_C,XPRECIO_D,XEXMIN,XEXMAX,XUBICA,XPESO
PRIVATE XUNIDAD, XALTURA,XANCHO,XPROFUN,XCAN_BUL,XCOMPUESTO,xresumen,xmarca,xprov_asig
PRIVATE MODIFICA,UREG,LREG, LCODART
PRIVATE XDES_A, XDES_B,XDES_C,XDES_D, APERTURO:=.F.,OLDKEY
PRIVATE BUFCPS:=G_CAMINO+[BUFCDI]+estacion+[.DBF]

IF TYPE("XALMACEN")='U'
   PRIVATE XALMACEN
ENDIF

PRIVATE XFECHA_MOV:=CTOD(G_FECHA),XTIPINV:='EN'
PRIVATE XNUMDOC:='AP000001',XCANTIDAD:=1, XCOSTO_TOT:=0,MCOSTO_TOT:=SPACE(12)
PRIVATE XORIGEN:='INV'
PRIVATE XCODMOVART:=''
PRIVATE XPRECIO_MAX,XPRECIO_MIN
PRIVATE xmarca
PRIVATE Insertar
PRIVATE xprov_asig
PRIVATE xFACTOR
PRIVATE xTIPPROD
PRIVATE ES_EDITA:= TYPE([EDITA_INV])<>[U] .AND. POPC3[3]
PRIVATE ed_renglon:=.f.
PRIVATE EL_ALMACEN

/*
IF valtype(es_tpv) = 'C'
   EL_ALMACEN:=XALMACEN
   //EL_ALMACEN:=if(g_multi,ustr_almuser,Space(2))
ELSE
   XALMACEN:=if(g_multi,ustr_almuser,Space(2))
ENDIF
*/

IF valtype(xalmacen)='C'
   el_almacen := xalmacen
ENDIF

IF inv_filpr='S' .OR. inv_prdes='S'
   FiltrarInv()
ENDIF

DEFDEFINV()

SUBSIN()
PRINSEL()
IF TYPE([TIP_SEL])=[U]
 TIP_SEL=0
ENDIF
//IF type('P_ALMACEN')='C'
// EL_ALMACEN:=P_ALMACEN
//ENDIF
IF G_MULTI
 OKK:=ABRIR([MXEXTINV],MXEXTINV,[S],MXEXINX1)
ENDIF
IF OKK
IF ABRIR([MXCTAINV],MXCTAINV,[S],MXCTINX1,MXCTINX5,MXCTINX2,MXCTINX3,MXCTINX4)
 IF TIP_SEL<>2
  OLDKEY:=SETKEY(-8,{|P,L,V|EDITAR_PRECIO(P,L,V)})
 ENDIF
 UCTL3:=CTL_MEN[3]
 CTL_MEN[3]:=.F.
 t1:=subopcion
 subopcion:=4
 T2:=gestion
 gestion:='INV'
 SET KEY -31 TO Mov_Inv
 SET KEY -32 TO Ver_provee
 SET KEY -34 TO VEREQUIV()
 SET KEY 289 TO FichaInv()
 SET KEY 286 TO Almacenes()
 EDC([SIN],[ SELECCION DE ARTICULOS ],RSUBTIT,.T.,.T.,.T.,ES_EDITA,.T.,.f.,.F.,.F.,tip_sel=2,IF(TIP_SEL=2,CMEN,'F9=O/Precios Alt+F2=Mov. Alt+F3=Prov. Alt+F5=Equiv. Alt+F=Ficha Alt+A=Almacs.'))
 SET KEY -31 TO
 SET KEY -32 TO
 SET KEY -34 TO
 SET KEY 289 TO
 SETKEY(286,'')
 subopcion:=t1
 gestion:=t2
 CTL_MEN[3]:=UCTL3
 CERRAR('MXCTAINV')
 IF TIP_SEL<>2
  SETKEY(-8,OLDKEY)
 ENDIF
ENDIF
IF G_MULTI.AND.OKK
 CERRAR('MXEXTINV')
ENDIF
ENDIF
FINSEL()
*EN_CTA=.F.
RETURN

PROCEDURE DEFSIN
SAVE SCREEN
INIT_VER()
SELECT MXCTAINV
SET ORDER TO 1
DEFINV()
RETURN

PROCEDURE ASGSIN
LOCAL UREG
SAVE SCREEN
INIT_VER()
SELECT MXCTAINV
UREG:=RECNO()
SET ORDER TO 1
GOTO (UREG)
ASGINV()
RETURN

PROCEDURE RPLSIN
RPLINV()
RETURN

PROCEDURE GETSIN
 IF ABRIR([MXMARINV],MXMARINV,[S],MXMAINX1)
   IF ABRIR([MXGRUINV],MXGRUINV,[S],MXGRINX1)
      SELECT MXCTAINV
      IF inv_filpr='S' .OR. inv_prdes='S'
         SET FILTER TO
      ENDIF
      MASINV()
      GETINV()
      gestion:=t2
      SELECT MXCTAINV
      IF inv_filpr='S' .OR. inv_prdes='S'
         FiltrarInv()
      ENDIF
      gestion:='INV'
      CERRAR('MXGRUINV')
   endif
   CERRAR('MXMARINV')
ENDIF
SELECT MXCTAINV
RESTORE SCREEN
RETURN

PROCEDURE ELISIN
ELIINV()
RETURN

PROCEDURE SUBSIN

IF XCODALT='S'
 RSUBTIT[ 1]=PAD([C¢digo],15)
 RSUBTIT[ 2]=PAD([C¢digo Alt.],15)
 RSUBTIT[ 3]=PAD([Nombre del Art¡culo],32)
 RSUBTIT[ 4]=[Exi/Actual]
ELSE
 RSUBTIT[ 1]=PAD([C¢digo],15)
 RSUBTIT[ 2]=PAD([Nombre del Art¡culo],32)
 RSUBTIT[ 3]=[Exi/Actual]
 RSUBTIT[ 4]=[Precio Unitario]
ENDIF
RETURN

PROCEDURE MASSIN
* MASCARA DE LA VENTANA
LINSIM()
RETURN

PROCEDURE DETSIN
* DETALLE
PRENSEL()
IF XCODALT='S'
 @ CONTLIN, 2+MARGEN SAY CODART
 @ CONTLIN,18+MARGEN SAY ALTERNO
 @ CONTLIN,34+MARGEN SAY PAD(NOMART,32)
 @ CONTLIN,67+MARGEN SAY STR(EXIST(CTOD(G_FECHA),EL_ALMACEN),10,3)
ELSE
 @ CONTLIN, 2+MARGEN SAY CODART
 @ CONTLIN,18+MARGEN SAY PAD(NOMART,32)
 @ CONTLIN,51+MARGEN SAY STR(EXIST(CTOD(G_FECHA),EL_ALMACEN),10,3)
 @ CONTLIN,62+MARGEN SAY STR(PRECIO_A,15,2)
ENDIF
SETCOLOR(C_TEX3)
RETURN

PROCEDURE CRDSIN
* COORDENADAS DE LA VENTANA
TOPE    = 7
FONDO   = 14
COLIZQ  = 2
COLDER  = 78
RETURN
PROCEDURE SELSIN
* SELECCION
MODOSEL()
RETURN

PROCEDURE BUSSIN
PRIVATE TNOMART,TCODART,BNOMART,BCODART
VERTBUS([BSINFUN])
KEYBOARD DERECHA
RETURN

PROCEDURE BSINFUN
BCODART=SPACE(15)
BALTERNO=SPACE(15)
BNOMART=SPACE(35)
IF XCODALT='S'
 @ CONTLIN,2+MARGEN  GET BCODART
 @ CONTLIN,18+MARGEN GET BALTERNO
 @ CONTLIN,34+MARGEN GET BNOMART PICT "@S30"
ELSE
 @ CONTLIN,2+MARGEN  GET BCODART
 @ CONTLIN,18+MARGEN GET BNOMART PICT "@S48"
ENDIF
CUR_ON()
READ
CUR_OFF()
IF XCODALT='S'
 SALIR=ESCAPAR() .OR. ( EMPTY(BCODART) .AND. EMPTY(BALTERNO) .AND. EMPTY(BNOMART) )
ELSE
 SALIR=ESCAPAR() .OR. ( EMPTY(BCODART) .AND. EMPTY(BNOMART) )
ENDIF
IF !SALIR
 IF !EMPTY(BCODART)
    TCODART=TRIM(BCODART)
    YKEY=TCODART
    SET ORDER TO 1
 ENDIF
 IF XCODALT='S'
  IF !EMPTY(BALTERNO)
     TALTERNO=BALTERNO
     YKEY=TALTERNO
     SET ORDER TO 2
  ENDIF
 ENDIF
 IF !EMPTY(BNOMART)
    TNOMART=UPPER(TRIM(BNOMART))
    YKEY=TNOMART
    SET ORDER TO 3
 ENDIF
ENDIF
RETURN

Static Function MOV_INV()
 local el_Cod:=codart,el_file,cSelAnt:=Select(),saldo
 el_file:=g_camino+'BMINV'+estacion+'.DBF'
 DbCreate(el_file,{{'texto','C',68,0}})
 if ABRIR('TRAINV',mxtrainv,'S',mxtrinx1)
    if ABRIR('INVENT',el_file,'S')
       DbSelectar('TRAINV')
       if DbSeek(el_cod)
          saldo:=0
          Do While codmovart==el_cod .and. !eof()
             saldo:=saldo+if(tipinv$[EN.AE.ET.DV] .OR. tipinv=[E ],cantidad,-cantidad)
             el_texto:=Dtoc(fecha_mov)+' '+numdoc+' '+tipinv+' '+origen+' '+prov_cli+' '+if(tipinv$[EN.AE.ET.DV] .OR. tipinv=[E ],Str(cantidad,10,3),Space(10))+' '+if(tipinv$[SA.AS.ST.DC] .OR. tipinv=[S ],Str(cantidad,10,3),Space(10))+' '+Str(saldo,10,3)
             invent->(DbAppend())
             invent->texto:=el_texto
             DbSkip()
          Enddo
          DbSelectart('INVENT')
          Edit_Ventana('VMOVINV',{Alltrim(Str(saldo,10,3))})
       endif
       Cerrar('INVENT')
    endif
    Cerrar('TRAINV')
 endif
 DbSelectar(cSelAnt)
RETURN NIL

Function FichaInv()
   Local sPant:=SaveScreen(0,0,24,80),selant:=Select(),colant:=SetColor(),sal:=.f.
   IF !ABRIR( [MXMARINV],cf('MXMARINV'),[S],cf('MXMAINX1') )
      Pare('No se Pudo abrir el archivo de Marcas')
      DbSelectar(selant)
      Return nil
   ENDIF
   IF !ABRIR( [MXGRUINV],cf('MXGRUINV'),[S],cf('MXGRINX1') )
      Pare('No se Pudo abrir el archivo de Grupos')
      Cerrar('MXMARINV')
      DbSelectar(selant)
      Return nil
  ENDIF
  DbSelectar('MXCTAINV')
  MASINV()
  DETINV()
  @ 18,39 SAY 'XXXXXXXXXXXXXXX'
  @ 19,39 SAY 'XXXXXXXXXXXXXXX'
  mm:='Presione Cualquier tecla para cerrar esta ventana'
  @ 21,(80-Len(mm))/2 SAY mm COLOR c_tit2
  inkey(0)
  Cerrar('MXMARINV')
  Cerrar('MXGRUINV')
  SetColor(colant)
  DbSelectar(selant)
  RestScreen(0,0,24,80,sPant)
Return nil

Function Almacenes(p)
 Local xcod:=MXCTAINV->codart,xfec:=xemision,li:=tope-2,ci:=colizq+1,excepto:=xalmacen
 Local aAlm:={},selant:=Select(),lf,cf,colant:=SetColor(),sPant,la_ex:=0,ExisT:=0
 Local ret_alm:=Space(2)
 excepto:=if(Pcount()>4,excepto,'  ')
 ExisT:=if(!Empty(excepto),Exist(xemision,excepto),0)
 if !usada('MXCTAALM')
    if !abrir('MXCTAALM',MXCTAALM,'S',MXCTALX1)
       Pare('El Maestro de almacenes no esta disponible')
       DbSelectar(selant)
       Return ret_alm
    endif
 endif
 DbSelectar('MXEXTINV')
 DbGotop()
 if !DbSeek(xcod)
    Pare('No existe movimiento en otros almacenes para el articulo: '+Alltrim(xcod))
    DbSelectar(selant)
    Return ret_alm
 endif
 if !usada('MXCTAALM')
    abrir('MXCTAALM',MXCTAALM,'S',MXCTALX1)
 endif
 Do while codmovart=xcod .and. !eof()
   if !almacen$excepto
     la_ex:=Exist(xfec,almacen)
     ExisT :=ExisT+la_ex
     if la_ex<>0
        AaDd(aAlm,almacen+' '+if(MXCTAALM->(DbSeek(MXEXTINV->almacen)),Pad(MXCTAALM->nomalm,30),Pad('No Registrado',30))+'  '+Str(la_ex,13,3) )
     endif
   endif
   DbSkip()
 Enddo
 if Len(aAlm)=0
    Pare('Existe registros en otros almacenes, pero estan en cero (0)')
    DbSelectar(selant)
    Return ret_alm
 endif
 el_tit:='Cd '+Pad('Nombre del almac‚n',30)+'     Existencia'
 cf:=(Len(el_tit)+1)+ci
 lf:=if(4+Len(aAlm)+li>23,23,li+4+Len(aAlm))
 sPant:=SaveScreen(li,ci,lf+1,cf+1)
 boxsom(' EXISTENCIA POR ALMACEN COD.: '+ALLTRIM(xcod)+' ',li,ci,lf,cf,3)
 SetColor(c_tit3)
 @ li+1,ci+1  SAY el_tit color c_tex3
 @ li+2,ci+1  SAY Repl([Ä],Len(el_tit)) color c_tex3
 @ lf-1,ci+20 SAY 'Existencia Total:'
 @ lf-1,cf-13 SAY Str(ExisT,13,3)
 SetColor(c_tex3)
 off_key()
 la_opcion:=Achoice(li+3,ci+1,lf-2,cf-1,aAlm)
 on_Key()
 DbSelectar(selant)
 SetColor(colant)
 RestScreen(li,ci,lf+1,cf+1,sPant)
 if pcount()<>0
    if lastkey()=27
       ret_alm:=Space(2)
    else
       ret_alm:=substr(aAlm[la_opcion],1,2)
    endif
 endif
Return ret_alm

* PROCEDIMIENTO PARA VER LOS PROVEEDORES DEL ARTICULO
Function Ver_Provee
   Local Ult_sel:=Select(),bufhpr,bufhntx,lContinua:=.f.
   SET KEY -32 TO
   BUFHPR := cf('BUFHPR'+ESTACION+'.DBF')
   bufhntx:= cf('BUFHPR'+ESTACION+'.NTX')
   IF ABRIR([MXHISCPA],CF("MXHISCPA.DBF"),[S],CF("MXHICPX1.NTX")) .AND. ABRIR('MXCTAPRO',CE("MXCTAPRO.DBF"),"S",CE("MXCTPAX1.NTX"))
      DBSELECTAR("MXHISCPA")
      DbSeek(mxctainv->codart)
      HacerBuffer('MXHISCPA->CODART=mxctainv->codart',bufhpr)
      IF file(bufhpr)
         ABRIR([BUFHPR],bufhpr,[E])
         index on fecha_cpa to (bufhntx)
         dbgotop()
         Edit_ventana('VERPROVE')
      ENDIF
      CERRAR('BUFHPR')
      CERRAR('MXHISCPA')
      CERRAR('MXCTAPRO')
      FERASE(BUFHPR)
      FERASE(BUFHNTX)
   ENDIF
   DBSELECTAR(ult_sel)
   SET KEY -32 TO Ver_provee
RETURN nil

/*
* PROCEDIMIENTO PARA VER LOS PROVEEDORES DEL ARTICULO
Function Ver_Provee
   Local Ult_sel:=Select(),buffer,bufntx,lContinua:=.f.
   Private el_cod
   SET KEY -32 TO
   el_cod := mxctainv->codart
   buffer:=cf('BUFHPR'+ESTACION+'.DBF')
   IF ABRIR([MXHISCPA],CF("MXHISCPA.DBF"),[S],CF("MXHICPX1.NTX"))
      if DbSeek(el_cod)
         HacerBuffer('CODART=mxctainv->codart',buffer)
         lContinua:=.t.
      else
         Pare("NO HAY HISTORICO DISPONIBLE PARA ESTE ARTICULO")
         lContinua:=.f.
      endif
      CERRAR('MXHISCPA')
   ENDIF

   IF lContinua .AND. ABRIR('MXCTAPRO',CE("MXCTAPRO.DBF"),"S",CE("MXCTPAX1.NTX")) .AND. ABRIR([BUFFER],buffer,[E],buffer)
      browse()
      Edit_ventana('VERPROVE')
      CERRAR('MXCTAPRO')
      CERRAR('BUFFER')
      FERASE(BUFFER)
      FERASE(QUITAR_EXTENSION(buffer)+'.NTX')
   ENDIF
   DBSELECTAR(ult_sel)
   SET KEY -32 TO Ver_provee
RETURN nil
*/























