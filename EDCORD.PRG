PROCEDURE PAG0306
DECLARE HCAMPO[5],HSUBTIT[5],RCAMPO[8],RSUBTIT[8]
**
PRIVATE XNUMORD,XEMISION,XPROVEE,XNOMCLI,XESTATUS,XENTREGA
PRIVATE XITEM,MITEM,XUNIDAD,XCANTIDAD,XCOSTO,XCOSTO_TOT,XDESCRIP, XIVA,XBULTO,XDESC,xCodCon,xcoddpto
PRIVATE XCOMEN1,XCOMEN2,XTOT_ORD,XDESPACHO,XDESCUENTO
PRIVATE BUFFER,BUFCON,BUFRQC,REC_DCTO
PRIVATE LEMISION,LNUMORD,LISTARQC:=''
PRIVATE BITEM,BDESCRIP,BNUMORD,YFACTOR
PRIVATE ENTRO,II_LISTASI
PRIVATE NO_CONFOR,   UNIREF,DUPLI_DOC:=.F.,en_dup:=.f.
PRIVATE  D_REFRES:=BLK('RF2()')
PRIVATE CLI_DESC := 0
PRIVATE XDESCUENTO
PRIVATE ES_D:=.F.
PRIVATE PV_MUL:=RECUPVMUL()
PRIVATE DCTOS:=0
PRIVATE XHORA, XCAMBIO

PRIVATE XTPCORR:='ORD'
PRIVATE CFILNUM:=MXNUMORD

** NUEVOS DATOS PARA LA ORDEN
PRIVATE XF_CLIENTE
PRIVATE XF_RIF
PRIVATE XF_NIT
PRIVATE XF_DIR1
PRIVATE XF_DIR2
PRIVATE XF_DIR3
PRIVATE XF_DIR4
PRIVATE XF_TLF1
PRIVATE XF_TLF2
PRIVATE XF_FAX
PRIVATE XF_EMAIL
PRIVATE XF_CENVIO
PRIVATE XF_MENVIO
PRIVATE XF_TENTREG
PRIVATE XF_FPAGO
*****************************

BUFCON=G_CAMINO+[BUFCAL]+estacion+[.DBF]
BUFRQC=G_CAMINO+[BUFRQC]+estacion+[.DBF]
II_LISTASI=[]
EDITA_INV=.T.
NO_LOOP=.T.
STORE .F. TO ENTRO
BITEM    = SPACE(15)
BDESCRIP = SPACE(50)
BNUMORD  = SPACE(8)
REC_DCTO=99999
NO_CONFOR = .T.

BUFFER=G_CAMINO+[BUFORD]+ESTACION+[.DBF]

CAMORD()
SUBORD()
CAMROR()
SUBROR()
MENSAJ(MEN3)

IF AbrirOrd()
   dbselectar([MXCTAINV])
   IF inv_filpr='S'
      FiltrarInv()
   ENDIF
   dbselectar([MXCTAPRO])
   if !es_cob
      filtrarpro()
   endif
   SELECT MXENCORD
   GO BOTTOM
   INIT_VER()
   SET KEY -7 TO DUP_ORD
   SET KEY 279 TO REIMPRIM
   //SET KEY -8 TO CARGADOR()
   //EDC([ORD],[ ORDENES DE COMPRA ],HSUBTIT,HCAMPO,.F.,.F.,.T.,.F.,.F.,.T.,.T.,.F.,[F8=Duplicar  F9=Contadores  Alt+I=Reimprimir])
   CARGACORR(XTPCORR)  // NUEVA FUNCION PARA LA CARGA DE LOS CORRELATIVOS DEL DOC.. 11.3.2005
   EDC([ORD],[ ORDENES DE COMPRA ],HSUBTIT,HCAMPO,.F.,.F.,.T.,.F.,.F.,.T.,.T.,.F.,[F8=Duplicar  Alt+I=Reimprimir])
   SET KEY -7 TO
   //SET KEY -8 TO
   SET KEY 279 TO
ENDIF
DELEBUFF()
RETURN

FUNCTION VMODORD

IF ESTATUS='OK'
 PARE('ORDEN DE COMPRA NO PUEDE SER MODIFICADA')
 RETURN .F.
ENDIF

RETURN(!CHK_BLOCK(EMISION))

FUNCTION VELIORD
RETURN (!CHK_BLOCK(EMISION))

PROCEDURE SUBORD
HSUBTIT[ 1]=[N£mero  ]
HSUBTIT[ 2]=[Emisi¢n ]
HSUBTIT[ 3]=[Proveedor]
HSUBTIT[ 4]=[Estatus]
HSUBTIT[ 5]=[        Total]
RETURN

PROCEDURE CAMORD
HCAMPO[ 1]=[NUMORD]
HCAMPO[ 2]=[EMISION]
HCAMPO[ 3]=[PROVEE]
HCAMPO[ 4]=[ESTATUS]
HCAMPO[ 5]=[TOT_ORD]
RETURN

PROCEDURE DEFORD
* VALORES POR DEFECTO
ENTRO=.F.
//XNUMORD    = PROXINUM(MXNUMORD)
XNUMORD    = PROXINUM(CFILNUM)    // NUEVA FORMA DE LLAMAR AL CORRELATIVO
XEMISION   = CTOD(G_FECHA)
XPROVEE   = SPACE(  8)
XNOMCLI    = SPACE( 60)
XDESCUENTO = 0.00
STORE SPACE(30) TO XCOMEN1,XCOMEN2
XESTATUS='PE'
XENTREGA=XEMISION
XHORA      = STRTRAN(TIME(),':')
XCAMBIO    = cambioMon(xemision,xhora)
STORE 0 TO XTOT_ORD
XDESCUENTO:=0
@  4,24 SAY  SPACE(35)
IF !DUPLI_DOC
 CRDROR()
 @ TOPE,COLIZQ CLEAR TO FONDO,COLDER
 CRDORD()
 FP_TOTAL()
 SELECT MXRENORD
 IF File(buffer) .AND. Dbvacio(Buffer)=1
    IF DECIS('Existe un archivo temporal con informaci¢n, ¨ Desea usarlo ?')='N'
       COPY STRUCTURE TO (BUFFER)
    ENDIF
 ELSE
    COPY STRUCTURE TO (BUFFER)
 ENDIF
ELSE
 XPROVEE  :=MXENCORD->PROVEE
 XCOMEN1  :=MXENCORD->COMEN1
 XCOMEN2  :=MXENCORD->COMEN2
ENDIF

*** NUEVOS DATOS PARA LA ORDEN
XF_CLIENTE := PAD(G_EMPRESA,40)
XF_RIF     := G_CIF
XF_NIT     := G_NIT
XF_DIR1    := G_DIREC1
XF_DIR2    := G_DIREC2
XF_DIR3    := G_DIREC3
XF_DIR4    := G_DIREC4
XF_TLF1    := G_TLF1
XF_TLF2    := G_TLF2
XF_FAX     := G_FAX
XF_EMAIL   := G_EMAIL
XF_CENVIO  := SPACE(40)
XF_MENVIO  := SPACE(20)
XF_TENTREG := SPACE(40)
XF_FPAGO   := SPACE(30)
*****************************

SELECT MXENCORD
RETURN

PROCEDURE ASGORD
* ASIGNACION DE REGISTRO A MEMORIA

STORE NUMORD  TO XNUMORD  ,LNUMORD
STORE EMISION TO XEMISION ,LEMISION
STORE PROVEE  TO XPROVEE  ,LPROVEE

XNOMCLI    = MXCTAPRO->NOMCLI
XDESCUENTO:=DESCUENTO
XCOMEN1    = COMEN1
XCOMEN2    = COMEN2
XESTATUS   = ESTATUS
XENTREGA   = ENTREGA
XCAMBIO    = CAMBIO
XTOT_ORD   = TOT_ORD
XDESCUENTO = DESCUENTO

** NUEVOS DATOS PARA LA ORDEN
XF_CLIENTE := F_CLIENTE
XF_RIF     := F_RIF
XF_NIT     := F_NIT
XF_DIR1    := F_DIR1
XF_DIR2    := F_DIR2
XF_DIR3    := F_DIR3
XF_DIR4    := F_DIR4
XF_TLF1    := F_TLF1
XF_TLF2    := F_TLF2
XF_FAX     := F_FAX
XF_EMAIL   := F_EMAIL
XF_CENVIO  := F_CENVIO
XF_MENVIO  := F_MENVIO
XF_TENTREG := F_TENTREG
XF_FPAGO   := F_FPAGO
*****************************

SELECT MXRENORD
SEEK(XNUMORD)
IF File(buffer) .AND. Dbvacio(Buffer)=1
   IF DECIS('Existe un archivo temporal con informaci¢n, ¨ Desea usarlo ?')='N'
      COPY TO (BUFFER) WHILE NUMORD = XNUMORD
   ENDIF
ELSE
   COPY TO (BUFFER) WHILE NUMORD = XNUMORD
ENDIF
SELECT MXENCORD
RETURN

PROCEDURE RPLORD
* REEMPLAZO EN EL REGISTRO

IF MODIFICA
 GEN_ORD(1)
ENDIF

SELECT MXENCORD

MENSAJ([Grabando Orden de compra ...])
AUDITORIA([Acceso a mantenimiento de orden de compra],xnumord,numord,IF(MODIFICA,[MOD],[INC]))
REPLACE NUMORD    WITH XNUMORD,;
        EMISION   WITH XEMISION,;
        PROVEE    WITH XPROVEE,;
        COMEN1    WITH XCOMEN1,;
        COMEN2    WITH XCOMEN2,;
        ESTATUS   WITH XESTATUS,;
        ENTREGA   WITH XENTREGA,;
        TOT_ORD   WITH XTOT_ORD,;
        DESCUENTO WITH XDESCUENTO
        GMoneda(xcambio)
IF cpa_mdoc='S'
   REPLACE F_CLIENTE WITH XF_CLIENTE,;
           F_RIF     WITH XF_RIF,;
           F_NIT     WITH XF_NIT,;
           F_DIR1    WITH XF_DIR1,;
           F_DIR2    WITH XF_DIR2,;
           F_DIR3    WITH XF_DIR3,;
           F_DIR4    WITH XF_DIR4,;
           F_TLF1    WITH XF_TLF1,;
           F_TLF2    WITH XF_TLF2,;
           F_FAX     WITH XF_FAX,;
           F_EMAIL   WITH XF_EMAIL,;
           F_CENVIO  WITH XF_CENVIO,;
           F_MENVIO  WITH XF_MENVIO,;
           F_TENTREG WITH XF_TENTREG,;
           F_FPAGO   WITH XF_FPAGO
ENDIF
IF MODIFICA
 SELECT MXRENORD
 IF DBSEEK(LNUMORD)
    DO WHILE NUMORD = LNUMORD .AND. !EOF()
       IF !EMPTY(NUM_RQC)
          REV_REQUIS(NUM_RQC,ITEM)
       ENDIF
       dbskip()
    ENDDO
 ENDIF
ENDIF
SELECT MXRENORD
WHILE !FLOCK()
ENDDO
IF MODIFICA
 SEEK(LNUMORD)
 DELETE WHILE NUMORD = LNUMORD
ENDIF
APPEND FROM (BUFFER)
UNLOCK
delete file (buffer)
GEN_ORD()

ActRqc()

LIMPIA()
DELETE FILE (BUFFER)

II_LISTASI=[]
LISTARQC:=''

SELECT MXENCORD
DbCloseAll()
DOC_EMI(' ORDEN DE COMPRA ',G_LIBORD,'MXENCORD',XNUMORD)
DbCloseAll()
AbrirOrd()
PANORD()

RETURN

PROCEDURE MASORD
* MASCARA DE LA VENTANA

SETCOLOR(C_BOX2)
BOXTIT([ ORDEN DE COMPRA ],2,0,maxrow()-2,79)
L78(7)

SETCOLOR(C_TEX2)
@  3, 2 SAY [N£mero.....:]
@  3,25 SAY [Fecha:]
@  3,43 SAY [Entrega:]
@  3,63 SAY [Estatus:]
@  4, 2 SAY [Proveedor..:]
@  4,61 SAY [Tasa US$:        ]
@  5, 2 SAY [Comentarios:]
*****************************
@ maxrow()-5, 1 SAY [Nombre:                                                   Total:]
MASROR()
RETURN



FUNCTION CHPROORD
PARAMETER EL_CLI,EL_NUME,EL_ALIAS
private INC_CLI:=.F.
IF LASTKEY()=5
 RETURN .T.
ENDIF

SELE MXCTAPRO
SEEK(EL_CLI)
IF !FOUND()
 DO SELINFCLI
 SELECT MXCTAPRO
 IF LASTKEY()=27
  XPROVEE=SPACE(8)
  KEYBOARD SALIDA
  WAIT
 ENDIF
 IF INC_CLI
  XPROVEE =CODCLI
 ENDIF
 SELECT &EL_ALIAS
 RETURN .F.
ELSE
  @  4,24 SAY PAD(NOMCOD(EL_CLI,[MXCTAPRO],[NOMCLI]),35)
ENDIF

SELECT &EL_ALIAS
RETURN .T.


PROCEDURE GETORD
* CAPTURA REGISTRO
PRIVATE ed_renglon,cScr
ed_renglon=.t.
SET KEY -7 TO
SET KEY 279 TO
DO WHILE .T.
 KEYBOARD SALIDA+SALIDA
 @  3,15 GET XNUMORD  PICT [@!] VALID (CHKDUP(XNUMORD,[Orden de Compra]) .AND. !EMPTY(XNUMORD)) .OR. LASTKEY()=5
 @  3,32 GET XEMISION VALID CHKEJER(XEMISION)  .AND. !CHK_BLOCK(XEMISION)
 @  3,52 GET Xentrega
 @  3,72 SAY Xestatus
 @  4,15 GET XPROVEE VALID CHPROORD(XPROVEE,@XNUMORD,'MXENCORD')
 @  4,71 GET XCAMBIO PICT '9999.99' WHEN cpa_edcamb='S' VALID !Empty(xcambio)
 @  5,15 GET XCOMEN1
 @  6,15 GET XCOMEN2
 LEER()
 IF ESCAPAR()
  IF ENTRO
   BELL()
   IF DECIS([¨ Desea cancelar la operaci¢n ?],1)=[S]
    II_LISTASI=[]
    LISTARQC:=''
    SALIR=.T.
   ELSE
    LOOP
   ENDIF
  ELSE
   II_LISTASI=[]
   LISTARQC:=''
   SALIR=.T.
  ENDIF
 ENDIF
 IF SALIR
  DUPLI_DOC:=.F.
  //CANCNUM(MXNUMORD,XNUMORD)
  CANCNUM(CFILNUM,XNUMORD)
  DELETE FILE (BUFFER)
  SET KEY -7 TO DUP_ORD
  SET KEY 279 TO REIMPRIM
  RETURN
 ENDIF
 IF EMPTY(XPROVEE)
  KEYBOARD REPLICATE(SALIDA,2)
  LOOP
 ENDIF

 ENTRO=.T.
 IF ABRIR([MXCTASER],MXCTASER,[S],MXCTSEX1)
 IF ABRIR([BUFFER],BUFFER,[E])
  IF DUPLI_DOC
   REPLACE ALL DESPACHO WITH 0, DESBULTO WITH 0,ESTATUS WITH 'PE'
   DUPLI_DOC:=.F.
  ENDIF
  GO TOP
  DO WHILE !EOF()
   dctos:=if(substr(item,1,2)='*D',1,0)
   if dctos=1
    EXIT
   endif
   DbSkip()
  ENDDO
  GO TOP
  RECAORD()
  SETCOLOR(C_TEX2)
  FP_TOTAL()
  SETCOLOR(C_TIT2)
  VACIO=.F.
  DO WHILE .T.
   SET KEY -7 TO SER0000
   SET KEY -8 TO REQUISI
   ustre:='USTREPAG'+Alltrim(Str(subopcion))
   ClvT:=&ustre
   &ustre.:='SSS'
   if cpa_propro='S'
    SELANT:=SELECT()
    DBSELECTAR('MXCTAINV')
    SET FILTER TO
    SET FILTER TO PROV_ASIG==XPROVEE
    DBGOTOP()
    DBSELECTAR(SELANT)
   endif
   EDC([ROR],[],RSUBTIT,RCAMPO,.T.,.T.,.T.,.F.,.F.,.T.,.T.,.F.,'F8=SERVICIOS F9=REQUISICIONES',.T.)
   &ustre.:=ClvT
   SET KEY -7 TO
   SELECT BUFFER
   GO TOP
   REPLACE ALL  NUMORD WITH XNUMORD , EMISION WITH XEMISION, PROVEE WITH XPROVEE
   XESTATUS:=ESTAT_ORD()
   GO TOP
   vacio=LASTREC()=0 .OR. EOF()
   EXIT
  ENDDO

  CERRAR([BUFFER])

  IF VACIO
   SELECT MXENCORD
   LOOP
  ENDIF

  SAVE SCREEN TO cScr
  IF cpa_mdoc='S'
     MasDatosOrd()
  ENDIF
  CONFORME=CONFIRMA()
  IF CONFORME<>[S]
   IF CONFORME=[C]
    //CANCNUM(MXNUMORD,XNUMORD)
    CANCNUM(CFILNUM,XNUMORD)
    DELETE FILE (BUFFER)
   ENDIF
   II_LISTASI=[]
   LISTARQC:=''
  ENDIF

 ENDIF
 CERRAR('MXCTASER')

 ENDIF

 RESTORE SCREEN FROM cScr
 EXIT

ENDDO
SET KEY -7 TO DUP_ORD
SET KEY 279 TO REIMPRIM
SELECT MXENCORD
RETURN

PROCEDURE DETORD
private xemision
IF !EOF()
 SETCOLOR(C_TIT2)
 @  3,15 SAY NUMORD
 @  3,32 SAY EMISION
 @  3,52 say entrega
 @  3,72 SAY estatus
 @  4,15 SAY PROVEE
 @  4,24 SAY PAD(NOMCOD(PROVEE,[MXCTAPRO],[NOMCLI]),35)
 @  4,71 SAY str(CAMBIO,7,2)
 @  5,15 SAY COMEN1
 @  6,15 SAY COMEN2
 XTOT_ORD=TOT_ORD
 xemision:=emision
 FP_TOTAL()
 WNUMORD=NUMORD
 BORDE=[ NUMORD=WNUMORD ]
 SELECT MXRENORD
 SEEK(WNUMORD)
 CRDROR()
 APL=[ROR]
 PAGINA()
 APL=[ORD]
 CRDORD()
 SELECT MXENCORD
ENDIF
RETURN


PROCEDURE ELIORD
* ELIMINACION DE PEDIDO
PRIVATE WNUMORD,MODIFICA,XNUMORD
MODIFICA=.T.
SELECT MXENCORD
WNUMORD=NUMORD
XNUMORD=NUMORD
GEN_ORD(1)
SELECT MXRENORD
IF DBSEEK(XNUMORD)
   DO WHILE NUMORD = XNUMORD .AND. !EOF()
      IF !EMPTY(NUM_RQC)
         REV_REQUIS(NUM_RQC,ITEM)
      ENDIF
      dbskip()
   ENDDO
ENDIF
SELECT MXENCORD
AUDITORIA([Acceso a mantenimiento de orden de compra],numord,,[ELI])
DELETE
SELECT MXRENORD
SEEK(WNUMORD)
IF FOUND()
 DO WHILE !FLOCK()
 ENDDO
 DELETE WHILE NUMORD = WNUMORD
 UNLOCK
ENDIF
SELECT MXENCORD
RETURN

PROCEDURE CRDORD
* COORDENADAS DE LA VENTANA
TOPE= 3
FONDO=maxrow()-3
COLIZQ= 1
COLDER=78
CONTLIN=TOPE
RETURN

PROCEDURE BUSORD
PRIVATE TNUMORD
BUS([UBUSORD])
RETURN

PROCEDURE UBUSORD

@  3,15 GET BNUMORD PICT [@!]
LEER()
SALIR=ESCAPAR()
IF !SALIR
 TNUMORD=TRIM(BNUMORD)
 YKEY=TNUMORD
ENDIF
RETURN

******************************************************************

PROCEDURE PANORD
local mmenuscr
MMENUSCR=SAVESCREEN(maxrow()-4,0,maxrow(),79)
INIT_VER()
MASORD()
RESTSCREEN(maxrow()-4,0,maxrow(),79,MMENUSCR)
RETURN


STATIC FUNCTION GEN_ORD
PARAMETER REVERSO
PRIVATE XCANTIDAD,XITEM,XESTATUS,XDESPACHO

REVERSO= PCOUNT()=1

SELECT MXRENORD
SEEK(XNUMORD)
DO WHILE !EOF() .AND. NUMORD=XNUMORD
 XITEM=ITEM
 SELECT MXCTAINV
 SEEK(XITEM)
 IF FOUND()
  DO MENSAJ WITH [Actualizando ORDENES...]
  SELECT MXRENORD
  XCANTIDAD = CANTIDAD
  XDESPACHO = DESPACHO
  XESTATUS  = ESTATUS
  SELECT MXCTAINV
  IF XESTATUS="PE"
   REC_LOCK(0)
   IF REVERSO
    REPLACE ORDENA WITH ORDENA-(XCANTIDAD-XDESPACHO)
   ELSE
    REPLACE ORDENA WITH ORDENA+(XCANTIDAD-XDESPACHO)
   ENDIF
   UNLOCK
  ENDIF
 ENDIF
 SELECT MXRENORD
 SKIP
ENDDO
RETURN 0


***********************************************************************************
*************************************************************
***********************************
********************

PROCEDURE SUBROR
RSUBTIT[ 1]=PAD([C¢digo],15)
RSUBTIT[ 2]=[Und]
RSUBTIT[ 3]=[Bulto]
RSUBTIT[ 4]=[Cantidad/Bulto]
RSUBTIT[ 5]=[    Costo/Und]
RSUBTIT[ 6]=[ %Dcto]
RSUBTIT[ 7]=[         Total]
RSUBTIT[ 8]=[]
RETURN

PROCEDURE CAMROR
RCAMPO[ 1]=[ITEM]
RCAMPO[ 2]=[UNIDAD]
RCAMPO[ 3]=[BULTO]
RCAMPO[ 4]=[CANTIDAD]
RCAMPO[ 5]=[COSTO]
RCAMPO[ 6]=[DESC]
RCAMPO[ 7]=[COSTO_TOT]
RCAMPO[ 8]=[DESCRIP]
RETURN

PROCEDURE DEFROR
* VALORES POR DEFECTO
XITEM      = SPACE( lon_item)
XUNIDAD    = SPACE(  3)
STORE 1 TO XBULTO,XCANTIDAD
STORE 0 TO XCOSTO,XDESC,XCOSTO_TOT
XDESCRIP   = SPACE(50)
XDESPACHO=0
XIVA       = ' '
xCodCon    := Space(20)
xcoddpto   :=Space(10)
@ MAXROW()-5,44 SAY space(12)
RETURN


PROCEDURE ASGROR
* ASIGNACION DE REGISTRO A MEMORIA
XITEM      = ITEM
MITEM      = ITEM
XUNIDAD    = UNIDAD
XBULTO     = BULTO
XCANTIDAD  = CANTIDAD
XCOSTO     = E_IMPORTE(COSTO,XEMISION)
XDESC      = DESC
XCOSTO_TOT = if(substr(item,1,2)='*D',-COSTO_TOT,costo_tot)
XDESCRIP   = DESCRIP
XIVA       = IVA
XDESPACHO  = DESPACHO
xCodCon    := CodCon
xcoddpto:=coddpto
RETURN

PROCEDURE RPLROR
* REEMPLAZO EN EL REGISTRO

IF MODIFICA
 FP_RESTA()
ENDIF


REPLACE ITEM WITH XITEM,;
        UNIDAD   WITH XUNIDAD,;
        BULTO    WITH XBULTO,;
        CANTIDAD WITH XCANTIDAD  ,;
        COSTO   WITH XCOSTO,;
        DESC     WITH XDESC,;
        COSTO_TOT WITH XCOSTO_TOT,;
        DESCRIP  WITH XDESCRIP,;
        IVA      WITH XIVA,;
        CodCon   WITH xCodCon,;
        coddpto with xcoddpto


REPLACE ESTATUS WITH IF(DESPACHO=CANTIDAD,'OK','PE')

FP_SUMA()
FP_TOTAL()

RETURN


PROCEDURE MASROR
* MASCARA DE LA VENTANA
SETCOLOR(C_INV2)
@  8,1 SAY HAZ_SUB(RSUBTIT)
SETCOLOR(C_BOX2)
L78(9)
L78(maxrow()-6)
L78(maxrow()-4)
SETCOLOR(C_TEX2)
RETURN

PROCEDURE GETROR
PRIVATE ES_D:=.F.
* CAPTURA REGISTRO
MENSAJ([<ENTER>=INVENTARIO  F8=SERVICIOS F9=REQUISICIONES <ESC>=SALIR],MAXROW())
ES_D=.F.
YFACTOR=1
CHKU=.F.
BULTO:=1
IF !MODIFICA .AND. CPA_SERV='S'
   XITEM:=PAD('\',15)
ENDIF
@ CONTLIN, 2 GET XITEM     VALID CHKOTEM()
@ maxrow()-5,8 GET XDESCRIP PICT '@S35'
@ CONTLIN,18 GET XUNIDAD    WHEN W1() PICT [!!!] VALID SEL_UNI(UNIREF,@XUNIDAD,CHKU)
@ CONTLIN,22 GET XBULTO     WHEN W1() PICT "99999" VALID XBULTO>0
@ CONTLIN,28 GET XCANTIDAD  WHEN W1() PICT "9999999999.999"
@ CONTLIN,43 GET XCOSTO     WHEN W1() .and. !w2() pict "999999999.999" valid ABS(cambul(xcantidad,xbulto)*XCOSTO*YFACTOR)<VAL("999999999.999")
@ CONTLIN,57 GET XDESC      WHEN !empty(xitem) .or. W2() PICT "999.99"  VALID XDESC>=0 .AND. XDESC<=100.00
@ CONTLIN,64 GET XCOSTO_TOT WHEN (!w2().and.!Empty(xitem).and.xcosto=0) .or. (W2().and.Empty(xDesc)) PICT "9999999999.999"
LEER()
SALIR:=ESCAPAR().or.ABS(cambul(xcantidad,xbulto)*XCOSTO*YFACTOR)>=VAL("9999999999.999")
IF !SALIR .AND. !EMPTY(XITEM)
 CUENTACON()
ENDIF

FUTREN()

RETURN

STATIC FUNC W1
Private sale
if xitem='*D'
 sale:=.f.
else
 sale:=!EMPTY(XITEM)
endif
RETURN (sale)

STATIC FUNC W2
RETURN(SubStr(xitem,1,2)=[*D])


STATIC PROC FUTREN
IF !SALIR
 if es_d
  if xDesc=0
   xDesc:=Abs((xcosto_Tot*100/xtot_ord))
  else
   xCosto_Tot:=Abs((xDesc*xTot_Ord/100))
  endif
  xdescuento:= -xcosto_tot
  xcosto_tot:= -xcosto_tot
  xcosto := 0
  STORE 0 TO XBULTO,XCANTIDAD
  XIVA=' '
  es_d=.f.
 else
  IF XCOSTO=0
   if !w2()
    XCOSTO=DIV(XCOSTO_TOT,XCANTIDAD)
   endif
  ENDIF
  IF CHKU
   XUNIDAD=UNIREF
   XCANTIDAD=XCANTIDAD*YFACTOR
  ENDIF
  if !w2()
   XCOSTO:=CAMBIO_IMPORTE(XCOSTO,Xemision)
   XCOSTO_TOT=( CAMBUL(XCANTIDAD,XBULTO)*XCOSTO*(1-(XDESC/100.00)) )
  endif
 endif
ENDIF
RETURN


PROCEDURE DETROR
LOCAL UCOLOR
* DETALLE
IF !EMPTY(ITEM)
 @ CONTLIN, 2 SAY substr(ITEM,1,15)
 @ CONTLIN,18 SAY if(substr(item,1,2)<>'*D',UNIDAD,"---" )
 @ CONTLIN,22 SAY if(substr(item,1,2)<>'*D',BULTO,"-----" )
 @ CONTLIN,28 SAY if(substr(item,1,2)<>'*D',str(CANTIDAD,14,3),"--------------" )
 @ CONTLIN,43 SAY if(substr(item,1,2)<>'*D',Str(e_importe(COSTO,xemision),13,3),repl("-",13))
 @ CONTLIN,57 SAY DESC
 @ CONTLIN,64 SAY str(e_importe(COSTO_TOT,xemision),14,3)
ELSE
 @ CONTLIN,2 SAY PAD('*COMENTARIO*',16)
 @ CONTLIN,18 SAY space(60)
ENDIF
IF ALIAS()='BUFFER'
 @ maxrow()-5,8 SAY PAD(DESCRIP,35)
 ucolor=setcolor()
 setcolor(c_TIT2)
 @ MAXROW()-5,44 SAY "D="+STR(DESPACHO,10,3)
 setcolor(ucolor)
ENDIF
RETURN


PROCEDURE GETBROR
* CAPTURA DE CAMPOS EN MODO BROWSE
PRIVATE FFF,CCC
YFACTOR=1
FFF=ROW()
CCC=COL()
CHKU=.F.
DO CASE
CASE ICAMPO=1
 @ FFF,CCC GET XITEM VALID CHKOTEM()
CASE ICAMPO=2
 @ FFF,CCC GET XUNIDAD WHEN W1() VALID SEL_UNI(UNIREF,@XUNIDAD,CHKU)
CASE ICAMPO=3
 @ FFF,CCC GET XBULTO  WHEN W1()   PICT "99999" VALID XBULTO>0
CASE ICAMPO=4
 @ FFF,CCC GET XCANTIDAD WHEN W1() PICT "999999.999"
CASE ICAMPO=5
 @ FFF,CCC GET XCOSTO  WHEN W1()  PICT '99999999.999' VALID XCOSTO>0 .AND. CAMBUL(XCANTIDAD,XBULTO)*XCOSTO<89999999.99
CASE ICAMPO=6
 @ FFF,CCC GET XDESC  WHEN W1()    PICT "999.99"  VALID XDESC>=0 .AND. XDESC<=100.00
CASE ICAMPO=8
 @ FFF,CCC GET XDESCRIP PICT '@S35'
OTHERWISE
 SALIR=.T.
 RETURN
ENDCASE
READ
SALIR=ESCAPAR(1)
FUTREN()
RETURN

FUNCTION VMODROR
PRIVATE devo:=.t.
IF !EMPTY(NUM_RQC)
 PARE([ATENCION: Rengl¢n proviene de la REQUISICION No. ]+AllTrim(NUM_RQC))
ENDIF
if substr(item,1,2)='*D'
 Pare('Este renglon es un descuento, no puede ser modificado')
 devo:=.f.
endif
RETURN devo

FUNCTION VELIROR
 IF !EMPTY(NUM_RQC)
  PARE([ATENCION: Rengl¢n proviene de la REQUISICION No. ]+AllTrim(NUM_RQC))
 ENDIF
RETURN .T.

PROCEDURE ELIROR
* ELIMINACION DE REGISTRO
UREG=RECNO()
if substr(item,1,2)='*D'
 dctos=0
endif
DELETE
PACK
RECAORD()
RETURN

PROCEDURE CRDROR
* COORDENADAS DE LA VENTANA
TOPE = 10
FONDO =maxrow()-7
COLIZQ = 1
COLDER =78
CONTLIN=TOPE
RETURN

************* CHEQUEO ITEM
FUNCTION CHKOTEM(lax)
LOCAL PRICHAR
IF LASTKEY()=5
 RETURN .T.
ENDIF
UNIREF = []
CHKU   = .F.
PRICHAR=SUBSTR(XITEM,1,1)
DO CASE
 CASE PRICHAR$[*]
  IF SUBSTR(XITEM,2,1)=[D] .AND. Empty(lax)
   IF dctos=1 .AND. !MODIFICA
    PARE([No acepta mas de un descuento])
    RETURN .F.
   ENDIF
   ES_D=.T.
   XDESCRIP=PAD([* DESCUENTO *],50)
   IF !MODIFICA
      STORE 0 TO XBULTO,XCANTIDAD
   ENDIF
   XIVA=' '
   XDESC=CLI_DESC
  ELSE
   IF !MODIFICA
      XBULTO:=1
      XCANTIDAD:=1
   ENDIF
   XIVA=' '
   CHKU=.f.
   SELECT BUFFER
   *KEYBOARD REPLICATE(SALIDA,4)
  ENDIF
 CASE PRICHAR$[/]
   if !ChkCDup(xitem)
    RETURN .f.
   endif
   if !modifica
      XDESCRIP=Space(50)
      STORE 1 TO XBULTO,XCANTIDAD
   endif
   XIVA=' '
   RETURN .T.
 CASE PRICHAR$[\]
  SELE MXCTASER
  SEEK(SUBSTR(XITEM,2,5))
  IF !FOUND()
   SER0000()
   SELE BUFFER
   RETURN .F.
  ELSE
   XDESCRIP=PAD(MXCTASER->NOMSER,50)
   XCOSTO=e_importe(MXCTASER->PRESER,xemision)
   XIVA   =MXCTASER->IVA
   IF !MODIFICA
      XCODCON:=REFCON
   ENDIF
   CHKU=.F.
   IF cpa_pbul='S'
      KEYBOARD REPLICATE(SALIDA,2)
   ELSE
      KEYBOARD REPLICATE(SALIDA,3)
   ENDIF
  ENDIF
  SELE BUFFER
 OTHERWISE
if !Empty(xitem)
   if !ChkCDup(xitem)
      RETURN .f.
   endif
endif
SELECT MXCTAINV
SEEK(XITEM)
IF !FOUND()
 SELINFINV()
 IF LASTKEY()=27
  KEYBOARD SALIDA
  WAIT
  SELECT BUFFER
  RETURN .T.
 ENDIF
 SELECT BUFFER
 RETURN .F.
ELSE
 CHKU=.T.
 STORE UNIDAD TO XUNIDAD,UNIREF
 XDESCRIP   = PAD(MXCTAINV->NOMART,50)
 XIVA       = MXCTAINV->IVA
 if !modifica
  XCODCON:=CODCON
  XCOSTO     = e_importe(MXCTAINV->ULT_COSTO,xemision)
  XCANTIDAD  = if(!empty(MXCTAINV->CAN_BUL),MXCTAINV->CAN_BUL,1)
 elseif modifica.and. xitem<>mitem
  XCOSTO     = e_importe(MXCTAINV->ULT_COSTO,xemision)
  XCOSTO_TOT = 0
 endif
 IF cpa_pbul='S'
    KEYBOARD REPLICATE(SALIDA,2)
 ELSE
    KEYBOARD REPLICATE(SALIDA,3)
 ENDIF
ENDIF
ENDCASE
SELECT BUFFER
RETURN .T.

******************************************************************


PROCEDURE BUSROR
* BUSQUEDA EN RENGLONES DE CONT.
PRIVATE TITEM,TDESCRIP
VERTBUS([UBUSROR])
RETURN

PROCEDURE UBUSROR
* BUSQUEDA
@ CONTLIN, 2 GET BITEM
@ CONTLIN,15 GET BDESCRIP PICT '@S35'
LEER()
SALIR=ESCAPAR()
IF !SALIR
 TITEM=UPPER(TRIM(BITEM))
 TDESCRIP=UPPER(TRIM(BDESCRIP))
 BCONDICION=IF(EMPTY(TITEM),[.T.],[ITEM=TITEM])+ IF(EMPTY(TDESCRIP),[],[.AND. TDESCRIP$DESCRIP])
 POR_INDICE=.F.
ENDIF
RETURN


******************************************************************

static PROCEDURE FP_SUMA
 XTOT_ORD= XTOT_ORD + COSTO_TOT
RETURN

static PROCEDURE FP_RESTA
 XTOT_ORD= XTOT_ORD - COSTO_TOT
RETURN

static PROCEDURE FP_TOTAL
* DISPLAY DE TOTALES
SETCOLOR(C_TIT2)
@ maxrow()-5,65 SAY e_importe(XTOT_ORD,xemision) PICT '9999999999.999'
SETCOLOR(C_TEX2)
RETURN

STATIC FUNCTION RECAORD
PRIVATE R_REG
R_REG=RECNO()
GO TOP
XTOT_ORD=0
DO WHILE !EOF()
 FP_SUMA()
 SKIP
ENDDO
GOTO R_REG
FP_TOTAL()
RETURN 0

static PROCEDURE DUP_ORD
PARAMETER P1,P2,P3
IF en_dup
 RETURN
ENDIF
en_dup:=.t.
DUP_DOC('MXRENORD',MXENCORD->NUMORD,'MXENCORD->NUMORD=MXRENORD->NUMORD')
en_dup:=.f.
RETURN

STATIC PROC ESTAT_ORD
LOCAL TODOPED:=.T.
SELECT BUFFER
GO TOP
WHILE !EOF()
 IF ESTATUS<>[OK]
   TODOPED=.F.
   EXIT
 ENDIF
 SKIP
ENDDO
GO TOP
RETURN IF(TODOPED,'OK','PE')

PROC RF2
DO CASE
 CASE APL='ORD'
  DETORD()
 CASE APL='ROR'
  POSPAG(RECNO())
  FP_TOTAL()
ENDCASE
RETURN

PROCEDURE Requisi
 Parameters a1,b1,c1
 Private en_menuh:=(a1=[menuh])
 if en_cta
  RETURN nil
 endif
 en_cta:=.T.
 inc_requisi()
 if en_menuh
  on_key()
 else
  KEYBOARD escape
 endif
 en_cta:=.F.
RETURN nil

Static Function Inc_Requisi
 Private tiene,ulias,z_numero
 tiene = .f.
 ulias = select()
 if !abrir('MXENCRQC',mxencrqc,'S',mxenrqx1)
    Pare('No se pudo abrir el archivo de requisiciones')
    DbSelectar(ulias)
    RETURN nil
 endif
 DbGotop()
 COPY TO (bufcon) FOR estatus='PE'
 if DbVacio(bufcon,[E])=0
    Pare('No hay requisiciones pendientes')
    Cerrar('MXENCRQC')
    DELETE FILE (BufCon)
    DbSelectar(ulias)
    Return nil
 endif
 if !abrir('BUFCON',bufcon,[S])
    Pare('No se pudo abrir el temporal de requisiciones')
    Cerrar('MXENCRQC')
    DbSelectar(ulias)
    RETURN nil
 endif
 tiene=.t.
 z_numero=SPACE(8)
 satelrqc([ REQUISICIONES ],'numrqc','emision','DPT_SUC','REQSELPROG',listarqc)
 cerrar('BUFCON')
 DELETE FILE (bufcon)
 if EMPTY(z_numero)
    Cerrar('MXENCRQC')
    DbSelectar(ulias)
    RETURN nil
 endif
 if abrir("MXRENRQC",mxrenrqc,[S],mxrerqx1)
    listarqc:=z_numero
    if !DbSeek(z_numero)
       Pare('No se encontrar¢n renglones de esta requisici¢n')
       Cerrar('MXENCRQC')
       Cerrar('MXRENRQC')
       DbSelectar(ulias)
       RETURN nil
    endif
    COPY TO (bufrqc) WHILE numrqc=z_numero
    Cerrar('MXRENRQC')
    Abrir([BUFRQC],bufrqc,[E])
    RepRqc()
    DELETE ALL FOR (CANTIDAD-ENT_ORD)<=0
    /*
    if cpa_pedpro='S'
     DELETE FOR (costo_tot<=0 .AND.!Empty(item) .and. cantidad<=0)  .or. provee<>xprovee
    else
     DELETE FOR costo_tot<=0 .AND.!Empty(item) .and. cantidad<=0
    endif
    __DbPack()
    */
    Cerrar('BUFRQC')
    if DbVacio(bufrqc,[E])=0
       Pare('La requisici¢n seleccionada no tiene articulos pendientes')
       Cerrar('MXENCRQC')
       DbSelecTar(ulias)
       RETURN nil
    endif
    *******************************************************
    SELECT BUFFER
    APPEND FROM (bufrqc)
    REPLACE ALL num_rqc with Listarqc
    SUM costo_tot to xtot_ord
    FP_TOTAL()
    if en_menuh
       RECALCULA:=.T.
       DbGotop()
       NoFlecha()
       PriPag()
    else
       RECALCULA:=.T.
    endif
    Cerrar('MXENCRQC')
 endif
 DbSelecTar(ulias)
RETURN 0

procedure ReqSelProg
Private devo
devo:=.T.
if numrqc$listarqc
  PARE([Requisici¢n ya fue incluida])
  devo:=.f.
endif
if devo
 z_numero=&campo1
else
 en_sel:=.t.
endif
RETURN nil

Static Function RepRqc()
 Local el_item,t1,xEnt_ord,xCantidad,xTotal:=0,el_alma
 Dbgotop()
 do while !eof()
    el_alma:=almacen
    el_item:=Item
    xEnt_ord:=ent_ord
    xCantidad:=cantidad
    /*
    t1:=if(MXCTAINV->(DBSEEK(el_item)),MXCTAINV->existe_act,0)
    t1:=if(!g_multi,if(MXCTAINV->(DBSEEK(el_item)),MXCTAINV->existe_act,0),exist(xemision,el_alma))
    xTotal:=(xCantidad-xEnt_ord)-t1
    REPLACE cantidad  with xTotal
    REPLACE costo_tot with CAMBUL((xTotal),bulto)*costo
    */
    REPLACE cantidad  with (CANTIDAD-ENT_ORD)
    REPLACE costo_tot with (CANTIDAD-ENT_ORD)*costo
    DbSkip()
 enddo
RETURN nil

Static Function ActRqc()
 local selant:=Select(),_ok:=.t.,codtemp,itemtem,continua:=.f.
 if abrir("MXRENRQC",mxrenrqc,[S],mxrerqx1)
  DbSelectar('MXRENORD')
  if dbseek(xnumord)
   Do While numord=xnumord .and. !eof()
    if !empty(num_rqc)
     continua:=.t.
     CodTemp:=num_rqc
     ItemTem:=item
     Dbselectar('MXRENRQC')
     if DBSEEK(CodTemp)
      Do while numrqc=CodTemp .and. !eof()
       if item=itemTem
        if rlock()
         REPLACE ent_ord with ent_ord+MXRENORD->cantidad
         Unlock
        endif
       endif
       DbSkip()
      Enddo
     endif
    endif
    DbSelectar('MXRENORD')
    DbSkip()
   Enddo
  endif
  Dbselectar('MXRENRQC')
  if continua
   if DBSEEK(CodTemp)
    nReg:=Recno()
    Do while numrqc=CodTemp .and. !eof()
     el_item:=item
     *t1:=if(MXCTAINV->(dbseek(el_item)),MXCTAINV->existe_act,0)
     *if ent_ord>=(round(cantidad-t1,2)) .AND. rlock()
     if ent_ord>=cantidad .AND. rlock()
      REPLACE ESTATUS WITH 'OK'
      COMMIT
      UNLOCK
     endif
     DbSkip()
    Enddo
   endif
  endif
  if continua
   if dbseek(CodTemp)
    Do while numrqc=CodTemp .and. !eof()
     if estatus='PE'
      _ok:=.f.
      EXIT
     endif
     DbSkip()
    enddo
   endif
  endif
  Cerrar('MXRENRQC')
 endif
 if continua
  if _ok .and. abrir('MXENCRQC',mxencrqc,'S',mxenrqx1) .and. dbseek(CodTemp) .and. rLock()
   REPLACE estatus WITH 'OK'
   UNLOCK
   Cerrar('MXENCRQC')
  endif
 endif
 Dbselectar(selant)
Return nil

Static Function Reimprim
 Local el_repor,el_tipo,SalvaP:=SaveScreen(0,0,24,79),SelAnt:=Select()
 Off_Key()
 if !Empty(g_libord)
    DOC_EMI(' ORDEN DE COMPRA ',G_LIBORD,'MXENCORD',MXENCORD->NUMORD)
    RestScreen(0,0,24,79,SalvaP)
    DbSelectar(selant)
 else
    Pare('No existen reportes definidos')
 endif
 On_Key()
Return nil


FUNCTION REV_REQUIS(CNRqc,cItem)
   LOCAL uSel:=Select(),cEst:='OK'
   IF ABRIR("MXENCRQC",MXENCRQC,[S],MXENRQX1)
      IF ABRIR("MXRENRQC",MXRENRQC,[S],MXRERQX1)
         IF DBSEEK(cNRqc)
            DO WHILE NUMRQC=cNRqc  .AND. !EOF()
               IF item=citem .AND. rLock()
                  REPLACE ENT_ORD WITH (ENT_ORD-MXRENORD->CANTIDAD),ESTATUS WITH 'PE'
                  UNLOCK
               ENDIF
               DBSKIP()
            ENDDO
            IF DBSEEK(cNRqc)
               cEst:='OK'
               DO WHILE NUMRQC=cNRqc  .AND. !EOF()
                  IF ENT_ORD>=CANTIDAD
                     cEst:='PE'
                     EXIT
                  ENDIF
                  DBSKIP()
               ENDDO
            ENDIF
            IF cEst='PE' .AND. MXENCRQC->(DBSEEK(cNRqc)) .AND. MXENCRQC->(rLock())
               MXENCRQC->ESTATUS := cEst
               UNLOCK
            ENDIF
         ENDIF
         CERRAR('MXRENRQC')
      ENDIF
      CERRAR('MXENCRQC')
   ENDIF
   DbSelectar(uSel)
RETURN NIL


Static Function MasDatosOrd()
   Local getlist:={},nLin:=3,nCol:=8,cScr1,uColor:=SetColor(),nCursor:=SetCursor()
   OFF_KEY()
   //SAVE SCREEN TO cScr
   BOXSOM([ OTROS DATOS PARA LA ORDEN DE COMPRA ],nLin,nCol,nLin+17,nCol+62)
   SetCursor(1)
   SET COLOR TO C_TIT3
   @ ++nLin,nCol+1 SAY '        FACTURAR A:' GET XF_CLIENTE PICT "@S40" VALID !EMPTY(XF_CLIENTE)
   @ ++nLin,nCol+1 SAY '       Nro. DE RIF:' GET XF_RIF                 VALID !EMPTY(XF_CLIENTE)
   @ ++nLin,nCol+1 SAY '       Nro. DE NIT:' GET XF_NIT                 VALID !EMPTY(XF_CLIENTE)
   @ ++nLin,nCol+1 SAY '         DIRECCION:' GET XF_DIR1
   @ ++nLin,nCol+1 SAY '                  :' GET XF_DIR2
   @ ++nLin,nCol+1 SAY '                  :' GET XF_DIR3
   @ ++nLin,nCol+1 SAY '                  :' GET XF_DIR4
   @ ++nLin,nCol+1 SAY '         TELEFONOS:' GET XF_TLF1
   @ ++nLin,nCol+1 SAY '                  :' GET XF_TLF2
   @ ++nLin,nCol+1 SAY '               FAX:' GET XF_FAX
   @ ++nLin,nCol+1 SAY '             EMAIL:' GET XF_EMAIL  PICT '@S40'
   @ ++nLin,nCol+1 SAY 'CONDICION DE ENVIO:' GET XF_CENVIO
   @ ++nLin,nCol+1 SAY '     MODO DE ENVIO:' GET XF_MENVIO
   @ ++nLin,nCol+1 SAY '  FECHA DE ENTREGA:' GET XENTREGA VALID VAL_XENTREGA() .AND. !EMPTY(XENTREGA)
   @ ++nLin,nCol+1 SAY ' TIEMPO DE ENTREGA:' GET XF_TENTREG
   @ ++nLin,nCol+1 SAY '     FORMA DE PAGO:' GET XF_FPAGO
   READ
   SetCursor(nCursor)
   //RESTORE SCREEN FROM cScr
Return nil

Static Function Val_xEntrega()
   xF_TENTREG:=PAD('TIEMPO DE ENTREGA A '+ALLTRIM(STR(XENTREGA-XEMISION,4))+' DIAS',40)
Return .T.

Static Function AbrirOrd()
   Local lAbrir:=.f.
   IF ABRIR([MXENCORD],MXENCORD,[S],MXENORX1,MXENORX2,MXENORX3)
   IF ABRIR([MXRENORD],MXRENORD,[S],MXREORX1,MXREORX2)
   IF ABRIR([MXCTAINV],MXCTAINV,[S],MXCTINX1)
   IF ABRIR([MXCTAPRO],MXCTACLI,[S],MXCTCLX1)
      DBSELECTAR("MXENCORD")
      DBGOBOTTOM()
      lAbrir:=.t.
   ENDIF
   ENDIF
   ENDIF
   ENDIF
Return(lAbrir)

