* EDICION DE COTIZACIONES
PROCEDURE VEN0306

PARAMETERS P1,P2,P3

** OCAI 29/1/18
*DECLARE HCAMPO[5],HSUBTIT[5],RCAMPO[8],RSUBTIT[8]
DECLARE HCAMPO[5],HSUBTIT[5],RCAMPO[8-1],RSUBTIT[8-1]

**
Private hay_sucursal:=.f.
PRIVATE LEMISION,LnumCOT
PRIVATE XNOMVEN,XCOMEN1,XCOMEN2,XTOT_FAC
PRIVATE BUFFER
PRIVATE XITEM,MITEM,XUNIDAD,XBULTO,XCANTIDAD,XPRECIO,XDESC,XTOT_REN,XDESCRIP, XIVA, XCODCON, xCODDPTO,XRECARGO:=''
PRIVATE ENTRO,II_LISTASI,XDESPACHO
PRIVATE XNUMCOT,XEMISION,XCLIENTE,XNOMCLI,XCODVEN
PRIVATE XTRANSP,XESTATUS,XENTREGA
PRIVATE BITEM,BDESCRIP,BNUMCOT,YFACTOR
PRIVATE NO_CONFOR,  UNIREF,DUPLI_DOC:=.F.,en_dup:=.f.
PRIVATE XDIREC1,XDIREC2,XDIREC3,XDIREC4,XCIF,XTLF1,XTLF2,XTARIFA:='A    ',XULT_COSTO:=0
private edita_inv:=.t.
PRIVATE  D_REFRES:=BLK('RF2Z()')
Private xcambio
PRIVATE PV_MUL:=RECUPVMUL()
PRIVATE XCODSUC:=SPACE(2)
PRIVATE ES_OFERTA:=.F.
PRIVATE XNUMRMA
PRIVATE LPRECIO:=0

PRIVATE cMasPrecCot:=if(G_FUERTE,'99999999.999',PIC12)

PRIVATE XTPCORR:='COT'
PRIVATE CFILNUM:=ce([MXNUMCOT.DBF])

PRIVATE BUFCON:=G_CAMINO+[BUFCAL]+estacion+[.DBF]
PRIVATE BUFCEX:=G_CAMINO+[BUFCEX]+estacion+[.DBF]
PRIVATE LISTACEX:=''
PRIVATE XEMAIL,XNIT,XFAX

private PVFILE:=CE([MXCOT.MTD])
private PV_ME   :='N'
private PV_EDN  := 'N'
private PV_PVP  := 'S'
private PV_PAUSA := 'N'
private PV_REP   :=SPACE(8)
private PV_IMP  := 'S'
private pv_codcon:='S'

IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF
II_LISTASI=[]

PRIVATE nTReg := 0

STORE .F. TO ENTRO
BITEM    = SPACE(15)
BDESCRIP = SPACE(50)
BNUMCOT  = SPACE(8)

NO_CONFOR = .T.
BUFFER=G_CAMINO+[BUFCOT]+ESTACION+[.DBF]
DO CAMCOT
DO SUBCOT
DO CAMRCZ
DO SUBRCZ

MENSAJ(MEN3)
IF ABRIR([MXENCCOT],G_CAMINO+[MXENCCOT.DBF],[S],G_CAMINO+[MXENCZX1.NTX],G_CAMINO+[MXENCZX2.NTX],G_CAMINO+[MXENCZX3.NTX])
 IF ABRIR([MXRENCOT],G_CAMINO+[MXRENCOT.DBF],[S],G_CAMINO+[MXRECZX1.NTX],G_CAMINO+[MXRECZX2.NTX])
  IF ABRIR([MXCTAINV],MXCTAINV,[S],MXCTINX1,MXCTINX5)
    IF inv_filpr='S'.OR.inv_prdes='S'
      FiltrarInv()
   ENDIF
   IF ABRIR([MXCTACLI],MXCTACLI,[S],MXCTCLX1)
    if es_cob
       filtrarcli()
    endif
    IF ABRIR([MXCTAVDD],MXCTAVDD,[S],MXCTVDX1)
     SELECT MXENCCOT
     GO BOTTOM
     INIT_VER()
     SET KEY -7 TO DUP_COT
     SET KEY 279 TO REIMPRIM
     CARGACORR(XTPCORR)  // NUEVA FUNCION PARA LA CARGA DE LOS CORRELATIVOS DEL DOC.. 11.3.2005

** ocai 21/3/21
_elusu:=upper(_cualvdd())
_sivdd:=at('VD:',_elusu)#0
_elvdd:=IF(_sivdd,SUBS(_ELUSU,AT('VD:',_ELUSU)+3),'')

if !empty(_elvdd)
 _elvdd:=ltrim(_elvdd)
 _elvdd:=pad(_elvdd,5)
endif

if _sivdd.and.!empty(_elvdd)
 mxenccot->(dbsetfilter(blk("codven=_elvdd"),"codven=_elvdd"))
 mxenccot->(dbgobottom())
endif

     EDC([COT],[ COTIZACIONES ],HSUBTIT,HCAMPO,.F.,.F.,.T.,.F.,.F.,.T.,.T.,.F.,'F8=Duplicar Alt+I=Reimprimir')
     SET KEY -7 TO
     SET KEY 279 TO
    ENDIF
   ENDIF
  ENDIF
 ENDIF
ENDIF
DELEBUFF()
RETURN

FUNCTION VMODCOT

IF ESTATUS='OK'
 PARE([COTIZACION CON ESTATUS "OK" NO PUEDE SER ALTERADA])
 RETURN .F.
ENDIF

IF PV_ME='N'
 RETURN CHK_PAS(1)
ENDIF
RETURN(!CHK_BLOCK(EMISION))

FUNCTION VELICOT
IF PV_ME='N'
 RETURN CHK_PAS(1)
ENDIF
RETURN(!CHK_BLOCK(EMISION))

PROCEDURE SUBCOT
HSUBTIT[ 1]=[N£mero  ]
HSUBTIT[ 2]=[Emisi¢n ]
HSUBTIT[ 3]=[Cliente ]
HSUBTIT[ 4]=[Estatus]
HSUBTIT[ 5]=[        Total]
RETURN

PROCEDURE CAMCOT
HCAMPO[ 1]=[NUMCOT]
HCAMPO[ 2]=[EMISION]
HCAMPO[ 3]=[CLIENTE]
HCAMPO[ 4]=[ESTATUS]
HCAMPO[ 5]=[TOT_COT]
RETURN

PROCEDURE DEFCOT
* VALORES POR DEFECTO
ENTRO=.F.
XIVA=' '
//XNUMCOT    = IF(PV_EDN='S',PROXINUM(G_CAMINO+[MXNUMCOT.DBF]),'')
XNUMCOT    = IF(PV_EDN='S',PROXINUM(CFILNUM),'')
XNUMRMA    = SPACE(8)
XEMISION   = CTOD(G_FECHA)
XCLIENTE   = SPACE(  8)
XCODSUC    = SPACE(2)
XNOMCLI    = SPACE( 60)
XCODVEN    = SPACE(  5)
XEMAIL := SPACE(50)
XNIT   := SPACE(15)
XFAX   := SPACE(15)
STORE SPACE(25) TO XDIREC1,XDIREC2,XDIREC3,XDIREC4
STORE SPACE(15) TO XTLF1,XTLF2,XCIF
STORE SPACE(35) TO XCOMEN1,XCOMEN2

XTRANSP=SPACE(10)
XESTATUS='PE'
XENTREGA=XEMISION
STORE 0 TO XTOT_FAC
XCAMBIO := CambioMon(xemision,StrTran(Time(),':'))

@  4,25  SAY SPACE(35)
@  4,74  SAY SPACE(2)
@  5,25  SAY SPACE(35)

@ maxrow()-5,15  SAY SPACE(35)

IF !DUPLI_DOC
 CRDRCZ()
 @ TOPE,COLIZQ CLEAR TO FONDO,COLDER
 CRDCOT()
 FP_TOTAL()
 SELECT MXRENCOT
 COPY STRUCTURE TO (BUFFER)
ELSE
 XCLIENTE   = MXENCCOT->CLIENTE
 XCODVEN    = MXENCCOT->CODVEN
 XCOMEN1    = MXENCCOT->COMEN1
 XCOMEN2    = MXENCCOT->COMEN2
ENDIF

SELECT MXENCCOT
RETURN

PROCEDURE ASGCOT
* ASIGNACION DE REGISTRO A MEMORIA

STORE NUMCOT TO XNUMCOT,LNUMCOT
STORE EMISION TO XEMISION,LEMISION
STORE CLIENTE TO XCLIENTE,LCLIENTE
XCODSUC    = CODSUC
XCODVEN    = CODVEN
XCOMEN1    = COMEN1
XCOMEN2    = COMEN2
XTRANSP     = TRANSP
XESTATUS   = ESTATUS
XENTREGA   = ENTREGA
IF cliente='CONTADO'
   XNOMCLI := NOMCLI
   XDIREC1 := DIREC1
   XDIREC2 := DIREC2
   XDIREC3 := DIREC3
   XDIREC4 := DIREC4
   XTLF1   := TLF1
   XTLF2   := TLF2
   XFAX      = FAX
   XNIT      = NIT
   XCIF      = CIF
   XEMAIL    = EMAIL
ELSE
   XNOMCLI := MXCTACLI->NOMCLI
   XDIREC1 := MXCTACLI->DIREC1
   XDIREC2 := MXCTACLI->DIREC2
   XDIREC3 := MXCTACLI->DIREC3
   XDIREC4 := MXCTACLI->DIREC4
   XTLF1   := MXCTACLI->TLF1
   XTLF2   := MXCTACLI->TLF2
   XFAX      = MXCTACLI->FAX
   XNIT      = MXCTACLI->NIT
   XCIF      = MXCTACLI->CIF
   XEMAIL    = MXCTACLI->EMAIL
ENDIF

XTOT_FAC   = TOT_COT
XCAMBIO    = CAMBIO

XNUMRMA    = NUMRMA

SELECT MXRENCOT
SEEK(XNUMCOT)
COPY TO (BUFFER) WHILE NUMCOT = XNUMCOT
SELECT MXENCCOT
RETURN

PROCEDURE RPLCOT
* REEMPLAZO EN EL REGISTRO

IF MODIFICA
 GEN_COT(1)
 REV_CEX(LNUMCOT)
ENDIF

SELECT MXENCCOT

DO MENSAJ WITH [Grabando Cotizaci¢n ...]
AUDITORIA([Acceso a mantenimiento de cotizaciones],xnumcot,numcot,IF(MODIFICA,[MOD],[INC]))
REPLACE NUMCOT WITH XNUMCOT,EMISION  WITH XEMISION,CLIENTE WITH XCLIENTE
REPLACE CODSUC WITH XCODSUC
REPLACE CODVEN WITH XCODVEN
REPLACE COMEN1 WITH XCOMEN1,COMEN2 WITH XCOMEN2, TRANSP WITH XTRANSP, ESTATUS WITH XESTATUS
REPLACE ENTREGA WITH XENTREGA
REPLACE TOT_COT WITH XTOT_fac
REPLACE NUMRMA WITH XNUMRMA
REPLACE NOMCLI    WITH xNOMCLI
REPLACE CIF       WITH xCIF
REPLACE NIT       WITH xNIT
REPLACE DIREC1    WITH xDIREC1
REPLACE DIREC2    WITH xDIREC2
REPLACE DIREC3    WITH xDIREC3
REPLACE DIREC4    WITH xDIREC4
REPLACE TLF1      WITH xTLF1
REPLACE TLF2      WITH xTLF2
REPLACE FAX       WITH xFAX
REPLACE EMAIL     WITH xEMAIL

GMoneda(xcambio)

SELECT MXRENCOT
DO WHILE !FLOCK()
ENDDO

IF MODIFICA
 SEEK(LNUMCOT)
 DELETE WHILE NUMCOT = LNUMCOT
ENDIF
APPEND FROM (BUFFER)
UNLOCK
GEN_COT()

ActCex()
listacex:=''
LIMPIA()
DELETE FILE (BUFFER)
// Codigo nuevo para el rma por Ruben Sanchez
if r_m_m_a .and. !empty(xnumrma)
   if abrir('RMARECEP',rmarecep,'C',rmarecx1,rmarecx2)
      if dbseek(xnumrma) .and. rlock()
         RMARECEP->numcot:=xnumcot
         dbunlock()
      endif
      cerrar('RMARECEP')
   endif
endif
//

SELECT MXENCCOT
*DOC_EMI(' COTIZACIONES ',G_LIBCOT,'MXENCCOT',XNUMCOT)

******************
IF PV_IMP='S'
 IF PV_PAUSA='N'
  KEYBOARD SALIDA
 ENDIF
 IF EMPTY(PV_REP)
  DOC_EMI(' COTIZACIONES ',G_LIBCOT,'MXENCCOT',XNUMCOT)
 ELSE
  PRE_EMI(TRIM(PV_REP)+'.INF',' COTIZACION ','MXENCCOT',XNUMCOT)
 ENDIF
ENDIF

******************

CLEAR TYPEAHEAD

PANCOT()

** 21/3/21
if _sivdd.and.!empty(_elvdd)
 mxenccot->(dbsetfilter(blk("codven=_elvdd"),"codven=_elvdd"))
 mxenccot->(dbgobottom())
endif

RETURN

PROCEDURE MASCOT
* MASCARA DE LA VENTANA

SETCOLOR(C_BOX2)
BOXTIT(titulo,2,0,maxrow()-2,79)
L78(8)
L78(10)
L78(maxrow()-6)
L78(maxrow()-4)
SETCOLOR(C_TEX2)
@  3, 2 SAY [N£mero.....:          Fecha:            Entrega:            Tasa US$:      ]
@  4, 2 SAY [Cliente....:                                                Sucursal:      ]
@  5, 2 SAY [Vendedor...:                                                 Estatus:      ]
@  6, 2 SAY [Comentarios:]
@  6,63 SAY [ Tarifa:]
if r_m_m_a
   @  7,57 SAY [N£mero de RMA:        ]
endif
@ maxrow()-5, 1 SAY [Nombre:                                                 Total:]
MASRCZ()
RETURN

PROCEDURE GETCOT
* CAPTURA REGISTRO
private ed_renglon

SET KEY -7 TO
SET KEY 279 TO

DO WHILE .T.
 *KEYBOARD SALIDA+SALIDA

 IF !DUPLI_DOC
  KEYBOARD SALIDA+IF(PV_EDN='S',SALIDA,'')
 ENDIF

 ED_RENGLON:=.F.
 IF PV_EDN='N'.AND.!MODIFICA
  @ 3,15 SAY SPACE(8)
 ENDIF

 IF vtas_mdcli='N'
    SET KEY -9 TO TOMA_DATO1()
    MENSAJ('F10 = Mas datos del '+IF(GESTION='VEN','Cliente','Proveedor'))
 ENDIF

 @  3,15 GET XNUMCOT  PICT [@!] WHEN PV_EDN='S' VALID CHK_CLAVE(XNUMCOT,'Cotizaci¢n')
 @  3,31 GET XEMISION VALID CHKEJER(XEMISION)  .AND. !CHK_BLOCK(XEMISION)
 @  3,51 GET XENTREGA valid xentrega>=xemision
 @  3,72 GET XCAMBIO PICT '9999.99' WHEN vtas_edcamb='S' VALID !Empty(xcambio)
 @  4,15 GET XCLIENTE VALID IF(XCLIENTE='CONTADO',.T.,CHCLI(@XCLIENTE))
 @  4,25 GET XNOMCLI VALID TOMA_DATO1(vtas_mdcli) PICT "@S35"
 @  4,72 GET Xcodsuc  WHEN hay_sucursal VALID BSucursal(@xcodsuc,5,50)
 @  5,15 GET XCODVEN VALID CHVEN(XCODVEN,5,25)
 @  5,71 SAY XESTATUS
 @  6,15 GET XCOMEN1
 @  7,15 GET XCOMEN2
 @  6,71 GET XTARIFA
 if r_m_m_a
    @  7,71 GET XNUMRMA VALID valrma(@xnumrma)
 endif

 LEER()
 IF ESCAPAR()
  IF ENTRO
   DO BELL
   IF DECIS([¨ Desea cancelar la operaci¢n ?],1)=[S]
    SALIR=.T.
   ELSE
    LOOP
   ENDIF
  ELSE
   SALIR=.T.
  ENDIF
 ENDIF

 IF SALIR
  SET KEY -7 TO DUP_COT
  IF PV_EDN='S'
   //CANCNUM(G_CAMINO+[MXNUMCOT.DBF],XNUMCOT)
   CANCNUM(CFILNUM,XNUMCOT)
  ENDIF
  DUPLI_DOC:=.F.
  SET KEY 279 TO REIMPRIM
  RETURN
 ENDIF

 IF EMPTY(XCLIENTE)
  KEYBOARD REPLICATE(SALIDA,2)
  LOOP
 ENDIF

 @  6,71 SAY XTARIFA

 ENTRO=.T.

 IF ABRIR([MXCTASER],MXCTASER,[S],MXCTSEX1)
 IF ABRIR([BUFFER],BUFFER,[E])
  IF DUPLI_DOC
   REPLACE ALL DESPACHO WITH 0, DESBULTO WITH 0,ESTATUS WITH 'PE'
   DUPLI_DOC:=.F.
  ENDIF

  GO TOP
  RECACOT()
  GO TOP

  CLEAR TYPEAHEAD
  FP_TOTAL()
  SETCOLOR(C_TIT2)
  ed_renglon=.t.

  SET KEY -7 TO SER0000
  SET KEY -9 TO COTEXPR
  ustre:='USTREVEN'+Alltrim(Str(subopcion))
  ClvT:=&ustre
  &ustre.:='SSS'
  nTReg := LastRec()
  EDC([RCZ],[],RSUBTIT,RCAMPO,.T.,.T.,.T.,.F.,.F.,.F.,.F.,.F.,'<ENTER>=INVENTARIO  F8=SERVICIOS   F10=COT.EXPRESS  <ESC>=GRABAR',.T.)
  &ustre.:=ClvT

  SET KEY -7 TO
  SET KEY -9 TO

  @ maxrow()-5,8 SAY SPACE(35)

  nTReg:=LastRec()
  IF nTReg>0 .AND. pv_rengpag > 0 .AND. nTReg > pv_rengpag
     Alerta('SE INCLUYERON MAS RENGLONES QUE EL LIMITE POR RENGLONES ACEPTADO...')
     Alerta('LIMITE = '+ALLTRIM(STR(pv_rengpag,2))+' REGISTRO(S), DEBE BORRAR='+alltrim(str(ABS(PV_RENGPAG-nTReg))))
     LOOP
  ENDIF

  SELECT BUFFER
  GO TOP
  IF LASTREC()=0 .OR. EOF()
   SELECT MXENCCOT
   PANCOT()
   CONFORME='N'
  ELSE
   CONFORME=CONFIRMA()
  ENDIF

  DO CASE
   CASE CONFORME='S'
   IF PV_EDN='N'.AND.!MODIFICA
      //XNUMCOT:=PROXINUM(G_CAMINO+[MXNUMCOT.DBF])
      XNUMCOT:=PROXINUM(CFILNUM)
    ENDIF
    SELECT MXENCCOT
    XNUMCOT:=NEW_NUM(XNUMCOT)
    SELECT BUFFER
    XESTATUS:=ESTAT_COT()
    REPLACE ALL  NUMCOT WITH XNUMCOT , EMISION WITH XEMISION, cliente with xcliente,codsuc with xcodsuc
   CASE CONFORME='C'
    IF PV_EDN='S'
     //CANCNUM(G_CAMINO+[MXNUMCOT.DBF],XNUMCOT)
     CANCNUM(CFILNUM,XNUMCOT)
    ENDIF
  ENDCASE
  CERRAR([BUFFER])
 ENDIF
 CERRAR('MXCTASER')
 ENDIF

EXIT
ENDDO
SELECT MXENCCOT
SET KEY -7 TO DUP_COT
SET KEY 279 TO REIMPRIM
RETURN

PROCEDURE DETCOT
private xemision
IF !EOF()
 SETCOLOR(C_TIT2)

 @  3,15 SAY NUMCOT
 @  3,31 SAY EMISION
 @  3,51 say entrega
 @  3,72 SAY CAMBIO PICT '9999.99'
 @  4,15 SAY CLIENTE
 IF CLIENTE='CONTADO'
    @  4,25 SAY PAD(NOMCLI,35)
 ELSE
    @  4,25 SAY PAD(NOMCOD(CLIENTE,[MXCTACLI],[NOMCLI]),35)
 ENDIF
 @  4,72 SAY codsuc
 @  5,15 SAY CODVEN
 TEMP=NOMCOD(CODVEN,[MXCTAVDD],[NOMVEN])
 @  5,25 SAY IF(TEMP=[****],SPACE(35),TEMP)
 @  5,71 SAY estatus
 @  6,15 SAY COMEN1
 @  7,15 SAY COMEN2
 if r_m_m_a
    @  7,71 SAY NUMRMA
 else
    @  7,71 SAY SPACE(8)
 endif
 XTOT_FAC=TOT_COT
 xemision:=emision
 DO FP_TOTAL
 WNUMCOT=NUMCOT
 BORDE=[ NUMCOT=WNUMCOT ]

 SELECT MXRENCOT
 SEEK(WNUMCOT)
 DO CRDRCZ
 APL=[RCZ]
 DO PAGINA
 APL=[COT]
 DO CRDCOT
 SELECT MXENCCOT
ENDIF
RETURN

PROCEDURE ELICOT
* ELIMINACION DE COTIZACION
PRIVATE WNUMCOT,MODIFICA,XNUMCOT
MODIFICA=.T.
SELECT MXENCCOT
WNUMCOT=NUMCOT
XNUMCOT=NUMCOT
GEN_COT(1)
REV_CEX(WNUMCOT)
SELECT MXENCCOT
AUDITORIA([Acceso a mantenimiento de cotizaciones],numcot,,[ELI])
DELETE
SELECT MXRENCOT
SEEK(WNUMCOT)
IF FOUND()
 DO WHILE !FLOCK()
 ENDDO
 DELETE WHILE NUMCOT = WNUMCOT
 UNLOCK
ENDIF
SELECT MXENCCOT
RETURN

PROCEDURE CRDCOT
* COORDENADAS DE LA VENTANA
TOPE= 3
FONDO=maxrow()-3
COLIZQ= 1
COLDER=78
CONTLIN=TOPE
RETURN

PROCEDURE BUSCOT
PRIVATE TNUMCOT
BUS([UBUSCOT])
RETURN

PROCEDURE UBUSCOT

@  3,15 GET BNUMCOT PICT [@!]
LEER()
SALIR=ESCAPAR()
IF !SALIR
 TNUMCOT=TRIM(BNUMCOT)
 YKEY=TNUMCOT
ENDIF
RETURN


FUNCTION CHKCZitem
LOCAL PRICHAR

IF LASTKEY()=5
 RETURN .T.
ENDIF

UNIREF = []
CHKU   = .F.

PRICHAR=SUBSTR(XITEM,1,1)

DO CASE
 CASE PRICHAR$[*]
  RETURN .T.
 CASE PRICHAR$[\/]
  SELE MXCTASER
  SEEK(SUBSTR(XITEM,2,5))
  IF !FOUND()
   SER0000()
   SELE BUFFER
   RETURN .F.
  ELSE
   XDESCRIP=PAD(MXCTASER->NOMSER,50)
   XPRECIO=MXCTASER->PRESER
   LPRECIO=MXCTASER->PRESER
   XIVA   =MXCTASER->IVA
   IF !MODIFICA
      XCODCON:=REFCON
   ENDIF
   CHKU=.F.
   IF vtas_pbul='S'
      KEYBOARD REPLICATE(SALIDA,2)
   ELSE
      KEYBOARD REPLICATE(SALIDA,3)
   ENDIF
  ENDIF
  SELE BUFFER
OTHERWISE
  SELECT MXCTAINV
  SET ORDER TO 1
  LO_ENCONTRO:=.F.
  IF EMPTY(XITEM)
    SELINFINV()
    IF LASTKEY()=27
       KEYBOARD SALIDA
       WAIT
       SELECT BUFFER
       RETURN .T.
    ENDIF
     SELECT BUFFER
     RETURN .F.
  ENDIF
  IF DBSEEK(XITEM)
     LO_ENCONTRO:=.T.
  ELSE
     // LO_ENCONTRO:=.F.
     LO_ENCONTRO:=CHEQ_CODEXT()
  ENDIF
  IF LO_ENCONTRO
    CHKU=.T.
    XITEM:=CODART
    STORE UNIDAD TO XUNIDAD,UNIREF
    XULT_COSTO = MXCTAINV->ULT_COSTO
    XDESCRIP   = PAD(MXCTAINV->NOMART,50)
    XIVA       = MXCTAINV->IVA
    IF !MODIFICA
       XCODCON:=CODCON
    ENDIF
    IF !MODIFICA
      GET_TARIFZ()
      LPRECIO:=XPRECIO
      XCANTIDAD  = IF(MXCTAINV->CAN_BUL>0,MXCTAINV->CAN_BUL,1)
    ELSEIF MODIFICA .AND. MITEM <> XITEM
      GET_TARIFZ()
      LPRECIO:=XPRECIO
    ENDIF
  ELSE
     SET ORDER TO 2
     IF DBSEEK(XITEM)
        LO_ENCONTRO:=.T.
     ELSE
        LO_ENCONTRO:=.F.
     ENDIF
  ENDIF
  SET ORDER TO 1
  IF LO_ENCONTRO
    CHKU=.T.
    XITEM:=CODART
    STORE UNIDAD TO XUNIDAD,UNIREF
    XULT_COSTO = MXCTAINV->ULT_COSTO
    XDESCRIP   = PAD(MXCTAINV->NOMART,50)
    XIVA       = MXCTAINV->IVA
    IF !MODIFICA
       XCODCON:=CODCON
    ENDIF
    IF !MODIFICA
      GET_TARIFZ()
      LPRECIO:=XPRECIO
      XCANTIDAD  = IF(MXCTAINV->CAN_BUL>0,MXCTAINV->CAN_BUL,1)
    ELSEIF MODIFICA .AND. MITEM <> XITEM
      GET_TARIFZ()
      LPRECIO:=XPRECIO
    ENDIF
  ENDIF
  IF !LO_ENCONTRO
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
     SELECT BUFFER
     IF vtas_pbul='S'
        KEYBOARD REPLICATE(SALIDA,2)
     ELSE
        KEYBOARD REPLICATE(SALIDA,3)
     ENDIF
  ENDIF
ENDCASE

SELECT BUFFER

RETURN .T.

******************************************************************

PROCEDURE PANCOT
LOCAL MMENUSCR
MMENUSCR=SAVESCREEN(maxrow()-4,0,maxrow(),79)
INIT_VER()
MASCOT()
RESTSCREEN(maxrow()-4,0,maxrow(),79,MMENUSCR)
RETURN

FUNCTION GEN_COT
PARAMETER REVERSO
PRIVATE XCANTIDAD,XITEM,XESTATUS,XDESPACHO

REVERSO= PCOUNT()=1

SELECT MXRENCOT
SEEK(XNUMCOT)
WHILE !EOF() .AND. NUMCOT=XNUMCOT
 XITEM=ITEM
 SELECT MXCTAINV
 SEEK(XITEM)
 IF FOUND()
  MENSAJ([Actualizando COTIZACIONES ...])
  SELECT MXRENCOT
  XCANTIDAD = CAMBUL(CANTIDAD,BULTO)
  xdespacho = DESPACHO*IF(DESBULTO>0,DESBULTO,1)
  XESTATUS  = ESTATUS
  SELECT MXCTAINV
  IF XESTATUS="PE"
   REC_LOCK(0)
   IF REVERSO
     REPLACE COTIZA WITH COTIZA-(XCANTIDAD-XDESPACHO)
   ELSE
     REPLACE COTIZA WITH COTIZA+(XCANTIDAD-XDESPACHO)
   ENDIF
   UNLOCK
  ENDIF
 ENDIF
 SELECT MXRENCOT
 SKIP
ENDDO
RETURN 0

******************** RENGLONES DE LA FACTURA *******************************
PROCEDURE SUBRCZ
RSUBTIT[ 1]=PAD([C¢digo],15)
RSUBTIT[ 2]=[Und]
RSUBTIT[ 3]=[I  Bulto]
RSUBTIT[ 4]=[Cantidad/Bulto]
RSUBTIT[ 5]=[      Precio]

** OCAI 29/1/18
** RSUBTIT[ 6]=[ %Dcto]
** RSUBTIT[ 7]=[       Total]
   RSUBTIT[ 6]=[            Total  ]
** RSUBTIT[ 8]=[]
   RSUBTIT[ 7]=[]

RETURN

PROCEDURE CAMRCZ
RCAMPO[ 1]=[ITEM]
RCAMPO[ 2]=[UNIDAD]
RCAMPO[ 3]=[BULTO]
RCAMPO[ 4]=[CANTIDAD]
RCAMPO[ 5]=[PRECIO]

** OCAI 29/1/18
**RCAMPO[ 6]=[DESC]
**RCAMPO[ 7]=[TOT_REN]
**RCAMPO[ 8]=[DESCRIP]
RCAMPO[ 6]=[TOT_REN]
RCAMPO[ 7]=[DESCRIP]

RETURN

PROCEDURE DEFRCZ
* VALORES POR DEFECTO
XITEM      = SPACE( lon_item)
XUNIDAD    = SPACE(  3)
XIVA       = SPACE(1)
STORE 1 TO XBULTO,XCANTIDAD
STORE 0 TO XPRECIO,XDESC,XTOT_REN, XULT_COSTO
XDESCRIP   = SPACE(50)
XDESPACHO=0
xcodcon:=Space(20)
xcoddpto:=Space(10)
@ MAXROW()-5,44 SAY space(12)
RETURN

PROCEDURE ASGRCZ
* ASIGNACION DE REGISTRO A MEMORIA

XITEM      = ITEM
MITEM      = ITEM
XUNIDAD    = UNIDAD
XBULTO     = BULTO
XCANTIDAD  = CANTIDAD
XPRECIO    = E_IMPORTE(PRECIO,XEMISION)
XDESC      = DESC
XTOT_REN   = TOT_REN
XDESCRIP   = DESCRIP
XIVA       = IVA
XDESPACHO  = DESPACHO
xcodcon:=codcon
xcoddpto:=coddpto
RETURN

PROCEDURE RPLRCZ
* REEMPLAZO EN EL REGISTRO
IF MODIFICA
 FP_RESTA()
ENDIF
REPLACE ITEM WITH XITEM,UNIDAD WITH XUNIDAD,BULTO WITH XBULTO,CANTIDAD WITH XCANTIDAD
REPLACE PRECIO WITH XPRECIO, DESC WITH XDESC,TOT_REN WITH XTOT_REN,DESCRIP WITH XDESCRIP
REPLACE IVA WITH XIVA,codcon WITH xcodcon,coddpto WITH xcoddpto
REPLACE ESTATUS WITH IF(DESPACHO=CANTIDAD,'OK','PE'),OFERTA WITH ES_OFERTA,;
        CODVEN WITH XCODVEN,NUMRMA WITH XNUMRMA
FP_SUMA()
FP_TOTAL()
RETURN

PROCEDURE MASRCZ
* MASCARA DE LA VENTANA
SETCOLOR(C_INV2)
@  9,1 SAY HAZ_SUB(RSUBTIT)
SETCOLOR(C_BOX2)
L78(10)
L78(maxrow()-6)
L78(maxrow()-4)
SETCOLOR(C_TEX2)
RETURN


PROCEDURE GETRCZ
* CAPTURA REGISTRO
MENSAJ([<ENTER>=INVENTARIO  F8=SERVICIOS <ESC>=SALIR],MAXROW())
ES_D=.F.
YFACTOR=1
CHKU=.F.
IF !MODIFICA .AND. vtas_serv='S'
   xITEM:=Pad('\',15)
ENDIF
nTreg:=Lastrec()
IF !MODIFICA .AND. nTReg>0 .AND. pv_rengpag > 0 .AND. nTReg >= pv_rengpag
   Alerta('SE HA REBASADO EL LIMITE DE RENGLONES POR PAGINA, NO PUEDE CONTINUAR INCLUYENDO REGISTROS...')
   salir:=.t.
   RETURN salir
ENDIF
@ CONTLIN, 2 GET XITEM PICT ITEM_MASK VALID CHKCZITEM() .AND. BOfertas({xitem,xcliente,xcodsuc,xemision},@XPRECIO)
@ MAXROW()-5,8 GET XDESCRIP PICT '@S35'
@ CONTLIN,18 GET XUNIDAD WHEN W1()   PICT [!!!] VALID SEL_UNI(UNIREF,@XUNIDAD,CHKU)
@ CONTLIN,22 GET XIVA    WHEN W1()   PICT '!' VALID CHKIVA(XIVA)
@ CONTLIN,25 GET XBULTO    WHEN W1()  PICT "99999" VALID XBULTO>0
@ CONTLIN,31 GET XCANTIDAD WHEN W1()  PICT "9999999999.999" VALID XCANTIDAD>=XDESPACHO.AND.XCANTIDAD>0.AND.IF(!MODIFICA,TOMAR_TARIFA(XITEM,XTARIFA,CAMBUL(XCANTIDAD,XBULTO)*YFACTOR),.t.)
@ CONTLIN,46 GET XPRECIO   WHEN W1().AND.PV_PVP='S'  PICT cMasPrecCot VALID VALPRE()

** 29/1/18 OCAI
* CONTLIN,59 GET XDESC     WHEN W1()  PICT "999.99"  VALID XDESC>=0 .AND. XDESC<=100.00


READ

** 29/1/18 OCAI
**SALIR=ESCAPAR().or.ABS(cambul(xcantidad,xbulto)*Xprecio*YFACTOR)>=VAL(cMasPrecCot)
SALIR=ESCAPAR().or.ABS(cambul(xcantidad,xbulto)*Xprecio*YFACTOR)>=VAL(PIC15)


FUTREN()
IF !SALIR .AND. !EMPTY(XITEM) .AND. PV_CODCON='S'
   CUENTACON()
ENDIF

nTReg:=LastRec()
IF !MODIFICA .AND. nTReg>0 .AND. pv_rengpag > 0 .AND. nTReg >= pv_rengpag
   Alerta('SE HA REBASADO EL LIMITE DE RENGLONES POR PAGINA, NO PUEDE CONTINUAR INCLUYENDO REGISTROS...')
   salir:=.t.
   RETURN salir
ENDIF

RETURN

STATIC PROC FUTREN
IF !SALIR
  IF !W1()
   STORE 0 TO XBULTO,XCANTIDAD,XTOT_REN,XDESC
   XIVA=' '
  ELSE
   IF CHKU
    XUNIDAD=UNIREF
    XCANTIDAD=XCANTIDAD*YFACTOR
   ENDIF
   XPRECIO:=CAMBIO_IMPORTE(XPRECIO,XEMISION)
   XTOT_REN=ROUND( CAMBUL(XCANTIDAD,XBULTO)*XPRECIO*(1-(XDESC/100.00)),3 )
*   @ CONTLIN,66 SAY XTOT_REN  PICT PIC12
  ENDIF
ENDIF
RETURN

STATIC FUNC W1
RETURN (!EMPTY(XITEM))

PROCEDURE DETRCZ
LOCAL UCOLOR
* DETALLE
IF !EMPTY(ITEM)
 @ CONTLIN, 2 SAY substr(ITEM,1,15)
 @ CONTLIN,18 SAY UNIDAD
 @ CONTLIN,22 SAY IVA
 @ CONTLIN,24 SAY if(oferta,'*',' ')
 @ CONTLIN,25 SAY BULTO
 @ CONTLIN,31 SAY CANTIDAD
 @ CONTLIN,46 SAY E_IMPORTE(PRECIO,xemision) PICT cMasPrecCot


** OCAI 29/1/18
*@ CONTLIN,59 SAY DESC
*@ CONTLIN,66 SAY E_IMPORTE(TOT_REN,xemision) PICT cMasPrecCot
 @ CONTLIN,63 SAY E_IMPORTE(TOT_REN,xemision) PICT PIC15


ELSE
 @ CONTLIN,2 SAY PAD('*COMENTARIO*',16)
 @ CONTLIN,18 SAY space(60)
ENDIF
IF ALIAS()='BUFFER'
 @ maxrow()-5,8 SAY PAD(DESCRIP,35)
 ucolor=setcolor()
 setcolor(c_TIT2)

**OCAI 29/1/18
*@ MAXROW()-5,44 SAY "D="+STR(DESPACHO,10,3)
 @ MAXROW()-5,44 SAY "D="+STR(DESPACHO,10,2)

 setcolor(ucolor)
ENDIF
RETURN


PROCEDURE GETBRCZ
* CAPTURA DE CAMPOS EN MODO BROWSE
PRIVATE FFF,CCC
YFACTOR=1
FFF=ROW()
CCC=COL()
CHKU=.F.
DO CASE
CASE ICAMPO=1
 @ FFF,CCC GET XITEM VALID CHKCZITEM()
CASE ICAMPO=2
 @ FFF,CCC GET XUNIDAD WHEN W1() VALID SEL_UNI(UNIREF,@XUNIDAD,CHKU)
CASE ICAMPO=3
 @ FFF,CCC GET XBULTO  WHEN W1()   PICT "99999" VALID XBULTO>0
CASE ICAMPO=4
 @ FFF,CCC GET XCANTIDAD WHEN W1() PICT "9999999999.999"
CASE ICAMPO=5
 @ FFF,CCC GET XPRECIO  WHEN W1()  PICT cMasPrecCot VALID XPRECIO>0 .AND. CAMBUL(XCANTIDAD,XBULTO)*XPRECIO<89999999.99

** OCAI 29/1/18
*CASE ICAMPO=6
* @ FFF,CCC GET XDESC  WHEN W1()    PICT "999.99"  VALID XDESC>=0 .AND. XDESC<=100.00
*CASE ICAMPO=8
CASE ICAMPO=7

 @ FFF,CCC GET XDESCRIP PICT '@S35'
OTHERWISE
 SALIR=.T.
 RETURN
ENDCASE
READ
SALIR=ESCAPAR(1)
FUTREN()
RETURN


FUNCTION VMODRCZ
PRIVATE devo:=.t.
IF !EMPTY(NUM_CEX)
 PARE([ATENCION: Rengl¢n proviene de la COT. EXPRESS No. ]+AllTrim(NUM_CEX))
ENDIF
if substr(item,1,2)='*D'
 Pare('Este renglon es un descuento, no puede ser modificado')
 devo:=.f.
endif
RETURN devo

FUNCTION VELIRCZ
 IF !EMPTY(NUM_CEX)
  PARE([ATENCION: Rengl¢n proviene de la COT. EXPRESS No. ]+AllTrim(NUM_CEX))
 ENDIF
RETURN .T.

PROCEDURE ELIRCZ
* ELIMINACION DE REGISTRO
UREG=RECNO()
DELETE
PACK
RECACOT()
RETURN

PROCEDURE CRDRCZ
* COORDENADAS DE LA VENTANA
TOPE = 11
FONDO =maxrow()-7
COLIZQ = 1
COLDER =78
CONTLIN=TOPE
RETURN

PROCEDURE BUSRCZ
* BUSQUEDA EN RENGLONES DE CONT.
PRIVATE TITEM,TDESCRIP
VERTBUS([UBUSRCZ])
RETURN

PROCEDURE UBUSRCZ
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
 XTOT_FAC= XTOT_FAC + TOT_REN
RETURN

static PROCEDURE FP_RESTA
 XTOT_FAC= XTOT_FAC - TOT_REN
RETURN

static PROCEDURE FP_TOTAL
* DISPLAY DE TOTALES
SETCOLOR(C_TIT2)

** OCAI 29/1/18
*@ maxrow()-5,66 SAY e_importe(XTOT_FAC,xemision) PICT PIC12
@ maxrow()-5,63 SAY e_importe(XTOT_FAC,xemision) PICT PIC15

SETCOLOR(C_TEX2)
RETURN

static FUNCTION RECACOT
LOCAL R_REG
R_REG=RECNO()
GO TOP
xtot_fac=0
DO WHILE !EOF()
 FP_SUMA()
 SKIP
ENDDO
GOTO R_REG
FP_TOTAL()
RETURN 0

static PROCEDURE DUP_COT
PARAMETER P1,P2,P3
IF en_dup
 RETURN
ENDIF
en_dup:=.t.
DUP_DOC('MXRENCOT',MXENCCOT->NUMCOT,'MXENCCOT->NUMCOT=MXRENCOT->NUMCOT')
en_dup:=.f.
RETURN

STATIC PROC ESTAT_COT
LOCAL TODOCOT:=.T.
SELECT BUFFER
GO TOP
WHILE !EOF()
 IF ESTATUS<>[OK]
   TODOCOT=.F.
   EXIT
 ENDIF
 SKIP
ENDDO
GO TOP
RETURN IF(TODOCOT,'OK','PE')


FUNCTION TOMAR_TZ(YCODART,YTARIFA,YCANTIDAD)
LOCAL EL,USEL:=SELECT(), XMONEDA:=''
IF !ALLTRIM(YTARIFA)$'ABCDEFGHIJKLMNOPQRSTUVWXYZ  '.AND.CHKU
IF ABRIR('MXLISPRE',MXLISPRE,'S',MXLIPRX1)
 IF DBSEEK(EL:=TRIM(YCODART+YTARIFA))
  DO WHILE CODART+CODPRE=EL
   XPRECIO:=E_IMPORTE(PRECIO,XEMISION)
   XDESC  :=POR_DES
   XMONEDA:=FACTOR
   IF YCANTIDAD<=CANTIDAD .OR. CANTIDAD=0
    EXIT
   ENDIF
   SKIP
  ENDDO
  CAMBIO_MZ(XMONEDA)
 ENDIF
 CERRAR('MXLISPRE')
ENDIF
SELECT(USEL)
ENDIF
RETURN .T.

FUNCTION CAMBIO_MZ(YMONEDA)
LOCAL USEL:=SELECT()
IF !EMPTY(YMONEDA)
IF ABRIR('MXMONEY',CE('MXMONEY.DBF'),'S')
 LOCATE FOR MONEDA=YMONEDA
 IF !EOF()
  XCAMBIO:=CAMBIO
  XPRECIO:=round(XPRECIO*CAMBIO,g_redondo)
 ENDIF
 CERRAR('MXMONEY')
ENDIF
ENDIF
SELECT(USEL)
RETURN NIL

PROC RF2Z
DO CASE
 CASE APL='COT'
  DETCOT()
 CASE APL='RCZ'
  POSPAG(RECNO())
  FP_TOTAL()
ENDCASE
RETURN

Static Function Reimprim
 Local el_repor,el_tipo,SalvaP:=SaveScreen(0,0,24,79),SelAnt:=Select()
 Off_Key()
 IF PV_IMP='S'
  IF PV_PAUSA='N'
   KEYBOARD SALIDA
  ENDIF
  IF EMPTY(PV_REP)
   el_repor:=g_libcot
   el_tipo:=1
  ELSE
   el_repor:=AllTrim(pv_rep)+'.inf'
   el_tipo:=2
  ENDIF
  Reimp(el_repor,'MXENCCOT',MXENCCOT->NUMCOT,' COTIZACIONES ',el_tipo)
 ENDIF
 RestScreen(0,0,24,79,SalvaP)
 DbSelectar(selant)
 On_Key()
Return nil

Function CotExpr(a1,b1,c1)
 Private en_menuh:=(a1=[menuh])
 if en_cta
  RETURN nil
 endif
 en_cta:=.T.
 inc_CotExp()
 if en_menuh
  on_key()
 else
  KEYBOARD escape
 endif
 en_cta:=.F.
RETURN nil

Static Function Inc_CotExp
 Private tiene,ulias,z_numero
 tiene = .f.
 ulias = select()
 if !abrir('MXENCCEX',mxenccex,'S',mxencex1)
    Pare('No se pudo abrir el archivo de cot. express')
    DbSelectar(ulias)
    RETURN nil
 endif
 DbGotop()
 COPY TO (bufcon) FOR estatus='PE'
 if DbVacio(bufcon,[E])=0
    Pare('No hay cotizaci¢n express pendientes')
    Cerrar('MXENCCEX')
    DELETE FILE (BufCon)
    DbSelectar(ulias)
    Return nil
 endif
 if !abrir('BUFCON',bufcon,[S])
    Pare('No se pudo abrir el temporal de cotizaci¢n express')
    Cerrar('MXENCCEX')
    DbSelectar(ulias)
    RETURN nil
 endif
 tiene=.t.
 z_numero=SPACE(8)
 satelite([ COT. EXPRESS ],'nombre_cli','numcex','emision', 'CEXSELPROG',listacex,{pad([Nombre Cliente],30),pad([N£mero],8),pad([Emisi¢n],8)})
 cerrar('BUFCON')
 DELETE FILE (bufcon)
 if EMPTY(z_numero)
    Cerrar('MXENCCEX')
    DbSelectar(ulias)
    RETURN nil
 endif
 if abrir("MXRENCEX",mxrencex,[S],mxrecex1)
    listacex:=z_numero
    if !DbSeek(z_numero)
       Pare('No se encontrar¢n renglones de esta cotizaci¢n express')
       Cerrar('MXENCCEX')
       Cerrar('MXRENCEX')
       DbSelectar(ulias)
       RETURN nil
    endif
    COPY TO (bufcex) WHILE numcex=z_numero
    Cerrar('MXRENCEX')
    Abrir([BUFCEX],bufcex,[E])
    // RepCex()
    REPLACE ALL CANTIDAD WITH CANTIDAD-ENT_COT
    COMMIT
    DELETE FOR !empty(item) .and. cantidad < 1
    COMMIT
    __DbPack()
    Cerrar('BUFCEX')
    if DbVacio(bufcex,[E])=0
       Pare('La cot. express seleccionada no tiene articulos pendientes')
       Cerrar('MXENCCEX')
       DbSelecTar(ulias)
       RETURN nil
    endif
    *******************************************************
    SELECT BUFFER
    APPEND FROM (bufcex)
    REPLACE ALL num_cex with Listacex
    SUM tot_ren to xtot_fac
    FP_TOTAL()
    if en_menuh
       RECALCULA:=.T.
       DbGotop()
       NoFlecha()
       PriPag()
    else
       RECALCULA:=.T.
    endif
    Cerrar('MXENCCEX')
 endif
 DbSelecTar(ulias)
RETURN 0

PROCEDURE CEXSELPROG
 Private devo
 devo:=.T.
 if numcex$listacex
   PARE([Cot. Express ya fue incluida])
   devo:=.f.
 endif
 if devo
  z_numero=&campo2
 else
  en_sel:=.t.
 endif
RETURN nil

Static Function ActCex()
 local cont:=.f.,nReg,xEst:='OK',selant:=select()
 SELECT MXRENCOT
 if dbseek(xnumcot)
    nReg:=Recno()
    do while numcot=xnumcot .and. !eof()
       if !empty(num_cex)
          cont:=.t.
       endif
       dbskip()
    enddo
 endif
 if cont .and. abrir("MXENCCEX",mxenccex,[S],mxencex1) .and. abrir("MXRENCEX",mxrencex,[S],mxrecex1,mxrecex2)
    SELECT MXRENCOT
    DbGoto(nReg)
    Do While numcot=xnumcot .and. !eof()
       xnum_cex:=num_cex
       xcantida:=cantidad
       xCodigo :=item
       if !empty(xnum_cex).and.mxrencex->(dbseek(xnum_cex))
          do while mxrencex->numcex=xnum_cex .and. !eof()
             if mxrencex->item=xcodigo .and. mxrencex->(rlock())
                mxrencex->ent_cot := mxrencex->ent_cot + xcantida
                mxrencex->(DbCommit())
                if mxrencex->cantidad<=mxrencex->ent_cot
                   mxrencex->estatus := 'OK'
                endif
                mxrencex->(DbCommit())
                mxrencex->(DbUnlock())
             endif
             mxrencex->(dbskip())
          enddo
       endif
       MXRENCOT->(dbskip())
    enddo
    SELECT MXRENCOT
    DbGoto(nReg)
    Do While numcot=xnumcot .and. !eof()
       xnum_cex:=num_cex
       if !empty(xnum_cex) .and. mxrencex->(dbseek(xnum_cex))
          do while mxrencex->numcex=xnum_cex .and. !eof()
             if mxrencex->estatus='PE'
                xEst:='PE'
                EXIT
             endif
             mxrencex->(dbskip())
          enddo
          if mxenccex->(dbseek(xnum_cex)) .and. mxenccex->(rlock())
             REPLACE mxenccex->estatus with xEst
             mxenccex->(DbUnlock())
             mxenccex->(DbCommit())
          endif
       endif
       dbskip()
    enddo
    cerrar('MXRENCEX')
    cerrar('MXENCCEX')
 endif
 DbSelectar(selant)
Return nil


Static Function Rev_cex(wnumcot)
 SELECT MXRENCOT
 SEEK(WNUMCOT)
 if found()
    if abrir('MXENCCEX',mxenccex,'S',mxencex1) .and. abrir('MXRENCEX',mxrencex,'S',mxrecex1,mxrecex2)
       SELECT MXRENCOT
       do while numcot=wnumcot .and. !eof()
          if !empty(num_cex)
             xnum_cex := num_cex
             xcantida := cantidad
             xCodigo  := Item
             SELECT MXRENCEX
             DbSetOrder(1)
             if dbseek(xnum_cex)
                do while mxrencex->numcex=xnum_cex .and. !eof()
                   if mxrencex->item=xcodigo
                      mxrencex->(rlock())
                      REPLACE ent_cot WITH ent_cot-xcantida
                      DbCommit()
                      if cantidad>ent_cot
                         REPLACE estatus WITH 'PE'
                         DbCommit()
                      endif
                      if estatus='PE'.and. mxenccex->(dbseek(xnum_cex)) .and. mxenccex->(rlock())
                         REPLACE mxenccex->estatus with 'PE'
                         mxenccex->(DbUnlock())
                      endif
                      mxrencex->(DbUnlock())
                   endif
                   mxrencex->(dbskip())
                enddo
             endif
          endif
          SELECT MXRENCOT
          DbSkip()
       enddo
       cerrar('MXRENCEX')
       cerrar('MXENCCEX')
    endif
 endif
Return nil

Function ValRma(xnumrma)
 Private selant:=select(),retorno:=.f.,aRMA:={},nI:=0
 if empty(xnumrma)
    return .t.
 endif
 if !abrir('RMARECEP',rmarecep,'C',rmarecx3)
    pare('NO SE PUDO ACCESAR AL ARCHIVO DE R.M.A.')
    return .t.
 endif
 if dbseek(xcliente+xnumrma) .and. garantia=[N] .and. estatus=[PE] .and. empty(numcot)
    cerrar('RMARECEP')
    dbselectar(selant)
    return .t.
 endif
 if dbseek(xcliente)
    do while codcli=xcliente .and. !eof()
       if garantia=[N].and.estatus=[PE].and. empty(numcot)
          aadd(aRMA,numrma+[ ]+dtoc(emision))
       endif
       dbskip()
    enddo
    if Len(aRMA)>0
       nI:=BoxSel(aRMA,8,50,1,[R.M.A.],[N£mero   Fecha],[],1)
       if nI>0
          xnumrma:=substr(aRMA[nI],1,8)
       endif
    endif
    retorno:=.t.
 else
    pare('NO EXISTEN R.M.A. PARA ESTE CLIENTE')
 endif
  cerrar('RMARECEP')
  dbselectar(selant)
Return retorno







