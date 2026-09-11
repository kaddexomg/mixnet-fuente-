PROCEDURE INV0304
*PARAMETERS P1,P2,P3
DECLARE HCAMPO[22],HSUBTIT[22]
PRIVATE XCODART,XFECHA_CREA:=CTOD(G_FECHA),XNOMART,XNOMALT,XGRUPO,XALTERNO,XIVA
PRIVATE XPRECIO_A,XPRECIO_B,XPRECIO_C,XPRECIO_D,XEXMIN,XEXMAX,XUBICA,XPESO
PRIVATE XUNIDAD, XALTURA,XANCHO,XPROFUN,XCAN_BUL,XCOMPUESTO,xresumen
PRIVATE XDES_A, XDES_B,XDES_C,XDES_D,XCODCON,VTAS_CODCON:="S"
PRIVATE BCODART,BNOMART, XPESO, LCODART, APERTURO:=.F.,BNOMALT,BALTERNO,CPA_CODCON
PRIVATE XALMACEN:='  ',XFECHA_MOV:=CTOD(G_FECHA),XTIPINV:='EN',XCODDPTO,XCODCON
PRIVATE XNUMDOC:='AP000001',XCANTIDAD:=1, XCOSTO_TOT:=0,MCOSTO_TOT:=SPACE(12)
PRIVATE XPRECIO_MAX,XPRECIO_MIN
PRIVATE XORIGEN:='INV'
Private xmarca
Private Insertar
Private xprov_asig
PRIVATE xFACTOR
PRIVATE xTIPPROD

PRIVATE  D_REFRES:=BLK('DETINV()')

*, XCODMOVART:=''
DEFDEFINV()
XIVA      = [A]
BUFCPS=G_CAMINO+[BUFCDI]+estacion+[.DBF]
CPA_CODCON:=inv_codcon
BCODART=SPACE(LON_ITEM)
BNOMART=SPACE(50)
BNOMALT:=SPACE(50)
BALTERNO:=SPACE(LON_ITEM)

DO CAMINV
DO SUBINV
IF ABRIR([MXFACINV],CE([MXFACINV.DBF]),[S],CE([MXFAINX1.NTX]))
IF ABRIR([MXMARINV],MXMARINV,[S],MXMAINX1)
IF ABRIR([MXGRUINV],MXGRUINV,[S],MXGRINX1)
IF ABRIR([MXCTAINV],MXCTAINV,[S],MXCTINX1,MXCTINX2,MXCTINX3,MXCTINX4,MXCTINX5)
 SET ORDER TO 1
 INIT_VER()
 SET KEY -7 TO MenuOpc()
 /*
 SET KEY -7 TO VER_SERIAL
 SET KEY -8 TO EDITAR_PRECIO
 SET KEY -9 to ANALIINV
 SET KEY -37 TO VER_EXALM
 SET KEY -38 TO VER_PROVEE
 SET KEY -27 TO RESUMEN()
 SET KEY -28 TO STATUSARTICULO()
 SET KEY -29 TO EQUIVALENCIAS()
 SET KEY -10 TO TECLASACTIVAS()
 SET KEY -40 TO factores()
 SET KEY -41 TO CODEXT()
 */
 //EDC([INV],[ ARTICULOS ],HSUBTIT,HCAMPO,.T.,.F.,.T.,.F.,.F.,.F.,.T.,.F.,'Shift+F1=Ver teclas activas en esta ventana')
 EDC([INV],[ ARTICULOS ],HSUBTIT,HCAMPO,.T.,.F.,.T.,.F.,.F.,.F.,.T.,.F.,'F8 = Menu de Opciones')
 SET KEY -7 TO
 *SET KEY -41 TO
 *SET KEY -40 TO
 *SET KEY -29 TO
 *SET KEY -10 TO
 *SET KEY -27 TO
 *SET KEY -38 TO
 *SET KEY -37 TO
 *SET KEY -9 TO
 *SET KEY -8 TO
ENDIF
ENDIF
ENDIF
ENDIF
RETURN

PROCEDURE SUBINV
HSUBTIT[ 1]=[Art¡culo       ]
HSUBTIT[ 2]=[Nombre]
HSUBTIT[ 3]=[Grupo]
HSUBTIT[ 4]=[Alterno        ]
HSUBTIT[ 5]=if(panama,[ITBM],[IVA])
HSUBTIT[ 6]=[Precio A]
HSUBTIT[ 7]=[Precio B]
HSUBTIT[ 8]=[Precio C]
HSUBTIT[ 9]=[Precio D]
HSUBTIT[10]=[Ex. min.]
HSUBTIT[11]=[Ex. max.]
HSUBTIT[12]=[UBICACION     ]
HSUBTIT[13]=[Peso]
HSUBTIT[14]=[UNIDAD]
HSUBTIT[15]=[ ALTURA]
HSUBTIT[16]=[  ANCHO]
HSUBTIT[17]=[PROFUNDIDAD]
HSUBTIT[18]=[CANT/BULTO]
HSUBTIT[19]=[COMPUESTO]
HSUBTIT[20]=[FECHA CREACION]
HSUBTIT[21]=[Precio Max.]
HSUBTIT[22]=[Precio Min.]
RETURN

PROCEDURE CAMINV
HCAMPO[ 1]=[CODART]
HCAMPO[ 2]=[NOMART]
HCAMPO[ 3]=[GRUPO]
HCAMPO[ 4]=[ALTERNO]
HCAMPO[ 5]=[IVA]
HCAMPO[ 6]=[PRECIO_A]
HCAMPO[ 7]=[PRECIO_B]
HCAMPO[ 8]=[PRECIO_C]
HCAMPO[ 9]=[PRECIO_D]
HCAMPO[10]=[EXMIN]
HCAMPO[11]=[EXMAX]
HCAMPO[12]=[UBICA]
HCAMPO[13]=[PESO]
HCAMPO[14]=[UNIDAD]
HCAMPO[15]=[ALTURA]
HCAMPO[16]=[ANCHO]
HCAMPO[17]=[PROFUN]
HCAMPO[18]=[CAN_BUL]
HCAMPO[19]=[COMPUESTO]
HCAMPO[20]=[FECHA_CREA]
HCAMPO[21]=[PRECIO_MAX]
HCAMPO[22]=[PRECIO_MIN]
RETURN

PROCEDURE DETINV
@  3,09 SAY CODART
@  3,38 SAY ALTERNO
@  3,67 SAY FECHA_CREA
@  4,09 SAY NOMART
@  5,09 SAY NOMALT
@  5,71 SAY PROV_ASIG
@  4,69 SAY if(estatus$[1],[Inactivo],[Activo  ])
@  6,09 SAY GRUPO
@  6,15 SAY if(empty(grupo),space(35),NOMCOD(GRUPO,[MXGRUINV],[NOMGRUART]))
@  6,63 SAY PRECIO_MAX PICT '99999999999.99'
@  7,09 SAY marca
@  7,15 SAY IF(EMPTY(marca),SPACE(35),NOMCOD(marca,[MXMARINV],[NOMMARART]))
@  7,63 SAY PRECIO_MIN PICT '99999999999.99'
@  8,63 SAY UBICA
@  8,15 SAY IVA
@  8,22 SAY TIPPROD
@  8,35 SAY FACTOR

@  9,63 SAY UNIDAD

@  9,14 SAY BS(PRECIO_A)  PICT '9999999999.99'
@  9,32 SAY DES_A PICT '99.99'
@ 10,14 SAY BS(PRECIO_B)  PICT '9999999999.99'
@ 10,32 SAY DES_B PICT '99.99'
@ 11,14 SAY BS(PRECIO_C)  PICT '9999999999.99'
@ 11,32 SAY DES_C PICT '99.99'
@ 12,14 SAY BS(PRECIO_D)  PICT '9999999999.99'
@ 12,32 SAY DES_D PICT '99.99'

@ 10,63 SAY PESO  PICT [999999.999]
@ 11,63 SAY ALTURA
@ 12,63 SAY ANCHO
@ 13,63 SAY PROFUN
@ 13,21 SAY EXMIN
@ 14,63 SAY CAN_BUL   pict '999999.999'
@ 14,21 SAY EXMAX

@ 15,63 SAY COMPUESTO
@ 15,21 SAY CODCON
otr_det()
RETURN

static proc otr_det
 @ 17,15 say Pad(Alltrim(Str(cotiza,11,3)),11)
 @ 18,15 SAY Pad(Alltrim(Str(pedido,11,3)),11)
 @ 19,15 SAY Pad(Alltrim(Str(ordena,11,3)),11)

 @ 17,39 SAY ult_prove
 @ 18,39 SAY Pad(Alltrim(trans(BS(ult_costo),'999,999,999.999')),15)
 @ 19,39 SAY Pad(Alltrim(trans(BS(costo_act),'999,999,999.999')),15)

 @ 19,64 SAY Pad(Alltrim(trans(existe_act,'999,999,999.999')),15)

 return

PROCEDURE GETBINV
* CAPTURA DE CAMPOS EN MODO BROWSE
PRIVATE FFF,CCC
FFF=ROW()
CCC=COL()
DO CASE
CASE ICAMPO=1
@  FFF,CCC GET XCODART  WHEN CHK_MODIF() PICT [@!] VALID CHK_CLAVE(XCODART,[Art¡culo])

CASE ICAMPO=2
@  FFF,CCC GET XNOMART

CASE ICAMPO=3
@  FFF,CCC GET XGRUPO PICT [@!] VALID CHGRI(XGRUPO)

CASE ICAMPO=4
@  FFF,CCC GET XALTERNO

CASE ICAMPO=5
@  FFF,CCC GET XIVA  PICT '!'  VALID XIVA$[A,B,C,D ]

CASE ICAMPO=6
@  FFF,CCC GET XPRECIO_A PICT PIC14

CASE ICAMPO=7
@  FFF,CCC GET XPRECIO_B PICT PIC14

CASE ICAMPO=8
@  FFF,CCC GET XPRECIO_C PICT PIC14

CASE ICAMPO=9
@  FFF,CCC GET XPRECIO_D PICT PIC14

CASE ICAMPO=10
@ FFF,CCC GET XEXMIN PICT [999999.999]

CASE ICAMPO=11
@ FFF,CCC GET XEXMAX PICT [999999.999] VALID XEXMIN<=XEXMAX

CASE ICAMPO=12

@  FFF,CCC GET XUBICA  PICT [XXXX:XXXX:XXXX]

CASE ICAMPO=13
@  FFF,CCC GET XPESO PICT [999999.999]

CASE ICAMPO=14
@  FFF,CCC GET XUNIDAD PICT [@!]

CASE ICAMPO=15
@  FFF,CCC GET XALTURA PICT '999.999'

CASE ICAMPO=16
@  FFF,CCC GET XANCHO  PICT '999.999'

CASE ICAMPO=17
@ FFF,CCC GET XPROFUN PICT '999.999'

CASE ICAMPO=18
@ FFF,CCC GET XCAN_BUL PICT '999999.999'

CASE ICAMPO=19
@ FFF,CCC GET XCOMPUESTO PICT '!'

CASE ICAMPO=20
@ FFF,CCC GET XFECHA_CREA VALID !EMPTY(XFECHA_CREA)
CASE ICAMPO=21
@ FFF,CCC GET XPRECIO_MAX VALID XPRECIO_MAX>=XPRECIO_MIN
CASE ICAMPO=22
@ FFF,CCC GET XPRECIO_MIN VALID XPRECIO_MIN<=XPRECIO_MAX
ENDCASE
READ
SALIR=ESCAPAR()
 IF !SALIR .AND. XCOMPUESTO='S'
  COMPONE()
 ENDIF

RETURN


PROCEDURE CRDINV
* COORDENADAS DE LA VENTANA
TOPE    = 3
FONDO   = MAXROW()-5
COLIZQ  = 1
COLDER  = 78
RETURN


PROCEDURE BUSINV
PRIVATE TCODART,TNOMART,TNOMALT,TALTERNO
BUS([BINVFUN])
*SET ORDER TO 1
SETCOLOR(C_TIT2)
RETURN

PROCEDURE BINVFUN
* BUSQUEDA
@  3,09 GET BCODART  PICT [@!]
@  3,38 GET BALTERNO PICT [@!]
@  4,09 GET BNOMART PICT [@!]
@  5,09 GET BNOMALT PICT [@!]
LEER()
SALIR=ESCAPAR() .OR. ( EMPTY(BCODART) .AND. EMPTY(BNOMART) .AND. EMPTY(BNOMALT).AND. EMPTY(BALTERNO))

IF !SALIR
 IF !EMPTY(BCODART)
  TCODART=TRIM(BCODART)
  YKEY=TCODART
  SET ORDER TO 1
 ENDIF
 IF !EMPTY(BNOMART)
  TNOMART=UPPER(TRIM(BNOMART))
  YKEY=TNOMART
  SET ORDER TO 2
 ENDIF
 IF !EMPTY(BNOMALT)
  TNOMALT=UPPER(TRIM(BNOMALT))
  YKEY=TNOMALT
  SET ORDER TO 4
 ENDIF
 IF !EMPTY(BALTERNO)
  TALTERNO=UPPER(TRIM(BALTERNO))
  YKEY=TALTERNO
  SET ORDER TO 5
 ENDIF
ENDIF

RETURN

PROCEDURE DEFINV
APERTURO:=.F.
RETURN

PROCEDURE DEFDEFINV
* VALORES POR DEFECTO
XCODART    := SPACE( lon_item)
XFECHA_CREA:= CTOD(G_FECHA)
LCODART    := SPACE( lon_item)
XNOMART    := SPACE( 50)
XNOMALT    := SPACE( 50)
XGRUPO     := SPACE(  5)
XALTERNO   := SPACE(lon_item)
XIVA       := SPACE(  1)
STORE 0.00 TO XPRECIO_A,XPRECIO_B,XPRECIO_C,XPRECIO_D,;
              XDES_A,XDES_B,XDES_C,XDES_D,XPRECIO_MAX,XPRECIO_MIN
XEXMIN     := 0.000
XEXMAX     := 0.000
XUBICA     := SPACE(14)
XPESO      := 0.000
XUNIDAD    := SPACE(  3)
XALTURA    := 0.000
XANCHO     := 0.000
XPROFUN    := 0.000
XCAN_BUL   := 1.000
XCOMPUESTO := 'N'
XCODCON    := SPACE(20)
XCODDPTO   := SPACE(10)
xresumen   := Space(1)
xmarca     := Space(5)
XPROV_ASIG := SPACE(8)
xFACTOR    := space(2)
XTIPPROD:=SPACE(1)
RETURN

PROCEDURE ASGINV
* ASIGNACION DE REGISTRO A MEMORIA
STORE CODART TO XCODART,LCODART
XNOMART    := NOMART
XNOMALT    := NOMALT
XFECHA_CREA:= FECHA_CREA
XGRUPO     := GRUPO
XALTERNO   := ALTERNO
XIVA       := IVA
XPRECIO_A  := PRECIO_A
XPRECIO_B  := PRECIO_B
XPRECIO_C  := PRECIO_C
XPRECIO_D  := PRECIO_D
XPRECIO_MAX:= PRECIO_MAX
XPRECIO_MIN:= PRECIO_MIN

XDES_A     := DES_A
XDES_B     := DES_B
XDES_C     := DES_C
XDES_D     := DES_D

XEXMIN     := EXMIN
XEXMAX     := EXMAX
XUBICA     := UBICA
XPESO      := PESO
XUNIDAD    := UNIDAD
XALTURA    := ALTURA
XANCHO     := ANCHO
XPROFUN    := PROFUN
XCAN_BUL   := CAN_BUL
XCOMPUESTO := COMPUESTO
XCODCON    := CODCON
XRESUMEN   := RESUMEN
xmarca     := marca
XPROV_ASIG := PROV_ASIG
xFACTOR    := FACTOR
XTIPPROD   := TIPPROD

RETURN

PROCEDURE RPLINV
* REEMPLAZO EN EL REGISTRO
AUDITORIA([Acceso a mantenimiento de la ficha de art¡culos],xcodart,codart,IF(MODIFICA,[MOD],[INC]))
REPLACE CODART     WITH XCODART    ,;
        FECHA_CREA WITH XFECHA_CREA,;
        NOMART     WITH XNOMART    ,;
        NOMALT     WITH XNOMALT    ,;
        GRUPO      WITH XGRUPO     ,;
        MARCA      WITH XMARCA     ,;
        ALTERNO    WITH XALTERNO   ,;
        IVA        WITH XIVA       ,;
        PRECIO_A   WITH XPRECIO_A  ,;
        PRECIO_B   WITH XPRECIO_B  ,;
        PRECIO_C   WITH XPRECIO_C  ,;
        PRECIO_D   WITH XPRECIO_D  ,;
        DES_A      WITH XDES_A  ,;
        DES_B      WITH XDES_B  ,;
        DES_C      WITH XDES_C  ,;
        DES_D      WITH XDES_D  ,;
        PRECIO_MAX WITH XPRECIO_MAX,;
        PRECIO_MIN WITH XPRECIO_MIN,;
        EXMIN      WITH XEXMIN     ,;
        EXMAX      WITH XEXMAX     ,;
        UBICA      WITH XUBICA     ,;
        PESO       WITH XPESO      ,;
        UNIDAD     WITH XUNIDAD    ,;
        ALTURA     WITH XALTURA    ,;
        ANCHO      WITH XANCHO     ,;
        PROFUN     WITH XPROFUN    ,;
        CAN_BUL    WITH XCAN_BUL   ,;
        COMPUESTO  WITH XCOMPUESTO ,;
        CODCON     WITH XCODCON    ,;
        RESUMEN    WITH XRESUMEN   ,;
        FECHA_MOD  WITH Date()     ,;
        HORA_MOD   WITH STRTRAN(TIME(),':') ,;
        PROV_ASIG  WITH XPROV_ASIG,;
        FACTOR     WITH xFACTOR   ,;
        TIPPROD    WITH xTIPPROD

IF XCOMPUESTO='S'
 RPL_COMPOS()
ENDIF
HAZ_APER()

RETURN

STATIC PROC HAZ_APER
PRIVATE  XCODMOVART:=XCODART
IF APERTURO
IF ABRIR([MXEXTINV],MXEXTINV,[S],MXEXINX1)
 IF ABRIR([MXTRAINV],MXTRAINV,[S],MXTRINX1,MXTRINX2,MXTRINX3)
  ADD_REC(0)
  ARC_INV()
  CERRAR('MXTRAINV')
 ENDIF
 CERRAR('MXEXTINV')
ENDIF
SELECT MXCTAINV
APERTURO:=.F.
ENDIF
RETURN

STATIC FUNCTION TOMA_aper
LOCAL USCR:=SAVESCREEN(4,25,20,60), UGET, UCOL:=SETCOLOR()
* SAVE GET TO UGET
 BOXSOM(' APERTURA ',4,25,10,58,1)
 SETCOLOR(C_TEX1)
 CUR_ON()
 @ 5,27 SAY 'Almac‚n....:' GET XALMACEN when g_multi VALID IF(EMPTY(XALMACEN),.T.,CHALM(XALMACEN))
 @ 6,27 SAY 'Fecha......:' GET XFECHA_MOV VALID CHKEJER(XFECHA_MOV)
 @ 7,27 SAY 'N£mero.....:' GET XNUMDOC PICT [@!] VALID CHNUMINV(XNUMDOC)
 @ 8,27 SAY 'Cantidad...:' GET XCANTIDAD  PICT "999999.999"  VALID XCANTIDAD>=1
 @ 9,27 SAY 'Costo total:' GET MCOSTO_TOT  PICT '@K#99999999999' VALID CONVCOS1()
 READ
 /*
 IF INV_CODCON='S'.AND. !SALIR
    CUENTACON()
 ENDIF
 */
 CUR_OFF()
*RESTORE GET FROM UGET
 RESTSCREEN(4,25,20,60,USCR)
 SETCOLOR(UCOL)
 APERTURO:=!ESCAPAR()
 SELECT MXCTAINV
RETURN .T.

STATIC FUNCTION CONVCOS1
IF LASTKEY()=5
 RETURN(.T.)
ENDIF
IF UPPER(SUBSTR(MCOSTO_TOT,1,1))='*'
 XCOSTO_TOT=VAL(SUBSTR(MCOSTO_TOT,2))
 XCOSTO_TOT=XCOSTO_TOT*XCANTIDAD
ELSE
 XCOSTO_TOT=VAL(MCOSTO_TOT)
ENDIF
RETURN(XCOSTO_TOT>0 .AND. XCOSTO_TOT <999999999.99)

STATIC FUNCTION CHNUMINV
PARAMETER EL_NUM
IF LASTKEY()=5
 RETURN .T.
ENDIF
IF EMPTY(EL_NUM)
 PARE([N£mero de documento debe contener informaci¢n])
 RETURN .F.
ENDIF
RETURN .T.

PROCEDURE MASINV
* MASCARA DE LA VENTANA
SETCOLOR(C_BOX2)
BOXTIT(TITULO,2,0,22,79)
L78(16)
L78(20)
SETCOLOR(C_TEX2)
@  3, 2 SAY [C¢digo:                Cod. Alterno:                 F. Creaci¢n:          ]
@  4, 2 SAY [Nombre:                                                    Status:         ]
@  5, 2 SAY [                                                           Proveedor:        ]
@  6, 2 SAY [Grupo.:]
@  6,51 SAY [Precio Max.:]
@  7, 2 SAY [Marca.:]
@  7,51 SAY [Precio Min.:]

@  8,44 SAY [Ubicaci¢n f¡sica..:]
@  8, 2 SAY if(panama,[Tipo I.T.B.M......:],[Tipo I.V.A.:])
@  8,18 SAY [Uso:      Factor:]

@  9,44 SAY [Unidad de medida..:]
@  9, 2 SAY [Precio A...:               %D:]
@ 10,44 SAY [Peso..............:]
@ 10, 2 SAY [Precio B...:               %D:]
@ 11,44 SAY [Altura............:]
@ 11, 2 SAY [Precio C...:               %D:]
@ 12,44 SAY [Ancho.............:]
@ 12, 2 SAY [Precio D...:               %D:]
@ 13,44 SAY [Profundidad.......:]
@ 13, 2 SAY [Existencia m¡nima.:          ]
@ 14,44 SAY [Cantidad/Bulto....:         ]
@ 14, 2 SAY [Existencia m xima.:          ]
@ 15,44 SAY [Compuesto ? ......:]
@ 15, 2 SAY [C¢digo contable...:]

@ 17,2 SAY [Ac.Cotizac.:             Ult. Prove:                 ]
@ 18,2 SAY [Ac. Pedidos:             Ult. Costo:                 ]
@ 19,2 SAY [Ac. Ord.Cpa:             C.promedio:                 Ex.Act.:                ]

SETCOLOR(C_TIT2)

RETURN

PROCEDURE GETINV
* CAPTURA REGISTRO
IF !MODIFICA
 @  6,15 SAY SPACE(35)
 @  7,15 SAY SPACE(35)
 @ 17,14 SAY SPACE(11)
 @ 18,14 SAY SPACE(11)
 @ 19,14 SAY SPACE(11)
 @ 17,39 SAY SPACE(14)
 @ 18,39 SAY SPACE(14)
 @ 19,39 SAY SPACE(14)
 @ 19,64 SAY SPACE(15)
else
 otr_det()
ENDIF

SET ORDER TO 1

@  3,09 GET XCODART WHEN CHK_MODIF() PICT [@!] VALID CHK_CLAVE(XCODART,[Art¡culo])
@  3,38 GET XALTERNO   PICT [@!]
@  3,67 GET XFECHA_CREA
@  4,09 GET XNOMART
@  5,09 GET XNOMALT
@  5,71 GET XPROV_ASIG
@  6,09 GET XGRUPO PICT [@!] VALID CHGRI(XGRUPO)
@  7,09 GET xmarca PICT [@!] VALID CHMAI(xmarca)

@  8,15 GET XIVA  PICT '!'  VALID XIVA$[A,B,C,D ]
@  8,22 GET xTIPPROD PICT '@!' VALID ValTipProd(@xTipProd,8,22)
@  8,35 GET XFACTOR VALID if(empty(xFactor),.t.,chk_ven('MXFACINV','FACTOINV',xFactor))

@  9,14 GET XPRECIO_A PICT '9999999999.99'
@  9,32 GET XDES_A PICT '99.99'

@ 10,14 GET XPRECIO_B PICT '9999999999.99'
@ 10,32 GET XDES_B PICT '99.99'

@ 11,14 GET XPRECIO_C PICT '9999999999.99'
@ 11,32 GET XDES_C PICT '99.99'

@ 12,14 GET XPRECIO_D PICT '9999999999.99'
@ 12,32 GET XDES_D PICT '99.99'

@ 13,21 GET XEXMIN PICT [999999.999]
@ 14,21 GET XEXMAX PICT [999999.999] VALID XEXMIN<=XEXMAX
@ 15,21 GET XCODCON  VALID MIREF_CON(XCODCON)

@  6,63 GET XPRECIO_MAX PICT [99999999999.99]
@  7,63 GET XPRECIO_MIN PICT [99999999999.99] VALID XPRECIO_MIN<=XPRECIO_MAX

@  8,63 GET XUBICA  PICT [XXXX:XXXX:XXXX]
@  9,63 GET XUNIDAD PICT [@!]
@ 10,63 GET XPESO PICT [999999.999]
@ 11,63 GET XALTURA PICT '999999.999'
@ 12,63 GET XANCHO  PICT '999999.999'

&& ocai 16/1/18
* 13,63 GET XPROFUN PICT '999999.999'
@ 13,63 GET XPROFUN PICT '9999999999'

@ 14,63 GET XCAN_BUL PICT '999999.999'
@ 15,63 GET XCOMPUESTO PICT '!'
READ
CLEAR GETS
SALIR=ESCAPAR()

&& OCAI 21/04/17
IF !SALIR
 _JJPVP(Xprofun)
ENDIF

IF !SALIR
   IF !MODIFICA
      xCol:=5
      xLin:=3
      Salva_P:=SaveScreen(xLin-1,xCol-1,xLin+17,xCol+70)
      insertar:=ReadInsert()
      BoxSom(' INFORMACION ADICIONAL DEL ARTICULO ',xLin,xCol,xLin+15,xCol+69,2)
      Cur_on()
      xresumen=MemoEdit(xresumen,xLin+1,xCol+1,xLin+14,xCol+68,.T.,[TTFUNC],67)
      ReadInsert(insertar)
      Cur_off()
      RestScreen(xLin-1,xCol-1,xLin+17,xCol+70,Salva_P)
   ENDIF
ENDIF
IF !SALIR
 IF XCOMPUESTO='S'
  COMPONE()
 ELSE
  IF !MODIFICA
   TOMA_APER()
  ENDIF
 ENDIF
ENDIF
RETURN

******************************************
STATIC FUNCTION CHK_MODIF
RETURN(IF(MODIFICA,EMPTY(MXCTAINV->FECHA_COS),.T.))

FUNCTION CHGRI
PARAMETER EL_CODGRU
PRIVATE ENC
ENC=.T.
IF EMPTY(EL_CODGRU)
   RETURN(.T.)
ENDIF
SELE MXGRUINV
SEEK(EL_CODGRU)
IF !FOUND()
 GRI0000()
 IF LASTKEY()=27
  KEYBOARD SALIDA
  WAIT
 ENDIF
 ENC=.F.
ELSE
 @ 6,15 SAY NOMCOD(EL_CODGRU,[MXGRUINV],[NOMGRUART])
ENDIF
SELECT MXCTAINV
RETURN ENC

FUNCTION CHMAI
PARAMETER EL_CODMAR
PRIVATE ENC
ENC=.T.
IF EMPTY(EL_CODMAR)
   RETURN(.T.)
ENDIF
SELE MXMARINV
SEEK(EL_CODMAR)
IF !FOUND()
 MAI0000()
 IF LASTKEY()=27
  KEYBOARD SALIDA
  WAIT
 ENDIF
 ENC=.F.
ELSE
 @ 7,15 SAY NOMCOD(EL_CODMAR,[MXMARINV],[NOMMARART])
ENDIF
SELECT MXCTAINV
RETURN ENC

FUNCTION RPL_COMPOS
IF ABRIR([MXCOMINV],MXCOMINV,[S],MXCOINX1)
 SELECT MXCOMINV
 SEEK(LCODART)
 IF FOUND()
  DO WHILE !FLOCK()
  ENDDO
  DELETE WHILE CODART=LCODART
  UNLOCK
 ENDIF
 APPEND FROM (BUFCPS)
 CERRAR('MXCOMINV')
 DELETE FILE (BUFCPS)
ENDIF
SELECT MXCTAINV
RETURN 0

FUNCTION VELIINV
IF !EMPTY(MXCTAINV->FECHA_COS)
 PARE('ARTICULO CON MOVIMIENTO. NO PUEDE SER ELIMINADO')
 RETURN .F.
ENDIF
RETURN .T.

PROCEDURE ELIINV
PRIVATE LCODART
* ELIMINACION DE REGISTRO
SELECT MXCTAINV
IF COMPUESTO='S'
 LCODART=MXCTAINV->CODART
 IF ABRIR([MXCOMINV],MXCOMINV,[S],MXCOINX1)
  SEEK(LCODART)
  IF FOUND()
   DO WHILE !FLOCK()
   ENDDO
   DELETE WHILE CODART=LCODART
   UNLOCK
  ENDIF
  CERRAR('MXCOMINV')
 ENDIF
ENDIF
SELECT MXCTAINV
AUDITORIA([Acceso a mantenimiento de la ficha de art¡culos],codart,,[ELI])
DELETE
RETURN

func analiINV(x1,x2,x3)
if file('graf.exe')
 __xsavescreen()
 SWPRUNCMD('graf '+g_camino+' 2 '+mxctaINV->codART,0,'','')
 __xrestscreen()
else
 pare('M¢dulo GRAF.EXE no se encuentra')
endif
cur_on()
cur_off()
return


***********************************
*FUNCIONES Y PROCEDIMIENTO NUEVOS *
***********************************

*****************************************************************************
*  PROCEDIMIENTO INCLUIDOS EL DIA 3.9.97  PARA VER EXISTENCIAS POR ALMACEN  *
*****************************************************************************
* PROCEDIMIENTO PARA VER LA EXISTENCIA POR ALMACEN
PROCEDURE VER_EXALM()
LOCAL UCOLOR:=SETCOLOR(), UROW:= ROW(), UCOL:= COL(),ULT_SEL:=SELECT()
PARAMETER P1,P2,P3
DECLARE ULT_REC[18],ULT_IND[18],ABIERTA[18]
AFILL(ABIERTA,.F.)

*SET KEY -37 TO

DECLARE W_EDC:={},W_MEM:={},W_SUB:={},W_BRO:={}
PRIVATE W_MAS,W_DEF,W_ASG,W_GET,W_RPL,W_ELI,W_BUS,W_SEL,W_CRD,W_DET,W_LEN:=0
PRIVATE W_ID,W_PREVALINC,W_PREVALMOD,W_PREVALELI
PRIVATE W_VERTICAL,W_EDITABLE,W_MOVIL
BUFFER := G_CAMINO+'BUFALM'+ESTACION+'.DBF'
BUFNTX := G_CAMINO+'BUFALM'+ESTACION+'.NTX'
PRIVATE W_OPEN,W_CLOSE,W_BOTSEL,W_MENSA24,W_TITULO:=''

PRIVATE  NO_CONFOR:=.f.
*,P1:=IF(READVAR()='P_OPCH','MENUH',NIL)
private P1:=IF(type('P_OPCH')<>'U','MENUH',NIL)
PRIVATE W_VAR:={}
PRIVATE W_VAR1,W_VAR2,W_VAR3,W_VAR4,W_VAR5,YCOD_REF

YCOD_REF:= FIELDGET(1)

IF ABRIR([MXEXTINV],MXEXTINV,[S],MXEXINX1)
  SEEK(YCOD_REF)
  IF FOUND()
   BUFFER := G_CAMINO+'BUFALM'+ESTACION+'.DBF'
   BUFNTX := G_CAMINO+'BUFALM'+ESTACION+'.NTX'
   COPY TO (BUFFER) WHILE CODMOVART=YCOD_REF
  ELSE
   PARE("NO HAY EXISTENCIA DISPONIBLES POR ALMACEN")
   CERRAR('MXEXTINV')
   SELECT(ULT_SEL)
   *SET KEY -37 TO VER_EXALM
   RETURN
  ENDIF
  setcolor(c_tex1)
  if ( W_LEN:=load_window('VERALMAF'))>0
   W_INI()
   W_ELI:={||DBDELETE(),LOTOCO:=.F.}  &&(ELIMINAR)
   W_RPL:={||W_RPLREG(),LOTOCO:=.F.}  &&(REEMPLAZO AL INCLUIR/MODIFICAR)
   IF abrir('TEMMBA',BUFFER,'E')
     INDEX ON CODMOVART TO (BUFNTX)
   ENDIF
   CERRAR('TEMMBA')
   IF ABRIR('MXCTAALM',MXCTAALM,"S",MXCTALX1)
   IF abrir('TEMMBA',BUFFER,'E',BUFNTX)
     SET RELATION TO ALMACEN INTO MXCTAALM
     DBGOTOP()
     EDC([_W],' ALMACENES, ART: '+ALLTRIM(YCOD_REF)+" ",W_SUB,W_BRO,.f.,.T.,.F.,.T.,.T.,.T.,.F.,.t.,'')
     CERRAR('TEMMBA')
     CERRAR('MXCTAALM')
   ENDIF
   ENDIF
  ENDIF
ENDIF
SELECT (ULT_SEL)
SETCOLOR(UCOLOR)
@ UROW,UCOL SAY []
DELETE FILE (BUFFER)
DELETE FILE (BUFNTX)
*SET KEY -37 TO VER_EXALM
RETURN

Static Function Resumen()
 Local salva_p,xCol:=5,xLin:=4,el_cursor:=SetCursor(),insertar:=ReadInsert()
 if rlock()
   *SET KEY -27 TO
   SAVE SCREEN TO Salva_P
   OFF_KEY()
   xresumen:=resumen
   BoxSom(' INFORMACION ADICIONAL DEL ARTICULO ',xLin,xCol,xLin+15,xCol+69,2)
   @ 23,35 SAY 'ESC=Salir' COLOR C_MENSAJE
   Setcursor(1)
   xresumen=MemoEdit(xresumen,xLin+1,xCol+1,xLin+14,xCol+68,.T.,[TTFUNC],67)
   Setcursor(el_cursor)
   ON_KEY()
   ReadInsert(insertar)
   if decis([¨ Desea Grabar los cambios ?],1)=[S]
      REPLACE resumen WITH xresumen
   endif
   RESTORE SCREEN FROM Salva_P
   Unlock
   *SET KEY -27 TO resumen
 endif
RETURN nil

FUNCTION NOBROWSE
 KEYBOARD ESCAPE
RETURN 0

FUNCTION TTFUNC
 PARAMETERS mode, line, col
 PRIVATE keypress,ret_val
 ret_val = 0
 IF mode = 0
    * idle
 ELSE
   * keystroke exception
   keypress = LASTKEY()
   IF keypress=27
     ret_val = 23
    ENDIF
 ENDIF
RETURN ret_val
/*
Static Function TeclasActivas()
 Local __Lapant,xL:=5,xC:=36,xl1:=5,xc1:=36
 __Lapant:=SaveScreen(xL1-1,xC1-1,xL1+14,xC1+41)
 BoxSom(' TECLAS DE FUNCION ACTIVAS ',xL-1,xC-1,xl+13,xC+40,1)
 @ ++xL,xC+1 SAY 'F8      = Tabla de seriales           ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'F9      = Tablas de otros precios     ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'F10     = Activar el gr fico          ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'Alt+F8  = Existencia por almac‚n      ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'Alt+F9  = Lista de proveedores        ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'Ctrl+F8 = Ventana de resumen          ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'Ctrl+F9 = Activar/Desactivar art¡culo ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'Ctrl+F10= Equivalencias               ' COLOR c_tex3
 @ ++xL,xC+1 SAY 'F11     = Factor de c lculo de precios' COLOR c_tex3
 @ ++xL,xC+1 SAY 'F12     = Equivalencias en Cod. Ext.  ' COLOR c_tex3
 @ ++xL,xC+1 SAY '                                      ' COLOR c_tex3
 @ ++xL,xC+1 SAY '      Presione cualquier tecla        ' COLOR c_tit3
 Esperar()
 RestScreen(xL1-1,xC1-1,xL1+14,xC1+41,__Lapant)
Return nil
*/
Static Function StatusArticulo()
 if rlock()
    mxctainv->estatus:=if(mxctainv->estatus$'1','0','1')
    commit
    unlock
    @  4,69 SAY if(estatus$[1],[Inactivo],[Activo  ]) COLOR c_tit2
 endif
Return nil

Function EQUIVALENCIAS()
 Local buffer:=g_camino+'BUFEQU'+estacion+'.DBF',_cod:=codart,SelAnt:=Select()
 DbCreate(buffer,{{'codart','C',15,0},{'equiva','C',15,0},{'nomequ','C',50,0}})
 if ABRIR('MXEQUINV',mxequinv,'S',mxeqinx1)
    if ABRIR('EQUIVA',buffer,'E')
       DbSelectar('MXEQUINV')
       if DbSeek(_cod)
          Do while mxequinv->codart=_cod .and. !eof()
             equiva->(DbAppend())
             equiva->codart:=mxequinv->codart
             equiva->equiva:=equiva
             equiva->nomequ:=BUSCAR_UBICAR('MXCTAINV',_cod,'nomart',1)
             Dbskip()
          Enddo
       endif
       DbSelectar('EQUIVA')
       EDIT_VENTANA('EDCEQUIV',{_cod})
       CERRAR('EQUIVA')
    endif
    if decis([¨ Desea actualizar lista de equivalencias ?],1)=[S]
       DbSelectar('MXEQUINV')
       Do While !Flock()
       Enddo
       if DbSeek(_Cod)
          DELETE while mxequinv->codart = _cod
       endif
       APPEND FROM (buffer)
       UNLOCK
    endif
    CERRAR('MXEQUINV')
 endif
 DbSelectar(SelAnt)
Return nil

Function Factores()
 *SET KEY -40 TO
 edit_ventana('FACTOINV')
 *SET KEY -40 TO Factores()
return nil

Function CodExt()
 Local buffer:=g_camino+'BUFCEX'+estacion+'.DBF',_cod:=codart,SelAnt:=Select()
 DbCreate(buffer,{{'CODART','C',15,0},{'CODEXT','C',15,0},{'NOMPRO','C',50,0}})
 *SET KEY -41 TO
 if ABRIR('MXCEXINV',g_camino+'MXCEXINV.DBF','S',g_camino+'MXCEINX1.NTX',g_camino+'MXCEINX2.NTX')
    SET ORDER TO 1
    if ABRIR('CODEXT',buffer,'E')
       DbSelectar('MXCEXINV')
       if DbSeek(_cod)
          Do while MXCEXINV->codart=_cod .and. !eof()
             CODEXT->(DbAppend())
             CODEXT->codart:=MXCEXINV->codart
             CODEXT->codext:=MXCEXINV->codext
             CODEXT->nompro:=MXCEXINV->nompro
             Dbskip()
          Enddo
       endif
       DbSelectar('CODEXT')
       EDIT_VENTANA('EDCODEXT',{_cod})
       CERRAR('CODEXT')
    endif
    if decis([¨ Desea actualizar Cod. Externos ?],1)=[S]
       DbSelectar('MXCEXINV')
       Do While !Flock()
       Enddo
       if DbSeek(_Cod)
          DELETE while MXCEXINV->codart = _cod
       endif
       APPEND FROM (buffer)
       UNLOCK
    endif
    CERRAR('MXCEXINV')
 endif
 DbSelectar(SelAnt)
 *SET KEY -41 TO CodExt()
Return nil

Static Function ValTipProd(cOpc,xl,xc)
 Local retorno:=.t.,nOpc,aOpc,uColor:=SetColor()
 IF cOpc$'SN0123'
    RETURN .T.
 ENDIF
 aOpc:={"0 = Todo Uso             ",;
        "1 = Producto Terminado   ",;
        "2 = Materia Prima        ",;
        "3 = Materiales Indirectos"}
 nOpc:=la_opcion(aOpc,xl,xc,4,1,[ DIFERENTES USOS ])
 IF lastkey()=27 .OR. nOpc=0
    cOpc:=' '
    retorno:=.f.
 ENDIF
 IF nOpc=1
    cOpc:='0'
 ELSEIF nOpc=2
    cOpc:='1'
 ELSEIF nOpc=3
    cOpc:='2'
 ELSEIF nOpc=4
    cOpc:='3'
 ENDIF
 SetColor(uColor)
Return(retorno)
/*
Function ValTipProd(xtipprod)
 local retorno:=.t.
 IF !EMPTY(xTipProd)
    FOR  x:= 1 TO len(alltrim(xtipprod))
      IF !substr(xtipprod,x,1)$'PM'
         pare('P=PRODUCCION, M=MATERIA PRIMA, O PM=PRODUCCION Y MAT. PRIMA, VACIO=TODO')
         retorno:=.f.
         exit
     ENDIF
    NEXT
 ENDIF
Return retorno
*/

Static Function MenuOpc()
   Local nOpc,uColor:=SetColor(),uSel:=Select()
   Local aMenu:={'Seriales                    ',;
                 'Otros Precios               ',;
                 'Gr fico                     ',;
                 'Existencia por Almac‚n      ',;
                 'Proveedores de este Art¡culo',;
                 'Resumen                     ',;
                 'Activa/Desactiva Art¡culo   ',;
                 'Equivalencias de C¢digo     ',;
                 'Factor de C lculo de Precios',;
                 'Codigo Externo              '}
   DO WHILE .T.
      off_key()
      nOpc:=La_Opcion(aMenu,3,2,10,1,[ MAS OPCIONES ])
      on_key()
      IF LastKey()=27 .OR. nOpc=0
         EXIT
      ENDIF
      DO CASE
         CASE nOpc=1
              Ver_Serial()
         CASE nOpc=2
              Editar_Precio()
         CASE nOpc=3
              Analiinv()
         CASE nOpc=4
              Ver_exalm()
         CASE nOpc=5
              Ver_Provee()
         CASE nOpc=6
              Resumen()
         CASE nOpc=7
              StatusArticulo()
         CASE nOpc=8
              Equivalencias()
         CASE nOpc=9
              Factores()
         CASE nOpc=10
              CodExt()
      ENDCASE
   ENDDO
   SetColor(uColor)
   dbSelectar(uSel)
Return nil


