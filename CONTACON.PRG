* PROCESO DE CONTABILIZACION
PROCEDURE CON0504
   DECLARE ULT_REC[15],ULT_IND[15],ABIERTA[15]
   AFILL(ABIERTA,.F.)
   PRIVATE XASIENTO,XFECHA_ASI, XDESC_ASI,XDEBITO,XCREDITO,XSIGNO,ycamotr
   PRIVATE II_LISTASI,I_MANUAL,TIP_SEL,FECHA_INI,FECHA_FIN,UF_FILE,q_lista:=''
   PRIVATE ZONA,GRUPO,CODCON,ALICLI,ALI,PATHNOM,G_CAMNOM,g_CAMNTR
   PRIVATE LAPRIMERAVEZ, UFECHA_INI,UFECHA_FIN, encontrado:=.f., diaria, d_fecha_fin
   PRIVATE xORGDATO
   LAPRIMERAVEZ=.T.

   ***********************************************************
   DECLARE FICHERO[40],A1[40]
   DECLARE rFUENTE[20],DESTINO[20],RELACION[20]
   PRIVATE ULT_A1,ULT_A31,ULT_A4,CAMINO
   ***********************************************************
   ULT_A1=0
   PATHNOM=G_CAMINO+[CAMNOM.NOM]
   G_CAMNOM=[]
   G_CAMNTR=[]
   IF FILE(PATHNOM)
      RESTORE FROM (PATHNOM) ADDITIVE
   ENDIF
   G_CAMNOM=PAD(G_CAMNOM,40)
   G_CAMNTR=PAD(G_CAMNTR,40)
   SAVE SCREEN
   SETCOLOR(C_TEX1)
   if syspania
      BOXSOM([],8,2,10,75,1)
      @ 9,3 SAY [    UBICACION OTROS DATOS:] GET G_CAMNTR
   else
      BOXSOM([],7,2,10,75,1)
      @ 8,3 SAY [UBICACION DATOS DE NOMINA:] GET G_CAMNOM
      @ 9,3 SAY [    UBICACION OTROS DATOS:] GET G_CAMNTR
   endif
   CUR_ON()
   READ
   CUR_OFF()
   RESTORE SCREEN
   G_CAMNOM=TRIM(G_CAMNOM)
   G_CAMNTR=TRIM(G_CAMNTR)
   SAVE TO (PATHNOM) ALL LIKE G_CAMN??

   II_LISTASI=[]
   *
   BOXSOM([ CONTABILIZAR ],6,2,9,33)
   SETCOLOR(C_TEX1)
   @ 7,3 PROMPT ' Todas las definiciones       '
   @ 8,3 PROMPT ' Selecci¢n manual             '
   MENU TO MODO_ARC
   IF MODO_ARC=0
      RETURN
   ENDIF
   I_MANUAL=MODO_ARC=2
   IF I_MANUAL
      TIP_SEL=3
      ARCHIMIN()
      q_lista:=ii_listasi
      IF EMPTY(II_LISTASI)
         RETURN
      ENDIF
   ENDIF
   MENSAJ([Indique el intervalo de fechas a contabilizar])

   STORE CTOD(G_FECHA) TO M->FECHA_INI,M->FECHA_FIN

   UF_FILE=G_CAMINO+[MXUFEC.MEM]

   IF FILE(UF_FILE)
      RESTORE FROM (UF_FILE) ADDITIVE
   ELSE
      STORE FECNUL TO UFECHA_INI,UFECHA_FIN
   ENDIF

   BOXSOM([ ULTIMO PROCESO ],10,2,13,33)
   @  11,4 SAY [Desde: ]+DTOC(UFECHA_INI)
   @  12,4 SAY [Hasta: ]+DTOC(UFECHA_FIN)
   BOXSOM([ FECHAS ],14,2,17,33)
   @  15,4 SAY [Desde:] GET M->FECHA_INI VALID !EMPTY(M->FECHA_INI)
   @  16,4 SAY [Hasta:] GET M->FECHA_FIN VALID !EMPTY(M->FECHA_FIN) .AND. M->FECHA_FIN>=M->FECHA_INI
   CUR_ON()
   READ
   CUR_OFF()
   IF ESCAPAR()
      RETURN
   ENDIF
   IF M->FECHA_INI<=UFECHA_FIN
      PARE([PRECAUCION: Fecha inicial menor a fecha final de la £ltima contabilizaci¢n])
   ENDIF

   INTERDESC=SUBSTR(DTOC(M->FECHA_INI),1,5)+IF(M->FECHA_INI<>M->FECHA_FIN,[-]+SUBSTR(DTOC(M->FECHA_FIN),1,5),[])

   IF DECIS([¨ Desea generar asientos diferidos ?])=[S]
      ZERO   := DECIS([¨ Generar documentos con importe igual a cero ?],1)=[S]
      DIARIA := DECIS([¨ Contabilizaci¢n diaria ?],1)=[S]
      IF DECIS([¨ Desea continuar con el proceso ?])=[S]
         MENSAJ(MEN3)
         IF ABRIR([REGLAS],MXREGLAS,[E],MXRELA)
            IF ABRIR([PENCON],MXPENCON,[E],MXPECOX1)
               IF ABRIR([PRECON],MXPRECON,[E],MXPRCOX1)
                  IF ABRIR([ENCDEF],MXENCDEF,[E],MXENDEX1)
                     IF ABRIR([RENDEF],MXRENDEF,[E],MXREDEX1)
                        **********************************************************
                        d_fecha_fin:=M->fecha_fin
                        if diaria
                           M->fecha_fin  :=M->fecha_ini
                        endif
                        while M->fecha_fin<=d_fecha_fin
                           INTERDESC=SUBSTR(DTOC(M->FECHA_INI),1,5)+IF(M->FECHA_INI<>M->FECHA_FIN,[-]+SUBSTR(DTOC(M->FECHA_FIN),1,5),[])
                           SELECT ENCDEF
                           if i_manual
                              ii_listasi:=q_lista
                           endif
                           GO TOP
                           BEGIN SEQUENCE
                              DO WHILE !EOF()
                                 CTL_MAN()
                                 XASIENTO  = ASIENTO
                                 XFECHA_ASI= if(!diaria,CTOD(G_FECHA),M->fecha_ini)
                                 XDESC_ASI = TRIM(DESC_ASI)+[ ]+INTERDESC
                                 AUDITORIA([Generando asiento de contabilidad: ]+xdesc_asi,xasiento+' '+dtoc(xfecha_asi),,[INC])
                                 MENSAJ([Generando asiento ]+XASIENTO+[ ]+XDESC_ASI)
                                 XDEBITO :=0
                                 XCREDITO:=0
                                 encontrado:=.f.
                                 SELECT PENCON
                                 SEEK(DTOS(XFECHA_ASI)+XASIENTO)
                                 IF FOUND()
                                    if DECIS([Asiento de fecha ]+DTOC(XFECHA_ASI)+[ No.]+XASIENTO+[ ya existe. ¨ Desea concatenar ?],1)=[N]
                                       SELE PRECON
                                       SEEK(DTOS(XFECHA_ASI)+XASIENTO)
                                       DELETE WHILE FECHA_ASI=XFECHA_ASI .AND. ASIENTO=XASIENTO
                                       SELECT PENCON
                                       IF rlock()
                                          replace debitos with 0 , creditos with 0
                                          dbunlock()
                                       ENDIF
                                    endif
                                    encontrado:=.t.
                                 ENDIF
                                 SELECT RENDEF
                                 SEEK(XASIENTO)
                                 DO WHILE !EOF() .AND. ASIENTO=XASIENTO
                                    XREGLA    := REGLA
                                    XSIGNO    := IF(SIGNO=[-],-1,1)
                                    XCONCEPTO := CONCEPTO
                                    XCODMOV   := CODMOV
                                    XCODDPTO  := CODDPTO
                                    XREFER    := REFER
                                    MENSAJ([Generando asiento ]+XASIENTO+[ ]+XDESC_ASI+' C¢digo de Regla: '+xregla)
                                    * POR SI A
                                    IF SUBSTR(XREFER,1,1)$'\/' .OR. SUBSTR(XCONCEPTO,1,1)$"\/" .OR. SUBSTR(XCODDPTO,1,1)$"\/"
                                       MAC_DET(XREGLA,XCODMOV,XREFER,XCONCEPTO,XCODDPTO)
                                    ELSE
                                       IF SUBSTR(XCODMOV,1,1)$[\/]
                                          XCODTEM=TRIM(SUBSTR(XCODMOV,2))
                                          MAC_EXP(XREGLA)
                                       ELSE
                                          MAC_SUM(XREGLA)
                                       ENDIF
                                    ENDIF
                                    SELECT RENDEF
                                    SKIP
                                 ENDDO
                                 CABEZA_ASI()
                                 SELECT ENCDEF
                                 IF !I_MANUAL
                                    SKIP
                                 ENDIF
                              ENDDO
                           END
                           if diaria
                              M->fecha_ini:=M->fecha_ini+1
                              M->fecha_fin:=M->fecha_ini
                           else
                              d_fecha_fin:=ctod('01/01/1960')
                           endif

                        enddo
                        **************************************************************
                        UFECHA_INI=M->FECHA_INI
                        UFECHA_FIN=M->FECHA_FIN
                        SAVE TO (UF_FILE) ALL LIKE UFECHA_???
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   CLOSE DATABASE
RETURN

STATIC PROC CABEZA_ASI
   IF DIFCERO(XDEBITO) .OR. DIFCERO(XCREDITO) .OR. ZERO
      SELECT PENCON
      if !encontrado
         APPEND BLANK
      endif
      IF rlock()
         REPLACE DEBITOS WITH DEBITOS+XDEBITO, CREDITOS WITH CREDITOS+XCREDITO,;
            FECHA_ASI WITH XFECHA_ASI, ASIENTO WITH XASIENTO, DESC_ASI WITH XDESC_ASI
         dbunlock()
      ENDIF
   ENDIF
RETURN

FUNCTION INT_ABRIR(EL_TIPO)
   PRIVATE GG_CAMINO
   IF ycamotr$'N'
      CAMINO:=G_CAMINO
   ENDIF

   IF !EMPTY(xORGDATO) .AND. YCAMOTR$'N '
      GG_CAMINO := G_CAMINO
      g_CAMINO  := xORGDATO
   ENDIF
   DO CASE

      CASE EL_TIPO=[1]
         * FACTURA
         NOMBVEN()
         ABRIR([MXCTACLI],MXCTACLI,[C],MXCTCLX1)
         ABRIR([MXRENFAC],MXRENFAC,[C],MXREFAX1)
         ABRIR([MXENCFAC],MXENCFAC,[C],MXENFAX2)
         SET RELA TO NUMFAC INTO MXRENFAC
         MIENTRAS=[EMISION]

      CASE EL_TIPO$[23]
         * COBROS Y DOCUMENTOS O PAGOS Y DOCUMENTOS
         IF EL_TIPO=[2]
            NOMBVEN()
            ALICLI=[MXCTACLI]
            ALI   =[MXTRACOB]
         ELSE
            NOMBPAG()
            ALICLI=[MXCTAPAG]
            ALI   =[MXTRAPAG]
         ENDIF
         ABRIR(ALICLI,MXCTACLI,[C],MXCTCLX1)
         ABRIR(ALI,MXTRACOB,[C],MXTRCBX1)
         MIENTRAS=[EMISION]

      CASE EL_TIPO=[4]
         * MOVIMIENTO BANCARIO
         NOMBBAN()
         ABRIR([MXCTABAN],MXCTABAN,[C],MXCTBAX1)
         ABRIR([MXTRABAN],MXTRABAN,[C],MXTRBAX1)
         SET RELA TO CODMOVBAN INTO MXCTABAN

         MIENTRAS=[FECHA_MOV]

      CASE EL_TIPO=[5]
         * CAJA
         NOMBBAN()
         ABRIR([MXCTABAN],MXCTABAN,[C],MXCTBAX1)
         ABRIR([MXTRACAJ],MXTRACAJ,[C],MXTRCAX1)
         SET RELA TO CODBAN INTO MXCTABAN
         MIENTRAS=[FECHA_CAJ]

      CASE EL_TIPO=[6]
         * COMPRAS
         DO NOMBPAG
         ABRIR([MXCTACLI],MXCTACLI,[C],MXCTCLX1)
         ABRIR([MXRENCPA],MXRENCPA,[C],MXRECPX1)
         ABRIR([MXENCCPA],MXENCCPA,[C],MXENCPX2)
         SET RELA TO NUMCOM INTO MXRENCPA
         MIENTRAS=[EMISION]

      CASE EL_TIPO=[7]
         * INVENTARIO
         NOMBINV()
         ABRIR([MXCTAINV],MXCTAINV,[C],MXCTINX1)
         ABRIR([MXTRAINV],MXTRAINV,[C],MXTRINX2)
         MIENTRAS=[FECHA_MOV]
      CASE EL_TIPO=[8]
         * NOMINA
         NMHISTRA = G_CAMNOM+[\]+[NMHISTRA.DBF]
         NMHITRX4 = G_CAMNOM+[\]+[NMHITRX4.NTX]
         NMCTATRA = G_CAMNOM+[\]+[NMCTATRA.DBF]
         NMCTTRX1 = G_CAMNOM+[\]+[NMCTTRX1.NTX]
         IF ABRIR([CTATRA],NMCTATRA,[C],NMCTTRX1)
            IF ABRIR([HISTRA],NMHISTRA,[C],NMHITRX4)
               MIENTRAS=[DESDE]
            ELSE
               RETURN .F.
            ENDIF
         ELSE
            RETURN .F.
         ENDIF
      OTHERWISE
         IF CARGA_V(TRIM(YCJO)+[.CJO])
            IF abre_cjo()
               MIENTRAS=upper(INDEXKEY(0))
               IF MIENTRAS=[DTOS]
                  MIENTRAS=STRTRAN(MIENTRAS,"DTOS(")
                  MIENTRAS=SUBSTR(MIENTRAS,1,AT([)],MIENTRAS)-1)
               ENDIF
            ELSE
               RETURN .F.
            ENDIF
         ELSE
            RETURN .F.
         ENDIF
   ENDCASE
   IF !EMPTY(xORGDATO) .AND. YCAMOTR$'N '
      G_CAMINO:=GG_CAMINO
   ENDIF
   MIENTRAS=MIENTRAS+[ <= M->FECHA_FIN]
   SET SOFT ON
   SEEK(DTOS(M->FECHA_INI))
   SET SOFT OFF
RETURN (!EOF())


*******************
FUNCTION INT_CERRAR
   PARAMETER EL_TIPO
   DO CASE
      CASE EL_TIPO=[1]
         * FACTURA
         CERRAR([MXCTACLI])
         CERRAR([MXRENFAC])
         CERRAR([MXENCFAC])
      CASE EL_TIPO$[23]
         * COBROS Y DOCUMENTOS O PAGOS Y DOCUMENTOS
         CERRAR(ALICLI)
         CERRAR(ALI)

      CASE EL_TIPO=[4]
         * MOVIMIENTO BANCARIO
         CERRAR([MXTRABAN])
         CERRAR('MXCTABAN')

      CASE EL_TIPO=[5]
         * CAJA
         CERRAR([MXTRACAJ])
         CERRAR('MXCTABAN')

      CASE EL_TIPO=[6]
         * COMPRAS
         CERRAR([MXCTACLI])
         CERRAR([MXRENCPA])
         CERRAR([MXENCCPA])

      CASE EL_TIPO=[7]
         * INVENTARIO
         CERRAR([MXCTAINV])
         CERRAR([MXTRAINV])

      CASE EL_TIPO=[8]
         * NOMINA
         CERRAR([CTATRA])
         CERRAR([HISTRA])
      OTHERWISE
         CERRAR_V()
   ENDCASE
RETURN 0


*************
FUNCTION RF
   PARAMETER RF_FORMULA
   PRIVATE RES,OUT
   *
   OUT=0
   SELECT MXRENFAC
   DO WHILE !EOF() .AND. NUMFAC=MXENCFAC->NUMFAC
      RES=&RF_FORMULA
      OUT=OUT + RES
      SKIP
   ENDDO
   SELECT MXENCFAC
RETURN ABS(OUT)

FUNCTION RC
   PARAMETER RF_FORMULA
   PRIVATE RES,OUT
   *
   OUT=0
   SELECT MXRENCPA
   DO WHILE !EOF() .AND. NUMCOM=MXENCCPA->NUMCOM
      RES=&RF_FORMULA
      OUT=OUT + RES
      SKIP
   ENDDO
   SELECT MXENCCPA
RETURN ABS(OUT)

*******************************************************************
PROCEDURE ARCHIMIN
   PRIVATE ULT_ALIAS
   DECLARE ULT_REC[10],ULT_IND[10],ABIERTA[10],RSUBTIT[ 2]
   AFILL(ABIERTA,.F.)

   DO PRINSEL
   CTL_MEN[5]=.F.
   CTL_MEN[4]=.F.
   DO SUBMIN
   IF ABRIR([ENCDEF],MXENCDEF,[E],MXENDEX1)
      IF !VACIO()
         KEYBOARD CHR(4)
         DO EDC WITH [MIN],[ ASIENTOS DEFINIDOS ],RSUBTIT,.T.,.T.,.T.,.F.,.T.,.F.,.F.,.F.,.T.,IF(TIP_SEL=2,CMEN,.f.)
      ENDIF
      CERRAR()
   ENDIF
   DO FINSEL
   CTL_MEN[5]=.T.
   CTL_MEN[4]=.T.

RETURN

PROCEDURE SUBMIN
   RSUBTIT[ 1]=[Asiento ]
   RSUBTIT[ 2]=PAD([Descripci¢n asiento],30)
RETURN


PROCEDURE MASMIN
   * MASCARA DE LA VENTANA
   DO LINSIM
RETURN

PROCEDURE DETMIN
   * DETALLE
   PRENSEL()
   @ CONTLIN,2+MARGEN SAY ASIENTO
   @ CONTLIN,11+MARGEN SAY DESC_ASI
   SETCOLOR(C_TEX3)

RETURN

PROCEDURE CRDMIN
   * COORDENADAS DE LA VENTANA
   TOPE= 6
   FONDO=18
   COLIZQ= 7
   COLDER=47
   CONTLIN=TOPE
   MARGEN=COLIZQ-1
RETURN


PROCEDURE SELMIN
   * SELECCION
   DO MULTISEL
RETURN

STATIC FUNCTION RELACON
   PARAMETER LA_FU
   ZONA=SPACE(8)
   GRUPO=SPACE(3)
   CODCON=SPACE(20)
   DO CASE
      CASE LA_FU=[1]
         * FACTURA
         SELECT MXCTACLI
         SEEK(MXENCFAC->CLIENTE)
         IF FOUND()
            ZONA=MXCTACLI->ZONA
            GRUPO=MXCTACLI->GRUPO
            CODCON=MXCTACLI->CODCON
         ENDIF
         SELECT MXENCFAC

      CASE LA_FU$[23]
         * CXC O CXP

         XCODCLI=CODMOVCLI
         SELECT &ALICLI
         SEEK(XCODCLI)
         IF FOUND()
            ZONA=ZONA
            GRUPO=GRUPO
            CODCON=CODCON
         ENDIF
         SELECT &ALI

      CASE LA_FU=[6]
         * COMPRAS
         SELECT MXCTACLI
         SEEK(MXENCCPA->PROVEE)
         IF FOUND()
            ZONA=MXCTACLI->ZONA
            GRUPO=MXCTACLI->GRUPO
            CODCON=MXCTACLI->CODCON
         ENDIF
         SELECT MXENCCPA

      CASE LA_FU=[7]
         * INVENTARIO
         SELECT MXCTAINV
         SEEK(MXTRAINV->CODMOVART)
         IF FOUND()
            GRUPO=MXCTAINV->GRUPO
         ENDIF
         SELECT MXTRAINV

      CASE LA_FU=[8]
         * NOMINA
         SELECT CTATRA
         SEEK(HISTRA->CODTRA)
         SELECT HISTRA
         CODTRA =HISTRA->CODTRA
         CODCON =HISTRA->CODCON
         MONTO  =HISTRA->IMPORTE
         VARIAC_1=HISTRA->VARIAC_1
         VARIAC_2=HISTRA->VARIAC_2
         VARIAC_3=HISTRA->VARIAC_3
         VARIAC_4=HISTRA->VARIAC_4
         VARIAC_5=HISTRA->VARIAC_5

   ENDCASE
RETURN 0

STATIC FUNCTION RPLTACON
   IF rlock()
      REPLACE CODMOV WITH XCODMOV,CODDPTO WITH XCODDPTO,REFER WITH XREFER,CONCEPTO WITH XCONCEPTO
      REPLACE IMPORTE WITH XIMPORTE+IMPORTE
      REPLACE ASIENTO WITH XASIENTO, FECHA_ASI WITH XFECHA_ASI
      dbunlock()
   ENDIF
   IF XIMPORTE>0
      XDEBITO =XDEBITO  + XIMPORTE
   ELSE
      XCREDITO=XCREDITO - XIMPORTE
   ENDIF
RETURN 0

*********************************************************************
STATIC PROC MAC_SUM
   PARAMETER XREGLA
   *********************************
   XIMPORTE=XSIGNO*MAC_LAREN(XREGLA)
   *********************************
   IF DIFCERO(XIMPORTE) .OR. ZERO
      SELECT PRECON
      APPEND BLANK
      RPLTACON()
   ENDIF
RETURN

STATIC FUNCTION MAC_LAREN
   PARAMETER LA_REGLA
   PRIVATE RES,YCONDICION, YFORMULA,YFUENTE,RESULT,OUTPUT, MIENTRAS
   OUTPUT=0
   SELECT REGLAS
   SEEK(LA_REGLA)
   BEGIN SEQUENCE
      IF FOUND()
         YFORMULA=TRIM(FORMULA)
         YCONDICION=IF(EMPTY(CONDICION),[.T.],TRIM(CONDICION))
         YFUENTE=FUENTE
         YCJO   =CJO
         MIENTRAS=[.T.]
         YCAMOTR = CAMOTR
         xORGDATO  := IF(EMPTY(ORGDATO),' ',ALLTRIM(ORGDATO)+'\')
         IF INT_ABRIR(YFUENTE)
            BEGIN SEQUENCE
               DO WHILE !EOF() .AND. &MIENTRAS
                  RELACON(YFUENTE)
                  IF &YCONDICION
                     RESULT=&YFORMULA
                     OUTPUT=OUTPUT + RESULT
                     IF VAL(YFUENTE)<=8
                        IF RLOCK()
                           REPLACE ASIENTO WITH XASIENTO, FECHA_ASI WITH XFECHA_ASI
                           DBUNLOCK()
                        ENDIF

                     ENDIF

                     replAsi()

                  ENDIF
                  SKIP
               ENDDO
            END
         ENDIF
         INT_CERRAR(YFUENTE)

      ENDIF
   END
RETURN ABS(OUTPUT)

STATIC FUNCTION MAC_EXP
   PARAMETER LA_REGLA
   PRIVATE RES,YCONDICION, YFORMULA,YFUENTE,RESULT,OUTPUT, MIENTRAS,SW
   OUTPUT=0
   SELECT REGLAS
   SEEK(LA_REGLA)
   BEGIN SEQUENCE
      IF FOUND()
         YFORMULA   = TRIM(FORMULA)
         YCONDICION = IF(EMPTY(CONDICION),[.T.],TRIM(CONDICION))
         YFUENTE = FUENTE
         ycjo    = cjo
         ycamotr:=camotr
         xORGDATO  := IF(EMPTY(ORGDATO),' ',ALLTRIM(ORGDATO)+'\')
         MIENTRAS=[.T.]
         IF INT_ABRIR(YFUENTE)
            BEGIN SEQUENCE
               DO WHILE !EOF() .AND. &MIENTRAS
                  RELACON(YFUENTE)
                  IF &YCONDICION
                     XSELE=SELECT()
                     * MACRO-EXPANSION DEL CODIGO
                     XCODMOV=SUBSTR(&XCODTEM,1,20)
                     RESULT=&YFORMULA
                     XIMPORTE=ABS(RESULT)*XSIGNO
                     IF VAL(YFUENTE)<=8
                        IF rlock()
                           REPLACE ASIENTO WITH XASIENTO, FECHA_ASI WITH XFECHA_ASI
                           dbunlock()
                        ENDIF
                     ENDIF
                     REPLASI()
                     SELECT PRECON
                     SEEK(DTOS(XFECHA_ASI)+XASIENTO)
                     SW=.T.
                     IF FOUND()
                        DO WHILE FECHA_ASI=XFECHA_ASI.AND.ASIENTO=XASIENTO.AND.!EOF()
                           IF CODMOV==PAD(XCODMOV,20)
                              SW=.F.
                              EXIT
                           ENDIF
                           SKIP
                        ENDDO

                     ENDIF
                     IF DIFCERO(XIMPORTE) .OR. ZERO
                        IF SW
                           APPEND BLANK
                        ENDIF
                        RPLTACON()
                     ENDIF
                     SELECT (XSELE)
                  ENDIF
                  SKIP
               ENDDO
            END
         ENDIF
         INT_CERRAR(YFUENTE)

      ENDIF
   END
RETURN 0

STATIC FUNCTION MAC_DET
   PARAMETER LA_REGLA,MAC_COD,MAC_REF,MAC_CON,MAC_DPT
   PRIVATE RES,YCONDICION, YFORMULA,YFUENTE,RESULT, MIENTRAS,YCJO
   PRIVATE XCODMOV,XCONCEPTO,XREFER,XIMPORTE,XSELE,XCODDPTO
   SELECT REGLAS
   SEEK(LA_REGLA)
   BEGIN SEQUENCE
      IF FOUND()
         YFORMULA   = TRIM(FORMULA)
         YCONDICION = IF(EMPTY(CONDICION),[.T.],TRIM(CONDICION))
         YFUENTE = FUENTE
         ycjo    = cjo
         MIENTRAS=[.T.]
         ycamotr:=camotr
         xORGDATO  := IF(EMPTY(ORGDATO),' ',ALLTRIM(ORGDATO)+'\')
         IF INT_ABRIR(YFUENTE)
            BEGIN SEQUENCE
               DO WHILE !EOF() .AND. &MIENTRAS
                  RELACON(YFUENTE)
                  IF &YCONDICION
                     XSELE=SELECT()
                     XCODMOV   := MAC(MAC_COD)
                     XREFER    := MAC(MAC_REF)
                     XCONCEPTO := MAC(MAC_CON)
                     XCODDPTO  := MAC(MAC_DPT)
                     RESULT=&YFORMULA
                     XIMPORTE=ABS(RESULT)*XSIGNO
                     IF VAL(YFUENTE)<=8
                        IF rlock()
                           REPLACE ASIENTO WITH XASIENTO, FECHA_ASI WITH XFECHA_ASI
                           dbunlock()
                        ENDIF
                     ENDIF

                     ReplAsi()

                     IF DIFCERO(XIMPORTE) .OR. ZERO
                        SELECT PRECON
                        APPEND BLANK
                        RPLTACON()
                     ENDIF
                     SELECT (XSELE)
                  ENDIF
                  SKIP
               ENDDO
            END
         ENDIF
         INT_CERRAR(YFUENTE)
      ENDIF
   END
RETURN 0

STATIC FUNCTION MAC
   PARAMETER EXP
   PRIVATE SAL
   EXP:=ALLTRIM(EXP)
   IF SUBSTR(EXP,1,1)='/'
      SAL:=SUBSTR(EXP,2)
      SAL:=&SAL
   ELSE
      SAL:=EXP
   ENDIF
RETURN SAL

FUNCTION CARGA_V
   PARAMETER CUAL_CJO
   private width,NOTE,J,I,LINES,XLINEA,HANDLE
   HANDLE=FOPEN(CUAL_CJO)
   IF FERROR() <> 0
      PARE([ERROR AL ABRIR EL FICHERO ]+CUAL_CJO)
      RETURN .F.
   ENDIF
   FSEEK(HANDLE,0)
   CAMINO=ALLTRIM(LEE(HANDLE))+[\]
   IF !EMPTY(G_CAMNTR) .AND. !YCAMOTR$'N'
      CAMINO=ALLTRIM(G_CAMNTR)+[\]
   ELSE
      CAMINO:=G_CAMINO
   ENDIF
   IF !EMPTY(xORGDATO) .AND. YCAMOTR$'N '
      CAMINO:=xOrgDato
   ENDIF
   XLINEA=LEE(HANDLE)
   ULT_A1=VAL(SUBSTR(XLINEA,3))
   FOR J=1 TO ULT_A1
      FICHERO[J] = LEE(HANDLE)
      *A1[J]=TRIM(NAME(FICHERO[J]))
      IF SUBSTR(ALLTRIM(FICHERO[J]),1,1)$'/'
         Fichero[j]:=EVAL(blk(substr(fichero[j],2)))
         a1[j]     := 'AREA'+STRZERO(J,2)
      ELSE
         A1[J]=TRIM(NAME(FICHERO[J]))
      ENDIF
   NEXT

   EL_FILE=CAMINO+FICHERO[1]

   FOR I=1 TO ULT_A1
      II=ALLTRIM(STR(I,2))
      PUBLIC ULT_A3&II
      PUBLIC rINDICE&II.[15],rEXPRESIO&II.[15],ST&II.[15]
      XLINEA=LEE(HANDLE)
      K = VAL(SUBSTR(XLINEA,3))
      ULT_A3&II = K
      FOR J=1 TO K
         rINDICE&II.[J]    = LEE(HANDLE)
         IF SUBSTR(ALLTRIM(RINDICE&II.[J]),1,1)$'/'
            RINDICE&II.[J]:=EVAL(blk(substr(rindice&ii.[j],2)))
         ENDIF
         rEXPRESIO&II.[J] = LEE(HANDLE)
         ST&II.[J]        = SUBSTR(LEE(HANDLE),1,1)
      NEXT
   NEXT

   XLINEA=LEE(HANDLE)
   ULT_A4=VAL(SUBSTR(XLINEA,3))

   FOR J=1 TO ULT_A4
      rFUENTE[J]   = LEE(HANDLE)
      RELACION[J] = LEE(HANDLE)
      DESTINO[J]  = LEE(HANDLE)
   NEXT
   FCLOSE(HANDLE)
RETURN .t.

STATIC FUNCTION abre_cjo
   PRIVATE I,II,Q,QQ,J,RTEMP, II_ORDEN:=1
   PRIVATE IND1:=IND2:=IND3:=IND4:=IND5:=IND6:='', NUMIND
   SET DEFAULT TO (ALLTRIM(CAMINO))
   FOR J=1 TO ULT_A1
      IF !ABRIR(A1[J],FICHERO[J],[C])
         SET DEFAULT TO
         RETURN .F.
      ENDIF
   NEXT
   FOR I=2 TO ULT_A1
      II=ALLTRIM(STR(I,2))
      NUMIND=ULT_A3&II
      FOR Q=1 TO NUMIND
         QQ=STR(Q,1)
         IND&QQ=rINDICE&II.[Q]
      NEXT
      *ABRE ARCHIVOS INDICES
      RTEMP=ALLTRIM(A1[I])
      SELECT &rtemp
      IF !SET_INDEX(NUMIND,IND1,IND2,IND3,IND4,IND5,IND6)
         SET DEFAULT TO
         RETURN .F.
      ENDIF
   NEXT
   * ESTABLECE RELACIONES
   EL_SELE=A1[1]
   FOR J=1 TO ULT_A4
      ALI=TRIM(NAME(rFUENTE[J]))
      REL=TRIM(RELACION[J])
      DES=TRIM(NAME(DESTINO[J]))
      SELECT &ALI
      SET RELATION ADDITIVE TO &REL INTO &DES
   NEXT
   SELECT &EL_SELE
   DECLARE ORDEN[1]
   ORDEN[1]:=REXPRESIO1[1]

   IF '.'$rindice1[1]
      rindice1[1]:=quitar_ext(rindice1[1])
   ENDIF

   chkord_N(ALLTRIM(TRIM(fichero[1])),trim(rindice1[1]),ALLTRIM(rexpresio1[1]))
   *chkord_N(ALLTRIM(CAMINO+TRIM(fichero[1])),CAMINO+trim(rindice1[1]),ALLTRIM(rexpresio1[1]))
   SET DEFAULT TO
RETURN .T.

FUNCTION CERRAR_V
   PRIVATE I
   FOR I=1 TO ULT_A1
      CERRAR(A1[I])
   NEXT
RETURN 0

Function ReplAsi()
   local xx
   xx:=FIELDPOSX("ASIENTO")
   if xx > 0
      if rlock()
         REPLACE ASIENTO WITH XASIENTO
         UNLOCK
      endif
   endif
   x1:=FIELDPOSX("FECHA_ASI")
   if xx > 0
      if rlock()
         REPLACE FECHA_ASI WITH XFECHA_ASI
         UNLOCK
      endif
   endif
Return nil

