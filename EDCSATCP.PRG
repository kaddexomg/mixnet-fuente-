PROCEDURE SATELCPA
PARAMETERS M_TITUL,campo1,campo2, SELPROG, LA_LISTA
PRIVATE UCTL4,UCTL5
private opch:=6

DECLARE RSUBTIT[ 2]
SUBSPA()

uctl4=ctl_men[4]
uctl5=ctl_men[5]
store .f. to ctl_men[4],ctl_men[5]
ctl_men[5]:=.t.

EDC([SPA],M_TITUL,RSUBTIT,.f.,.t.,.T.,.F.,.T.,.F.,.F.,.F.,.T.,'')
ctl_men[4]=uctl4
ctl_men[5]=uctl5
RETURN

PROCEDURE SUBSPA
RSUBTIT[ 1]=[N£mero  ]
RSUBTIT[ 2]=[Fecha                          ]
RETURN

PROCEDURE MASSPA
* MASCARA DE LA VENTANA
LINSIM()
RETURN

PROCEDURE DETSPA
PRIVATE C1,C2
* DETALLE
C1:=&CAMPO1
C2:=&CAMPO2
C2:=DTOC(C2)
IF C1$LA_LISTA
 SETCOLOR(c_tit3)
ENDIF
@ CONTLIN, 2+MARGEN SAY PAD(C1+' '+C2,COLDER-COLIZQ-1)
SETCOLOR(c_TEX3)

RETURN

PROCEDURE CRDSPA
* COORDENADAS DE LA VENTANA
TOPE= 7
FONDO=14
COLIZQ= 8
COLDER=49
*COLDER=37
RETURN


PROCEDURE SELSPA
* SELECCION
DO &SELPROG
RETURN

PROCEDURE BUSSPA
* BUSQUEDA EN SATELITE
PRIVATE TNOMBRE_CLI,BNOMBRE_CLI,TNUMCEX,BNUMCEX
VERTBUS([UBUSSPA])
RETURN

PROCEDURE UBUSSPA
* BUSQUEDA
BNOMBRE_CLI=SPACE(30)
BNUMCEX    =SPACE(8)
@ CONTLIN, 2+MARGEN GET BNUMCEX
//@ CONTLIN, 33+MARGEN GET BNOMBRE_CLI
LEER()
SALIR=ESCAPAR()
IF !SALIR
// TNOMBRE_CLI=UPPER(TRIM(BNOMBRE_CLI))
 TNUMCEX    =UPPER(TRIM(BNUMCEX))
 //BCONDICION=IF(EMPTY(TNOMBRE_CLI),[.T.],[TNOMBRE_CLI$UPPER(NOMBRE_CLI)])
 //BCONDICION:=BCONDICION+IF(EMPTY(TNUMCEX),[],[.AND.TNUMCEX$UPPER(NUMCEX)])
 BCONDICION:=[TNUMCEX$UPPER((&campo1))]
 POR_INDICE= .F.
ENDIF
RETURN
******************************************************************


***********************************************************************************************************************
***********************************************************************************************************************
***********************************************************************************************************************
***********************************************************************************************************************
***********************************************************************************************************************
***********************************************************************************************************************
PROCEDURE SATELRQC
PARAMETERS M_TITUL,campo1,campo2,campo3, SELPROG, LA_LISTA
PRIVATE UCTL4,UCTL5
private opch:=6

DECLARE RSUBTIT[ 3]
SUBRQC()
uctl4=ctl_men[4]
uctl5=ctl_men[5]
store .f. to ctl_men[4],ctl_men[5]
ctl_men[5]:=.T.
EDC([RQC],M_TITUL,RSUBTIT,.f.,.t.,.T.,.F.,.T.,.F.,.F.,.F.,.T.,'')
ctl_men[4]=uctl4
ctl_men[5]=uctl5
RETURN

PROCEDURE SUBRQC
RSUBTIT[1]=PAD([N£mero],12)
RSUBTIT[2]=pad([Fecha],12)
RSUBTIT[3]=[Cod. Dpto.]
RETURN

PROCEDURE MASRQC
* MASCARA DE LA VENTANA
LINSIM()
RETURN

PROCEDURE DETRQC
PRIVATE C1,C2
* DETALLE
C1:=&CAMPO1
C2:=&CAMPO2
C2:=DTOC(C2)
C3:=&CAMPO3
IF C1$LA_LISTA
 SETCOLOR(c_tit3)
ENDIF
@ CONTLIN, 2+MARGEN SAY PAD(C1+'   '+C2+'       '+C3,COLDER-COLIZQ-1)
SETCOLOR(c_TEX3)
RETURN

PROCEDURE CRDRQC
* COORDENADAS DE LA VENTANA
TOPE= 7
FONDO=14
COLIZQ= 8
COLDER=45
*COLDER=37
RETURN

PROCEDURE SELRQC
* SELECCION
DO &SELPROG
RETURN

PROCEDURE BUSRQC
* BUSQUEDA EN SATELITE
PRIVATE TNOMBRE_CLI,BNOMBRE_CLI,TNUMCEX,BNUMCEX
VERTBUS([UBUSSPA])
RETURN

PROCEDURE UBUSRQC
* BUSQUEDA
BNOMBRE_CLI=SPACE(30)
BNUMCEX    =SPACE(8)
@ CONTLIN, 2+MARGEN GET BNUMCEX
//@ CONTLIN, 33+MARGEN GET BNOMBRE_CLI
LEER()
SALIR=ESCAPAR()
IF !SALIR
// TNOMBRE_CLI=UPPER(TRIM(BNOMBRE_CLI))
 TNUMCEX    =UPPER(TRIM(BNUMCEX))
 //BCONDICION=IF(EMPTY(TNOMBRE_CLI),[.T.],[TNOMBRE_CLI$UPPER(NOMBRE_CLI)])
 //BCONDICION:=BCONDICION+IF(EMPTY(TNUMCEX),[],[.AND.TNUMCEX$UPPER(NUMCEX)])
 BCONDICION:=[TNUMCEX$UPPER((&campo1))]
 POR_INDICE= .F.
ENDIF
RETURN

