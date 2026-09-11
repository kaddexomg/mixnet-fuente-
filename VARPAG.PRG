FUNCTION PAG0601
* OPC6[1]=[Cambio fecha]
MOD0601()
RETURN NIL

FUNCTION PAG0602
* OPC6[3]=[Cambio operador]
MOD0603()
RETURN NIL

FUNCTION PAG0603
LOCAL opcsel:=0

if cpa_contm='S'
   opcsel:=La_Opcion({'Contadores Sencillos ',;
                      'Contadores Multiples '},4,30,5,1,[])
endif

IF opcsel=2
  NVOCONTAD()
  RETURN NIL
ENDIF

* CONTADORES
IF ABRIR('NUMORD',MXNUMORD,[E])
 XNUMORD=NUMERO
 IF ABRIR('NUMREQ',MXNUMREQ,[E])
  XNUMREQ=NUMERO
  IF ABRIR('NUMCRP',MXNUMCRP,[E])
     XNUMCRP=NUMERO
   IF ABRIR('NUMABP',MXNUMABP,[E])
      XNUMABP=NUMERO
   IF ABRIR('NUMcpa',MXNUMcpa,[E])
      XNUMcpa=NUMERO
   IF ABRIR('NUMCP2',MXNUMCP2,[E])
      XNUMCP2=NUMERO
   IF ABRIR('NUMPDE',MXNUMPDE,[E])
      XNUMPDE=NUMERO
   IF ABRIR('NUMRQZ',MXNUMRQZ,[E])
      XNUMRQZ=NUMERO
   IF ABRIR('NUMRIV',CE('MXNUMRIV'),[E])
      XNUMRIV=NUMERO
   IF ABRIR('NUMISL',CE('MXNUMISL'),[E])
      XNUMISL=NUMERO
     BOXSOM([ CONTADORES CXP ],8,35,19,70)
     @ 09,36 SAY 'Requisiciones........:' get XNUMRQZ VALID !EMPTY(XNUMRQZ)
     @ 10,36 SAY 'Ordenes de Compra....:' get XNUMORD VALID !EMPTY(XNUMORD)
     @ 11,36 SAY 'Recepciones..........:' get XNUMREQ VALID !EMPTY(XNUMREQ)
     @ 12,36 SAY 'Compras..............:' get XNUMcpa VALID !EMPTY(XNUMcpa)
     @ 13,36 SAY 'Notas de Cr‚dito.....:' get XNUMCRP VALID !EMPTY(XNUMCRP)
     @ 14,36 SAY 'Notas de D‚bito......:' get XNUMPDE VALID !EMPTY(XNUMPDE)
     @ 15,36 SAY 'Comprobantes A/C.....:' get XNUMABP VALID !EMPTY(XNUMABP)
     @ 16,36 SAY 'Comprobantes Cr‚dito.:' get XNUMCP2 VALID !EMPTY(XNUMCP2)
     @ 17,36 SAY 'Comprobante Ret. ISLR:' get XNUMISL VALID !EMPTY(XNUMISL)
     @ 18,36 SAY 'Comprobante Ret. IVA.:' get XNUMRIV VALID !EMPTY(XNUMRIV)
     LEER()
     IF !ESCAPAR()
      ININUM('NUMORD')
      ININUM('NUMREQ')
      ININUM('NUMcpa')
      ININUM('NUMCRP')
      ININUM('NUMABP')
      ININUM('NUMCP2')
      ININUM('NUMPDE')
      ININUM('NUMRQZ')
      ININUM('NUMISL')
      ININUM('NUMRIV')
      AUDITORIA([Acceso a la opci¢n de contadores (Salvados)])
    ENDIF
    ENDIF
    ENDIF
    ENDIF
   ENDIF
  ENDIF
 ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
RETURN NIL

FUNCTION PAG0604
LOCAL XLIB1,XLIB2,XLIB3,XLIB4,XLIB5,XLIB6
XLIB1=PAD(G_LIBORD,8)
XLIB2=PAD(G_LIBREC,8)
XLIB3=PAD(G_LIBCPA,8)
XLIB4=PAD(G_LIBDPA,8)
XLIB5=PAD(G_LIBRQC,8)
XLIB6=PAD(G_LIBETI,8)

BOXSOM(" CATALOGOS ",8,35,15,77,2)
SETCOLOR(C_TEX2)
@  9,37 SAY [Ordenes de compra..........:] GET XLIB1
@ 10,37 SAY [Recepciones................:] GET XLIB2
@ 11,37 SAY [Compras....................:] GET XLIB3
@ 12,37 SAY [Pagos/Abonos/Cancelaciones.:] GET XLIB4
@ 13,37 SAY [Requisiciones..............:] GET XLIB5
@ 14,37 SAY [Etiquetas..................:] GET XLIB6
LEER()
IF !ESCAPAR()
 G_LIBORD=ALLTRIM(XLIB1)
 G_LIBREC=ALLTRIM(XLIB2)
 G_LIBCPA=ALLTRIM(XLIB3)
 G_LIBDPA=ALLTRIM(XLIB4)
 G_LIBRQC=ALLTRIM(XLIB5)
 G_LIBETI=ALLTRIM(XLIB6)

 SAVE TO LIBREP.MEM ALL LIKE G_LIB*
 AUDITORIA([Acceso a la opci¢n de catalogos (Salvados)])
ENDIF
RETURN NIL

FUNCTION PAG0605
Private PVFILE:=CE([MXCOMPPG.MTD]),xLI:=4,xCI:=1,ucpa_cciva
IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF
uCpa_cciva:=cpa_cciva
BOXSOM(" Par metros generales  | PgDn = Mas par metros",xLI,xCI,xLI+17,xCI+59,2)
SETCOLOR(C_TEX2)
@ ++xLI,xCI+1 SAY [ ¨ Es obligatorio el c¢digo contable ?.........(S/N):] GET cpa_codcon PICT '!' VALID cpa_codcon$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Traer el c¢digo contable de Ficha Prov. ?...(S/N):] GET cpa_codfic PICT '!' VALID cpa_codfic$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Activar edici¢n de cambio ?.................(S/N):] GET cpa_edcamb PICT '!' VALID cpa_edcamb$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Selecciona el costo por proveedor ?.........(S/N):] GET cpa_cospro PICT '!' VALID cpa_cospro$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Activar Impresi¢n de Etiquetas ?............(S/N):] GET cpa_etiq   PICT '!' VALID cpa_etiq$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Permite duplicar item en renglones ?........(S/N):] GET cpa_ditems PICT '!' VALID cpa_ditems$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Filtrar en reng. Productos por Proveedor ?..(S/N):] GET cpa_propro PICT '!' VALID cpa_propro$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Pedir Proveedor en rengl¢n de Requisici¢n?..(S/N):] GET cpa_pedpro PICT '!' VALID cpa_pedpro$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Filtrar Plan contable por  Auxiliares ?.....(S/N):] GET cpa_filcon PICT '!' VALID cpa_filcon$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Activar retenci¢n de I.S.L.R. desde CxP ?...(S/N):] GET cpa_islr   PICT '!' VALID cpa_islr$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Ubicarse en campo bulto de reng. en doc. ?..(S/N):] GET cpa_pbul   PICT '!' VALID cpa_pbul$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Chequear Movimientos en compras ?...........(S/N):] GET cpa_chm    PICT '!' VALID cpa_chm$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Servicios como opci¢n inicial en reng. ?....(S/N):] GET cpa_serv   PICT '!' VALID cpa_serv$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Genera N/C por Ret.Iva al hacer la compra.?.(S/N):] GET cpa_reti   PICT '!' VALID cpa_reti$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Actualizar Precios desde Compras ?..........(S/N):] GET cpa_actp   PICT '!' VALID cpa_actp$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Preguntar por calculo de Retenci¢n de IVA ?.(S/N):] GET cpa_pregr  PICT '!' VALID cpa_pregr$'SN'
Leer()
xLI:=4
BOXSOM(" Mas par metros generales ",xLI,xCI,xLI+9,xCI+59,2)
@ ++xLI,xCI+1 SAY [ ¨ Activar Contadores Multiples ?..............(S/N):] GET cpa_contm  PICT '!' VALID cpa_contm$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Permitir eliminar NC por Retenci¢n de IVA ?.(S/N):] GET cpa_blknc  PICT '!' VALID cpa_blknc$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Pedir m s datos en la orden de compra ?.....(S/N):] GET cpa_mdoc   PICT '!' VALID cpa_mdoc$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Filtrar maestro de proveedor desactivados ?.(S/N):] GET cpa_fprov  PICT '!' VALID cpa_fprov$'SN'
@ ++xLI,xCI+1 SAY [ En pagar compras     S = Seleccionar   E = Eliminar:] GET cpa_SelEli PICT '!' VALID cpa_SelEli$'SE'
@ ++xLI,xCI+1 SAY [ Proveedor asignado al Pago de I.S.L.R.........:] GET cpa_codmih
@ ++xLI,xCI+1 SAY [ C¢digo contable para Ret. IVA Esp.:] GET cpa_cciva VALID IF(CPA_CODCON="S",MIREF_CON(@CPA_CCIVA),.T.)
leer()
IF !ESCAPAR()
 SAVE TO (PVFILE) ALL LIKE cpa_*
 AUDITORIA([MODIFICACION Y ALTERACION DE PARAMETROS],cpa_cciva,ucpa_cciva,'MOD')
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF
RETURN NIL

FUNCTION PAG0606
edit_ventana('EDCRET')
RETURN NIL

FUNCTION PAG0607
mod0605()
RETURN NIL

FUNCTION PAG0608
RETURN(ASIGNA_COLOR())

PROCEDURE PAG0311
 EDITOR_DEF()
RETURN










