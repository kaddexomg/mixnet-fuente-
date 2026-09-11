FUNCTION MIXNET03()  // ESTA FUNCION PERMITE EL ACCESO AL DLL (NO BORRAR)
RETURN NIL

FUNCTION VEN0601
 * OPC6[1]=[Cambio fecha]
 MOD0601()
RETURN NIL

FUNCTION VEN0602
 * OPC6[3]=[Cambio operador]
 MOD0603()
RETURN NIL

FUNCTION VEN0603
LOCAL opcsel:=0, nLin:=4,nCol:=16

PRIVATE MULTIFILE:=CE([MXMUL]+GESTION+[.MEM])

if vtas_contm='S'
   opcsel:=La_Opcion({'Contadores Sencillos ',;
                      'Contadores Multiples '},4,30,5,1,[])
endif

IF opcsel=2
  NVOCONTAD()
  RETURN NIL
ENDIF

IF FILE(MULTIFILE)
 RESTORE FROM (MULTIFILE) ADDITIVE
ELSE
 PROX_MULTI=[CM000001]
ENDIF
* CONTADORES
IF ABRIR('NUMCEX',MXNUMCEX,[E])
 XNUMCEX=NUMERO
IF ABRIR('NUMCOT',MXNUMCOT,[E])
 XNUMCOT=NUMERO
IF ABRIR('NUMPED',MXNUMPED,[E])
 XNUMPED=NUMERO
 IF ABRIR('NUMALB',MXNUMALB,[E])
  XNUMALB=NUMERO
  IF ABRIR('NUMFAC',MXNUMFAC,[E])
    XNUMFAC=NUMERO
   IF ABRIR('NUMRAP',MXNUMRAP,[E])
    XNUMRAP=NUMERO
    IF ABRIR('NUMCRE',MXNUMCRE,[E])
     XNUMCRE=NUMERO
    IF ABRIR('NUMABN',MXNUMABN,[E])
     XNUMABN=NUMERO
    IF ABRIR('NUMCR2',MXNUMCR2,[E])
     XNUMCR2=NUMERO
    IF ABRIR('NUMDEB',MXNUMDEB,[E])
     XNUMDEB=NUMERO
    IF ABRIR('NUMAPA',CE('MXNUMAPA.DBF'),[E])
     XNUMAPA=NUMERO
    IF ABRIR('NUMCTV',CE('MXNUMCTV.DBF'),[E])
     XNUMCTV=NUMERO
    IF ABRIR('NUMCPV',CE('MXNUMCPV.DBF'),[E])
     XNUMCPV=NUMERO
    IF ABRIR('NUMNCR',CE('MXNCRETI.DBF'),[E])
     XNUMNCR=NUMERO
    IF ABRIR('NUMCNC',CE('MXNUMCNC.DBF'),[E])
      XNUMCNC=NUMERO
     CERRAR('NUMCNC')
     BoxTurn(BOXSOM([ CONTADORES ],nLin,nCol,nLin+17,nCol+39),.f.)
     @ ++nLin,nCol+1 SAY 'Cotizaciones..............:' get XNUMCOT VALID !EMPTY(XNUMCOT)
     @ ++nLin,nCol+1 SAY 'Pedidos...................:' get XNUMPED VALID !EMPTY(XNUMPED)
     @ ++nLin,nCol+1 SAY if(g_tpedne='N',;
                         'Notas de entrega..........:',;
                         'Requisiciones.............:') get XNUMALB VALID !EMPTY(XNUMALB)
     @ ++nLin,nCol+1 SAY 'Apartados.................:' get XNUMAPA VALID !EMPTY(XNUMAPA)
     @ ++nLin,nCol+1 SAY 'Facturas al Mayor.........:' get XNUMFAC VALID !EMPTY(XNUMFAC)
     @ ++nLin,nCol+1 SAY 'Punto de Ventas...........:' get XNUMRAP VALID !EMPTY(XNUMRAP)
     @ ++nLin,nCol+1 SAY 'Cotizacion Express........:' get XNUMCEX VALID !EMPTY(XNUMCEX)
     @ ++nLin,nCol+1 SAY 'Notas de Cr‚dito..........:' get XNUMCRE VALID !EMPTY(XNUMCRE)
     @ ++nLin,nCol+1 SAY 'Comprobantes Cr‚dito......:' get XNUMCR2 VALID !EMPTY(XNUMCR2)
     @ ++nLin,nCol+1 SAY 'Notas de D‚bito...........:' get XNUMDEB VALID !EMPTY(XNUMDEB)
     @ ++nLin,nCol+1 SAY 'Comprobantes C/A..........:' get XNUMABN VALID !EMPTY(XNUMABN)
     @ ++nLin,nCol+1 SAY 'Cancelaci¢n Multiple......:' get PROX_MULTI VALID !EMPTY(PROX_MULTI)
     @ ++nLin,nCol+1 SAY 'Num.Control Factura Mayor.:' get XNUMCTV VALID !EMPTY(XNUMCTV)
     @ ++nLin,nCol+1 SAY 'Num.Control Punto de venta:' get XNUMCPV VALID !EMPTY(XNUMCPV)
     @ ++nLin,nCol+1 SAY 'Retenci¢n Iva.............:' get XNUMNCR VALID !EMPTY(XNUMNCR)
     @ ++nLin,nCol+1 SAY 'Num.Control N/Cr‚ditos....:' get XNUMCNC VALID !EMPTY(XNUMCNC)
     leer()
     IF !ESCAPAR()
      ININUM([NUMCEX])
      ININUM([NUMCOT])
      ININUM('NUMPED')
      ININUM('NUMALB')
      ININUM('NUMFAC')
      ININUM('NUMRAP')
      ININUM('NUMCRE')
      ININUM('NUMABN')
      ININUM('NUMCR2')
      ININUM('NUMDEB')
      ININUM('NUMAPA')
      ININUM('NUMCTV')
      ININUM('NUMCPV')
      ININUM('NUMNCR')
      ABRIR('NUMCNC',CE('MXNUMCNC.DBF'),[E])
      ININUM('NUMCNC')
      CERRAR()
      SAVE TO (MULTIFILE) ALL LIKE PROX_MULTI
      AUDITORIA([Acceso a la opci¢n de contadores (Modificados)])
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
ENDIF
ENDIF
ENDIF
ENDIF
ENDIF
RETURN NIL

FUNCTION VEN0604
* DEFINICIONES
XLIB0=PAD(G_LIBCOT,8)
XLIB1=PAD(G_LIBPED,8)
XLIB2=PAD(G_LIBALB,8)
XLIB3=PAD(G_LIBFAC,8)
XLIB4=PAD(G_LIBRAP,8)
XLIB5=PAD(G_LIBCOB,8)
XLIB6=PAD(G_LIBAPA,8)
XLIB7=PAD(G_LIBCEX,8)

BOXSOM("",7,35,16,77,2)
SETCOLOR(C_TEX2)
@  8,37 SAY [Cotizaciones Express.......:] GET XLIB7
@  9,37 SAY [Cotizaciones...............:] GET XLIB0
@ 10,37 SAY [Pedidos....................:] GET XLIB1
@ 11,37 SAY if(g_tpedne='N',[Notas de entrega...........:],[Requisiciones..............:]) GET XLIB2
@ 12,37 SAY [Facturas...................:] GET XLIB3
@ 13,37 SAY [Punto de ventas............:] GET XLIB4
@ 14,37 SAY [Cobros/Abonos/Cancelaciones:] GET XLIB5
@ 15,37 SAY [Apartados..................:] GET XLIB6
leer()
IF !ESCAPAR()
 G_LIBCOT=ALLTRIM(XLIB0)
 G_LIBPED=ALLTRIM(XLIB1)
 G_LIBALB=ALLTRIM(XLIB2)
 G_LIBFAC=ALLTRIM(XLIB3)
 G_LIBRAP=ALLTRIM(XLIB4)
 G_LIBCOB=ALLTRIM(XLIB5)
 G_LIBAPA=ALLTRIM(XLIB6)
 G_LIBCEX=ALLTRIM(XLIB7)
 SAVE TO LIBREP.MEM ALL LIKE G_LIB*
 AUDITORIA([Acceso a la opci¢n de librerias (Modificadas)])
ENDIF

RETURN  NIL

FUNCTION VEN0605
local m
BOXSOM(' PARAMETROS ',6,38,14,57)
m:=mix_menu2({{ 7,39,' Cotizaciones     '},;
              { 8,39,' Pedidos          '},;
              { 9,39,if(g_tpedne='N',' Notas de Entrega ',' Requisici¢n      ')},;
              {10,39,' Facturas Mayor   '},;
              {11,39,' Facturas P.V.    '},;
              {12,39,' Ctas x Cobrar    '},;
              {13,39,' Generales        '}})
if m<>0
 do case
 case m=1
  par_cot()
 case m=2
  par_ped()
 case m=3
  par_alb()
 case m=4
  PAR_FAC()
 case m=5
  par_pvt()
 case m=6
  PAR_CXC()
 case m=7
  PAR_GEN()
 endcase
endif
RETURN

Static Function par_cxc
private PVFILE:=CE([MXCXC.MTD]),xLI:=7,xCI:=3
private PV_ADPP:= 'S'
PRIVATE PV_cdpp := SPACE(5)
private PV_ndcd:= 'S'
private PV_linc:= 'N'
Private clvcxc
Private pv_ccriva:=space(20)
IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF
clvant:=pv_cdpp
BOXSOM(" Par metros CxC ",XLI,XCI,XLI+5,XCI+60,2)
SETCOLOR(C_TEX2)
SETCURSOR(1)
xxli:=xli+1
@ ++XLI,xCI+1 SAY [ ¨ Activar descuento por pronto pago... ? (S/N):] GET PV_adpp  PICT '!' VALID pv_adpp$'SN'
@ ++XLI,xCI+1 SAY [ ¨ Activar N.D. por Cheque Devuelto.... ? (S/N):] GET PV_ndcd  PICT '!' VALID PV_ndcd$'SN'
@ ++XLI,xCI+1 SAY [ ¨ Activar N.C. por lotes en CxC....... ? (S/N):] GET PV_linc  PICT '!' VALID PV_linc$'SN'
@ ++XLI,xCI+1 SAY [ CODIGO DE CUENTA DE RET. ESP. IVA:] GET pv_ccriva
LEER()
SETCURSOR(0)
IF LASTKEY() <> 27
 SAVE TO (PVFILE) ALL LIKE PV_*
ENDIF
RETURN NIL

Static FUNCTION par_fac
private PVFILE:=CE([MXFAP.MTD]),xLI:=xLi1:=4,xCI:=1,xxli1,xxli2,xxli3,xxli4
private PV_PROG := SPACE(8)
private PV_IMP  := 'S'
private PV_EDN  := 'N'
private PV_ENC  := 'N'
private PV_CHM  := 'S'
private PV_PVP  := 'S'
private PV_PAUSA := 'N'
private PV_REP   :=SPACE(8)
private PV_ME   :='N'
private PV_mul  :='S'
private PV_NEG  :='S'
private PV_desnot  :='S'
Private PV_descar  := 'S'
Private PV_codcon  := 'N'
Private PV_BLOQLCR := 'N'
Private PV_CLAVLCR := SPACE(5)
Private PV_DRG     := "S"
Private PV_EDRG    := "S"
Private PV_EDFE    := "S"
Private pv_limd    := 'N'
Private pv_blodesc := 'N'
Private pv_clavblo := Space(5)
Private pv_limdesc := 'N'
Private pv_clavdesc:= Space(5)
Private pv_porcdesc:= 0.00
Private PV_ABRECAJ := 'N'
Private PV_PUERTO  := '1'
Private PV_RENGPAG := 0
Private PV_ACTNCNC := 'S'    // ACTIVAR NUMERO DE CONTROL POR NOTAS DE CREDITO DIF. AL DE LA FACTURA
Private pv_refconri  := space(20)
Private pv_refconuxm := space(20)
Private pv_cpfinal   := 'S'
Private pv_edcli      := 'S'
Private pv_edvdd      := 'S'
Private pv_edvdd      := 'S'
Private pv_edtrp      := 'S'
Private pv_edtar      := 'S'


IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF
BOXSOM(" Par metros factura ",XLI,XCI,XLI+17,XCI+77,2)
SETCOLOR(C_TEX2)
SETCURSOR(1)
@ ++xLI,xCI+1 SAY [ ¨ Modifica/elimina factura ?(S/N):] GET PV_ME  PICT '!' VALID PV_me$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar No. de factura ?...(S/N):] GET PV_EDN PICT '!' VALID PV_EDN$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar No. de control ?...(S/N):] GET PV_ENC PICT '!' VALID PV_ENC$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar Fecha de Emisi¢n ?.(S/N):] GET PV_EDFE PICT '!' VALID PV_EDFE$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar Nombre de Cliente?.(S/N):] GET PV_EDCLI PICT '!' VALID PV_EDCLI$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar C¢digo Vendedor ?..(S/N):] GET PV_EDVDD PICT '!' VALID PV_EDVDD$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar Transporte ?.......(S/N):] GET PV_EDTRP PICT '!' VALID PV_EDTRP$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar Tarifa ?...........(S/N):] GET PV_EDTAR PICT '!' VALID PV_EDTAR$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar el precio ?........(S/N):] GET PV_PVP PICT '!' VALID PV_PVP$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Editar desc. rengl¢n ? ...(S/N):] GET PV_EDRG PICT '!' VALID PV_EDRG$'SN' WHEN PV_DRG="S"
@ ++xLI,xCI+1 SAY [ ¨ Descargar de almac‚n ? ...(S/N):] GET PV_descar PICT '!' VALID PV_descar$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Facturar sin existencia ?.(S/N):] GET PV_Neg PICT '!' VALID PV_NEG$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Multip. cantidad*bulto ?..(S/N):] GET PV_mul PICT '!' VALID PV_mul$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Calc. desc. por rengl¢n ?.(S/N):] GET PV_DRG PICT '!' VALID PV_DRG$'SN'
@ ++xLI,xCI+1 SAY [ ¨ Limitar desc. rengl¢n ?...(S/N):] GET PV_limd PICT '!' VALID PV_limd$'SN' WHEN pv_edrg="S"
@ ++xLI,xCI+1 SAY [ ¨ Chequear Movim. en Fact ?.(S/N):] GET PV_CHM PICT '!' VALID PV_CHM$'SN'
xli:=xli1
@ ++xLI,xCI+39 SAY [ ¨ No. Control Dif. en N/C ?.(S/N):] GET PV_ACTNCNC PICT '!' VALID PV_ACTNCNC$'SN'
@ ++xLI,xCI+39 SAY [¨ Desglosar ]+if(g_tpedne='N',[N/E],[REQ])+[ en factura ?.(S/N):] GET PV_desnot PICT '!' VALID PV_desnot$'SN'
@ ++xLI,xCI+39 SAY [¨ Activar contabi. en reng.?.(S/N):] GET PV_codcon PICT '!' VALID PV_codcon$'SN'
@ ++xLI,xCI+39 SAY [¨ Imprimir factura ? ........(S/N):] GET PV_IMP PICT '!' VALID PV_IMP$'SN'
@ ++xLI,xCI+39 SAY [¨ Pausa de impresi¢n ? ......(S/N):] GET PV_pausa PICT '!' VALID PV_pausa$'SN'
xxli1:=xli
@ ++xLI,xCI+39 SAY [¨ Activar limite de cr‚dito ?(S/N):] GET PV_BLOQLCR PICT '!' VALID ActPassw(@pv_bloqlcr,@pv_clavlcr,xxli1,xci+39)
xxli2:=xli
@ ++xLI,xCI+39 SAY [¨ Bloquear descuentos ?......(S/N):] GET PV_BLODESC PICT '!' VALID ActPassw(@pv_blodesc,@pv_clavblo,xxli2+1,xci+39)
xxli3:=xli
@ ++xLI,xCI+39 SAY [¨ Limitar descuento General ?(S/N):] GET PV_LIMDESC PICT '!' VALID ActPassw(@pv_limdesc,@pv_clavdesc,xxli3+1,xci+39)
@ ++xLI,xCI+39 SAY [¨ Activar apertura de caja  ?(S/N):] GET PV_ABRECAJ PICT '!' VALID PV_ABRECAJ$'SN'
@ ++xLI,xCI+39 SAY [¨ Cond. Pago Final Factura  ?(S/N):] GET PV_CPFINAL PICT '!' VALID PV_CPFINAL$'SN'

@ ++xLI,xCI+39 SAY [# de Puerto 1=COM1, 2=COM2, 3=COM3:] GET PV_PUERTO  PICT '9' VALID PV_PUERTO$'123'

@ ++xLI,xCI+39 SAY [% de Descuento General.....:] GET PV_PORCDESC PICT '999.99' VALID pv_porcdesc>=0
@ ++xLI,xCI+39 SAY [Formato de impresi¢n ......:] GET PV_REP  PICT '@!'
@ ++xLI,xCI+39 SAY [Renglones por Pagina.......:] GET PV_RENGPAG PICT '99' VALID PV_RENGPAG >= 0
@ ++xLI,xCI+39 SAY [Cta. Ret. Iva...:] GET PV_REFCONRI  VALID valrefcon(@PV_REFCONRI)
@ ++xLI,xCI+39 SAY [Cta. Ret. 1x1000:] GET PV_REFCONUXM VALID valrefcon(@PV_REFCONUXM)
LEER()
//@ ++xLI,xCI+39 SAY [Programa para abrir la caja:] GET PV_PROG PICT '@!'
SETCURSOR(0)
IF LASTKEY() <> 27
  pv_refconri  := PAD(pv_refconri,20)
  pv_refconuxm := PAD(pv_refconuxm,20)
 SAVE TO (PVFILE) ALL LIKE PV_*
 AUDITORIA([Acceso a la opci¢n de parametros de facturas (Modificados)])
ENDIF
RETURN NIL

** Cambio de ejercicio
FUNCTION VEN0606
mod0605()
RETURN NIL

FUNCTION VEN0607
RETURN(ASIGNA_COLOR())

static FUNCTION par_cot
private PVFILE:=CE([MXCOT.MTD])

private PV_IMP  := 'S'
private PV_EDN  := 'N'
private PV_PVP  := 'S'
private PV_PAUSA := 'N'
private PV_REP   :=SPACE(8)
private PV_ME   :='N'
private pv_codcon:='S'
private pv_rengpag := 0
IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF

BOXSOM(" Par metros de cotizaci¢n ",9,14,18,75,2)
SETCOLOR(C_TEX2)
@ 10,15 SAY [ ¨ Permite modificar o eliminar cotizaci¢n ?(S/N):] GET PV_ME  PICT '!' VALID PV_me$'SN'
@ 11,15 SAY [ ¨ Permite editar el n£mero de cotizaci¢n ? (S/N):] GET PV_EDN PICT '!' VALID PV_EDN$'SN'
@ 12,15 SAY [ ¨ Permite editar el precio ? ..............(S/N):] GET PV_PVP PICT '!' VALID PV_PVP$'SN'
@ 13,15 SAY [ ¨ Activar c¢digo contable en renglones.....(S/N):] GET PV_codcon PICT '!' VALID PV_codcon$'SN'
@ 14,15 SAY [ ¨ Imprimir cotizaci¢n ? ...................(S/N):] GET PV_IMP PICT '!' VALID PV_IMP$'SN'
@ 15,15 SAY [ ¨ Pausa de impresi¢n ? ....................(S/N):] GET PV_pausa PICT '!' VALID PV_pausa$'SN'
@ 16,15 say [ Formato de impresi¢n ...........................:] GET PV_REP  PICT '@!'
@ 17,15 SAY [Renglones por Pagina.............................:] GET PV_RENGPAG PICT '99' VALID PV_RENGPAG >= 0
LEER()
IF !ESCAPAR()
 SAVE TO (PVFILE) ALL LIKE PV_*
 AUDITORIA([Acceso a la opci¢n de parametros de cotizaci¢n (Modificados)])
ENDIF
RETURN NIL

STATIC FUNCTION par_ped
private PVFILE:=CE([MXPED.MTD])
private PV_IMP  := 'S'
private PV_EDN  := 'N'
private PV_PVP  := 'S'
private PV_PAUSA := 'N'
private PV_REP   :=SPACE(8)
private PV_ME   :='N'
private pv_codcon:='S'
private pv_cccli :='N'
private pv_asc   :='N'
Private pv_edrg  :='S'
Private pv_rengpag := 0

Private xlin := 7

IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF

BOXSOM(" Par metros pedido ",xLin,15,19,75,2)
SETCOLOR(C_TEX2)
@ ++xLin,17 SAY [ ¨ Permite modificar o eliminar pedido ?..(S/N):] GET PV_ME  PICT '!' VALID PV_me$'SN'
@ ++xLin,17 SAY [ ¨ Permite editar el n£mero de pedido ? ..(S/N):] GET PV_EDN PICT '!' VALID PV_EDN$'SN'
@ ++xLin,17 SAY [ ¨ Permite editar el precio ? ............(S/N):] GET PV_PVP PICT '!' VALID PV_PVP$'SN'
@ ++xLin,17 SAY [ ¨ Permite editar el descuento en rengl¢n.(S/N):] GET PV_EDRG PICT '!' VALID PV_EDRG$'SN'
@ ++xLin,17 SAY [ ¨ Activar c¢digo contable en renglones...(S/N):] GET PV_codcon PICT '!' VALID PV_codcon$'SN'
@ ++xLin,17 SAY [ ¨ Asigna el costo promedio al precio ?...(S/N):] GET pv_asc PICT '!' VALID pv_asc$'SN'
@ ++xLin,17 SAY [ ¨ Asigna el c¢digo contable del cliente..(S/N):] GET PV_cccli PICT '!' VALID PV_cccli$'SN'
@ ++xLin,17 SAY [ ¨ Imprimir pedido ? .....................(S/N):] GET PV_IMP PICT '!' VALID PV_IMP$'SN'
@ ++xLin,17 SAY [ ¨ Pausa de impresi¢n ? ..................(S/N):] GET PV_pausa PICT '!' VALID PV_pausa$'SN'
@ ++xLin,17 say [ Formato de impresi¢n .........................:] GET PV_REP  PICT '@!'
@ ++xLin,17 SAY [Renglones por Pagina...........................:] GET PV_RENGPAG PICT '99' VALID PV_RENGPAG >= 0
LEER()

IF !ESCAPAR()
 SAVE TO (PVFILE) ALL LIKE PV_*
 AUDITORIA([Acceso a la opci¢n de parametros de pedidos (Modificados)])
ENDIF
RETURN NIL

static FUNCTION par_alb
private PVFILE:=CE([MXALB.MTD]),xLin:=9,xCol:=3

private PV_IMP   := 'S'
private PV_EDN   := 'N'
private PV_PVP   := 'S'
private PV_PAUSA := 'N'
private PV_REP   := SPACE(8)
private PV_ME    := 'N'
Private pv_asc   := 'N'
Private pv_codcon:= 'S'
Private pv_cccli := 'N'
Private pv_actcxc:= 'N'
Private pv_rengpag := 0
IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF

BOXSOM(' Par metros '+g_tedne+' ',xLin,xCol,xLin+12,xCol+Len(g_tedne)+52,2)
SETCOLOR(C_TEX2)
@ ++xLin,xCol+1 SAY [ ¨ Permite modificar o eliminar ]+g_tedne+[ ? (S/N):] GET PV_ME  PICT '!' VALID PV_me$'SN'
@ ++xLin,xCol+1 SAY [ ¨ Permite editar el n£mero de ]+g_tedne+[ .? (S/N):] GET PV_EDN PICT '!' VALID PV_EDN$'SN'
@ ++xLin,xCol+1 SAY [ ¨ Permite editar el precio ? ................(S/N):] GET PV_PVP PICT '!' VALID PV_PVP$'SN'
@ ++xLin,xCol+1 SAY [ ¨ Asigna el costo promedio al precio ? ......(S/N):] GET PV_asc PICT '!' VALID PV_asc$'SN'
@ ++xlin,xcol+1 SAY [ ¨ Activar c¢digo contable en renglones.......(S/N):] GET PV_codcon PICT '!' VALID PV_codcon$'SN'
@ ++xlin,xcol+1 SAY [ ¨ Asigna el c¢digo contable del cliente......(S/N):] GET PV_cccli PICT '!' VALID PV_cccli$'SN'
@ ++xLin,xCol+1 SAY [ ¨ Imprimir ]+g_tedne+[ ? ....................(S/N):] GET PV_IMP PICT '!' VALID PV_IMP$'SN'
@ ++xLin,xCol+1 SAY [ ¨ Pausa de impresi¢n ? ......................(S/N):] GET PV_pausa PICT '!' VALID PV_pausa$'SN'
@ ++xLin,xCol+1 SAY [ ¨ Actualizar Cuentas por Cobrar ?............(S/N):] GET PV_actcxc PICT '!' VALID PV_actcxc$'SN'
@ ++xLin,xCol+1 say [ Formato de impresi¢n .............................:] GET PV_REP  PICT '@!'
@ ++xLin,xCol+1 SAY [Renglones por Pagina...............................:] GET PV_RENGPAG PICT '99' VALID PV_RENGPAG >= 0
LEER()
IF !ESCAPAR()
 SAVE TO (PVFILE) ALL LIKE PV_*
 AUDITORIA([Acceso a la opci¢n de parametros de notas de entregas (Modificados)])
ENDIF
RETURN NIL

static FUNCTION par_gen
PVFILE:=CE([MXVTASPG.MTD])
xCol:=2
xLin:=5
IF FILE(PVFILE)
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF
BOXSOM(" Par metros generales ",xLin,xCol,xLin+16,xCol+73,2)
SETCOLOR(C_TEX2)
@ xLin+01,xCol+1 SAY [ ¨ Validar el c¢digo de cuenta contable ? ............... (S/N):] GET vtas_codcon PICT '!' VALID vtas_codcon$'SN'
@ xLin+02,xCol+1 SAY [ ¨ Activar edici¢n de cambio ? ...........................(S/N):] GET vtas_edcamb PICT '!' VALID vtas_edcamb$'SN'
@ xLin+03,xCol+1 SAY [ ¨ muestra el c¢digo alterno en selecci¢n de art¡culos ?  (S/N):] GET vtas_codalt PICT '!' VALID vtas_codalt$'SN'
@ xLin+04,xCol+1 SAY [ ¨ Activar autorizaci¢n de pedidos ?......................(S/N):] GET vtas_autor  PICT '!' VALID ActPassw(vtas_autor,@vtas_cautor,xLin+3,xCol+30)
@ xLin+05,xCol+1 say [ ¨ Filtrar maestro de clientes Desactivados ?.............(S/N):] GET vtas_descli PICT '!' VALID vtas_desclit$'SN'
@ xLin+06,xCol+1 SAY [ ¨ Activar evaluaci¢n de ofertas ?........................(S/N):] GET vtas_acto PICT '!' VALID vtas_acto$'SN'
@ xLin+07,xCol+1 say [ ¨ Desea controlar almac‚n por Rengl¢n ?..................(S/N):] GET vtas_almren PICT '!' VALID vtas_almren$'SN'
@ xLin+08,xCol+1 say [ ¨ Controlar lista de precios por almac‚n ?...............(S/N):] GET vtas_lisalm PICT '!' VALID vtas_lisalm$'SN'
@ xLin+09,xCol+1 say [ ¨ Filtrar Plan contable por Cuentas Auxiliares ?.........(S/N):] GET vtas_filcon PICT '!' VALID vtas_filcon$'SN'
@ xLin+10,xCol+1 say [ ¨ Actualizar pedidos solo por cantidad sin bultos ?......(S/N):] GET vtas_pedxca PICT '!' VALID vtas_pedxca$'SN'
@ xLin+11,xCol+1 SAY [ ¨ Ubicarse en campo bulto de renglones en documentos ?...(S/N):] GET vtas_pbul   PICT '!' VALID vtas_pbul$'SN'
@ xLin+12,xCol+1 SAY [ ¨ Servicios como opci¢n inicial en renglones ?...........(S/N):] GET vtas_serv   PICT '!' VALID vtas_serv$'SN'
@ xLin+13,xCol+1 SAY [ ¨ Activar Contadores Multiples ?.........................(S/N):] GET vtas_contm  PICT '!' VALID vtas_contm$'SN'
@ xLin+14,xCol+1 SAY [ ¨ Mostrar Mas datos de cliente por defecto en Documentos?(S/N):] GET vtas_mdcli  PICT '!' VALID vtas_mdcli$'SN'
@ xLin+15,xCol+1 say [ opci¢n Nota de entrega Titulo: R=Requisici¢n, N=Nt./Entrega...:] GET vtas_tedne  PICT '!' VALID vtas_tedne$'RN'

leer()

IF !ESCAPAR()
 SAVE TO (PVFILE) ALL LIKE VTAS_*
 AUDITORIA([Acceso a la opci¢n de parametros generales (Modificados)])
 RESTORE FROM (PVFILE) ADDITIVE
ENDIF
g_tedne:=if(vtas_tedne='R','Requisici¢n','Nt./Entrega')
g_tpedne:=vtas_tedne

OPC3[ 8]=IF(SYSPANIA,[Albar n],if(g_tpedne='N','Notas de entrega','Requisiciones'))
OPC4[14]=IF(SYSPANIA,[Albaranes],if(g_tpedne='N','Notas de entrega','Requisiciones'))
OPC5[4]=[Facturar ] +if(syspania,[albaranes],if(g_tpedne='N',g_tedne,'Requisiciones'))

RETURN NIL

FUNCTION VEN0312
 COBROPAGO()
RETURN

FUNCTION VEN0314
 EDITOR_DEF()
RETURN

** Tipo de Documento para el contador
Function MenuContad()
   Local cTipos,aTipos,nOpc
   IF ES_COB
      cTipos:='COT/PED/ALB/APA/FAC/PVE'
      aTipos:={'COTIZACIONES         ',;
               'PEDIDOS              ',;
               'NOTAS DE ENTREGA     ',;
               'APARTADOS            ',;
               'FACTURAS MAYOR       ',;
               'FACTURAS P.V.        '}
   ELSE
      cTipos:='RQZ/ORD'
      aTipos:={'REQUISICIONES        ',;
               'ORDEN DE COMPRA      '}
   ENDIF
   nOpc := La_Opcion(aTipos,ContLin+1,ColIzq+1,IF(ES_COB,6,2),2,[ TIPO DE DOCUMENTO ])
Return (if(lastkey()<>27,Substr(cTipos,4*(nOpc-1)+1,3),Space(3)))

























