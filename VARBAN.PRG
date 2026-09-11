FUNCTION BAN0601
* OPC6[1]=[Cambio fecha]
MOD0601()
RETURN NIL

FUNCTION BAN0602
* OPC6[3]=[Cambio operador]
MOD0603()
RETURN NIL

FUNCTION BAN0603
 local cFile:=CE('MXCONBAN.MTD')
 PRIVATE cban_dep:='DP00000001'
 PRIVATE cban_che:='CH00000001'
 PRIVATE cban_tra:='TR00000001'
 PRIVATE cban_cre:='NC00000001'
 PRIVATE cban_deb:='ND00000001'
 PRIVATE cban_anu:='AN00000001'
 PRIVATE cban_caj:='CJ00000001'

 IF file(cFile)
    RESTORE FROM (cFile) ADDITIVE
 ENDIF
 BOXSOM([ CONTADORES DE MOVIMIENTO BANCARIO ],5,15,13,57)
 cur_on()
 @  6,16 SAY 'Contador de dep¢sitos......:' get cban_dep VALID !EMPTY(cban_dep)
 @  7,16 SAY 'Contador de Cheques........:' get cban_che VALID !EMPTY(cban_che)
 @  8,16 SAY 'Contador de Transferencias.:' get cban_tra VALID !EMPTY(cban_tra)
 @  9,16 SAY 'Contador de Nota de cr‚dito:' get cban_cre VALID !EMPTY(cban_cre)
 @ 10,16 SAY 'Contador de Nota de d‚bito.:' get cban_deb VALID !EMPTY(cban_deb)
 @ 11,16 SAY 'Contador de Anulaci¢n......:' get cban_anu VALID !EMPTY(cban_anu)
 @ 12,16 SAY 'Contador de Caja...........:' get cban_caj VALID !EMPTY(cban_caj)
 READ
 cur_off()
 IF !ESCAPAR()
    SAVE ALL LIKE CBAN_* TO (cFile)
    AUDITORIA([Acceso a modificaci¢n de contadores bancarios],,,[MOD])
 ENDIF
RETURN NIL


FUNCTION BAN0604
* OPC6[4]=[Cat logos]
PRIVATE xlib1
XLIB1:=PAD(G_LIBTBA,8)

BOXSOM("",9,35,11,67,2)
SETCOLOR(C_TEX2)
@ 10,37 SAY [Movimientos: ] GET XLIB1
LEER()
IF !ESCAPAR()
 G_LIBTBA=ALLTRIM(XLIB1)
 SAVE TO LIBREP.MEM ALL LIKE G_LIB*
 AUDITORIA([Acceso a modificaci¢n de catalogos de bancos],,,[MOD])
ENDIF
RETURN NIL

FUNCTION BAN0605
RETURN(PARAM_BAN())

FUNCTION BAN0606
* OPC6[5]=[Cambio Ejercicio]
MOD0605()
RETURN NIL

FUNCTION BAN0607
RETURN(ASIGNA_COLOR())

FUNCTION BAN0308
 EDITOR_DEF()
RETURN NIL

FUNCTION Param_ban()
*** VARIABLE GENERALES DE BANCOS
/*
BAN_CODC := 'N'
BAN_CHCE := 'S'
BAN_EDIC := 'S'
BAN_TIDB := 0.00
BAN_EIDB := PAD([tipban$'CH.ND'.and.fecha_mov>=deb_desde.and.fecha_mov<=deb_hasta],200)
BAN_VCCO := 'S'
BAN_CCON := SPACE(20)
BAN_BCON := 'S'
BAN_EREG := 'N'
BAN_IMPB := 'N'
BAN_EITF := 'N'
*/
private BANFILE:=CE([MXBAN.MTD])

IF FILE(BANFILE)
 RESTORE FROM (BANFILE) ADDITIVE
ENDIF

BOXSOM(" Par metros de bancos ",5,7,19,59,2)
SETCOLOR(C_TEX2)
@  6,8 SAY [ ¨ Activar lista de c¢digo contable  ?.(S/N):] GET BAN_CODC PICT '!' VALID BAN_CODC$'SN'
@  7,8 SAY [ ¨ Valida c¢digo contable            ?.(S/N):] GET BAN_VCCO PICT '!' VALID BAN_VCCO$'SN'
@  8,8 SAY [ ¨ Permite cheques con importe = 0   ?.(S/N):] GET BAN_CHCE PICT '!' VALID BAN_CHCE$'SN'
@  9,8 SAY [ ¨ Permite editar n£mero de cheque   ?.(S/N):] GET BAN_EDIC PICT '!' VALID BAN_EDIC$'SN'
@ 10,8 SAY [ ¨ Bloquear los bancos al Conciliar  ?.(S/N):] GET BAN_BCON PICT '!' VALID BAN_BCON$'SN'
@ 11,8 SAY [ ¨ Eliminar Registro otros origenes  ?.(S/N):] GET BAN_EREG PICT '!' VALID BAN_EREG$'SN'
@ 12,8 SAY [ ¨ Mostrar Servicios en Caja ?.........(S/N):] GET BAN_MSER PICT '!' VALID BAN_MSER$'SN'
@ 13,8 SAY if(glb_cimpban='D',[ ¨ Implementar el calculo de I.D.B.  ?.(S/N):],;
                              [ ¨ Implementar el calculo de I.T.F.  ?.(S/N):]) GET BAN_IMPB PICT '!' VALID BAN_IMPB$'SN'
@ 14,8 SAY [ ¨ Eliminar Registro de I.T.F. ?.......(S/N):] GET BAN_EITF PICT '!' VALID BAN_EITF$'SN'
@ 15,8 SAY if(glb_cimpban='D',[ % de Impuesto al d‚bito bancario (IDB).....:],;
                              [ Porcentaje a retener por I.T.F..............]) GET BAN_TIDB PICT '99.99'  WHEN BAN_IMPB='S'
@ 16,8 SAY if(glb_cimpban='D',[ C¢digo contable para el IDB:],;
                              [ C¢digo contable para el ITF:]) GET BAN_CCON  WHEN BAN_IMPB='S' VALID valrefcon(@BAN_CCON)
@ 17,8 SAY if(glb_cimpban='D',[ Expresi¢n para el c lculo del IDB:],;
                              [ Expresi¢n para el c lculo del ITF:])
@ 18,9 GET BAN_EIDB PICT '@S49'  WHEN BAN_IMPB='S'

leer()
IF !ESCAPAR()
 SAVE TO (BANFILE) ALL LIKE BAN_*
 AUDITORIA([Acceso a modificar parametros (Salvados)])
ENDIF
RETURN NIL












