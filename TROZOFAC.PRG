
** Graba el iva en el archivo de intercambio
DBSELECTAR('MXRENFAC')
IF DBSEEK(XNUMFAC)
   DO WHILE NUMFAC=XNUMFAC .AND. !eof()
      IF IVA$'ABCD'
         cIVA:=IVA
         nPor:=iva(cIva,xemision)
         nBas:=TOT_REN
         nIva:=TOT_REN*nPor/100
         GrabarIva(cIva,nPor,nBas,nIva)
      ENDIF
      Dbskip()
   ENDDO
ENDIF
Dbselectar(uSel)
RETURN

STATIC FUNCTION GRABARIVA(cIva,nPor,nBas,nIva)
   Local uSel:=Select()
   IF !File(ce('MXREGIVA.DBF'))
      Cre_res('MXREGIVA.INI',CE('MXREGIVA.DBF'))
      INDLISTA('MXREGIVA')
   ENDIF
   IF ABRIR('MXREGIVA',ce('MXREGIVA.DBF'),'S',ce('MXREIVX1.NTX'))
      DBAPPEND()
      REPLACE CLIPRO   WITH XCLIENTE,;
              EMISION  WITH XEMISION,;
              NRORIF   WITH XCIF,;
              RAZSOC   WITH XNOMCLI,;
              NUMFAC   WITH XNUMFAC,;
              NCONTROL WITH XNCONTROL,;
              TIPTRAN  WITH XTIPTRANS,;
              TOTCIVA  WITH XTOT_FAC,;
              BASEIMP  WITH nBas,;
              PORIVA   WITH nPor,;
              IMPIVA   WITH nIva,;
              IVA      WITH cIva
      GMoneda(xcambio)
      CERRAR('MXREGIVA')
   ENDIF
   DbSelectar(uSel)
RETURN NIL

