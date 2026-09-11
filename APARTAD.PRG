Function VEN0309()
   Apartados()
Return nil

* Edicion de apartados
Function Apartados()
   Private ii_listasi:=''
   Private es_oferta:=.f.
   Private xTPCORR := 'APA'
   Private cFILNUM := ce('MXNUMAPA')
   //Private bufect:=G_CAMINO+[BUFECT]+estacion+[.DBF]
   CARGACORR(XTPCORR)  // NUEVA FUNCION PARA LA CARGA DE LOS CORRELATIVOS DEL DOC.. 11.3.2005
   Edit_ventana('ENCAPA')
Return nil

Function AbonosApa()
   Local SelAnt:=Select(),p_ant:=SaveScreen(0,0,24,80),ColAnt:=SetColor()
   Private p1,doc_origen,tip_origen,xmultican,xconcepto,ximporte,yconcepto,xcoddpto:=Space(10),xnumorg
   Private xfp,xformapag,xnumpag,xnompag,xcodtar,xnumapa,xcliente,xnumfac,xnum_doc
   Private xcodcaj,xemision,xhora,xcambio,nResto:=0,xcondpag:=1,xtot_fac,xabono
   Private bufecb:=g_camino+[BUFECB]+estacion+[.DBF],modifica:=.f.,pnumorg
   Private lnumfac:='',lorigen:='',lhora:=''
   if estatus='OK'
      Pare('Este apartado ha sido pasado a factura, no acepta mas abonos')
      Return nil
   endif
   nResto:=(tot_apa-abonos)
   if nresto<=0
      Pare('Apartado ha sido cancelado en su totalidad')
      Return nil
   endif
   xNumpag   := Space(16)
   xNompag   := Space(16)
   xcodtar   := Space(3)
   xcodcaj   := ustr_caja
   xmultican := .f.
   xconcepto := Pad('ABONO POR APARTADO DE MERCANCIA',34)
   yconcepto := Pad(xconcepto,34)
   xformapag := SPACE(2)
   xnumapa   := MXENCAPA->numapa
   xnumfac   := xnumapa
   xnum_doc  := xnumfac
   xnumorg   := xnumfac
   xcliente  := MXENCAPA->cliente
   xemision  := Ctod(g_fecha)
   xhora     := StrTran(Time(),':')
   xcambio   := cambioMon(xemision,xhora)
   doc_origen:= 'APA'
   tip_origen:= 'AB'
   pnumorg   := xnum_doc
   xabono    := 0
   lnumfac   := xnumapa
   lorigen   := 'APA'
   lhora     := xhora
   Off_key()
   p1:=SaveScreen(9,31,12,56)
   BOXSOM([ MONTO DEL ABONO ],9,31,11,55)
   cur_on()
   @ 10,32 SAY [  Abono:] COLOR c_tex3 GET nResto PICT [999999999.99] COLOR c_inv3 VALID !Empty(nResto)
   READ
   cur_off()
   RestScreen(9,31,12,56,p1)
   if lastkey()=27
      Return nil
   endif
   xtot_fac := nResto
   ximporte := xtot_fac
   xmontot  := ximporte
   For_pag(1)
   if !lastkey()=27
      xfp=if(xformapag='EF',1,if(xformapag='CH',2,if(xformapag='TA',3,if(xformapag='DP',4,if(xformapag='CB',5,0)))))
      if abrir('ABONOS',g_camino+'MXABOAPA.DBF','S',g_camino+'MXABAPX1.NTX')
         DbAppend()
         AUDITORIA([Acceso a mantenimiento de abonos en apartado],xcliente+xnumapa+dtoc(xemision),,[INC])
         REPLACE numapa WITH xnumapa,cliente WITH xcliente,fp WITH Str(xfp,1),;
            formapag WITH xformapag,numpag WITH xnumpag,nompag WITH xnompag,;
            codtar WITH xcodtar,importe WITH nResto,num_caj WITH xcodcaj,;
            emision WITH xemision,hora WITH xhora
         GMoneda(xcambio)
         COMMIT
         if xformapag='CB'
            GrabaComb()
            DELETE FILE (bufecb)
         endif
         InterArc(xnumapa,xnumapa)
         Cerrar('ABONOS')
         DbSelectar(selant)
         if Rec_Lock(0)
            REPLACE MXENCAPA->abonos WITH MXENCAPA->abonos+nResto
            UNLOCK
            COMMIT
         endif
      endif
   endif
   On_key()
   RestScreen(0,0,24,80,p_ant)
   SetColor(ColAnt)
   DbSelectar(selant)
Return nil

Function EliAboApa()
   Local selant:=Select()
   Private lcodcaj,lemision,ltip_origen,lhora,doc_origen,modifica:=.t.
   Private lnumero_cxc,lcliente,lnumfac,lemision,ltipcre,;
      lcondpag,lformapag,lnumpag,lnompag,labono,xtipdocan,lnumfac
   if abrir([MXTRACAJ],MXTRACAJ,[S],MXTRCAX1,MXTRCAX2,MXTRCAX3)
      if abrir('ABONOS',g_camino+'MXABOAPA.DBF','S',g_camino+'MXABAPX1.NTX')
         if DbSeek(MXENCAPA->numapa)
            do while numapa=mxencapa->numapa
               lcodcaj     := num_caj
               lnumero_cxc := numapa
               lcliente    := cliente
               lnumfac     := numapa
               lemision    := emision
               ltipcre     := '1'
               lcondpag    := 1
               lformapag   := formapag
               lnumpag     := numpag
               lnompag     := nompag
               labono      := importe
               xtipdocan   := ' '
               doc_origen  := 'APA'
               ltip_origen := 'AB'
               lhora       := hora
               Interev()
               DbSelectar('ABONOS')
               AUDITORIA([Acceso a mantenimiento de abonos en apartado],cliente+numapa+dtoc(emision),,[INC])
               if rlock()
                  DELETE
                  UNLOCK
               endif
               DbSkip()
            enddo
         endif
         Cerrar('ABONOS')
      endif
      Cerrar('MXTRACAJ')
   endif
   DbSelectar(selant)
Return nil


*****************************************************************************
*****************************************************************************
*****************************************************************************
Function FApartad()
   Off_Key()
   Selapartad()
   On_Key()
Return nil

Static Function SelApartad()
   Parameters a1
   Local aPend:={},aRecno:={},selant:=Select(),xabono,sel:=1
   if abrir('ENCAPA',CF('MXENCAPA.DBF'),'S',CF('MXENAPX3.NTX'))
      if DbSeek(Pad(xcliente,8))
         Do while cliente=xcliente .and. !eof()
            if !estatus$'OK'
               AaDd(aPend,numapa+' '+dtoc(emision)+' '+codven+' '+Trans(tot_apa,lat12)+' '+Tran(abonos,lat12)+' '+Tran(tot_apa-abonos,lat12))
               AaDd(aRecno,Recno())
            endif
            DbSkip()
         Enddo
      endif
      if Len(aPend)=0
         Pare('No hay apartados pendientes')
         Cerrar('ENCAPA')
         DbSelectar(selant)
         Return nil
      endif
      titco:=Pad('N£mero',8)+' '+Pad('Emisi¢n',10)+' Vend.      Importe       Abonos        Saldo'
      do while .t.
         sel:=BoxSel(aPend,5,5,1,' SELECCION DE APARTADO ',titco,,sel)
         if sel=0
            EXIT
         endif
         DbSelectar('ENCAPA')
         DbGoto(aRecno[sel])
         xnumapa:=numapa
         xabono  := abonos
         DbSelectar('BUFFER')
         RegT:=Recno()
         LOCATE FOR numapa=xnumapa
         if found()
            DbGoto(RegT)
            Pare('Apartado No. '+Alltrim(xnumapa)+', ya ha sido incluido en la factura')
            LOOP
         endif
         DbGoto(RegT)
         EXIT
      enddo
      BufTemp:=g_camino+'BUFAPA'+if(!Empty(estacion),estacion,'Z')+'.DBF'
      if abrir('RENAPA',CF('MXRENAPA.DBF'),'S',CF('MXREAPX1.NTX'))
         if DbSeek(xnumapa)
            listaapa:=listaapa+xnumapa+'/'
            COPY TO (BufTemp) WHILE numapa=xnumapa
            DbSelectar('BUFFER')
            APPEND FROM (buftemp)
            DbGotop()
            do while !Eof()
               if inv_divaf='S' .and. numapa=xnumapa
                  el_iva:=E_Importe(Iva(BUFFER->iva,xemision),xemision)
                  BUFFER->precio:=Div(BUFFER->precio,((el_iva/100)+1))
                  BUFFER->tot_ren:=BUFFER->precio*BUFFER->cantidad
                  DbCommit()
               endif
               DbSkip()
            enddo
            if xabono>0
               DbAppend()
               BUFFER->item:='-ABONOAPA'
               BUFFER->descrip:='Abono apartado: '+xnumapa
               BUFFER->cantidad:=1
               BUFFER->precio:=-xabono
               BUFFER->tot_ren:=-xabono
               BUFFER->numapa :=xnumapa
               BUFFER->emision:=xemision
            endif
            SUM tot_ren TO xtot_net
            DbGotop()
            NoFlecha()
            Pripag()
            RecaFac()
            Cerrar('RENAPA')
         endif
      endif
      Cerrar('ENCAPA')
      DbSelectar(selant)
      if !getrengact
         on_key()
      else
         KEYBOARD escape
      endif
      Return nil
   endif
Return nil

Function ActApa(reverso)
   Local qNumero,qLista,Selant:=Select()
   IF !empty(listaapa)
      qlista=listaapa
      MENSAJ("Actualizando apartado")
      if abrir('RENAPA',CF('MXRENAPA.DBF'),'S',CF('MXREAPX1.NTX'))
         if abrir('ENCAPA',CF('MXENCAPA.DBF'),'S',CF('MXENAPX1.NTX'))
            do while !Empty(qlista)
               qnumero=Trim(Substr(qlista,1,8))
               if DbSeek(qnumero)
                  REC_LOCK(0)
                  REPLACE ESTATUS WITH if(reverso=1,'PE','OK'),NUMFAC WITH if(reverso=1,Space(8),XNUMFAC)
                  UNLOCK
                  DbSelectar('renapa')
                  if DbSeek(qnumero)
                     do while numapa=qnumero .AND. !eof()
                        REC_LOCK(0)
                        REPLACE ESTATUS WITH if(reverso=1,'PE','OK'),NUMFAC WITH if(reverso=1,Space(8),XNUMFAC)
                        UNLOCK
                        DbSkip()
                     enddo
                  endif
               endif
               DbSelectar('encapa')
               qlista=StrTran(qlista,Substr(qlista,1,9))
            enddo
            Cerrar('ENCAPA')
         endif
         Cerrar('RENAPA')
      endif
   endif
   DbSelectar(selant)
Return nil


