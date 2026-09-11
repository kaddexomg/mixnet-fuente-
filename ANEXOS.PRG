FUNCTION MIXNET04()  // ESTA FUNCION PERMITE EL ACCESO AL DLL (NO BORRAR)
RETURN NIL

*************************************************************************************
FUNCTION _jjPVP(_jjcos)

if !difcero(_jjcos)
 return
endif

** factores indicados por marycruz garcia el 3/3/17
/*
_FA:=0.65
_FB:=0.40
_FC:=0.30
_FD:=0.25

XPRECIO_A:=ROUND(DIV(_jjcos,_FA,3),2)
XPRECIO_B:=ROUND(DIV(_jjcos,_FB,3),2)
XPRECIO_C:=ROUND(DIV(_jjcos,_FC,3),2)
XPRECIO_D:=ROUND(DIV(_jjcos,_FD,3),2)
*/

XPRECIO_D:=ROUND(DIV(_jjcos,0.7,3),2)
XPRECIO_C:=ROUND(DIV(XPRECIO_D,0.95,3),2)
XPRECIO_b:=ROUND(DIV(XPRECIO_c,0.9,3),2)
XPRECIO_a:=ROUND(DIV(XPRECIO_b,0.65,3),2)

@  9,14 SAY BS(XPRECIO_A)  PICT '9999999999.999'
@ 10,14 SAY BS(XPRECIO_B)  PICT '9999999999.999'
@ 11,14 SAY BS(XPRECIO_C)  PICT '9999999999.999'
@ 12,14 SAY BS(XPRECIO_D)  PICT '9999999999.999'

RETURN

*************************************************************************************

** ocai 21/3/21
function _cualvdd()

*** seguridad por vendedor
private _novdd:='',_ali:=select()

if abrir('MXCLA','MXCLA.MIX','S')
 loca for id=g_id

 if found()
  _novdd:=user
 endif

endif

cerrar('mxcla')

dbselectar(_ali)

return _novdd


*************************************************************************************

** ocai 9/12/21
function _modcos()
private _ite:=spac(15),_nom:=spac(35),_can:=0,_cnu:=0

if ctod(g_fecha)#date()
 pare('La fecha del dia y del Sistema deben coincidir, Verifique')
 return
endif

_yy:=savescreen(8,8,12,75)

if abrir('MXCTAINV',MXCTAINV,'S',MXCTINX1)
 if abrir('MXTRAINV',MXTRAINV,'S',MXTRINX1,MXTRINX2,MXTRINX3)
  if abrir('MXEXTINV',MXEXTINV,'S',MXEXINX1)

   _alm:=almacen

   do while .t.
    boxsom('JJ - Re-Definicion de Costo',8,8,11,74)

    set cursor on
    @ 09,09 say 'CODIGO:'+spac(15)+' NOMBRE:'+spac(35)
    @ 10,09 say 'COSTO UNITARIO:ACTUAL:'+spac(18)+' NUEVO:'+spac(18)
    @ 09,16 get _ite pict '!!!!!!!!!!!!!!!'
    read

    if empty(_ite)
     exit
    endif

    if mxctainv->(dbseek(_ite))
     _ite:=mxctainv->(codart)
     _nom:=left(mxctainv->(nomart),35)
     _can:=mxctainv->(costo_act)
     _cnu:=0
     set cursor off
     @ 09,39 get _nom
     @ 10,31 get _can pict tran18
     clear gets
     set cursor on
     @ 10,56 get _cnu pict '999999999999999.99'
     read
     set cursor off
     @ 10,56 get _cnu pict tran18
     clear gets

     respw:=decis('Procede a Ajustar Costo ?',1)

     if respw='S'
      *** procesamiento del ajuste
      _yy2:=savescreen(22,0,23,79)
      mensaj('... procesando ajuste de costo JJ')
      recalcos(_ite)
      _tocos:=mxctainv->(ACU_COS-ACU_CSA)
      _exact:=mxctainv->(ACU_ENT-ACU_SAL)
      _nucos:=_exact*_cnu
      _ajcos:=_nucos-_tocos
 
      sele mxtrainv
      _nop:=0
      _siva:=.t.

      do while .t.
       _nop++
       append blank
       if neterr()
        if _nop <=5
         loop
        else
         pare('.. archivo no disponible, intente de nuevo')
         _siva:=.f.
         exit
        endif
       else
        exit
       endif
      enddo

      if !_siva
       restscreen(22,0,23,79,_yy2)
       exit
      endif

      repla      ;
                CODMOVART       with    _ite                    ,;
                FECHA_MOV       with    ctod(g_fecha)           ,;
                hora            with    strtran(time(),':')     ,;
                TIPINV          with    'AC'                    ,;
                numdoc          with    'JJ_AJCOS'              ,;
                unidad          with    mxctainv->unidad        ,;
                CANTIDAD        with    0                       ,;
                COSTO_TOT       with    _ajcos                  ,;
                ORIGEN          with    'INV'                   ,;
                ALMACEN         with    _alm

      unlock

      recalcos(_ite)
      restscreen(22,0,23,79,_yy2)
     
     endif

    else
     _ite:=spac(15)
     _cnu:=0
     loop
    endif

    _ite:=spac(15)

   enddo

  endif
 endif
endif

cerrar('MXCTAINV')
cerrar('MXTRAINV')
cerrar('MXEXTINV')

restscreen(8,8,12,75,_yy)

return


*************************************************************************************
*************************************************************************************
*************************************************************************************
