* TRANSFERENCIAS ENTRE ALMACEN
PROCEDURE INV0307
   PRIVATE PV_CALT
   if g_multi
      EDIT_VENTANA('EDCTRF01')
   else
      Pare('Esta empresa no es multi-almac‚n')
   endif
RETURN

PROCEDURE INV0308
   PRIVATE PV_CALT
   Private en_dup:=.f.
   EDIT_VENTANA('ENCTRFDP')
RETURN NIL






