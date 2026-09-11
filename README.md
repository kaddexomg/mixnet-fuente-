# MixNet - Codigo Fuente (PRG)

Codigo fuente FoxPro/dBASE del ERP MixNet de JJ PAPER, C.A.

- 68 programas `.PRG` (procesos, ediciones, reportes)
- Original almacenado en: `\\servidor\MIX11\DLL-Pre\DLL\PRG`
- Copias de backup: carpeta `Originales/` (se excluyeron del repo, ver `.gitignore`)

## Estructura
Todos los PRG estan en la raiz (MixNet no usa subcarpetas de codigo).

## Notas
- Sistema legacy dBASE/FoxPro. Datos activos en `\\servidor\MIX11\comp01` (DBF).
- Importante: PRECIO_A/B en US$; PRECIO_C/D en Bs (C=A*cambio, D=B*cambio).
- Precio operativo real de venta = PRECIO_B.