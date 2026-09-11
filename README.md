# MixNet - Codigo Fuente (PRG)

Codigo fuente FoxPro/dBASE del ERP MixNet de JJ PAPER, C.A.
Programas `.PRG` (procesos, ediciones, reportes) del sistema.

## Contenido
- 55 programas `.PRG` (ediciones ED*, variaciones VAR*, reportes, etc.)
- Carpeta `Originales/` con copias de respaldo (no versionadas, ver `.gitignore`)

## Como ejecutar MixNet

El sistema corre bajo emulador DOS **vDos** (Windows 10/11 no ejecuta
DOS/FoxPro nativo). Los atajos y scripts de arranque estan en la maquina
de operaciones; aqui la configuracion de referencia.

### 1. Drive compartida
MixNet usa la unidad `M:` mapeada a `\\servidor\MIX11`.
En Windows:
```
net use M: \\servidor\MIX11
```

### 2. Emulador vDos
Instalado en `C:\vDos\`. Su `autoexec.txt` monta la red dentro del DOS
virtual y lanza el programa:

```
USE C: C:\
USE M: \\servidor\MIX11\
M:
mixnet.exe sv
EXIT
```

> Nota: dentro de vDos `C:` y `M:` son letras DOS virtuales, no mapean
> directamente al Windows C: (ver `C:\vDos\mix.bat.bat` y el acceso directo
> del escritorio al ejecutar con parametros).

### 3. Ejecutar
- Doble clic al acceso directo **Mixnet** del escritorio
  (TargetPath = `C:\vDos\vDos.exe`, WorkingDir = `C:\vDos`), que procesa
  `autoexec.txt` y arranca `mixnet.exe sv`, o
- Ejecutar el lote `M:\Mix11-21.bat` (hace `M:` + `mixnet aa`).

Los argumentos (`sv`, `aa`, `01`, `/s`) son el codigo de la empresa/sucursal
con la que inicia el sistema (MIX11 trabaja con `comp01` = JJ PAPER, C.A.).

### 4. Requisitos
- Windows (32/64 bits) con vDos instalado
- Acceso a la red del servidor (`servidor\MIX11`)
- El modulo compilado `MIXNET.EXE` + DLLs (`MIXNET01..04.DLL`) en `\\servidor\MIX11\`

## Datos (no versionados)
- Maestro: `\\servidor\MIX11\comp01\MXCTAINV.DBF`
- Facturas: `MXENCFAC.DBF` / `MXRENFAC.DBF`  |  Pedidos: `MXENCPED.DBF` / `MXRENPED.DBF`
- Inventario/movimientos: `MXTRAINV.DBF`
- Precios: `PRECIO_A`/`PRECIO_B` en US$; `PRECIO_C`/`PRECIO_D` en Bs (C=A*cambio, D=B*cambio).
  Precio operativo real de venta = **PRECIO_B**.

## Mejoras / integraciones
Repo abierto para fork + integraciones. Reportes de analisis generados:
`ANALISIS_PRECIOS_REALES.xlsx` y `ANALISIS_ACTIVIDAD.xlsx` en `\\servidor\MIX11\`.