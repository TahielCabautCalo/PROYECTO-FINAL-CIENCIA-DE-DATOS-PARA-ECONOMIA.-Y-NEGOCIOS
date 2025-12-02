# Análisis de Datos de Molinetes y Transporte

Este repositorio contiene el flujo de trabajo completo para el procesamiento, limpieza y análisis econométrico de bases de datos de molinetes.

## 📋 Requisitos y Librerías

El proyecto está diseñado para ser **totalmente reproducible y automático**.

No es necesario instalar las librerías manualmente una por una. Los scripts utilizan el gestor de paquetes `pacman`. 
Al iniciar la ejecución, el código verificará si posee las librerías necesarias; si no las tiene, las descargará e instalará automáticamente, y luego las cargará.

**Stack Tecnológico:**
* **Gestión de Entorno:** `pacman`, `fs`
* **Manipulación de Datos:** `tidyverse` (dplyr, tidyr, readr, stringr), `janitor`, `lubridate`, `zoo`
* **Bases de Datos:** `DBI`
* **Análisis Econométrico y Financiero:** `lmtest`, `sandwich`, `quantmod`, `broom`
* **Visualización:** `ggplot2`, `plotly`, `corrplot`
* **Lectura/Escritura:** `readxl`, `writexl`, `jsonlite`

## 📂 Estructura del Directorio

⚠️ **Importante:** Para la correcta ejecución, la carpeta raíz `proyecto` debe estar ubicada dentro de la carpeta **Documentos**.

```text
proyecto/
├── data/
│   ├── raw/              # Datos crudos originales (Inputs)
│   ├── clean/            # Datos limpios
│   └── processed/        # Datos procesados listos para modelado
├── output/
│   ├── tables/           # Tablas finales exportadas
│   └── figures/          # Gráficos estáticos e interactivos
├── scripts/              # Scripts de análisis (1 al 4)
└── README.md