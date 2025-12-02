#########################################################
#  TRABAJO FINAL CIENCIA DE DATOS PARA ECON Y NEGOCIOS
#  01. Limpieza de las bases
#########################################################



# Setup -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readr, dplyr, stringr, readxl, writexl, tidyr, DBI,
  lubridate, ggplot2, zoo, quantmod, httr, jsonlite,
  lmtest, sandwich, janitor, psych, corrplot, plotly,
  tidyverse, fs
)

rm(list=ls())

'%!in%' <- function(x,y)!('%in%'(x,y)) 
options(scipen = 999)

# Levantamos y limpiamos las bases ----------------------------------------

if (rstudioapi::isAvailable()) {
  ruta_script <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(ruta_script)
}

# Verificamos dónde quedamos parados (debería ser .../Proyecto/scripts)
print(paste("Trabajando en:", getwd()))


# Definimos directorios (Rutas relativas)
ruta_raw <- "../data/raw"
ruta_clean <- "../data/clean"

# Verificación de seguridad visual
print(paste("Buscando datos en:", file.path(getwd(), ruta_raw)))

# 1. Listamos TODOS los archivos .csv en la carpeta
if (!dir.exists(ruta_raw)) {
  stop("ERROR CRÍTICO: No se encuentra la carpeta 'data/raw'. Verifica estar en la carpeta raíz 'Proyecto'.")
}

todos_los_archivos <- dir_ls(ruta_raw, glob = "*.csv")

# 2. Separamos la lógica
archivo_precio <- str_subset(todos_los_archivos, "registro-historico")
archivos_molinetes <- todos_los_archivos[todos_los_archivos != archivo_precio]

# --- PROCESO A: PRECIOS ---
precios <- read_csv(archivo_precio) %>% 
  janitor::clean_names()

# --- PROCESO B: MOLINETES ---
print("Procesando molinetes... (Puede demorar unos minutos)")

molinetes <- archivos_molinetes %>% 
  map_dfr(function(x) {
    read_csv(x, col_types = cols(.default = "c")) %>% 
      janitor::clean_names() %>% 
      mutate(archivo_origen = path_file(x))
  })

# Unificamos columnas
molinetes <- molinetes %>% 
  mutate(
    pax_pagos = coalesce(pax_pagos, pax_pago),
    pax_franq = coalesce(pax_franq, pax_freq),
    pax_total = coalesce(pax_total, total) 
  ) %>% 
  select(
    periodo, fecha, desde, hasta, 
    linea, estacion, molinete, 
    pax_pagos, pax_pases_pagos, pax_franq, pax_total,
    archivo_origen
  ) %>% 
  mutate(
    pax_pagos = as.numeric(pax_pagos),
    pax_pases_pagos = as.numeric(pax_pases_pagos),
    pax_franq = as.numeric(pax_franq),
    pax_total = as.numeric(pax_total)
  ) %>% 
  mutate(
    pax_pagos = replace_na(pax_pagos, 0),
    pax_pases_pagos = replace_na(pax_pases_pagos, 0),
    pax_franq = replace_na(pax_franq, 0),
    pax_total = replace_na(pax_total, 0)
  )

# Guardamos los archivos
if(!dir.exists(ruta_clean)) dir.create(ruta_clean, recursive = TRUE)

write_rds(molinetes, file.path(ruta_clean, "molinetes_consolidado_2013_2019.rds"))
write_rds(precios, file.path(ruta_clean, "precio_boleto_historico.rds"))