#########################################################
#  TRABAJO FINAL CIENCIA DE DATOS PARA ECON Y NEGOCIOS
#  02. Procesamiento
#########################################################

rm(list=ls()) 


# Setup -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readr, dplyr, stringr, readxl, writexl, tidyr, DBI,
  lubridate, ggplot2, zoo, quantmod, httr, jsonlite,
  lmtest, sandwich, janitor, psych, corrplot, plotly,
  tidyverse, fs
)

options(scipen = 999)

# Levanto las bases -------------------------------------------------------

if (rstudioapi::isAvailable()) {
  ruta_script <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(ruta_script)
}

# Verificamos dónde quedamos parados (debería ser .../Proyecto/scripts)
print(paste("Trabajando en:", getwd()))


# Definimos rutas relativas
ruta_clean <- "../data/clean"
ruta_processed <- "../data/processed"



# Verificación: Chequeamos que exista la data limpia del script 01
if (!file.exists(file.path(ruta_clean, "molinetes_consolidado_2013_2019.rds"))) {
  stop("ERROR: No encuentro el archivo 'molinetes_consolidado_2013_2019.rds' en data/clean. 
       ¿Corriste el Script 01?")
}

# Crear carpeta de salida si no existe
if(!dir.exists(ruta_processed)) dir.create(ruta_processed, recursive = TRUE)

# Cargar los datos procesados (.rds)
molinetes <- read_rds(file.path(ruta_clean, "molinetes_consolidado_2013_2019.rds"))
precios <- read_rds(file.path(ruta_clean, "precio_boleto_historico.rds"))


# Procesamiento base MOLINETES ----------------------------------------------

message("⚙️ Procesando fechas y agrupando...")

# 1. CREAMOS UN DICCIONARIO DE FECHAS PARA CORREGIR NAs EN PERIODO
diccionario_fechas <- molinetes %>% 
  distinct(fecha) %>% 
  mutate(
    fecha_date = parse_date_time(fecha, orders = c("ymd", "dmy")),
    fecha_date = as_date(fecha_date)
  ) %>% 
  mutate(
    periodo_calculado = format(fecha_date, "%Y%m")
  )

# 2. Agrupamos por mes y año para el analisis 
molinetes_mensual <- molinetes %>%
  # Pegamos la tabla de referencia
  left_join(diccionario_fechas, by = "fecha") %>% 
  
  # Rellenamos el periodo usando la columna calculada
  mutate(
    periodo = coalesce(periodo, periodo_calculado)
  ) %>% 
  
  # Agrupamos
  filter(!is.na(periodo)) %>% 
  group_by(periodo) %>% 
  summarise(
    total_pasajeros     = sum(pax_total, na.rm = TRUE),
    total_pagos         = sum(pax_pagos, na.rm = TRUE),
    total_franquicias   = sum(pax_franq, na.rm = TRUE),
    total_pases         = sum(pax_pases_pagos, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(fecha_eje = ym(periodo))

rm(diccionario_fechas)

# Seleccionamos columnas finales
molinetes_mensual <- molinetes_mensual %>% 
  select(periodo, fecha_eje, total_pasajeros)


# Limpieza de PRECIO ------------------------------------------------------

precios <- precios %>% 
  filter(ano >= 2014) %>% 
  select(ano, mes_numero, precio)


# EXPORTO -----------------------------------------------------------------

message("💾 Guardando archivos en data/processed...")
write_xlsx(molinetes_mensual, file.path(ruta_processed, "molinetes_mensual.xlsx"))
write_xlsx(precios, file.path(ruta_processed, "precios.xlsx"))