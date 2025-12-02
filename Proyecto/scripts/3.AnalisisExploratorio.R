#########################################################

#  TRABAJO FINAL CIENCIA DE DATOS PARA ECON Y NEGOCIOS

# 03. Analisis Exploratorio

#########################################################

# Setup -------------------------------------------------------------------



if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readr, dplyr, stringr, readxl, writexl, tidyr, DBI,
  lubridate, ggplot2, zoo, quantmod, httr, jsonlite,
  lmtest, sandwich, janitor, psych, corrplot, plotly,
  tidyverse, fs, ggrepel, forecast, scales, knitr
)

rm(list=ls())

'%!in%' <- function(x,y)!('%in%'(x,y)) 
options(scipen = 999)

if (rstudioapi::isAvailable()) {
  ruta_script <- dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(ruta_script)
}

# Verificamos dónde quedamos parados (debería ser .../Proyecto/scripts)
print(paste("Trabajando en:", getwd()))

# Definición de Rutas -----------------------------------------------------

ruta_processed <- "../data/processed"
ruta_figs      <- "../output/figures"
ruta_tablas    <- "../output/tables"


# Cargar los datos --------------------------------------------------------



molinetes_mensual <- read_xlsx(file.path(ruta_processed, "molinetes_mensual.xlsx"))
precios           <- read_xlsx(file.path(ruta_processed, "precios.xlsx"))


# 1. Detección de Outliers (Tukey) ----------------------------------------

# Calculamos límites y clasificamos
molinetes_mensual <- molinetes_mensual %>%
  mutate(anio = year(fecha_eje)) %>% 
  group_by(anio) %>% 
  mutate(
    Q1 = quantile(total_pasajeros, 0.25, na.rm = TRUE),
    Q3 = quantile(total_pasajeros, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    limite_inferior = Q1 - 1.5 * IQR,
    limite_superior = Q3 + 1.5 * IQR
  ) %>%
  ungroup() %>% 
  mutate(
    categoria_outlier = case_when(
      total_pasajeros < limite_inferior ~ "Outlier Bajo",
      total_pasajeros > limite_superior ~ "Outlier Alto",
      TRUE ~ "Normal"
    )
  )

molinetes_mensual$categoria_outlier <- factor(molinetes_mensual$categoria_outlier, 
                                              levels = c("Outlier Bajo", "Normal", "Outlier Alto"))

# Gráfico 1: Boxplot con Outliers
g1 <- ggplot(molinetes_mensual, aes(x = factor(anio), y = total_pasajeros)) +
  geom_boxplot(outlier.shape = NA, color = "gray60", fill = "#E0E0E0", alpha = 0.5) +
  geom_point(aes(color = categoria_outlier), size = 3, alpha = 0.8) +
  geom_text_repel(
    data = filter(molinetes_mensual, categoria_outlier != "Normal"),
    aes(label = format(fecha_eje, "%b"), color = categoria_outlier),
    size = 3.5, box.padding = 0.5, point.padding = 0.3, force = 10, show.legend = FALSE
  ) +
  scale_color_manual(values = c("Outlier Bajo" = "#D32F2F", "Normal" = "#757575", "Outlier Alto" = "#1976D2")) +
  labs(title = "Identificación de Meses Atípicos (Outliers) por Año",
       subtitle = "Se etiqueta el mes si cae significativamente fuera del rango normal de SU año.",
       x = "Año", y = "Total de Pasajeros", color = "Clasificación del Mes",
       caption = "Elaboración propia en base a datos del Gobierno de la Ciudad de Buenos Aires") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 22, face = "bold"),
        plot.background = element_rect(fill = "white", color = NA))

print(g1)

ggsave(filename = file.path(ruta_figs, "1_outliers_boxplot_anual.png"),
       plot = g1, width = 12, height = 8, dpi = 300, bg = "white")


# 2. Descomposición STL (Estacionalidad) ----------------------------------

ts_pasajeros <- ts(molinetes_mensual$total_pasajeros, 
                   start = c(2013, 1), 
                   frequency = 12)

descomposicion <- stl(ts_pasajeros, s.window = "periodic")

# Extraer componentes
componentes <- descomposicion$time.series
molinetes_mensual <- molinetes_mensual %>%
  mutate(
    tendencia      = componentes[, "trend"], 
    estacionalidad = componentes[, "seasonal"], 
    residuo        = componentes[, "remainder"]
  )

# Gráfico 2: Descomposición STL
png(filename = file.path(ruta_figs, "2_descomposicion_stl.png"),
    width = 12, height = 8, units = "in", res = 300, bg = "white")

plot(descomposicion, main = "Descomposición de Series de Tiempo (STL)")
mtext("Elaboración propia en base a datos del Gobierno de la Ciudad de Buenos Aires", 
      side = 1, line = 4, adj = 1, cex = 0.8, col = "gray30")

dev.off()


# 3. Serie Desestacionalizada ---------------------------------------------

datos_desestacionalizados <- seasadj(descomposicion)
molinetes_mensual$sin_estacionalidad <- as.numeric(datos_desestacionalizados)

g3 <- ggplot(molinetes_mensual, aes(x = fecha_eje)) +
  geom_line(aes(y = total_pasajeros, color = "Original"), alpha = 0.5) +
  geom_line(aes(y = sin_estacionalidad, color = "Desestacionalizada"), size = 1) +
  scale_color_manual(name = "Serie", values = c("Original" = "gray", "Desestacionalizada" = "blue")) +
  labs(title = "Pasajeros: Original vs Desestacionalizada", 
       subtitle = "La línea azul muestra la tendencia real sin el efecto vacaciones",
       x = "Fecha", y = "Pasajeros",
       caption = "Elaboración propia en base a datos del Gobierno de la Ciudad de Buenos Aires") +
  theme_minimal()

print(g3)

ggsave(filename = file.path(ruta_figs, "3_serie_desestacionalizada.png"),
       plot = g3, width = 12, height = 8, dpi = 300, bg = "white")


# 4. Variación Interanual -------------------------------------------------

molinetes_mensual <- molinetes_mensual %>%
  arrange(fecha_eje) %>% 
  mutate(
    pasajeros_anio_anterior = lag(total_pasajeros, 12),
    var_interanual = (total_pasajeros / pasajeros_anio_anterior) - 1
  )

g4 <- ggplot(molinetes_mensual, aes(x = fecha_eje, y = var_interanual)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Crecimiento Interanual de Pasajeros",
       y = "% Variación vs año anterior", x = "Fecha",
       caption = "Elaboración propia en base a datos del Gobierno de la Ciudad de Buenos Aires") +
  theme_minimal()

print(g4)

ggsave(filename = file.path(ruta_figs, "4_variacion_interanual.png"),
       plot = g4, width = 12, height = 6, dpi = 300, bg = "white")


# 5. Evolución de Precios -------------------------------------------------

precios <- precios %>%
  mutate(fecha = make_date(year = ano, month = mes_numero, day = 1))

valores_unicos_precio <- sort(unique(precios$precio))

g_precios <- ggplot(precios, aes(x = fecha, y = precio)) +
  geom_step(color = "#2E7D32", size = 1.2, direction = "hv") + 
  scale_y_continuous(
    breaks = valores_unicos_precio, 
    labels = scales::dollar_format(prefix = "$")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Evolución de la Tarifa de Subte (2014-2019)",
       subtitle = "Valores nominales por mes",
       x = "Año", y = "Precio del Pasaje",
       caption = "Elaboración propia en base a datos del Gobierno de la Ciudad de Buenos Aires") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "gray85") 
  )

print(g_precios)

ggsave(filename = file.path(ruta_figs, "5_evolucion_precios_eje_completo.png"), 
       plot = g_precios, width = 12, height = 8, dpi = 300, bg = "white")


# Exportación de Tablas Finales -------------------------------------------

write_xlsx(molinetes_mensual, file.path(ruta_tablas, "molinetes_mensual_enriquecida.xlsx"))
write_xlsx(precios, file.path(ruta_tablas, "precios_procesado.xlsx"))
