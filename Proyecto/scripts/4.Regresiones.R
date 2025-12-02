#########################################################

#  TRABAJO FINAL CIENCIA DE DATOS PARA ECON Y NEGOCIOS

# 04. Regresiones

#########################################################


# Setup -------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  readr, dplyr, stringr, readxl, writexl, tidyr, DBI,
  lubridate, ggplot2, zoo, quantmod, httr, jsonlite,
  lmtest, sandwich, janitor, psych, corrplot, plotly,
  tidyverse, fs, broom
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

ruta_tablas <- "../output/tables"
ruta_figs   <- "../output/figures"

# Aseguramos que existan las carpetas
if(!dir.exists(ruta_figs)) dir.create(ruta_figs, recursive = TRUE)


# Levanto las bases -------------------------------------------------------

# 3. Cargar las tablas (Rutas relativas)

molinetes_mensual <- read_xlsx(file.path(ruta_tablas, "molinetes_mensual_enriquecida.xlsx"))
precios           <- read_xlsx(file.path(ruta_tablas, "precios_procesado.xlsx"))


# ==============================================================================
# 1. PREPARACIÓN DE DATOS (CORREGIDO)
# ==============================================================================

datos_regresion <- molinetes_mensual %>%
  mutate(
    anio_key = as.integer(year(fecha_eje)),
    mes_key  = as.integer(month(fecha_eje))
  ) %>%
  inner_join(precios %>% 
               mutate(anio_key = as.integer(year(fecha)), 
                      mes_key = as.integer(month(fecha))), 
             by = c("anio_key", "mes_key")) %>%
  arrange(fecha_eje) %>%
  mutate(
    ln_pasajeros = log(total_pasajeros),
    ln_precio    = log(precio), 
    
    # Variables de Control
    tendencia  = row_number(),       # Paso del tiempo (1, 2, 3...)
    mes_factor = as.factor(mes_key)  # Mes como categoría (Ene, Feb...)
  ) %>%
  # Limpieza final de infinitos o NAs
  filter(is.finite(ln_pasajeros) & is.finite(ln_precio))

# ==============================================================================
# 2. MODELO SIMPLE 
# ==============================================================================
# ln(Q) = Beta0 + Beta1 * ln(P)
# Este modelo suele dar mal porque ignora la inflación/tendencia

modelo_simple <- lm(ln_pasajeros ~ ln_precio, data = datos_regresion)

message("--- RESULTADOS MODELO SIMPLE (Suele ser engañoso) ---")
summary(modelo_simple)

# ==============================================================================
# 3. MODELO COMPLETO (Robust)
# ==============================================================================
# ln(Q) = Beta0 + Elasticidad*ln(P) + Tendencia + Estacionalidad

modelo_completo <- lm(ln_pasajeros ~ ln_precio + tendencia + mes_factor, data = datos_regresion)

message("--- RESULTADOS MODELO COMPLETO (El que vale) ---")
summary(modelo_completo)

# ==============================================================================
# 4. CONCLUSIÓN AUTOMÁTICA
# ==============================================================================

# Extraemos el coeficiente de elasticidad del modelo completo
coef_precio <- tidy(modelo_completo) %>% 
  filter(term == "ln_precio")

elasticidad <- coef_precio$estimate
p_valor     <- coef_precio$p.value


message("Elasticidad Precio: ", round(elasticidad, 4))
message("P-Valor: ", format(p_valor, scientific = FALSE))

if(p_valor < 0.05) {
  message("-> SIGNIFICATIVO: El aumento de tarifa SÍ reduce la cantidad de pasajeros.")
} else {
  message("-> NO SIGNIFICATIVO: No hay evidencia estadística de impacto.")
}

if(abs(elasticidad) < 1) {
  message("-> TIPO: INELÁSTICA (La gente viaja igual, aunque suba el precio).")
  message("   Por cada 10% que sube el precio, los pasajeros bajan un ", abs(round(elasticidad*10, 2)), "%.")
} else {
  message("-> TIPO: ELÁSTICA (La gente deja de viajar masivamente).")
}



# ==============================================================================
# 5. GRÁFICOS
# ==============================================================================

# 1. Extraemos los datos del modelo para graficar
resultados_grafico <- tidy(modelo_completo, conf.int = TRUE) %>%
  filter(term == "ln_precio") %>%
  mutate(modelo = "Modelo Completo\n(Controlado)")

# Agregamos el modelo simple para comparar lo mal que daba
resultados_simple <- tidy(modelo_simple, conf.int = TRUE) %>%
  filter(term == "ln_precio") %>%
  mutate(modelo = "Modelo Simple\n(Sesgado)")

datos_plot <- bind_rows(resultados_grafico, resultados_simple)

# 2. Creamos el gráfico de coeficientes
g_coef <- ggplot(datos_plot, aes(x = estimate, y = modelo, color = modelo)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  
  labs(title = "Elasticidad Precio de la Demanda del Subte",
       subtitle = "Comparación: El modelo simple da positivo (mal), el completo da negativo (bien)",
       x = "Elasticidad Precio ( % cambio en pasajeros ante 1% cambio en precio )",
       y = NULL,
       caption = "Elaboración propia en base a datos del Gobierno de la Ciudad de Buenos Aires") +
  scale_color_manual(values = c("Modelo Completo\n(Controlado)" = "#2E7D32", 
                                "Modelo Simple\n(Sesgado)" = "#D32F2F")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", size = 10))

print(g_coef)

# 3. Guardamos usando ruta_figs
ggsave(file.path(ruta_figs, "8_elasticidad_comparada.png"), 
       plot = g_coef, width = 10, height = 6, dpi = 300, bg = "white")



