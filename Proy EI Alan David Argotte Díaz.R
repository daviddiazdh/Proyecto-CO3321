library(tidyverse)

# Cargar el archivo CSV (limpiando espacios en blanco en los nombres de columnas)
df <- read.csv("quality diamonds.csv", strip.white = TRUE)

# Número de intervalos por Regla de Sturges (k = 17 para N = 50,000)
k_sturges <- round(1 + 3.322 * log10(nrow(df)))

# ==============================================================================
# PARTE 4: ANÁLISIS UNIVARIANTE
# ==============================================================================

# TABLAS DE FRECUENCIA DE TODAS LAS VARIABLES

# Variables Cualitativas (cut, color, clarity)
tablas_cualitativas <- lapply(df[c("cut", "color", "clarity")], function(col) {
  cbind(Frec_Abs = table(col), Porcentaje = round(prop.table(table(col)) * 100, 2))
})

# Variables Cuantitativas (carat, depth, table, Width, Length, Height, price)
var_cuant <- c("carat", "depth", "table", "Width", "Length", "Height", "price")

tablas_cuantitativas <- lapply(df[var_cuant], function(col) {
  val_min <- min(col, na.rm = TRUE)
  val_max <- max(col, na.rm = TRUE)
  cortes <- seq(from = val_min, to = val_max, length.out = k_sturges + 1)
  rangos <- cut(col, breaks = cortes, include.lowest = TRUE)
  cbind(Frec_Abs = table(rangos), Porcentaje = round(prop.table(table(rangos)) * 100, 2))
})

# Mostrar todas las tablas
tablas_cualitativas
tablas_cuantitativas

# GRÁFICOS CUALITATIVOS (3 tipos distintos)

# Barras verticales para cut
ggplot(df, aes(x = cut, fill = cut)) +
  geom_bar() +
  theme_minimal() +
  theme(legend.position = "none")

# Diagrama de sectores para color
ggplot(as.data.frame(table(df$color)), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void()

# Diagrama de puntos para clarity
ggplot(as.data.frame(table(df$clarity)), aes(x = Freq, y = reorder(Var1, Freq))) +
  geom_point(size = 3) +
  geom_segment(aes(x = 0, xend = Freq, y = Var1, yend = Var1)) +
  theme_minimal()


# GRÁFICOS CUANTITATIVOS (2 tipos distintos)

# Histograma para price (17 intervalos de Sturges)
ggplot(df, aes(x = price)) +
  geom_histogram(bins = k_sturges, fill = "steelblue", color = "white") +
  theme_minimal()

# Boxplot para carat
ggplot(df, aes(y = carat)) +
  geom_boxplot(fill = "coral") +
  theme_minimal()


# ==============================================================================
# PARTE 5: ANÁLISIS BIVARIANTE
# ==============================================================================

# Gráfico de Caja: Price vs Cut
ggplot(df, aes(x = cut, y = price, fill = cut)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none")

# Tabla de Contingencia: Color vs Clarity
contingencia_abs <- table(Color = df$color, Clarity = df$clarity)
contingencia_pct <- round(prop.table(contingencia_abs, margin = 1) * 100, 2)

# Mostrar resultados bivariantes
contingencia_abs
contingencia_pct