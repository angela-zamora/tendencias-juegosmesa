#===============================================================
# 1. Cargar las librerías necesarias
#===============================================================
library(tidyverse)
library(DescTools)
library(knitr) #para mejores tablas para presentar en un informe.

#=====================================================================
# 2. Importar la base de datos con su nombre original "BGG_Data_Set"
#=====================================================================

# ==================================================================
# 3: Limpieza, renombrado y creación de variables dicotómicas para 
#      representar las categorias (domain) en columnas separadas.
# ==================================================================

juegos_limpios <- BGG_Data_Set %>%
  rename(
    Ranking = BGG.Rank,
    Year_Published = Year.Published,
    Min_Players = Min.Players,
    Max_Players = Max.Players,
    Play_Time = Play.Time,
    Users_Rated = Users.Rated,
    Complexity_Average = Complexity.Average,
    Owned_Users = Owned.Users,
    Rating_Average = Rating.Average,
    Min_Age = Min.Age
  ) %>%
  mutate(
    Abstract = ifelse(str_detect(Domains, "Abstract Games"), 1, 0),
    Children = ifelse(str_detect(Domains, "Children's Games"), 1, 0),
    Customizable = ifelse(str_detect(Domains, "Customizable Games"), 1, 0),
    Family = ifelse(str_detect(Domains, "Family Games"), 1, 0),
    Party = ifelse(str_detect(Domains, "Party Games"), 1, 0),
    Strategy = ifelse(str_detect(Domains, "Strategy Games"), 1, 0),
    Thematic = ifelse(str_detect(Domains, "Thematic Games"), 1, 0),
    Wargames = ifelse(str_detect(Domains, "Wargames"), 1, 0)
  ) %>%
  select(-Mechanics)

# ===============================================
# 4: Análisis de Calidad de Datos (Data Quality)
# ===============================================

# --- 4.1 Control de Duplicados ---
# Identificamos si hay algún juego introducido dos veces
duplicados <- juegos_limpios[duplicated(juegos_limpios), ]
print(paste("Número de filas duplicadas:", nrow(duplicados))) #no se han detectado duplicados.


# --- 4.2 Tratamiento de NAs ---
# 1º Visualizamos gráficamente dónde faltan datos
PlotMiss(juegos_limpios, main = "Datos missing por variable")

# 2º Como el gráfico no deja ver resultados claros por el gran tamaño de la muestra, sacamos una tabla numérica con los NAs por columna
nas_por_columna <- colSums(is.na(juegos_limpios))
print(nas_por_columna[nas_por_columna > 0]) 

# 3º Tratamiento de NAs en la variable 'ID' (16 valores ausentes)
# Calculamos el identificador máximo actual para no sobreescribir juegos reales
max_id <- max(juegos_limpios$ID, na.rm = TRUE)

# Asignamos identificadores artificiales secuenciales partiendo de ese máximo (331787)
juegos_limpios$ID[is.na(juegos_limpios$ID)] <- seq(max_id + 1, max_id + sum(is.na(juegos_limpios$ID)))
max_id

# 4º Tratamiento de NAs en la variable 'Year_Published' (1 valor ausente)
# Estimamos el valor ausente en base a mediana de los valores válidos
mediana_anyo <- median(juegos_limpios$Year_Published, na.rm = TRUE)
juegos_limpios$Year_Published[is.na(juegos_limpios$Year_Published)] <- mediana_anyo

# 5º Tratamiento de NAs en la variable 'Owned_Users' (23 valores ausentes)
# Aplicamos la depuración directa de casos seleccionando solo aquellos sin NA mediante filter()
juegos_limpios <- juegos_limpios %>%
  filter(!is.na(Owned_Users))

# 6º COMPROBACIÓN FINAL DE COMPLETITUD. Verificamos que el dataset ha quedado completamente limpio de NAs
print("Total de celdas vacías (NA) restantes por columna:")
print(colSums(is.na(juegos_limpios)))

# --- 4.3 Detección de Valores Atípicos (Outliers) ---

# --- FASE 1: VISUALIZACIÓN GENERAL DE MÍNIMOS Y MÁXIMOS ---
# Seleccionamos todas las variables cuantitativas para comprobar posibles valores atípicos y decidir sobre cuales es necesario hacer una investigación en profundidad.
revision_rapida <- juegos_limpios %>%
  select(Year_Published, Min_Players, Max_Players, Min_Age, Play_Time, Rating_Average, Users_Rated, Owned_Users, Complexity_Average)

# En lugar de un summary() general, construimos un dataframe específico solo con Mínimos y Máximos
tabla_min_max <- data.frame(
  Variable = names(revision_rapida),
  Minimo = sapply(revision_rapida, min, na.rm = TRUE),
  Maximo = sapply(revision_rapida, max, na.rm = TRUE)
)

# Imprimimos la tabla con formato formal usando kable()
kable(tabla_min_max, 
      col.names = c("Variable Cuantitativa", "Valor Mínimo", "Valor Máximo"),
      align = "lcc", # Alineación: Izquierda para el nombre, Centro para los números
      caption = "Tabla 1. Exploración de rangos para el control de Razonabilidad",
      row.names = FALSE)

# --- FASE 2: ANÁLISIS ESPECÍFICO DE VALORES ATÍPICOS (Outliers) ---
# Usamos las pistas de la Fase 1 para aislar los errores reales.

# A) Atípicos en el año de publicación. Al detectarse valores negativos o de siglos pasados debemos analizar estos datos y decidir como tratarlos.
atipicos_anyo <- juegos_limpios %>%
  filter(Year_Published < 1950 )
print(paste("Juegos con año atípico detectados:", nrow(atipicos_anyo)))
# Filtramos la base de datos para mantener solo los juegos de la era moderna (1950 en adelante). Así eliminamos automáticamente los años negativos, errores de tecleo y juegos de otras épocas.
juegos_limpios <- juegos_limpios %>%
  filter(Year_Published >= 1950)

# B) Atípicos en el tiempo de Juego estudiados mediante dos métodos, z-score y Box-Plot.

# Método Z-score (Hipersensible a los valores extremos)
outliers_zscore <- juegos_limpios %>%
  filter(abs(scale(Play_Time)) > 3)
print(paste("Atípicos detectados por Z-score:", nrow(outliers_zscore)))

# Método Box-plot: para detectar valores atípicos leves (1,5 x RIC) y extremos (3 x RIC)

# 1. Cálculo de cuartiles y rango intercuartílico (RIC)
Q3 <- quantile(juegos_limpios$Play_Time, 0.75, na.rm = TRUE)
RIC <- IQR(juegos_limpios$Play_Time, na.rm = TRUE)

# 2. Definición de umbrales
lim_leve <- Q3 + 1.5 * RIC
lim_extremo <- Q3 + 3 * RIC

# 3. Conteo de atípicos
outliers_leves <- juegos_limpios %>% filter(Play_Time > lim_leve & Play_Time <= lim_extremo)
outliers_extremos <- juegos_limpios %>% filter(Play_Time > lim_extremo)

print(paste("Umbral Atípico Leve (> 1.5 RIC):", lim_leve, "minutos"))
print(paste("Juegos con duración levemente atípica:", nrow(outliers_leves)))

print(paste("Umbral Atípico Extremo (> 3 RIC):", lim_extremo, "minutos"))
print(paste("Juegos con duración extremadamente atípica:", nrow(outliers_extremos)))


# 4. Limpieza estadística definitiva.

# Aplicamos el filtro puramente matemático detallado en el informe:
# Eliminamos duraciones nulas (0) y los atípicos extremos (> 3 RIC)
juegos_limpios <- juegos_limpios %>%
  filter(Play_Time > 0 & Play_Time <= lim_extremo)

print(paste("Registros totales tras la limpieza estadística:", nrow(juegos_limpios)))

# 5. Visualización del BoxPlot

# 5.1. Calculamos Q1, Mediana y Q3
Q1 <- quantile(juegos_limpios$Play_Time, 0.25, na.rm = TRUE)
Mediana <- median(juegos_limpios$Play_Time, na.rm = TRUE)
Q3 <- quantile(juegos_limpios$Play_Time, 0.75, na.rm = TRUE)

# 5.2. Ajustamos los márgenes: c(abajo, izquierda, arriba, derecha)
# Aumentamos el margen inferior a 8 para dejar mucho espacio a la leyenda
par(mar = c(8, 2, 4, 2))

# 5.3. Dibujamos el Box-plot (Los puntos atípicos salen por defecto, pero sin texto)
boxplot(juegos_limpios$Play_Time,
        horizontal = TRUE, 
        main = "Análisis de atípicos: tiempo de juego",
        xlab = "Duración de la partida (Minutos)",
        col = "aliceblue",
        border = "midnightblue",
        outcol = "orange",   
        outpch = 16)         

# 5.4. Líneas de los umbrales de atipicidad
abline(v = lim_leve, col = "orange", lty = 2, lwd = 1.5)
abline(v = lim_extremo, col = "red", lty = 2, lwd = 1.5)

# 5.5. Etiquetado numérico de Q1 y Q3 POR ENCIMA de la caja (y = 1.25)
# Mostramos exclusivamente el valor numérico, sin texto
text(x = Q1, y = 1.25, labels = Q1, pos = 3, col = "midnightblue", cex = 0.8, font = 2)
text(x = Q3, y = 1.25, labels = Q3, pos = 3, col = "midnightblue", cex = 0.8, font = 2)

# 5.6. Etiquetado numérico de la Mediana POR DEBAJO de la caja (y = 0.75)
# Mostramos exclusivamente el valor numérico
text(x = Mediana, y = 0.75, labels = Mediana, pos = 1, col = "darkgreen", cex = 0.8, font = 2)

# 5.7. Leyenda más pequeña y colocada en el fondo del lienzo
legend("bottom",
       inset = c(0, -0.60),  # La empujamos hacia abajo para que no pise el título del eje X
       xpd = TRUE,          
       bty = "o",           
       bg = "white",        
       box.col = "black",   
       legend = c("Atípicos Leves", 
                  paste("Lím. Leve (", lim_leve, "min)", sep=""), 
                  paste("Lím. Extremo (", lim_extremo, "min)", sep="")),
       col = c("orange", "orange", "red"),
       pch = c(16, NA, NA),
       lty = c(NA, 2, 2),
       lwd = c(NA, 1.5, 1.5),
       cex = 0.7,            # Reducimos el tamaño para que sea más sutil
       horiz = TRUE)         

# 5.8. Restauramos los márgenes por defecto de RStudio
par(mar = c(5, 4, 4, 2) + 0.1)

# C) Atípicos en el número de jugadores.

# Conteo de errores en el número de jugadores ANTES de depurar
print(paste("Juegos con mínimo de jugadores igual a 0:", nrow(juegos_limpios %>% filter(Min_Players == 0))))
print(paste("Juegos con máximo de jugadores igual a 0:", nrow(juegos_limpios %>% filter(Max_Players == 0))))

# Opcional: Saber cuántos juegos en total tienen uno u otro error
errores_jugadores <- juegos_limpios %>% filter(Min_Players == 0 | Max_Players == 0)
print(paste("Total de juegos con errores en el número de jugadores:", nrow(errores_jugadores)))

# Depuración de la variable jugadores (Eliminamos los 0 por imposibilidad física)
juegos_limpios <- juegos_limpios %>%
  filter(Min_Players > 0 & Max_Players > 0)

# ==========================================
# 5. ANÁLISIS UNIVARIANTE CUALITATIVO
# ==========================================

# --- 5.1 Análisis variables cualitativas ---
# 1. Calculamos la Frecuencia Absoluta (ni) 
freq_abs <- c(
  Abstract = sum(juegos_limpios$Abstract, na.rm = TRUE),
  Children = sum(juegos_limpios$Children, na.rm = TRUE),
  Customizable = sum(juegos_limpios$Customizable, na.rm = TRUE),
  Family = sum(juegos_limpios$Family, na.rm = TRUE),
  Party = sum(juegos_limpios$Party, na.rm = TRUE),
  Strategy = sum(juegos_limpios$Strategy, na.rm = TRUE),
  Thematic = sum(juegos_limpios$Thematic, na.rm = TRUE),
  Wargames = sum(juegos_limpios$Wargames, na.rm = TRUE)
)

freq_abs_ord <- sort(freq_abs, decreasing = TRUE)

# 2. Definimos las dos bases para dividir
Total_Juegos <- nrow(juegos_limpios)           # 18.503 observaciones
Total_Etiquetas <- sum(freq_abs_ord)           # Suma total de todas las etiquetas distintas 

# 3. Calculamos las dos proporciones que has propuesto
# A) Relativa a las respuestas (Suma = 1)
freq_rel_etiquetas <- freq_abs_ord / Total_Etiquetas

# B) Porcentaje sobre el total de juegos (Suma > 100%)
porcentaje_juegos <- (freq_abs_ord / Total_Juegos) * 100

# 4. Construimos la tabla formal
tabla_frecuencias <- data.frame(
  Categoria = names(freq_abs_ord),
  Frecuencia_Absoluta = as.numeric(freq_abs_ord),
  Frec_Relativa_Respuestas = round(as.numeric(freq_rel_etiquetas), 4),
  Porcentaje_Total_Juegos = paste0(round(as.numeric(porcentaje_juegos), 2), "%")
)

# Imprimimos la tabla
kable(tabla_frecuencias, 
      col.names = c("Categoría", "Frec. Absoluta (ni)", "Frec. Relativa (fi)", "% sobre Total Juegos"),
      align = "lccc", 
      caption = "Tabla 2. Distribución de categorías (Respuesta múltiple)",
      row.names = FALSE)

# VISUALIZACIÓN: DIAGRAMA DE BARRAS 

# Ampliamos el margen inferior para las etiquetas rotadas
par(mar = c(7, 5, 4, 2))

# Calculamos un límite superior para el eje Y (15% más del máximo) para que quepan los números
limite_y <- max(freq_abs_ord) * 1.15

# Generamos el gráfico y guardamos las coordenadas X en 'x_barras'
x_barras <- barplot(freq_abs_ord,
                    main = "Frecuencia absoluta por categoría de juego",
                    ylab = "Número de Juegos (ni)",
                    col = "lightseagreen", 
                    border = NA,            # Eliminamos los bordes negros
                    las = 2,           
                    cex.names = 0.9,
                    ylim = c(0, limite_y))  # Forzamos la altura del eje Y

# Añadimos los valores exactos encima de cada barra
text(x = x_barras, 
     y = freq_abs_ord, 
     labels = freq_abs_ord, 
     pos = 3,              # Coloca el texto justo por encima de la barra
     cex = 0.85,           
     font = 2,             
     col = "darkslategray")

# Restauramos los márgenes
par(mar = c(5, 4, 4, 2) + 0.1)

#--- Analisis variables cuantitativas ----
# 1. Tabla resumen
tabla_matriz <- data.frame(
  Estadistico = c("Media", "Mediana", "Desviación Típica (SD)", "Coef. Variación (%)"),
  
  Valoraciones = c(mean(juegos_limpios$Rating_Average, na.rm = TRUE), 
                   median(juegos_limpios$Rating_Average, na.rm = TRUE), 
                   sd(juegos_limpios$Rating_Average, na.rm = TRUE), 
                   (sd(juegos_limpios$Rating_Average, na.rm = TRUE)/mean(juegos_limpios$Rating_Average, na.rm = TRUE))*100),
  
  Complejidad = c(mean(juegos_limpios$Complexity_Average, na.rm = TRUE), 
                  median(juegos_limpios$Complexity_Average, na.rm = TRUE), 
                  sd(juegos_limpios$Complexity_Average, na.rm = TRUE), 
                  (sd(juegos_limpios$Complexity_Average, na.rm = TRUE)/mean(juegos_limpios$Complexity_Average, na.rm = TRUE))*100),
  
  Tiempo = c(mean(juegos_limpios$Play_Time, na.rm = TRUE), 
             median(juegos_limpios$Play_Time, na.rm = TRUE), 
             sd(juegos_limpios$Play_Time, na.rm = TRUE), 
             (sd(juegos_limpios$Play_Time, na.rm = TRUE)/mean(juegos_limpios$Play_Time, na.rm = TRUE))*100),
  
  Edad = c(mean(juegos_limpios$Min_Age, na.rm = TRUE), 
           median(juegos_limpios$Min_Age, na.rm = TRUE), 
           sd(juegos_limpios$Min_Age, na.rm = TRUE), 
           (sd(juegos_limpios$Min_Age, na.rm = TRUE)/mean(juegos_limpios$Min_Age, na.rm = TRUE))*100)
)

kable(tabla_matriz, 
      col.names = c("Estadístico", "Notas (0-10)", "Complejidad (1-5)", "Tiempo (min)", "Edad (años)"),
      align = "lcccc",
      digits = 2,
      caption = "Tabla 3. Resumen estadístico comparativo de las variables cuantitativas")

# Calculamos los extremos del tiempo para el texto
print(paste("Mínimo tiempo:", min(juegos_limpios$Play_Time, na.rm = TRUE)))
print(paste("Máximo tiempo:", max(juegos_limpios$Play_Time, na.rm = TRUE)))

# VISUALIZACIÓN: PANEL MÚLTIPLE DE 4 HISTOGRAMAS (2x2)

# Configuramos el panel 2x2 sin márgenes exteriores extra y con números horizontales (las = 1)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 0.1, las = 1) 

limite_y <- c(0, 6000) 

# 1. Histograma de Valoraciones
mediana_nota <- median(juegos_limpios$Rating_Average, na.rm = TRUE)
hist(juegos_limpios$Rating_Average,
     main = "Valoración media",
     xlab = "Rating",
     ylab = "Nº de juegos",
     col = "lightblue", border = "black",
     ylim = limite_y)
segments(x0 = mediana_nota, y0 = 0, x1 = mediana_nota, y1 = limite_y[2], col = "red", lwd = 2, lty = 2)

# Imprimimos el texto en pequeño (cex=0.8) a la derecha de la línea (pos=4) a una altura de 5500
text(x = mediana_nota, y = 5500, labels = paste("Mediana =", round(mediana_nota, 2)), pos = 4, col = "red", cex = 0.8)

# 2. Histograma de Complejidad
mediana_comp <- median(juegos_limpios$Complexity_Average, na.rm = TRUE)
hist(juegos_limpios$Complexity_Average,
     main = "Nivel de complejidad",
     xlab = "Dificultad (1 a 5)",
     ylab = "Nº de juegos",
     col = "lightgreen", border = "darkgreen",
     ylim = limite_y)
segments(x0 = mediana_comp, y0 = 0, x1 = mediana_comp, y1 = limite_y[2], col = "red", lwd = 2, lty = 2)
text(x = mediana_comp, y = 5500, labels = paste("Mediana =", round(mediana_comp, 2)), pos = 4, col = "red", cex = 0.8)

# 3. Histograma de Tiempo de Juego
mediana_tiempo <- median(juegos_limpios$Play_Time, na.rm = TRUE)
hist(juegos_limpios$Play_Time,
     main = "Duración de la partida",
     xlab = "Minutos",
     ylab = "Nº de juegos",
     col = "khaki", border = "darkorange",
     ylim = limite_y)
segments(x0 = mediana_tiempo, y0 = 0, x1 = mediana_tiempo, y1 = limite_y[2], col = "red", lwd = 2, lty = 2)
text(x = mediana_tiempo, y = 5500, labels = paste("Mediana =", round(mediana_tiempo, 2)), pos = 4, col = "red", cex = 0.8)

# 4. Histograma de Edad Mínima
mediana_edad <- median(juegos_limpios$Min_Age, na.rm = TRUE)
hist(juegos_limpios$Min_Age,
     main = "Edad mínima recomendada",
     xlab = "Años",
     ylab = "Nº de juegos",
     col = "lightcoral", border = "darkred",
     ylim = limite_y)
segments(x0 = mediana_edad, y0 = 0, x1 = mediana_edad, y1 = limite_y[2], col = "red", lwd = 2, lty = 2)
text(x = mediana_edad, y = 5500, labels = paste("Mediana =", round(mediana_edad, 2)), pos = 4, col = "red", cex = 0.8)

# Restauramos el panel a la configuración estándar
par(mfrow = c(1, 1), las = 0)

# ==========================================
# 3.2 ANÁLISIS BIVARIANTE (Panel 2x2)
# ==========================================


#  GRÁFICO 1: MÚLTIPLES DIAGRAMAS DE CAJA (Tiempo de juego vs Complejidad) 

# 1. Agrupamos los datos en intervalos de complejidad usando mutate y cut
juegos_limpios <- juegos_limpios %>%
  mutate(
    Rango_Complejidad = cut(Complexity_Average, 
                            breaks = c(0, 2, 3, 4, 5), 
                            labels = c("Iniciación (1-2)", "Medio (2-3)", "Avanzado (3-4)", "Experto (4-5)"))
  )

# 2. Generamos los múltiples diagramas de caja[cite: 7]
boxplot(Play_Time ~ Rango_Complejidad, data = juegos_limpios,
        main = "Evolución del tiempo de juego según su complejidad",
        xlab = "Nivel de complejidad",
        ylab = "Duración de la partida (minutos)", 
        col = c("lightblue", "lightgreen", "khaki", "lightcoral"),
        border = "midnightblue")


#  GRÁFICO 2: DIAGRAMA DE BARRAS APILADAS (Complejidad por Categoría) 

# 1. Función para calcular los porcentajes de dificultad
calcular_proporciones <- function(variable_categoria) {
  juegos_filtrados <- juegos_limpios$Rango_Complejidad[juegos_limpios[[variable_categoria]] == 1]
  porcentajes <- prop.table(table(juegos_filtrados)) * 100
  return(porcentajes)
}

# 2. Construimos la matriz combinada
matriz_apilada <- cbind(
  Infantil = calcular_proporciones("Children"),
  Party = calcular_proporciones("Party"),
  Familiar = calcular_proporciones("Family"),
  Abstracto = calcular_proporciones("Abstract"),
  Tematico = calcular_proporciones("Thematic"),
  Coleccionable = calcular_proporciones("Customizable"),
  Estrategia = calcular_proporciones("Strategy"),
  Wargame = calcular_proporciones("Wargames")
)

# 3. Ampliamos el margen derecho (8) para encajar la caja de la leyenda holgadamente
par(mar = c(6, 4, 4, 8) + 0.1)

# 4. Generamos el diagrama de barras apiladas
barplot(matriz_apilada,
        main = "Distribución de complejidad por categoría",
        ylab = "Porcentaje del catálogo (%)",
        col = c("lightblue", "lightgreen", "khaki", "lightcoral"),
        las = 2,
        border = "white",
        cex.names = 0.9)

# 5. Añadimos la leyenda enmarcada en un rectángulo profesional
legend("topright", 
       inset = c(-0.45, 0),  # Desplaza la caja hacia el centro del margen derecho
       xpd = TRUE, 
       legend = rownames(matriz_apilada), 
       fill = c("lightblue", "lightgreen", "khaki", "lightcoral"), 
       title = "Dificultad",
       bty = "o",            # Crea el rectángulo alrededor de la leyenda
       box.col = "darkgray", # Color del borde del rectángulo
       bg = "white",         # Fondo del rectángulo
       cex = 0.8)

# 6. Restauramos los márgenes por defecto
par(mar = c(5, 4, 4, 2) + 0.1)

#GRÁFICO 3: DIAGRAMA DE DISPERSIÓN Y RECTA DE REGRESIÓN (Nota vs Complejidad)

# 1. Calculamos el modelo de regresión lineal
modelo_lineal <- lm(Rating_Average ~ Complexity_Average, data = juegos_limpios)

# Extraemos los extremos reales de la variable X (Complejidad) para limitar las líneas
min_x <- min(juegos_limpios$Complexity_Average, na.rm = TRUE)
max_x <- max(juegos_limpios$Complexity_Average, na.rm = TRUE)

# 2. PRIMER GRÁFICO: Diagrama de dispersión con transparencia
plot(juegos_limpios$Complexity_Average, juegos_limpios$Rating_Average,
     main = "Regresión: Complejidad vs Nota Media",
     xlab = "Complejidad (Variable X)",
     ylab = "Nota Media (Variable Y)",
     col = rgb(0.3, 0.3, 0.3, alpha = 0.15), 
     pch = 16, cex = 0.6)

# Calculamos los puntos exactos de la recta para el mínimo y máximo de complejidad
y_pred <- predict(modelo_lineal, newdata = data.frame(Complexity_Average = c(min_x, max_x)))

# Trazamos la recta de regresión acotada
lines(x = c(min_x, max_x), y = y_pred, col = "red", lwd = 2)

# 3. SEGUNDO GRÁFICO: Análisis de residuos
plot(juegos_limpios$Complexity_Average, resid(modelo_lineal),
     main = "Análisis de Residuos del Modelo",
     xlab = "Complejidad (Variable X)",
     ylab = "Residuos (Error de predicción)",
     col = rgb(0.1, 0.4, 0.6, alpha = 0.3), 
     pch = 16, cex = 0.5)

# Trazamos la línea del cero acotada utilizando segmentos
segments(x0 = min_x, y0 = 0, x1 = max_x, y1 = 0, col = "red", lwd = 2, lty = 2)

# RESUMEN NUMÉRICO DEL MODELO 
print("--- COVARIANZA Y CORRELACIÓN (Complejidad vs Nota) ---")
print(paste("Covarianza:", round(cov(juegos_limpios$Complexity_Average, juegos_limpios$Rating_Average, use = "complete.obs"), 4)))
print(paste("Correlación de Pearson:", round(cor(juegos_limpios$Complexity_Average, juegos_limpios$Rating_Average, use = "complete.obs"), 4)))

