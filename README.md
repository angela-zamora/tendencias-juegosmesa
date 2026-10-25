# Análisis de tendencias, complejidad y popularidad en el mercado de los juegos de mesa

Este repositorio contiene el código fuente en R y la memoria académica del proyecto de Análisis Estadístico Avanzado. El estudio explora empíricamente el mercado moderno de los juegos de mesa (1950-2022), analizando cómo los factores de diseño, las categorías y la curva de dificultad impactan en la recepción final del público. El documento final donde se detallan las conclusiones en formato informe será publicado en diciembre de 2026, tras la presentación final del proyecto

## Fuente de los Datos
El análisis se fundamenta en el **BoardGameGeek (BGG) Dataset**, extraído y consolidado por Melissa Monfared.
*   **Enlace original:** [Kaggle - BoardGameGeek Dataset](https://www.kaggle.com/datasets/melissamonfared/board-games)
*   **Muestra utilizada:** 18.503 observaciones validadas tras los procesos de auditoría y calidad del dato.

## Estructura del Repositorio
*   **Codigo_R_Proyecto_Final.R`**: Script completo en R que incluye la importación, limpieza estructural, auditoría de NAs y detección de valores atípicos, así como el modelado estadístico y la generación de gráficos.

## Metodología y Tecnologías Aplicadas
El proyecto se ha desarrollado íntegramente en el entorno RStudio, ejecutando las siguientes fases técnicas:
1.  **Data Quality (DQ):** Control de dimensiones de completitud, unicidad y razonabilidad. Tratamiento de valores ausentes e identificación de atípicos (Outliers) mediante los métodos Z-score y Box-plot.
2.  **Transformación de Datos:** Uso de `dplyr` y `stringr` para la normalización de variables cualitativas de respuesta múltiple y la generación de variables dicotómicas.
3.  **Análisis Estadístico:** Medidas de dispersión y localización, histogramas, análisis bivariante mediante diagramas de caja cruzados y validación empírica a través de regresión lineal simple y análisis de residuos.

**Paquetes de R utilizados:** `tidyverse`, `dplyr`, `stringr`, `DescTools`.
