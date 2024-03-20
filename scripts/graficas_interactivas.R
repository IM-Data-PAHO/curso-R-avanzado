# graficas_interactivas.R -----------------------------------------------
#' Descripción: Este script muestra ejemplos de cómo utilizar plotly para
#' convertir gráficas de ggplot2 a una versión interactiva para reportes y
#' tableros.
#' 
#' Los temas principales abordados son:
#' 
#' 1. Función ggplotly() para generar gráficas interactivas
#' 2. Configuraciones de la interactividad (config)
#' 3. Texto flotante (tooltips)
#' 
#' 
#' Creado por ------------------------------------------------------------------
#' Nombre: Alejandro Vásquez
#' Creado en: 2024-03-07
#' Editorial -------------------------------------------------------------------
#' Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# Requisitos -------------------------------------------------------------------
source("requirements.R")

# # cargar paquetes necesarios
# pacman::p_load(
#   ggplot2,
#   plotly
# )

# Lectura de datos --------------------------------------------------------
#' Para no complicarnos vamos a usar otra vez los datos de *iris*.
data(iris)

#' Además, trabajaremos con una de las gráficas que generamos en la clase 
#' anterior:
grafico <- ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species),
             alpha = 0.5)+
  labs(x = "Largo de sépalo (cm)",
     y = "Ancho de sépalo (cm)",
     title = "Figura 1. Morfometría floral del género Iris",
     color = "Especie",
     caption = "Datos obtenidos de Becker, Chambers, y Wilks (1988)"
) + 
  theme_bw()

#' plotly es un paquete para generar gráficas interactivas diseñadas para 
#' páginas web.

# 1. Función ggplotly() para generar gráficas interactivas -----------
#' Con plotly se pueden hacer gráficas con una lógica distinta a ggplot2. Por 
#' suerte, plotly nos permite convertir gráficas que hayamos hecho en ggplot2
#' a un objeto de tipo plotly, gracias a la función *ggplotly()*:
grafico_interactivo <- ggplotly(grafico)
grafico_interactivo


# 2. Configuraciones de la interactividad (config) -------------------
#' plotly contiene varias opciones interactivas por defecto que podemos 
#' modificar con la función *config()*

#' Cambiar idioma de los elementos de interactividad
grafico_interactivo %>% 
  config(locale = "es")

#' Quitar logo de plotly
grafico_interactivo %>% 
  config(locale = "es",
         displaylogo = FALSE)

#' Habilitar la opción de zoom
grafico_interactivo %>% 
  config(locale = "es",
         displaylogo = FALSE,
         scrollZoom = TRUE)

#' Esconder opciones del *modebar* específicas
#' Referencia [https://plotly.com/r/configuration-options/]
grafico_interactivo %>% 
  config(locale = "es",
         displaylogo = FALSE,
         scrollZoom = TRUE,
         modeBarButtonsToRemove = c(
           "zoom2d", "pan2d", "lasso2d",
           "hoverClosestCartesian", "hoverCompareCartesian"
         )
         )

#' Agregar más botones al *modebar*
#' Referencia [https://plotly.com/r/configuration-options/]
grafico_interactivo %>% 
  config(locale = "es",
         displaylogo = FALSE,
         scrollZoom = TRUE,
         modeBarButtonsToAdd = c(
           "drawline", # dibujar líneas rectas
           "drawopenpath", # dibujar líneas libres
           "drawcircle", # dibujar círculos
           "drawrect", #dibujar rectángulos
           "eraseshape" # borrador
         )
  )

#' Modificar la descarga de imagen
grafico_interactivo %>% 
  config(locale = "es",
         displaylogo = FALSE,
         toImageButtonOptions = list(
           format = "png", # archivo png
           filename = "morfometria_iris", # nombre de la gráfica
           height = 800, # altura en pixeles
           width = 1200, # ancho en pixeles
           scale = 1 # escala
         )
  )

#' Hacer que siempre se observe el *modebar*
grafico_interactivo %>% 
  config(
         displayModeBar = TRUE
  )

#' Ocultar el *modebar*
grafico_interactivo %>% 
  config(
    displayModeBar = FALSE
  )

# 3. Texto flotante (tooltips) ---------------------------------------
#' Podemos modificar el texto flotante de nuestros elementos gráficos. Para
#' empezar, debemos modificar el texto desde ggplot2

grafico_texto <- ggplot(data = iris) +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species,
                 text = paste0(
                   "Especie: ", Species, "<br>",
                   "Ancho de sépalo (cm): ", Sepal.Width, "<br>",
                   "Largo de sépalo (cm): ", Sepal.Length, "<br>"
                 )
                 ),
             alpha = 0.5)+
  labs(x = "Largo de sépalo (cm)",
       y = "Ancho de sépalo (cm)",
       title = "Figura 1. Morfometría floral del género Iris",
       color = "Especie",
       caption = "Datos obtenidos de Becker, Chambers, y Wilks (1988)"
  ) + 
  theme_bw()

grafico_texto

ggplotly(grafico_texto, tooltip = "text")  
