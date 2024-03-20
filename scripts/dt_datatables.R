# dt_datatables.R -----------------------------------------------
#' Descripción: Este script muestra ejemplos de cómo generar tablas interactivas
#'  con el paquete DT.


#' Los temas principales abordados son:
#' 1. Generación de tablas interactivas con *datatable()*
#' 2. Clases (diseños de las tablas)
#' 3. Nombres de filas
#' 4. Nombres de columnas
#' 5. Pie de página
#' 6. Filtros de columnas
#' 7. Argumento *options*
#' 
#' Creado por ------------------------------------------------------------------
#' Nombre: Alejandro Vásquez
#' Creado en: 2024-03-07
#' Editorial -------------------------------------------------------------------
#' Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# Requisitos -------------------------------------------------------------------
source("requirements.R")

# cargar paquetes necesarios
# pacman::p_load(
#   DT,
#   htmltools
# )

# 0. Datos para estos ejemplos --------------------------------------------
#' Para estos ejemplos utilizaremos la famosa tabla de datos que incluye R:
#' *iris*. Así que primero, debemos cargarla con la función *data()*:
data(iris)

#' Visualizemos rápidamente la estructura de *iris*:
head(iris)
glimpse(iris)

# 1. Generación de tablas interactivas ------------------------------------
#' Para convertir un dataframe a una tabla interactiva, simplemente debemos
#' pasar nuestro objeto con los datos a la función *datatable()*:
datatable(iris) # Observe cómo se ve en la pestaña *Viewer*


# 2. Clases CSS de las tablas ---------------------------------------------
#' Existen clases CSS (hojas de estilo para mejorar visualizaciones web). La
#' función *datatable()* permite utilizar clases predefinidas fáciles de llamar
#' a través del argumento *class*. 
#' En este enlace puede ver qué clases existen:
#' [https://datatables.net/manual/styling/classes]

# Observe estos ejemplos y describa los cambios observados:

datatable(iris, class = "cell-border")

datatable(iris, class = "compact")

datatable(iris, class = "hover")

datatable(iris, class = "row-border")

datatable(iris, class = "stripe")


# 3. Quitar nombres de filas -------------------------------------------------
#' *datatable()* por defecto coloca el nombre de las filas del dataframe. Si
#' nunca especificamos el nombre de dichas columnas, R les asigna números. Para
#' la visualización de la tabla suele no interesarnos mostrar estos nombres. 
#' Para quitarlos, podemos colocar como {FALSE} el parámetro *rownames*:

datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE
          )

# 4. Modificar nombres de columnas ----------------------------------------
#' Muchas veces, los nombres de columnas con las que trabajamos en los scripts
#' no son ideales para generar una visualización. Es decir, suelen faltar
#' espacios, tildes/acentos, símbolos, entre otros. Si bien podemos modificar
#' estos nombres con *dplyr* [¿Recuerdan cómo hacer esto?], puede ser más fácil
#' editarlas únicamente para la tabla que vamos a presentar utilizando el
#' parámetro *colnames*, pasándole un vector de caracteres con los nombres en 
#' el mismo orden de las columnas.

datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
                       )
)


# 5. Pie de página --------------------------------------------------------
#' También podemos colocar un pie de página o títulos en el *datatable()*
#' Para hacer un título:
datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
          ),
          caption = tags$caption(
            style = 'caption-side: top; text-align: center;',
            strong('Cuadro 1.'), em('Hola, soy un título.')
          )
)

#' Para hacer un pie de página:
datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
          ),
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            em('Hola, soy un pie de página.')
          )
)

# 6. Agregar filtros de columnas ------------------------------------------
#' Podemos agregar filtros de las columnas para que el usuario pueda
#' explorar más a fondo la tabla. Para esto, utilizamos el parámetro
#' *filter*:

datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          filter = "top",
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
          ),
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            em('Hola, soy un pie de página.')
          )
)


# 7. Argumento *options* --------------------------------------------------
#' En este argumento podemos hacer muchas más modificaciones a los objetos 
#' datatable(). A continuación veremos algunos ejemplos.

## Especificar elementos a modificar del datatable() -----------------------
#' Dominios de *datatable()*:
#' [https://datatables.net/reference/option/dom]

datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          filter = "top",
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
          ),
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            em('Hola, soy un pie de página.')
          ),
          options = list(
            dom = "Btp"
          )
)

## Paginación y scrolls -------------------------------------------------

# Paginación
datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          filter = "top",
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
          ),
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            em('Hola, soy un pie de página.')
          ),
          options = list(
            dom = "Btp",
            pageLength = 10
          )
)

# Scrolling
datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          filter = "top",
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
          ),
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            em('Hola, soy un pie de página.')
          ),
          options = list(
            dom = "Btp",
            paging = FALSE,
            scrollX = TRUE
          )
)

## Botones para descargas -----------------------------------------------

datatable(iris,
          class = c("stripe", "hover"),
          rownames = FALSE,
          filter = "top",
          colnames = c("Largo de sépalo (cm)",
                       "Ancho de sépalo (cm)",
                       "Largo de pétalo (cm)",
                       "Ancho de pétalo (cm)",
                       "Especie"
          ),
          caption = tags$caption(
            style = 'caption-side: bottom; text-align: center;',
            em('Hola, soy un pie de página.')
          ),
          extensions = "Buttons",
          options = list(
            dom = "Btp",
            paging = FALSE,
            scrollX = TRUE,
            buttons = list(
              "csv", "excel", "pdf"
            )
          )
)

