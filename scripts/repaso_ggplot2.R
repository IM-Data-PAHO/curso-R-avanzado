# repaso_ggplot2.R ---------------------------------------------------
#' Descripción: Este script muestra un repaso del uso de ggplot2 para
#' crear gráficas estáticas.
#' 
#' Los temas principales abordados son:
#' 
#' 1. Construcción del gráfico inicial con *ggplot()*
#' 2. Estéticos y geometrías
#' 3. Tipos de gráficos
#' 4. Colores
#' 5. Función labs() para etiquetas varias
#' 6. Temas
#' 
#' Creado por -------------------------------------------------------------------
#' Nombre: Alejandro Vásquez
#' Creado en: 2024-03-07
#' Editorial -------------------------------------------------------------------
#' Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# Requisitos -------------------------------------------------------------------
source("requirements.R")

# # cargar paquetes necesarios
# pacman::p_load(
#   ggplot2
# )



# Lectura de datos --------------------------------------------------------
#' Para no complicarnos vamos a usar otra vez los datos de *iris*.
data(iris)

# 1. Construcción del gráfico inicial con *ggplot()* ----------------------
#' Para hacer el objeto inicial del gráfico utilizamos *ggplot()*. Este 
#' necesita recibir el dataframe con los datos a graficar. Además se le pueden
#' indicar estéticos que se le aplicarían a todas las geometrías.

#' Generemos el gráfico inicial (vacío) para comparar el largo de sépalo 
#' (Sepal.Length) contra el ancho de sépalo (Sepal.Width).
grafico <- ggplot(data = iris, aes(x = Sepal.Length,
                                   y = Sepal.Width))

grafico


# 2. Estéticos y geometrías -----------------------------------------------
#' Note que el gráfico anterior aún no tiene elementos gráficos más allá de
#' los ejes. Para agregar dichos elementos, debemos usar geometrías para 
#' indicarle a ggplot2 qué debe graficar. Para agregar capas a nuestro gráfico
#' se le van "sumando" con el símbolo +.

# Gráfico de puntos:
grafico +
  geom_point()

# Estéticos en geometrías:
#' Recuerde que el objeto [grafico] tiene estéticos de ejes *x* y *y*. Si
#' queremos mostrar un estético particular para una geometría, pero no al resto
#' del gráfico, podemos indicarlo explícitamente en dicha geometría.
grafico +
  geom_point(aes(color = Species))

# 3. Ejemplos de gráficas -------------------------------------------------

## a. Gráficas de puntos --------------------------------------------------
#' Ya vimos un ejemplo de gráfico de puntos en el paso anterior, pero veamos
#' algunas configuraciones adicionales de los gráficos de puntos.

#' Cambiar el tamaño de los puntos con el argumento *size*
grafico +
  geom_point(aes(color = Species),
             size = 5)

#' Cambiar el tamaño de los puntos con el argumento *size* como un estético
#' según otra variable numérica. [A este gráfico se le llama bubble plot].
grafico +
  geom_point(aes(color = Species, size = Petal.Length))

#' Transparencia de los elementos gráficos:
grafico +
  geom_point(aes(color = Species, size = Petal.Length),
             alpha = 0.5)

## b. Gráficas de líneas ---------------------------------------------------
grafico +
  geom_line()

#' Cambiar color según una variable categórica
grafico +
  geom_line(aes(color = Species))

#' Cambiar el tipo de línea según una variable categórica
grafico +
  geom_line(aes(linetype = Species))

#' Cambiar más de un elemento gráfico para la misma variable categórica
grafico +
  geom_line(aes(linetype = Species, color = Species))

#' Cambiar el ancho de la línea
grafico +
  geom_line(aes(linetype = Species, color = Species),
            linewidth = 1)


## c. Gráficas de columnas y de barras -------------------------------------
#' Error cuando se le dan dos estéticos [x] y [y]
grafico +
  geom_bar()

#' *geom_bar()* no entiende dos estéticos [x] y [y] por defecto. Para no
#' volver a tener este problema, utilizaremos estéticos únicamente para
#' las geometrías y no para todo el gráfico.

grafico <- ggplot(data = iris)
grafico # Note que el gráfico está completamente vacío (no tiene ejes)

#' Gráficas de barra de frecuencias con una sola variable
grafico +
  geom_bar(aes(x = Sepal.Length))

#' Gráficas de barras con dos variables:
#' Para hacer que *geom_bar()* entienda que existen dos variables [x] y [y],
#' debemos indicar el parámetro [stat = "identity"]
grafico +
  geom_bar(aes(x = Species, y = Sepal.Length),
           stat = "identity")

#' Cambiar el color del relleno
grafico +
  geom_bar(aes(x = Species, y = Sepal.Length),
           stat = "identity", fill = "yellow")

#' Cambiar color del relleno según la especie
grafico +
  geom_bar(aes(x = Species, y = Sepal.Length, fill = Species),
           stat = "identity")


# 4. Colores --------------------------------------------------------------
#' Normalmente no se utilizan los colores que ggplot2 usa por defecto. Podemos
#' indicar colores específicos con varias funciones.

#' Colores en gráficas de puntos y líneas:
#' Se utiliza *scale_color_manual()*
grafico +
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  scale_color_manual(values = c(
    "#41b6c4",
    "#a1dab4",
    "#253494"
  ))

#' Colores en gráficas de barras:
#' Se utiliza *scale_fill_manual()*
grafico +
  geom_bar(aes(x = Species, y = Sepal.Length, fill = Species),
           stat = "identity"
           ) +
  scale_fill_manual(values = c(
    "#41b6c4",
    "#a1dab4",
    "#253494"
  ))

# 5. Función labs() para etiquetas varias ---------------------------------
#' Note que los nombres de los ejes y las leyendas no son los ideales debido a 
#' que usan los nombres de nuestras variables en el dataframe. Con la función
#' *labs()* podemos modificar los nombres de los elementos de nuestra gráfica.
#' Antes de empezar, guardaremos una gráfica de puntos que utilizaremos en
#' el resto de ejemplos de este script.
grafico_final <- grafico +
  geom_point(aes(x = Sepal.Length,
                 y = Sepal.Width,
                 color = Species, 
                 size = Petal.Length),
             alpha = 0.5)

grafico_final

#' Nombres de ejes [x] y [y]
grafico_final +
  labs(x = "Largo de sépalo (cm)",
       y = "Ancho de sépalo (cm)")

#' Título
grafico_final +
  labs(x = "Largo de sépalo (cm)",
       y = "Ancho de sépalo (cm)",
       title = "Figura 1. Morfometría floral del género Iris"
       )

#' Leyendas
grafico_final +
  labs(x = "Largo de sépalo (cm)",
       y = "Ancho de sépalo (cm)",
       title = "Figura 1. Morfometría floral del género Iris",
       size = "Largo de pétalo (cm)",
       color = "Especie"
  )

#' Pie de página
grafico_final_final <- grafico_final +
  labs(x = "Largo de sépalo (cm)",
       y = "Ancho de sépalo (cm)",
       title = "Figura 1. Morfometría floral del género Iris",
       size = "Largo de pétalo (cm)",
       color = "Especie",
       caption = "Datos obtenidos de Becker, Chambers, y Wilks (1988)"
  )
grafico_final_final

# 6. Temas ----------------------------------------------------------------
#' Por último, podemos modificar los temas (elementos generales visuales) de
#' las gráficas. R posee varios temas por defecto que funcionan bien. Veamos
#' algunos ejemplos:
# Tema clásico
grafico_final_final +
  theme_classic()

# Tema mínimo
grafico_final_final +  
  theme_minimal()

# Tema ligero
grafico_final_final +  
  theme_light()

# Tema oscuro
grafico_final_final +  
  theme_dark()


# Tema oscuro sobre claro
grafico_final_final +  
  theme_bw()

# Tema con grid fuerte
grafico_final_final +  
  theme_linedraw()



