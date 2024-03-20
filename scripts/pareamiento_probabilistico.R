# pareamiento_probabilistico.R -------------------------------------------------
# Descripción: Este script provee ejemplos de limpieza de datos y
# pareamiento probabilistico, basados en [1].
#
# Referencias:
#   1. https://htmlpreview.github.io/?https://github.com/djvanderlaan/reclin2/blob/master/inst/doc/introduction.html
#   2. https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance
#
# Creado por -------------------------------------------------------------------
# Nombre: Dan Alvarez, Ignacio Castro
# Creado en: 2024-02-22
# Editorial --------------------------------------------------------------------
# Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# Requisitos -------------------------------------------------------------------
source("requirements.R")

# cargar reclin2 por aparte
pacman::p_load(
  reclin2
)

# # cargar paquetes necesarios
# pacman::p_load(
#   rio,
#   reclin2,
#   dplyr,
#   janitor
# )

# Introducción -----------------------------------------------------------------
#
# ¿Qué queremos lograr?
# 
# Dadas dos bases de datos, que pueden contener errores de ingreso de datos,
# quisiéramos juntarlas. Por ejemplo, podríamos tener dos bases de datos que
# contengan el nombre, apellido y sexo de un individuo por fila. Una de ellas
# podría pertenecer al registro civil de un país, mientras que otra podría
# pertenecer al registro nominal de vacunación (RNVe). Además, podríamos
# encontrarnos ante la situación de que no contamos con un código único
# identificador en alguna (o ninguna) de las bases, por lo que la unión de
# ambas, sobre todo si tienen errores, no es trivial.
#
# ¿Cómo lo logramos?
#
# Aquí entra en juego el pareamiento probabilístico, una técnica estadística
# para "emparejar" registros que no encajan directamente.
#
# ¿Cómo se verían estas bases?
#
# Exploremos esto en las próximas secciones.

# Lectura ----------------------------------------------------------------------
# Hemos generado dos bases que servirán de ejemplo para esta lección.
# En concreto, estas bases son subconjuntos de las bases que ya conocemos,
# filtradas para todos aquellos ID que cumplan con ser de la forma
# "X.111.1XX-X".
registro_civil <- import("./data/pareamiento/registro_civil.xlsx")
rnve <- import("./data/pareamiento/rnve.xlsx")

# Exploración inicial ----------------------------------------------------------
# Exploremos las bases. ¿Qué columnas contienen y cómo se ven los datos?
head(rnve)
head(registro_civil)
# ¿Cuántas filas tienen?
nrow(rnve)
nrow(registro_civil)
# A partir de esta exploración, notamos dos cosas:
#
#   1.  El RNVe se compone solamente de primeras dosis y contiene 14
#       observaciones. Se nos proveen columnas nombre, apellido y sexo, y
#       otras tres con el sufijo "_original". Puesto que esta base ha sido
#       modificada para que contenga errores, las columnas con el sufijo antes
#       mencionado serán usadas solo para referencia.
#   2.  El registro civil se compone de 49 observaciones. ¿Por qué son más que
#       en el RNVe? Bueno, el RNVe nunca será idéntico al registro civil. Para
#       empezar, quienes no se hayan vacunado no aparecerán en el RNVe. Así
#       mismo, puesto que este RNVe se concentra en primeras dosis, hacen falta
#       las segundas dosis.
#
# Por lo tanto, este es un problema realista. Tenemos un RNVe que se concentra
# en primeras dosis y un registro civil que contiene la información de las
# 14 personas en nuestro RNVe, más otras 35 personas. Nosotros quisiéramos
# juntar ambas bases, pues para propósitos de seguimiento de cohortes, nos
# resulta importante tener tanto la información del registro civil, como los
# datos de vacunación de las personas.

# Definición del ejercicio -----------------------------------------------------
#
# Junte las bases rnve y registro_civil utilizando solamente las columnas
# "nombre", "apellido" y "sexo".
#
# ¿Cómo lo hacemos?
#
# Exploremos varias formas en las próximas secciones.

# Emparejamiento probabilístico ------------------------------------------------
#
# Puesto que las columnas "nombre", "apellido" y "sexo" contienen errores, no
# podemos hacer un simple dplyr::left_join. Las variables de cada fila
# simplemente no van a coincidir.
#
# Por lo tanto, debemos tomar un camino más largo: analizar cada par posible.
# Es decir, revisemos la fila 1 del RNVe contra la fila 1 del registro civil,
# luego revisemos la fila 1 del RNVe contra la fila 2 del registro civil, etc.
# Una vez hayamos agotado todas las combinaciones posibles de la fila 1 del
# RNVe, pasamos a la fila 2.
#
#   1. Fila 2 RNVe contra Fila 1 registro civil
#   2. Fila 2 RNVe contra Fila 2 registro civil
#   3. Fila 2 RNVe contra Fila 3 registro civil
#   4. ...
#
# Y así, hasta agotar todas las combinaciones posibles. ¿Cuántas hay? Si tenemos
# 14 filas en el RNVe y 49 en el registro civil, son 14*49 = 686 posibilidades.

## Emparejamiento --------------------------------------------------------------
# Afortunadamente, existen diversas formas de generar estas parejas automática-
# mente. Una de ellas la provee el paquete reclin2, que estaremos utilizando a
# lo largo de esta lección.
parejas <- reclin2::pair(rnve, registro_civil)
# Este paquete utiliza al paquete data.table para toda la transformación de
# datos interna. Por lo tanto, podemos hacer un print() para obtener alguna
# información importante.
print(parejas)
# Vemos que hay 686 parejas en total, como lo calculamos previamente. En sí,
# la función reclin2::pair() coloca en la columna ".x" todas las filas del RNVe,
# y en la columna ".y" todas las filas del registro civil.

## Comparación determinista ----------------------------------------------------
# Podemos realizar una comparación básica (o determinista) de cada pareja de
# filas. Para ello, usamos la función compare_pairs de reclin2.
parejas <- reclin2::compare_pairs(
  # Las parejas que vamos a comparar
  parejas,
  # Los nombres de las variables que queremos comparar
  on = c("nombre", "apellido", "sexo")
)
# ¿Qué hace esto? Simplemente nos dice si las parejas de filas coinciden (para
# cada columna). Por ejemplo, si la fila 1 del RNVe tiene el mismo nombre que
# la fila 1 del registro civil, nos devolverá TRUE. Si no, será FALSE. Revisemos
# los resultados para cada columna:
table(parejas$nombre)
table(parejas$apellido)
table(parejas$sexo)
# Vemos que para las columnas "nombre" y "apellido", no hay ningún par de filas
# que contenga lo mismo. Esto tiene sentido: si los strings de los nombres y
# apellidos no concuerdan (aunque sea por un solo caracter, o una sola
# mayúscula) esta comparación será FALSE. En cuanto a la columna sexo, vemos
# que sí hay algunas columnas que coinciden. Esto también tiene sentido:
# puesto que esta tiene menos variaciones, es más probable que algunas filas
# coincidan entre sí.
#
# En conclusión, simplemente comparar cada pareja de filas y tratar de buscar
# una coincidencia exacta no es un buen método de emparejamiento (a esto se le 
# llama emparejamiento determinístico). Para mejorarlo, debemos
# introducir una nueva métrica que permita ver si dos strings "se parecen",
# aunque no sean idénticos.

## Métricas para comparación de strings ----------------------------------------
# El paquete reclin2 provee varias funciones de comparación. Una de ellas
# implementa el método de Jaro-Winkler. Más allá de los detalles de este
# método, que está bien explicado en [2], podemos decir que nos provee un
# método cuantitativo para verificar si dos strings se parecen.
# 
# Hagamos una prueba:
string1 <- "Marta Del Carmen"
string2 <- "Martha del ccarmen"
string3 <- "Antonio Machado"
# Vemos que los dos primeros strings no son idénticos, pero nuestra intuición
# nos lleva a concluir que se parecen y, de hecho, el segundo parece ser
# producto de un error de tecleado. La métrica de Jaro-Winkler nos permite
# llevar esta intuición a números concretos.
### Comparación 1 --------------------------------------------------------------
comparar_1_1 <- 1 - stringdist::stringdist(string1, string1, method = "jw")
comparar_1_1
### Comparación 2 --------------------------------------------------------------
comparar_1_2 <- 1 - stringdist::stringdist(string1, string2, method = "jw")
comparar_1_2
### Comparación 3 --------------------------------------------------------------
comparar_1_3 <- 1 - stringdist::stringdist(string1, string3, method = "jw")
comparar_1_3
### Conclusiones ---------------------------------------------------------------
# De los tres pasos anteriores, podemos notar la siguiente:
#   1.  El resultado es 1 cuando los strings son idénticos (ver comparar_1_1)
#   2.  El resultado es más cercano a 1 cuando los strings no son idénticos,
#       pero se parecen (ver comparar_1_2)
#   3.  Entre más se acerca el resultado a 0, más disimilares son los strings
#       (ver comparar_1_3)

## Una comparación más 'inteligente' -------------------------------------------
# Con lo que vimos anteriormente, podemos pasar a hacer una comparación más
# "inteligente". Es decir, en lugar de esperar que dos strings sean iguales,
# tratemos de evaluar si al menos "se parecen". Para esto, usaremos el mismo
# método de Jaro-Winkler visto anteriormente. Afortunadamente, el paquete
# reclin2 ya incluye una implementación de este método, basada en la función
# stringdist() que vimos antes.
parejas <- compare_pairs(
  parejas,
  on = c("nombre", "apellido", "sexo"),
  # Este argumento le indica a la función que debe utilizar el método
  # de Jaro-Winkler. La función cmp_jarowinkler() se basa en stringdist().
  # NOTA: La función cmp_jarowinkler se puede utilizar independientemente. En
  #       ese caso, el argumento indica el valor de corte para similitud entre
  #       strings (es decir, si superan 0.9, se consideran iguales). Sin
  #       embargo, en este caso, el argumento no se utiliza, y podemos colocar
  #       cualquier número.
  default_comparator = cmp_jarowinkler(0.9)
)
print(parejas)
# ¿Cuál fue el resultado? Ahora, las columnas "nombre", "apellido" y "sexo" no
# contienen valores binarios (TRUE o FALSE), sino contienen un número entre 0
# y 1. De acuerdo a la misma lógica mostrada arriba, entre más se acerca el
# número a 1, más se parecen los strings. Este es el primer paso para un
# "emparejamiento probabilístico". Los números que hemos obtenidos representan
# una especie de probabilidad: nos permiten medir qué tan similares son dos
# strings (y tomar decisiones en base a ello).

## Una calificación final ------------------------------------------------------
# Recordemos que el proceso de emparejamiento probabilístico empezó con
# formar todas las parejas posibles de registros entre las bases que buscamos
# unir. Luego, obtuvimos una calificación de "similitud" para cada pareja y
# columna. Ahora, unamos estos resultados para obtener una métrica final. Esta
# la usaremos para determinar cuáles parejas (de todas las posibilidades que
# formamos) realmente se pertenecen la una a la otra.
#
# Podemos obtener una métrica simple haciendo una suma de la tres métricas
# obtenidas anteriormente. Puesto que cada columna contiene un número entre
# 0 y 1, el resultado final será un número entre 0 y 3, donde 3 es que las
# parejas tienen mayor "probabilidad" de pertenecerse entre sí.
#
# Hagamos esto con la función score_simple de reclin2.
parejas <- reclin2::score_simple(
  parejas,
  # Este será el nombre del puntaje final, resultado de la suma de los
  # puntajes de las tres columnas anteriores.
  "puntaje",
  # Las columnas que sumaremos
  on = c("nombre", "apellido", "sexo")
)
print(parejas)

## Seleccionando registros -----------------------------------------------------
# Ahora, con una calificación final para cada pareja de registros, debemos
# seleccionar aquellos que tienen mayor probabilidad de pertenecerse. Podemos
# explorar visualmente la base y obtener el valor mínimo y máximo para nuestro
# puntaje:
min(parejas$puntaje)
max(parejas$puntaje)
### Un enfoque básico ----------------------------------------------------------
# Dado que el puntaje mínimo es ~0.4 y el puntaje máximo es ~2.3, podríamos
# decir que aceptaremos todas aquellas puntaciones que estén por encima del 75%.
# Es decir, todas aquellas arriba de 1.8. Para ello, podemos usar la función
# select_threshold de reclin2.
parejas <- reclin2::select_threshold(
  parejas,
  # Este será el nombre de la nueva columna, que será TRUE para todos aquellos
  # puntajes que estén por sobre nuestro límite.
  variable = "limite",
  # La columna con el puntaje de cada pareja, esta se comparará contra el límite
  # definido abajo
  score = "puntaje",
  # Nuestro límite
  threshold = 1.8
)
print(parejas)
# ¿Fue esta una buena técnica? Para empezar, veamos cuántas parejas han pasado
# nuestro límite.
table(parejas$limite)
# Hay 83 parejas que superan el límite de 1.8 que elegimos. Esto es bueno (pues
# hemos reducido la cantidad de parejas posibles de 686 a 83), pero no lo
# suficiente. Recordemos: necesitábamos formar solo 14 parejas, pues queremos
# unir 14 registros de nuestro RNVe con el registro civil que se nos dió.

## Una exploración más profunda ------------------------------------------------
# Antes de pasar al siguiente paso, evaluemos un poco más la calidad de nuestro
# emparejamiento. Recordemos que para este ejercicio contamos con el ID de cada
# individuo en ambas bases. No lo hemos usado, pero lo usaremos ahora para
# evaluar cuántos de los individuos que han superado nuestro límite, realmente
# debieran emparejarse (y cuántos son un falso positivo).
#
# Usaremos la función compare_vars de reclin2 una vez más, pero esta vez para
# comparar los ID.
parejas <- compare_vars(
  parejas,
  # Una columna que será TRUE cuando los ID coincidan perfectamente.
  # FALSE si no.
  variable = "verdad",
  # El nombre de las columnas que usaremos para comparar (ambas se llaman ID)
  on_x = "ID", on_y = "ID"
)
print(parejas)
# Con esto, tenemos dos cosas: la columna "limite" nos dice todas aquellas
# parejas que han superado nuestra calificación mínima de 1.8. Y la columna
# "verdad" nos dice cuales registros realmente se pertenecen entre sí.
# Comparemos ambas.
table(parejas$verdad, parejas$limite)
# Interpretaremos el cuadro anterior de acuerdo a la siguiente leyenda:
#
# VN = VERDADEROS NEGATIVOS  FP = FALSOS POSITIVOS
# FN = FALSOS NEGATIVOS      VP = VERDADEROS POSITIVOS
#
# Por lo tanto, en nuestro primer acercamiento al
# emparejamiento probabilístico hemos encontrado:
#   1.  VP = 7 parejas que sí se pertenecen entre sí
#   2.  FN = 7 parejas que se pertencen entre sí, pero no los hemos identificado
#   3.  FP = 76 parejas que no se pertencen entre sí, pero que nuestro
#       algoritmo ha determinado que se parecen
#   4.  VP = 596 parejas que no se pertencen entre sí y nuestro algoritmo las ha
#       descartado correctamente
#
# Para tener un algoritmo exitoso, quisiéramos clasificar a todas las parejas
# en dos contenedores: todas aquellas parejas que no se pertencen, y todas
# aquellas que sí. En este caso, tenemos dos contenedores "de más" (sobrantes):
# los de los falsos positivos (parejas que nuestro algoritmo encontró pero
# que realmente no se pertencen) y los falsos negativos (parejas que nuestro
# algoritmo falló en encontrar).
#
# Entonces, tenemos algunos problemas por arreglar. Debemos corregir todos los
# falsos negativos y falsos positivos que hemos obtenido. Podríamos pensar
# en reducir el límite que elegimos anteriormente. De esa forma, reduciremos
# la cantidad de falsos negativos (pues incluiremos a más parejas dentro de
# nuestro modelo). Probemos hacerlo bajando el límite a 1.3.
parejas <- reclin2::select_threshold(parejas, "limite", score = "puntaje", threshold = 1.3)
table(parejas$verdad, parejas$limite)
# En efecto, ahora solo hay 1 falso negativo, pero surge un efecto indeseado:
# incrementan la cantidad de falsos positivos. Esto hace sentido: si reducimos
# la calificación mínima para aceptar parejas, entrarán más parejas que no
# debieran estar relacionadas.

### Un enfoque más 'inteligente' -----------------------------------------------
# A lo largo de este proceso, hemos obviado una restricción importante: para
# cada fila en nuestro RNVe, esperamos encontrar solamente 1 coincidencia en el
# registro civil. Es decir, no esperamos encontrar a una persona en el RNVe
# en dos instancias dentro del registro civil. Lo mismo aplica al contrario:
# cada persona dentro del registro civil debe aparecer solo una vez dentro
# de nuestro RNVe (puesto que estamos trabajando solo con primeras dosis).
#
# Por lo tanto, podemos aplicar una lógica de selección "egoista." Este método
# siempre usará un límite mínimo para hacer la selección, pero, además,
# impondrá la restricción que mencionamos anteriormente. Si una fila ya ha
# encontrado a su pareja, no se usará más. Usemos la función select_greedy de
# reclin2 para esto.
parejas <- reclin2::select_greedy(
  parejas,
  # La columna con el puntaje, que usaremos para descartar parejas que no
  # cumplan con el mínimo establecido en "threshold".
  score = "puntaje",
  # El nombre de una nueva columna, que almacenará el resultado de nuestro
  # algoritmo. Esta columna será TRUE cuando las parejas cumplan las condiciones
  # mencionadas arriba. FALSE de lo contrario.
  variable = "resultado",
  # El límite mínimo para aceptar a una pareja.
  threshold = 1.8
)
table(parejas$verdad, parejas$resultado)
# ¿Qué logramos? Por un lado se redujeron la cantidad de falsos positivos. Había
# 76 anteriormente, ahora hay solo 5. Esto hace sentido, pues gran cantidad
# de falsos positivos surgían porque podíamos emparejar filas de manera
# no exclusiva. Sin embargo, vemos que se redujeron la cantidad de "verdaderos
# positivos" (parejas que definitivamente se pertenecen) y aumentaron la
# cantidad de falsos negativos (parejas que nuestro algoritmo, erróneamente,
# no correlacionó).
#
# ¿Cuál es el problema? Bueno, el algoritmo solo será tan bueno como los datos
# que le proveamos. Y no nos esforzamos en proveerle buenos datos, pues tomamos
# las columnas "a como venían". Antes de hacer emparejamiento, conviene tomarse
# un tiempo para limpiar las bases de datos lo más posible.
#
# Por ejemplo, en este caso estamos usando strings para realizar el
# emparejamiento. Estos strings contienen errores, y algunos de ellos se pueden
# corregir de manera sencilla. Podemos hacer lo siguiente para tratar de
# estandarizar ambas bases lo más posible.
#   1.  Convertirlo todo en minúsculas. Si hubo errores de tecleado que
#       insertaron mayúsculas donde no debían ir, esto ya no será un problema.
#   2.  Eliminar acentos. A veces dos strings pueden ser idénticos, pero uno
#       puede contener acentos y el otro no. Si nos encargamos de eliminarlos
#       primero, podemos mejorar la nota de similitud entre ellos.
#   3.  Eliminar espacios extra. Por errores de tecleado, se podría insertar
#       más de un espacio entre los dos nombres de una persona y esto hará la
#       comparación más complicada. Podemos mejorar la nota si nos
#       encargamos de estandarizar la cantidad de espacios.
#
# Todo lo anterior lo logramos con una sola función: make_clean_names del
# paquete janitor.

# Una limpieza previa ----------------------------------------------------------
# Limpiamos el RNVe
rnve_estandar <- rnve %>% 
  # Mediante la función across() de dplyr, aplicamos la función make_clean_names
  # a las columnas "nombre", "apellido" y "sexo".
  mutate(
    across(
      c("nombre", "apellido", "sexo"),
      ~ janitor::make_clean_names(., allow_dupes = T)
    )
  )
# Limpiamos el registro civil
registro_civil_estandar <- registro_civil %>% 
  mutate(
    across(
      c("nombre", "apellido", "sexo"),
      ~ janitor::make_clean_names(., allow_dupes = T)
    )
  )

# Algoritmo final --------------------------------------------------------------
# Con la limpieza hecha, repetimos todos los pasos que mostramos anteriormente.
# Formamos todas las parejas
parejas_final <- pair(rnve_estandar, registro_civil_estandar)
# Calculamos una métrica de similitud para cada columna
parejas_final <- compare_pairs(
  parejas_final,
  on = c("nombre", "apellido", "sexo"),
  default_comparator = cmp_jarowinkler(0.9)
)
# Calculamos una métrica global
parejas_final <- score_simple(
  parejas_final,
  "puntaje",
  on = c("nombre", "apellido", "sexo"),
  w1 = c(nombre = 1, apellido = 1, sexo = 0.5)
)
# Agregamos el límite de concordancia 1 a 1
parejas_final <- select_greedy(
  parejas_final, score = "puntaje", variable = "resultado", threshold = 1.8
)
# Comparamos contra la verdad (a través del ID)
parejas_final <- compare_vars(parejas_final, "verdad", on_x = "ID", on_y = "ID")
# Evaluamos la calidad del emparejamiento mejorado.
table(parejas_final$verdad, parejas_final$resultado)
# Vemos que ahora, el emparejamiento es PERFECTO. De las 686 posibles parejas,
# hemos descartado las 672 que no se pertencen y hemos encontrado correctamente
# a las 14 que nos interesaban.
#
# Con esto listo, ¿cómo unimos al registro civil con el RNVe? El paquete
# reclin2 provee una función final para esto: link.
base_final <- reclin2::link(
  parejas_final,
  # Haremos la unión de acuerdo a la columna "resultado", que fue el resultado
  # del emparejamiento "egoista" que realizamos con select_greedy.
  selection = "resultado"
)

# Conclusiones -----------------------------------------------------------------
# El emparejamiento probabilístico puede ser una tarea compleja. En ocasiones,
# puede requerir cierta "astucia" y "creatividad". Sin embargo, existen
# herramientas que nos ayudan a hacer el trabajo un poco más fácil y, sobre
# todo, estructurado. Además, aprendimos que la calidad de nuestros resultados
# siempre dependerá de la calidad de los datos que le proveemos al algoritmo.
# Como bien dice aquella frase popular: "garbage in, garbage out" (basura entra,
# basura sale). Por lo tanto, tomarnos el tiempo de mejorar la calidad de
# nuestros datos siempre resultará en un beneficio notable a largo plazo.
