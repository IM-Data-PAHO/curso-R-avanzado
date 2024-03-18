# transformaciones_avanzadas_2.R -----------------------------------------------
# Descripción: Este script describe algunos temas misceláneos adicionales
# relacionados con la transformación de bases de datos.
#
# Los temas principales abordados son:
#
#   1.  Uso de across() para facilitar la aplicación de una fórmula a más de
#       una columna (sección 1).
#   2.  Búsqueda de registros faltantes en un RNVe (por medio de complete() y
#       fill()) (sección 2).
#   3.  Definición de funciones para reducir la cantidad de duplicación de
#       código (sección 3).
#   4.  Búsqueda de patrones en strings (sección 4).
#
# Creado por -------------------------------------------------------------------
# Nombre: Dan Alvarez
# Creado en: 2024-02-05
# Editorial --------------------------------------------------------------------
# Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# Requisitos -------------------------------------------------------------------
source("requirements.R")
source("./scripts/transformaciones_avanzadas_1.R")

# 1. Uso de across() -----------------------------------------------------------
#
# Uso de across() para aplicar cambios a más de una columna a la vez.
#
# En bases de datos anchas, es común querer aplicar la misma fórmula a más de
# una columna. El enfoque inicial puede ser ejecutar varias instrucciones
# mutate, copiando la fórmula cada vez y modificando los nombres de las
# columnas de interés.
#
# En este ejemplo veremos cómo podemos reducir la sintaxis y, al mismo tiempo,
# hacer el código más legible.
#
## Transformación de datos -----------------------------------------------------
# Primero, recordemos qué contiene dosis_mensual,
head(dosis_mensual)
# Para propósitos de este ejemplo, convertiremos dosis_mensual a formato ancho.
dosis_mensual_ancho <- dosis_mensual %>% 
  # Solo queremos la Primera y Segunda dosis
  filter(!(dosis %in% c("Campaña"))) %>% 
  # Usamos pivot_wider() del paquete tidyr, para realizar la conversión a
  # formato ancho.
  tidyr::pivot_wider(
    # Estas son las columnas que no vamos a mover
    id_cols = c(ano, mes, fecha),
    # Esta es la columna que contiene los nombres de nuestras dosis
    names_from = "dosis",
    # Esta es la columna que contiene el número de dosis aplicada, para cada
    # dosis
    values_from = "n_dosis"
  ) %>% 
  # Agregamos la población para poder calcular la cobertura en el siguiente paso
  left_join(pop_LT1 %>% mutate(year = year + 1), by = c("ano" = "year"))
head(dosis_mensual_ancho)

## Cálculo ---------------------------------------------------------------------
# Ahora, imaginemos que queremos calcular la cobertura para cada dosis. ¿Cómo
# lo hacemos?

### Enfoque inicial ------------------------------------------------------------
# Colocamos la misma fórmula dos veces, para calcular la cobertura de cada una
# de las dosis.
cobertura_mensual_ancho <- dosis_mensual_ancho %>% 
  # Agrupamos para fila (es decir, para cada año y mes)
  group_by(ano, mes) %>% 
  # Calculamos la cobertura para cada dosis
  summarise(
    Cobertura_Primera = round(Primera / n * 100, 2),
    Cobertura_Segunda = round(Segunda / n * 100, 2)
  )

### Enfoque mejorado -----------------------------------------------------------
# Usamos across() para solo colocar la fórmula una vez. Esto hace al código
# más compacto y reduce la probabilidad de cometer errores.
cobertura_mensual_ancho <- dosis_mensual_ancho %>% 
  # Agrupamos para fila (es decir, para cada año y mes)
  group_by(ano, mes) %>%
  # Calculamos la cobertura para cada dosis
  summarise(
    across(
      # En las columnas Primera y Segunda
      c(Primera, Segunda),
      # Aplicamos la función para calcular la cobertura
      # Donde:
      #   1.  ~ indica un lambda; es decir, una función de una sola línea
      #   2.  . le indica a la función que debe usar el valor de Primera o
      #       Segunda, dependiendo de qué se está evaluando.
      ~ round( . / n * 100, 2),
      # Y renombramos las columnas con el formato Cobertura_{.col}, donde .col
      # contiene el nombre de la columna que se está evaluando (Primera o
      # Segunda)
      .names = "Cobertura_{.col}"
    )
  )
head(cobertura_mensual_ancho)

# 2. Valores faltantes ---------------------------------------------------------
#
# Ejercicio: Tenemos un RNVe pequeño, y quisiéramos identificar a todas aquellas
# personas en la base que tienen una Primera dosis de SRP, pero no tienen una
# segunda dosis. ¿Cómo lo hacemos?
#
# Debemos encontrar a todas aquellas personas que aparecen en la base con una
# Primera dosis, y verificar si han tenido una segunda dosis. La función
# complete() del paquete tidyr nos puede ayudar, pues su propósito es convertir
# los valores faltantes implícitos en valores faltantes explícitos.
#
## Transformación de los datos -------------------------------------------------
# Empezamos por obtener un subconjunto del RNVe principal, para poder hacer el
# análisis más factible.
rnve_pequeno <- rnve %>% 
  # Obtenemos aquellos ID que tengan el patrón X.111.XXX.-X
  # NOTA: Más información sobre la sintaxis de esta línea en la sección
  #       4 de este script.
  filter(stringr::str_sub(ID, 3, 5) == "111") %>% 
  # Nos limitamos solo a la Primera y Segunda dosis
  filter(dosis %in% c("Primera", "Segunda")) %>% 
  # Nos limitamos solo a aquellos individuos que nacieron después de 2018
  filter(year(fecha_nac) >= 2018) %>% 
  # Seleccionamos las columnas de interés, puesto que el RNVe contiene más
  # información de la que necesitamos para este análisis
  select(ID, fecha_nac, fecha_vac, vacuna, dosis, nombre, apellido)

## Buscando faltantes ----------------------------------------------------------
# Buscamos a todos aquellos individuos faltantes en la base (es decir,
# aquellos que tienen una Primera dosis, pero no tienen una Segunda)
explicito <- rnve_pequeno %>% 
  # La función complete() del paquete tidyr se encarga de incluir todos aquellas
  # combinaciones de {ID, dosis} que hagan falta en la base.
  tidyr::complete(ID, dosis)
explicito <- explicito %>% 
  # Puesto que el paso anterior introdujo filas con NA en todas las columnas
  # que no son ID ni dosis, podemos rellenarlas con la información correcta
  # usando la función fill() del paquete tidyr.
  group_by(ID) %>% 
  tidyr::fill(
    # Queremos rellenar todas aquellas columnas que NO sean ID, dosis ni
    # fecha_vac
    c(-ID, -dosis, -fecha_vac),
    # Puesto que sabemos que para cada ID, solo tenemos a una persona, podemos
    # colocar la dirección de llenado como "downup". Es decir, dentro del grupo
    # seleccionado (ID), la función va a ver hacia abajo primero. Si encuentra
    # una fila con datos, los toma y los copia. Si no, mira hacia arriba y
    # repite el proceso.
    #
    # NOTA: También podríamos colocar la dirección "updown". En este caso,
    #       es indiferente.
    .direction = "downup"
  )
# Por último, podríamos filtrar la base para tener a aquellos individuos
# que tienen una dosis faltante.
faltantes <- explicito %>% 
  filter(is.na(fecha_vac))
# Otra manera de hacerlo, en caso no querramos depender de fecha_vac,
# es utilizando la función anti_join() de dplyr.
faltantes <- explicito %>%
  # Esta función devuelve todo aquello que esté en x (explicito) que no esté
  # en y (rnve_pequeno). Puesto que explicito contiene todo lo que está en
  # rnve_pequeno + aquellos individuos que no tienen una Segunda dosis, el
  # resultado de anti_join() es aquellos individuos faltantes.
  dplyr::anti_join(., rnve_pequeno, by = c("ID", "dosis"))
# En conjunto con esto, es posible que quisiéramos tener, no solo una base
# de los individuos faltantes, sino que cada fila (es decir, individuo) tenga
# también la fecha de vacunación de la Primera dosis. ¿Cómo hacemos esto?
# Probemos ensanchar la base...
explicito_ancho <- explicito %>% 
  pivot_wider(names_from = "dosis", values_from = "fecha_vac")
# De nuevo, podemos obtener a aquellos individuos que aún no tienen una
# Segunda dosis
faltantes_ancho <- explicito_ancho %>% 
  filter(is.na(Segunda))
# Para estos individuos, podríamos calcular la cantidad de meses que han
# transcurrido desde la Primera dosis
requieren_segunda_dosis <- faltantes_ancho %>% 
  # Convertimos fecha_vac a tipo Date. Esto, porque la función import() del
  # paquete rio (que utilizamos para importar las bases originales), utiliza
  # al paquete data.table, que define un tipo de objeto IDate. Este objeto es
  # más eficiente, pero en este caso requerimos que sea de tipo Date.
  mutate(
    across(
      c(Primera, Segunda),
      ~ as.Date(.)
    )
  ) %>% 
  # Calculamos cuantos meses han transcurrido entre hoy y la fecha de
  # de vacunación de la Primera dosis
  mutate(
    meses_transcurridos = lubridate::time_length(
      # Restamos la fecha de hoy con la fecha de vacunación de Primera dosis
      lubridate::today() - Primera,
      # Queremos los resultados en meses
      unit = "months"
    )
  ) %>% 
  # Redondeo al entero más cercano
  mutate(meses_transcurridos = round(meses_transcurridos)) %>% 
  # Encontramos a todos aquellos individuos con más de 6 meses transcurridos
  # desde la Primera dosis
  filter(meses_transcurridos > 6)

# 7. Definición de funciones ---------------------------------------------------
#
# Definición de funciones.
#
# Las funciones son útiles para reducir la cantidad de código, en casos donde
# repetimos el uso de cierta sección de código varias veces.
#
# Usando la sección 2 como base, veremos cómo podemos definirlo todo en una
# función y realizar los cálculos allí definidos, de manera más compacta.
#
# Definición -------------------------------------------------------------------
# Una función tiene la forma estándar:
# 
# nombre <- function(parametros) {
#   proceso
# }
calcular_individuos_faltantes <- function(df, fecha = today(), meses = 6) {
  # NOTA: La función sigue el mismo proceso descrito en la sección 2. Por lo
  #       mismo, se han omitido la mayoria de los comentarios.
  # Agregamos los registros faltantes
  explicito <- df %>% 
    complete(ID, dosis) %>% 
    group_by(ID) %>% 
    fill(
      c(-ID, -dosis, -fecha_vac),
      .direction = "downup"
    )
  # Ensanchamos la base
  explicito_ancho <- explicito %>% 
    pivot_wider(names_from = "dosis", values_from = "fecha_vac")
  # Obtenemos solo aquellos registros faltantes
  faltantes_ancho <- explicito_ancho %>% 
    filter(is.na(Segunda))
  # Encontramos a todos aquellos individuos con más de N meses transcurridos
  # desde la fecha indicada en los argumentos
  requieren_segunda_dosis <- faltantes_ancho %>% 
    mutate(across(c(Primera, Segunda), ~ as.Date(.))) %>% 
    # Calculamos cuantos meses han transcurrido entre hoy y la fecha de
    # de vacunación de la Primera dosis
    mutate(
      meses_transcurridos = lubridate::time_length(
        fecha - Primera,
        unit = "months"
      )
    ) %>% 
    mutate(meses_transcurridos = round(meses_transcurridos)) %>% 
    # Solo aquellos con más de N meses
    filter(meses_transcurridos > meses)
  # Podemos utilizar la función return() para que el resultado de la función
  # que definimos, sea requieren_segunda_dosis. Si no lo hacemos, el resultado
  # será simplemente el último valor calculado.
  return(requieren_segunda_dosis)
}
# Para usar una función, la sintaxis es:
#
#   resultado <- nombre(parametros)
#
# A esto se le llama, mandar a "llamar" a la funcion.
# Además, si una función tiene un parémetro predefinido, podemos obviarlo
# y mandarla a llamar con un único parámetro.
requieren_segunda_dosis <- calcular_individuos_faltantes(rnve_pequeno)
head(requieren_segunda_dosis)
# O, podemos sobreescribir el segundo parámetro predefinido con un valor que
# nosotros prefiramos. Por ejemplo, si queremos cambiar la fecha límite.
requieren_segunda_dosis <- calcular_individuos_faltantes(
  rnve_pequeno, fecha = ymd("2024-01-01"), meses = 6
)
head(requieren_segunda_dosis)
# Podemos agrandar la base de datos. Por ejemplo, si quisieramos encontrar a
# todas aquellas personas nacidas en el primer trimestre de 2018,
# que ya tienen una Primera dosis, pero aun no tienen Segunda.
rnve_grande <- rnve %>% 
  # Nos limitamos solo a la Primera y Segunda dosis y nacidos en el primer
  # trimestre de 2018
  filter(dosis %in% c("Primera", "Segunda")) %>% 
  filter(year(fecha_nac) == 2018 & month(fecha_nac) <= 3) %>% 
  # Seleccionamos las columnas de interés.
  select(ID, fecha_nac, fecha_vac, vacuna, dosis, nombre, apellido)
# NOTA: Este proceso puede demorar segundos o minutos dependiendo del
#       procesador, pues la función es compleja y no está optimizada para
#       velocidad. En un computador con i5-5300U y 16 GB de RAM, tomó aprox. 20
#       segundos.
requieren_segunda_dosis <- calcular_individuos_faltantes(rnve_grande)

# 4. Búsqueda de patrones ------------------------------------------------------
#
# Búsqueda y reemplazo de patrones
#
# Existe una serie de funciones para la búsqueda y reemplazo inteligente
# de patrones. Utilizaremos el paquete stringr y las expresiones
# regulares para explorar algunas de estas opciones. Una referencia util
# para expresiones regulares es: https://regex101.com/
#
# Un enfoque inicial y básico sería el siguiente. Supongamos que en la base
# del registro civil, sabemos que a todos los hombres se les asigna un numero
# entre 0 y 4 en la segunda posición. Es decir, los ID tienen el siguiente
# patrón:
#
#   X.0XX.XXX-X
#   X.1XX.XXX-X
#   X.2XX.XXX-X
#   X.3XX.XXX-X
#   X.4XX.XXX-X
#
# Por lo tanto, queremos obtener todos aquellos ID que contengan esos
# patrones. ¿Cómo lo hacemos?
#
## Búsqueda de ID --------------------------------------------------------------
# Podríamos intentar obtener la posición 3 de ese string, y verificar si
# esta dentro del rango esperado.
filtro_id <- registro_civil %>%
  # Accedemos al tercer caracter y verificamos si cumple con lo esperado
  filter(stringr::str_sub(ID, 3, 3) %in% c("0", "1", "2", "3", "4"))
glimpse(filtro_id)
# ¿Qué pasa si ahora se nos dice que queremos encontrar todos aquellos ID
# que tienen números entre 0 y 4 en las posiciones 3 y 5?
# Podemos efecturarlo de la misma manera.
filtro_substring <- function(busqueda) {
  registro_civil %>% 
    # Accedemos al tercer caracter y al quinto caracter,
    # y verificamos si cada uno cumple con lo esperado
    filter(
      stringr::str_sub(ID, 3, 3) %in% busqueda &
        stringr::str_sub(ID, 5, 5) %in% busqueda
    )
}
# Mandamos a "llamar" a la función filtro_substring con un vector que tiene
# los dígitos que queremos incluir en la búsqueda.
filtro_id_substring <- filtro_substring(c("0", "1", "2", "3", "4"))
glimpse(filtro_id_substring)
# Como podemos ver, entre más condiciones tenemos, más compleja se puede
# volver la expresión. Podríamos usar expresiones regulares para reducir
# la longitud del código.
filtro_regex <- function(patron) {
  registro_civil %>%
    # Usamos la funcion str_starts() de stringr, para verificar que el inicio
    # del string coincida con el patrón que estamos indicando.
    #
    # El patrón de búsqueda dice lo siguiente:
    #   .  |  .  |  [0-4]  |  .  |  [0-4]  |
    #  (1)   (2)     (3)     (4)     (5)
    # 1, 2, 4: Coinciden con cualquier caracter
    # 3, 4: Coinciden con los digitos entre 0 y 4 (inclusivo)
    filter(stringr::str_starts(ID, patron))
}
filtro_id_regex <- filtro_regex("..[0-4].[0-4]")
glimpse(filtro_id_regex)
# Podemos ver que ambos procesos arrojan el mismo resultado:
setdiff(filtro_id_regex, filtro_id_substring)
# El algunos casos, el uso de regex es ligeramente más rápido.
# En este ejemplo en específico usamos la función mark() del paquete bench
# para calcular el tiempo de ejecución.
bench::mark(
  filtro_substring(c("0", "1", "2", "3", "4")),
  filtro_regex("..[0-4].[0-4]")
)
# En otros casos, es más lento.
bench::mark(
  registro_civil %>% filter(stringr::str_sub(ID, 3, 3) %in% c("0", "1", "2", "3", "4")),
  filtro_regex("..[0-4]")
)

## Búsqueda de apellidos ---------------------------------------------------------
# Otros ejemplos de búsqueda pueden visualizarse al filtrar los nombres y
# apellidos en la base de registro civil.
#
# Por ejemplo, quisiera contar el número de individuos cuyo segundo apellido
# termine en "EZ" o "AZ".
filtro_apellidos_1 <- registro_civil %>%
  # Usamos ahora la función str_ends(), para buscar solo al final del string.
  # El patrón [AE]Z busca todo aquello que coincida con AZ o EZ.
  # Nótese que, igual que con el [0-4], usamos [] para agrupar.
  filter(stringr::str_ends(apellido, "[AE]Z"))
# ¿Y si quisiéramos todos aquellos cuyo primero o segundo apellido termine en
# AZ o EZ?
filtro_apellidos_2 <- registro_civil %>%
  # Usamos ahora la función str_detect(), para buscar en todo el string.
  #
  # El nuevo patrón [AE]Z\b es similar, pero agrega \b, que se asegura de que
  # las letras que estamos buscando solo estén al final de una palabra (es
  # decir, al final de un apellido).
  #
  # NOTA: Siempre agregamos una diagonal adicional, cuando estamos usando
  #       caracteres especiales en regex. \b es uno de dichos caracteres.
  filter(stringr::str_detect(apellido, "[AE]Z\\b"))
