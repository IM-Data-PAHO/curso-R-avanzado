# transformaciones_avanzadas_1.R -----------------------------------------------
# Descripción: Este script provee varios ejemplos de transformaciones de datos
# avanzadas, relacionadas con la vacunación contra el sarampión y el
# seguimiento de una campaña de vacunación contra el sarampión. 
# 
# Los temas principales abordados son:
#
#   1.  Cálculo de coberturas a partir de un RNVe y un registro civil (sección
#       1).
#   2.  Seguimiento de cohortes (sección 2).
#   3.  Agrupación de un RNVe para obtención de coberturas mensuales y
#       semanales (sección 3.)
#   4.  Uso de cumsum() para el cálculo de coberturas acumuladas (sección 4.)
#   5.  Uso de lag() para el cálculo de incrementos en cobertura (sección 5).
#   6.  Cálculo de susceptibles a nivel municipal (ADM2) y nacional (sección 6).
#   7.  Seguimiento del avance de una campaña de vacunación a nivel
#       nacional y departamental (ADM1) (sección 7).
#
# Creado por -------------------------------------------------------------------
# Nombre: Dan Alvarez, Ignacio Castro
# Creado en: 2024-02-05
# Editorial --------------------------------------------------------------------
# Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# Requisitos -------------------------------------------------------------------
source("requirements.R")

# # cargar paquetes necesarios
# pacman::p_load(
#   tidyr,
#   dplyr,
#   lubridate,
#   stringr,
#   rio,
#   ggplot2,
#   bench
# )

# Lectura ----------------------------------------------------------------------
# Leer las bases usando rio::import
registro_civil_original <- rio::import("./data/registro_civil.csv")
rnve_original <- rio::import("./data/RNVE.csv")
# Filtremos el registro civil hasta el año pasado y el RNVe hasta el día
# de hoy.
registro_civil <- registro_civil_original %>%
  filter(year(fecha_nac) <= year(today()) - 1)
rnve <- rnve_original %>% filter(fecha_vac <= today())

# 1. Coberturas en RNVe --------------------------------------------------------
#
# Para calcular la cobertura en un RNVe, necesitamos:
#
#   1. El RNVe
#   2. La población objetivo
#
# Cada fila del RNVe representa un acto de vacunación. Asumiendo un registro
# sin duplicados y una vacuna de una sola dosis, cada fila representa una
# vacuna aplicada. Por lo tanto, la cobertura resulta ser el cociente de el 
# número de filas en el RNVe y la población objetivo.
#
# ¿Cómo traducimos esta pregunta a código?
#
## Exploración -----------------------------------------------------------------
# ¿Qué columnas tiene nuestra base de datos?
glimpse(rnve)
# ¿Qué tipo de datos contienen estas columnas?
as_tibble(rnve)
# De lo anterior, observamos que el registro contiene actos de vacunación con la
# siguiente información: ID, fecha_vac, vacuna, dosis...
#
# Y podríamos hacernos las siguientes preguntas:
#
# 1. ¿Qué vacunas están contenidas en esta tabla?
unique(rnve$vacuna)
table(rnve$vacuna)
# 2. ¿Cuáles dosis se aplican para cada vacuna?
table(rnve$vacuna, rnve$dosis)
# 3. ¿Cuántos datos hay para cada año?
listado_anos <- rnve %>%
  # calculemos el año para todas las fechas de vacunación de los individuos
  mutate(ano = lubridate::year(fecha_vac)) %>%
  # pull() representa una manera un poco más limpia acceder a una columna
  pull(ano)
table(listado_anos)
## Cálculo ---------------------------------------------------------------------
# Calcularemos la cobertura de las dosis 1 y 2 de SRP, para cada año, y
# utilizaremos la población del año previo como la población objetivo.
#
# Primero, calculamos el número de dosis.
dosis <- rnve %>% 
  # Calculemos el año de cada evento de vacunación
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  # Agrupemos por año y dosis
  group_by(ano, dosis) %>% 
  # Calculemos cuantas vacunas fueron aplicadas para cada año y dosis
  # NOTA: tally() es lo mismo que summarise(n = n()); o sea, contar cuántas
  #       filas hay en cada grupo
  # NOTA: Con el argumento name, cambiamos el nombre de la columna. En lugar
  #       de llamarse n, se llamará total_dosis.
  tally(name = "total_dosis")
head(dosis)
# Para calcular la cobertura necesitamos la población objetivo. Esto último lo
# podemos obtener directo del registro civil. Recordemos que en nuestro
# registro civil, cada fila representa el nacimiento de un individuo. Por lo
# tanto, la cantidad de filas en el registro representa la cantidad de personas
# que nacieron.
pop_LT1_rn <- registro_civil %>% 
  # Obtenemos los años de cada fecha de nacimiento
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  # Para cada año, calculamos cuántas filas hay (o sea, cuántos nacimientos)
  group_by(ano) %>% 
  tally()
head(pop_LT1_rn)
# Queremos usar la población del año anterior como la población objetivo.
# Modifiquemos pop_LT1_rn para que se ajuste.
pop_LT1_rn <- pop_LT1_rn %>%
  # El año anterior ahora será el año actual
  mutate(ano = ano + 1)
# Con esto listo, podemos calcular la cobertura.
cobertura <- dosis %>% 
  # Juntemos el numero de dosis por año y vacuna (dosis) con la población
  # objetivo para ese año (pop_LT1_rn).
  left_join(., pop_LT1_rn, by = c("ano")) %>% 
  # De tal forma que la cobertura resulta ser:
  mutate(cobertura = total_dosis / n  * 100)
head(cobertura)
# Ejercicio:
# En este caso, calculamos la población objetivo a partir del registro civil,
# y con ello calculamos cobertura. Podría darse el caso de que no tengamos
# un registro civil, pero sí tengamos las proyecciones demográficas del país
# para los años de interés. En ese caso, ¿qué cambia en nuestro procedimiento?
# ¿Se simplifica o se complica?       

# 2. Seguimiento de cohorte ----------------------------------------------------
# Una de las bondades que nos entregan los registros nominales de vacunación es
# la posibilidad de realizar el seguimiento longitudinal de las personas y,
# gracias a esto, hacer el seguimiento de las cohortes. 
#
# En la siguiente sección veremos una forma de estimar las coberturas de
# vacunación de cada cohorte según fecha de nacimiento. Los valores pueden ser 
# distintos a los obtenidos por los cálculos anteriores.   

## Partir desde el registro civil ----------------------------------------------
# El seguimiento de las cohortes debe partir por la base de dato que nos brinde
# la información más exhaustiva (lograr capturar al mayor número de personas), 
# en este caso, el Registro Civil. Sin embargo, en realidades distintas, pudiese
# ser egresos hospitalarios, registro de centros educacionales, seguridad
# social, etc.
#
# Una mirada a nuestro Registro Civil.
glimpse(registro_civil)
# Generaremos una base de datos con las variables de interés
rc <- registro_civil %>% 
  select(ID, nombre, apellido, fecha_nac)

## Registro nominal de vacunación ----------------------------------------------
# Realizaremos un pareamiento deterministico (es un concepto que veremos más 
# adelante, pero es bueno que les empiece a sonar) de la base de datos del
# Registro civil con el Registro Nominal de Vacunación usando solo el ID. Esto 
# resulta posible porque las bases de datos usadas en este curso no tienen
# errores en el ID, pero esto no sucede en la realidad. En lecciones posteriores
# veremos una forma de aproximarnos a las soluciones de ese problema.
#
# Una mirada al Registro Nominal de Vacunación.
# Como vimos anteriormete cada fila corresponde a un acto de vacunación.
glimpse(rnve)
# Generamos una base de datos con las variables de interés
rnve_2 <- rnve %>% 
  select(ID, nombre, apellido, fecha_nac, dosis, fecha_vac)

## Pareamiento determinístico --------------------------------------------------
# Utilizaremos left_join para parear ambas bases de datos.
# A continuación utilizaremos una serie de funciones para llegar a la base de 
# datos que necesitamos, una base de datos que tenga en cada fila una persona y 
# en cada columna las fechas en las que ha recibido cada vacuna.
rc_rnve <- rc %>% 
  # Utilizaremos left_join para parear ambas bases de datos.
  left_join(rnve_2, by = c("ID", "nombre", "apellido", "fecha_nac")) %>%
  # Usaremos pivot_wider para transformar la base de datos de long (larga) a 
  # wide (ancha).
  pivot_wider(
    # Estas son las columnas que no vamos a mover
    id_cols = c(ID, nombre, apellido, fecha_nac),
    # Esta es la columna que contiene los nombres de nuestras dosis
    names_from = "dosis",
    # Esta es la columna que contiene la fecha de vacunación, para cada dosis
    values_from = "fecha_vac") %>% 
  # Eliminar columna NA
  select(-`NA`) %>% 
  # Evaluar esquema completo de vacunación. 
  # Usaremos la función if_all() para evaluar si la condición se cumple en todas
  # las variables especificadas. Esto será particularmente útil cuando quieran 
  # evaluar esquemas completos de vacunación con una mayor cantidad de
  # vacunas/dosis
  mutate(Completo = if_all(5:6, ~ !is.na(.x))) %>% 
  # Obtener año de nacimiento para asignar a cada persona a una cohorte.
  mutate(ano_nac = year(fecha_nac)) %>% 
  # Agrupamos por ano_nac y la variable esquema completo
  group_by(ano_nac, Completo)%>% 
  # Contamos. 
  tally() %>% 
  pivot_wider(
    # Estas son las columnas que no vamos a mover
    id_cols = c(ano_nac),
    # Esta es la columna que contiene el estado de esquema completo
    names_from = "Completo",
    # Esta es la columna que contiene el número de personas en cada grupo
    values_from = "n") %>% 
  # Renombrar variables
  rename(si = `TRUE`,
         no = `FALSE`) %>% 
  # Calcular la cobertura.
  mutate(cobertura_esq_compelto = (si/(si+no))*100)

# 3. Agrupaciones --------------------------------------------------------------
#
# Podríamos buscar calcular el número de dosis aplicadas mensual o semanalmente,
# a partir del RNVe provisto. Para ello, debemos realizar una agrupación,
# similar a la agrupación anual realizada en la sección 1.
#
## Agrupación mensual ----------------------------------------------------------
dosis_mensual <- rnve %>% 
  # Calculamos el año y mes de cada fila (es decir, acto de vacunación)
  mutate(ano = year(fecha_vac), mes = month(fecha_vac)) %>% 
  # Agrupamos para cada año, mes y vacuna, y calculamos cuántas dosis
  # (es decir, filas) hay para el grupo
  group_by(ano, mes, dosis) %>% 
  tally(name = "n_dosis") %>% 
  # Volvemos a construir la fecha, para poder graficar más fácilmente.
  #   1. La funcion ym() de lubridate reconstruye una fecha a partir de un
  #   string.
  #   2. La función str_glue() de stringr es una manera más sintetizada de
  #   concatenar un string. El equivalente en paste sería:
  #   paste(ano, mes, sep = "-") o paste0(ano, "-", mes).
  mutate(fecha = lubridate::ym(
    stringr::str_glue("{ano}-{mes}")
  ))
head(dosis_mensual)
## Agrupación semanal ----------------------------------------------------------
dosis_semanal <- rnve %>% 
  # Calculamos el año y la semana epidemiológica de cada acto de vacunación
  #   1. La función isoweek() de lubridate es equivalente al sistema
  #   ISO 8601, que también corresponde a una semana epidemiológica que inicia
  #   en Lunes.
  mutate(ano = year(fecha_vac), semana_epi = lubridate::epiweek(fecha_vac)) %>% 
  # Agrupamos para cada año, semana y dosis, y contamos cuántas vacunaciones
  # hubo en el grupo.
  group_by(ano, semana_epi, dosis) %>% 
  tally(name = "n_dosis") %>% 
  # Volvemos a construir la fecha, para poder graficar más fácilmente.
  #   1. La función get_date() del paquete aweek permite reconstruir una fecha
  #   a partir de un año y semana dadas. Colocamos que el inicio de la semana
  #   es el Lunes, para mantener la consistencia con el uso de isoweek()
  #   anteriormente.
  mutate(fecha = aweek::get_date(
    week = semana_epi, year = ano, day = 1, start = "Monday"
  ))
tail(dosis_semanal)
## Visualizando los resultados -------------------------------------------------
### Mensual --------------------------------------------------------------------
ggplot(
  dosis_mensual,
  aes(x = fecha, y = n_dosis, fill = dosis)
) +
  geom_bar(stat = "identity", position = "stack")
### Semanal --------------------------------------------------------------------
ggplot(
  dosis_semanal,
  aes(x = fecha, y = n_dosis, fill = dosis)
) +
  geom_bar(stat = "identity", position = "stack")

# 4. Coberturas cumulativas ----------------------------------------------------
#
# Para las gráficas anteriores, quisieramos agregar la cobertura alcanzada para
# cada mes. ¿Cómo lo hacemos?
#
## Cálculo ---------------------------------------------------------------------
# Calculamos el incremento de cobertura para cada mes
cobertura_mensual_1 <- dosis_mensual %>% 
  # Para calcular cobertura, necesitamos la población. Por lo tanto,
  # agregamos la población a la base de dosis mensuales.
  # Para esto usamos pop_LT1_rn, una tabla de población por año que calculamos
  # en la sección 1.
  left_join(pop_LT1_rn, by = "ano") %>% 
  # Calculamos la cobertura para cada fila (es decir, para cada mes)
  group_by(ano, mes, dosis) %>% 
  mutate(cobertura_mes = round(n_dosis / n * 100, 2))
# Calculamos la cobertura acumulada para cada mes
cobertura_mensual <- cobertura_mensual_1 %>% 
  # Calculamos la cobertura acumulada para cada mes
  group_by(ano, dosis) %>% 
  mutate(cobertura = cumsum(cobertura_mes))
## Visualización ---------------------------------------------------------------
# Hacemos un poco de limpieza a los datos que queremos mostrar
para_graficar <- cobertura_mensual %>%
  # Mostramos los datos de la Primera y Segunda dosis
  filter(dosis != "Campaña") %>% 
  # Por el momento, concetremonos en mostrar los datos de los años que ya han
  # terminado
  filter(ano <= 2023)
# Ahora sí, graficamos
ggplot(
  para_graficar,
  aes(x = fecha, fill = dosis)
) +
  # Nombramos los ejes y la leyenda
  labs(y = "Dosis", fill = "Dosis", linetype = "Cobertura") +
  # Mostramos el numero de dosis aplicadas cada mes
  geom_bar(aes(y = n_dosis), stat = "identity", position = "stack") +
  # Mostramos la cobertura acumulada para cada mes.
  # NOTA: Los datos de n_dosis alcanzan aprox 10,000, mientras que las coberturas
  #       son entre 0 y 100. Por lo tanto, se multiplica el valor de coberturas
  #       por 100 para igualar los dos ejes.
  geom_line(aes(y = cobertura * 100, linetype = dosis), linewidth = 1) +
  # Modificamos el eje vertical
  scale_y_continuous(
    # Ajustamos los limites entre 0 y 10,000 dosis
    limits = c(0, 10e3),
    # Agregamos un segundo eje horizontal
    # NOTA: Se aplica el mismo factor de conversion, pero a la inversa.
    sec.axis = sec_axis( trans= ~./100, name = "Cobertura (%)")
  ) +
  # Agregamos una faceta por año y dejamos el eje X libre para que muestre
  # cada fecha.
  facet_wrap(~ ano, scales = "free_x") +
  # Mejoramos la visualización
  theme_classic() +
  theme(text = element_text(size = 16))

# 5. Coberturas incrementales --------------------------------------------------
#
# Cálculo de cobertura diferencial a partir de un valor acumulado
#
# Si tenemos una base con la cobertura acumulada mensualmente, podríamos querer
# obtener el incremento de cobertura de cada mes. ¿Cómo hacemos esto?
#
## Transformación de los datos -------------------------------------------------
# De la base de cobertura mensual, quedemonos solo con la fecha, dosis y la
# cobertura acumulada de cada mes.
cobertura_acumulada_mensual <- cobertura_mensual %>% 
  select(ano, mes, fecha, dosis, cobertura)
## Cálculo ---------------------------------------------------------------------
# Si ahora quisiéramos obtener el incremento de cobertura en cada mes, podemos
# proceder de la siguiente manera.
prueba_1 <- cobertura_acumulada_mensual %>% 
  # Queremos realizar el calculo para cada año y dosis
  group_by(ano, dosis) %>% 
  # Aseguremosnos de ordenar correctamente de acuerdo al mes, pues el
  # incremento de cobertura de un mes a otro solo es válido si los meses están
  # ordenados.
  arrange(mes) %>% 
  # Calculamos el incremento de cobertura utilizando la función lag() del
  # paquete dplyr. Esto se encarga de obtener el valor directamente anterior.
  #
  # NOTA: También existe la función lead(). Ambas tienen el argumento n, que
  #       permite ver n posiciones hacia el futuro (lead) o hacia el pasado
  #       (lag). El valor predeterminado es 1.
  mutate(cobertura_mes = cobertura - lag(cobertura))
# Esto funciona, pero el resultado del primer mes para cada año es NA.
# ¿Cómo lo arreglamos?
prueba_2 <- cobertura_acumulada_mensual %>% 
  group_by(ano, dosis) %>% 
  arrange(mes) %>% 
  # Agregamos una revisión del valor de lag(cobertura). Si es NA, insertamos
  # solamente el valor de cobertura. lag(cobertura) será NA cuando estemos
  # en la primera posición (en este caso, en el mes de enero).
  mutate(cobertura_mes = case_when(
    # Caso 1 ~ Resultado 1
    is.na(lag(cobertura)) ~ cobertura,
    # Caso 2 (todo lo que no sea Caso 1) ~ Resultado 2
    TRUE ~ cobertura - lag(cobertura)
  ))

# 6. Cálculo de susceptibles ---------------------------------------------------
# Luego de realizar cálculo de coberturas, el siguiente punto en el seguimiento
# de la cohortes resulta ser el cálculo de susceptibles. Es decir, cuántos
# niños y niñas aún no han sido inmunizados, sea porque no han sido vacunados
# con ninguna dosis de la vacuna, o porque han sido vacunados pero, por la
# falla primaria de la vacuna, no han sido inmunizados. Este número se acumula
# conforme el tiempo transcurre, y quisiéramos tener una forma de calcular
# cuántos niños y niñas susceptibles se han acumulado conforme han pasado
# los años.
#
# Puesto que la cantidad de susceptibles depende tanto de la población como de
# la cantidad de niños/as vacunados con una primera dosis de la vacuna,
# procedemos a calcular ambas cantidades.

## Calcular vacunados y poblacion ----------------------------------------------
# Del RNVe sacamos la cantidad de vacunados.
vacunados_rnve <- rnve %>% 
  # Queremos a todos aquellos que tienen una primera dosis de la vacuna
  filter(dosis == "Primera") %>% 
  # Calculamos el año de la fecha de vacunación
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  # Y agrupamos por año de vacunación y municipio de residencia de la madre
  # NOTA: En este caso, agrupamos por municipio porque quisiéramos calcular
  #       la cantidad de susceptibles a nivel municipal (ADM2). Esto facilitará
  #       la planificación y el seguimiento de la campaña, para priorizar
  #       municipios que lo requieran (por ejemplo).
  group_by(ano, municipio_res_mad) %>% 
  # Con los grupos hechos, calculamos cuántos niños/as han sido vacunados con
  # una primera dosis
  tally(name = "vacunados_primera")
head(vacunados_rnve)
# A parte de la cantidad de vacunados, quisiéramos obtener la población por
# municipio. Esto lo podemos obtener a partir del registro civil.
pop_municipio <- registro_civil %>% 
  # Obtenemos los años de cada fecha de nacimiento
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  # Para cada año, calculamos cuántas filas hay (o sea, cuántos nacimientos)
  group_by(ano, municipio_res_mad) %>% 
  tally(name = "poblacion") %>% 
  # Agregamos 1 a los años. Esto, porque queremos utilizar la población de 
  # un año previo para calcular la cobertura del año actual. En otras palabras,
  # la población del año 2018 será la población objetivo para los vacunados
  # en el año 2019.
  mutate(ano = ano + 1)
head(pop_municipio)

## Calcular cobertura y susceptibles -------------------------------------------
# Podemos calcular la cobertura con los 2 datos que obtuvimos anteriormente.
cobertura_municipio <- vacunados_rnve %>% 
  # Quisiéramos unir ambas bases a través del año y el municipio.
  left_join(., pop_municipio, by = c("ano", "municipio_res_mad")) %>% 
  # Calculamos cobertura
  mutate(cobertura = vacunados_primera / poblacion  * 100)
head(cobertura_municipio)
# Ahora, realizamos el cálculo de susceptibles. Recordemos que los susceptibles
# se definen como:
#
# susceptibles = niñ@s sin primera dosis + niñ@s vacunados pero no inmunizados
susceptibles_municipio <- cobertura_municipio %>%
  # Definimos la falla primaria de la vacuna
  mutate(falla_primaria = 0.05) %>% 
  # Calculamos la cantidad de niños no vacunados
  mutate(no_vacunados = poblacion - vacunados_primera) %>% 
  # Agregamos la cantidad de niños vacunados pero no inmunizados
  mutate(susceptibles = no_vacunados + (vacunados_primera * falla_primaria))
head(susceptibles_municipio)

## Resumen anual ---------------------------------------------------------------
# Podríamos buscar hacer un resumen anual similar a lo que hemos encontrado
# anteriormente. Por ejemplo, podríamos querer mostrar la cantidad de
# susceptibles que han sido acumulados para cada año en los últimos 5 años.
#
# ¿Cómo lo hacemos?
#
# Afortunadamente, ya hemos avanzado en esto, pues tenemos una base con la
# cobertura por año (que se calculó en la sección 1).
glimpse(cobertura)
# Con estos datos, podemos usar los mismos pasos de antes para calcular los
# susceptibles para cada año.
susceptibles <- cobertura %>% 
  # Nos interesa obtener el número de susceptibles de los últimos 5 años. Este
  # número será importante, pues nos indica cuándo debemos implementar
  # una campaña de vacunación. Filtremos los años deseados.
  filter(ano <= 2023) %>% 
  # Utilizamos solo las primeras dosis
  filter(dosis == "Primera") %>% 
  # Definimos la falla primaria de la vacuna
  mutate(falla_primaria = 0.05) %>% 
  # Calculamos la cantidad de niños no vacunados
  mutate(no_vacunados = n - total_dosis) %>% 
  # Agregamos la cantidad de niños vacunados pero no inmunizados
  mutate(susceptibles = no_vacunados + (total_dosis * falla_primaria)) %>% 
  # Calculamos el acumulado de susceptibles conforme los años pasan
  #   1.  Usamos ungroup() en caso haya alguna agrupación previa que hayamos
  #       ingresado
  #   2.  Ordenamos la tabla de acuerdo al año, en orden ascendente
  #   3.  Calculamos el acumulado por medio de cumsum
  ungroup() %>% 
  arrange(ano) %>% 
  mutate(susceptibles_acumulado = cumsum(susceptibles))
head(susceptibles)
### Gráfica --------------------------------------------------------------------
# Visualicemos los datos que tenemos de manera gráfica.
ggplot(susceptibles, aes(x = ano)) +
  # Nombres de ejes
  labs(
    title = "Susceptibles acumulados en los últimos 5 años",
    x = "Año",
    y = "Susceptibles Acumulados"
  ) +
  # Cobertura en barras
  geom_bar(aes(y = cobertura * 400), position = "dodge", stat = "identity", fill = "#094775") +
  # Susceptibles acumulados en lineas
  geom_line(aes(y = susceptibles_acumulado), colour = "#ff671f", linewidth = 1) +
  # Ajustamos los dos ejes verticales
  scale_y_continuous(
    # Las dosis alcanzan cerca de 40 mil
    limits = c(0, 40e3),
    # Agregamos un segundo eje horizontal para cobertura (con el mismo factor
    # de conversion que en geom_bar)
    # NOTA: Aplicamos un factor de conversión de 400 para que el eje de 
    #       cobertura alcance 100% cuando el número de dosis alcance 40,000
    #       dosis.
    sec.axis = sec_axis( trans= ~./400, name = "Cobertura (%)")
  ) +
  # Ajuste de eje X
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  # Ajustes visuales
  theme_classic() +
  theme(text = element_text(size = 16))

# 7. Avance de campaña ---------------------------------------------------------
# El correcto seguimiento de los avances de una campaña de vacunación es clave
# para su buena ejecución. Además, fomenta la transparencia y la mejora
# continua. Por lo tanto, en esta sección exploraremos cómo obtener gráficas
# que permitan visualizar el avance de la campaña de vacunación que estamos
# simulando.
#
# Para empezar, esta campaña de vacunación se está aplicando a niños y niñas
# con al menos 13 meses de edad y no más de 5 años de edad. Por lo tanto,
# dentro de nuestro registro civil y RNVe, necesitamos filtrar a todos aquellos
# niñ@s que cumplan con estas características.

## Fechas de campaña -----------------------------------------------------------
# Empezamos por definir la fecha de inicio de la campaña, que ya conocemos.
fecha_campana <- as.Date("2024-03-04", "%Y-%m-%d")
fecha_campana
# Calculamos la fecha de nacimiento que corresponde a la edad mínima (13 meses)
# NOTA: El operador %m-% es un operador especial de lubridate que permite
#       realizar la resta de dos fechas en términos de meses.
fecha_edad_minima <- fecha_campana %m-% months(13)
fecha_edad_minima
# Calculamos la fecha de nacimiento que corresponde a la edad máxima (menos
# de 5 años)
fecha_edad_maxima <- fecha_campana %m-% months(12 * 5) - 1
fecha_edad_maxima

## Población objetivo ----------------------------------------------------------
# Con esto listo, calculamos la población objetivo de la campaña
# Hagamoslo a nivel departamental (ADM1) también.
pop_campana_adm1 <- registro_civil_original %>% 
  # Filtramos la fecha de nacimiento de acuerdo a las edades calculadas
  # anteriormente.
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  # Agrupamos de acuerdo all departamento de nacimiento
  group_by(departamento_res_mad) %>% 
  # Calculamos la cantidad de personas nacidas en ese departamento, que son
  # elegibles para la campaña. Llamamos a esta columna "población".
  tally(name = "poblacion")
head(pop_campana_adm1)

## Cobertura de campaña --------------------------------------------------------
# Ahora, quisiéramos calcular la cobertura de la campaña para cada departamento
# y cada día que ha pasado.
campana_departamento <- rnve_original %>% 
  # Puesto que estamos monitoreando solo la campaña, filtramos esas dosis
  filter(dosis == "Campaña") %>% 
  # La columna municipio_res_mad contiene el municipio y el departamento
  # separados por un "-". Puesto que este patrón es válido para todas las filas,
  # podemos obtener el departamento utilizando la función separate de tidyr.
  tidyr::separate(
    # La columna que queremos separar
    municipio_res_mad,
    # Los nombres de las nuevas columnas que se crearán.
    #   1.  municipio contendrá todo lo que esté a la izquierda del
    #       separador
    #   2.  departamento_res_mad contendrá todo lo que esté a la derecha
    #       del separador
    c("municipio", "departamento_res_mad"),
    # El caracter que queremos usar como separador.
    sep = "-"
  ) %>% 
  # En caso haya quedado espacio en blanco extra, lo eliminamos con la
  # función trimws.
  mutate(departamento_res_mad = trimws(departamento_res_mad)) %>% 
  # Ahora ya podemos agrupar por día de vacunación y el departamento de
  # residencia
  group_by(fecha_vac, departamento_res_mad) %>% 
  # Y con la agrupación hecha, calcular la cantidad de vacunados para esos
  # grupos
  summarise(
    vacunados = n()
  ) %>%
  # Como últimos pasos, quisiéramos calcular la cobertura, y para ello
  # necesitamos la población. Esta información la tenemos en pop_campana_adm1.
  left_join(., pop_campana_adm1, by = "departamento_res_mad") %>% 
  # Calculamos la cobertura
  mutate(cobertura = vacunados / poblacion * 100) %>% 
  # Y calculamos la cobertura acumulada para cada departamento (similar
  # al cálculo de susceptibles acumulados en la sección 6.)
  group_by(departamento_res_mad) %>% 
  arrange(fecha_vac) %>% 
  mutate(cobertura_acumulada = cumsum(cobertura))
head(campana_departamento)

## Cobertura nacional ----------------------------------------------------------
# La tabla anterior nos da un resumen por departamento y día de la campaña.
# Para obtener una que nos resuma el avance por día a nivel nacional, podemos
# realizar los mismos pasos de antes, pero sin agrupar por departamento.
#
# Primero, obtenemos la población objetivo de la campaña.
pop_campana_nacional <- registro_civil_original %>% 
  # Filtramos la fecha de nacimiento de acuerdo a las edades calculadas
  # anteriormente.
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  # Calculamos la cantidad de personas nacidas que son elegibles para la
  # campaña. Llamamos a esta columna "población".
  tally(name = "poblacion") %>% 
  # Puesto que para este punto tenemos una tabla con una sola fila,
  # sacamos el dato de población objetivo a nivel nacional
  pull(poblacion)
pop_campana_nacional
# Ahora, calculamos la cobertura
campana_nacional <- rnve_original %>% 
  # Puesto que estamos monitoreando solo la campaña, filtramos esas dosis
  filter(dosis == "Campaña") %>% 
  # Agrupamos por día solamente
  group_by(fecha_vac) %>% 
  # Y calculamos la cantidad de vacunados por dia
  summarise(
    vacunados = n()
  ) %>%
  # Calculamos la cobertura usando la poblacion objetivo a nivel nacional
  mutate(cobertura = vacunados / pop_campana_nacional * 100) %>% 
  # Desagrupamos, ordenamos por fecha de vacunacion y calculamos la cobertura
  # acumulada diaria
  ungroup %>% 
  arrange(fecha_vac) %>% 
  mutate(cobertura_acumulada = cumsum(cobertura))
head(campana_nacional)
### Gráfica --------------------------------------------------------------------
ggplot(
  campana_nacional,
  aes(x = fecha_vac)
) +
  # Nombramos los ejes y la leyenda
  labs(x = "Fecha", y = "Dosis", fill = "Dosis", linetype = "Cobertura") +
  # Mostramos el numero de dosis aplicadas cada mes
  geom_bar(aes(y = vacunados), stat = "identity", position = "stack") +
  # Mostramos la cobertura acumulada para cada mes.
  # NOTA: Los datos de n_dosis alcanzan aprox 7,500, mientras que las 
  #       coberturas son entre 0 y 100. Por lo tanto, se multiplica el valor de 
  #       coberturas por 75 para igualar los dos ejes.
  geom_line(aes(y = cobertura_acumulada * 75), linewidth = 1) +
  # Modificamos el eje vertical
  scale_y_continuous(
    # Ajustamos los limites entre 0 y 1,000 dosis
    limits = c(0, 7500),
    # Agregamos un segundo eje horizontal
    # NOTA: Aplicamos un factor de conversión de 75 para que el eje de cobertura
    #       alcance 100% cuando el número de dosis alcance 7,500 dosis.
    sec.axis = sec_axis( trans= ~./75, name = "Cobertura (%)")
  ) +
  # Mejoramos la visualización
  theme_classic() +
  theme(text = element_text(size = 16))
