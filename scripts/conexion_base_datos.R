# conexion_base_datos.R -------------------------------------------------
# Descripción: Este script contiene la clase de conexion con base de datos
# y uso de dbplyr
#
# Referencias:
#  
# Creado por -------------------------------------------------------------------
# Nombre: Rafael Leon
# Creado en: 2024-03-07
# Editorial --------------------------------------------------------------------
# Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# Carga de paquetes ####
source("requirements.R")
pacman::p_load(dplyr,
               DBI,
               RPostgres,
               bench)
# 1. Conexión a base de datos ####
# En el proceso de conexion a base de datos lo más importante es identificar las
# credenciales de conexión, adicionalmente es importante recordar que estas
# credenciales son secretas, revelarlas al público es un riesgo de seguridad que
# debemos mitigar
source("local_settings.R") #archivo con las credenciales, asegurarse que esté en
# .gitignore

# Abrimos la conexión con la base de datos. Las credenciales se encuentran en
# local_settings.R
con <- DBI::dbConnect(RPostgres::Postgres(),
                      host = host,
                      dbname = dbname,
                      user = user,
                      password = password,
                      port = 5432, )

# 2. Lectura de bases de datos al ambiente local #####
## Identificar las tablas de la base ####
dbListTables(con)
# tenemos 4 tablas en la base de datos

## Descargar una tabla a memoria local ######
# Vamos a descargar la base de registro nominal de vacunación y la base de 
# registro civil
rnve <- dbReadTable(con, "rnve")
registro_civil <- dbReadTable(con,"registro-civil")

# Escribimos los CSV para usarlos luego
write.csv(rnve, "data/RNVE.csv", row.names = F)
write.csv(registro_civil, "data/registro_civil.csv", row.names = F)

# ¿Qué hay dentro de la tabla RNVE? Identificar estructura, cantidad de registros,
# ¿En qué formato se descarga?
glimpse(rnve)
nrow(rnve)
colnames(rnve)

# ¿Qué hay dentro de la tabla de Registro Civil?
glimpse(registro_civil)
nrow(registro_civil)
colnames(registro_civil)

# ¿Creen que esto es algo viable para ustedes? ¿Cuantos filas tiene un registro
# nominal de uso nacional? Millones si no es que decenas de millones.

# Descargar las bases directamente deja de ser una opción cuando comenzamos a 
# trabajar con bases de datos reales

# Ejercicio: Realicen una tabla de resumen con # de primeras dosis administradas
# por departamento en la bases descargadas, la tabla debe tener cod_departamento,
# departamento_res_mad y número de dosis



resumen_dosis_muni <- rnve %>% 
  filter(dosis == "Primera") %>% 
  left_join(registro_civil, by="ID") %>% 
  group_by(cod_departamento, departamento_res_mad) %>% 
  tally()

# DBPLYR ####
# Veamos una forma en la que no requerimos descargar todo a la máquina utilizando
# dbplyr, una implementación de dplyr que opera de forma remota.
# Limitantes: Solo opera funciones nativas de dplyr o nativas de R.
# Esto quiere decir que no tenemos paquetes como lubridate, stringr y otras
# utilidades de trabajar local, tenemos que implementar las mismas soluciones
# con R Base y dplyr. 

rnve_online <- tbl(con, "rnve")
registro_civil_online <- tbl(con,"registro-civil")
# ¿Qué hay dentro de la tabla? Identificar estructura, cantidad de registros,
# ¿En qué formato se descarga?
glimpse(rnve_online)
count(rnve_online)
colnames(rnve_online)


glimpse(registro_civil_online)
count(registro_civil_online)
colnames(registro_civil_online)

## Query online ####
# Hagamos el mismo query con las tablas online

resumen_dosis_muni_online <- rnve_online %>% 
  filter(dosis == "Primera") %>% 
  left_join(registro_civil_online, by="ID") %>% 
  group_by(cod_departamento, departamento_res_mad) %>% 
  tally()

## show_query() ####
# show_query() es una función que nos permite ver hacia que expresión de SQL se 
# está traduciendo el código de dplyr que estamos utilizando. Esto puede servir
# para identificar errores, por ejemplo, si usamos una biblioteca que no esté
# implementada en dbplyr el query va a fallar y show_query también, hagamos la prueba

show_query(resumen_dosis_muni_online)

show_query(resumen_dosis_muni_online %>% 
             mutate(n_2 = stringr::str_glue(n,"error"))
           )

# show_query() sirve como herramienta de validación

## collect(), compute(), collapse() ####

# collect() es la forma en la que traemos un resultado de la base de datos remota 
# a la memoria de R. 
# si vemos ahora la tabla no está en RStudio todavía, esto significa que dbplyr
# ni siquiera ha ejecutado nada del lado del servidor de base de datos, esto solo
# ocurre cuando hacemos collect()

resumen_dosis_local <- collect(resumen_dosis_muni_online)

# compute() ejecuta el query del lado de la base de datos remota y almacena una tabla
# temporal a la base remota, esta tabla se puede referenciar desde R por el nombre
# que le asignamos. Compute ejecuta el query, pero no lo descarga a la máquina

resumen_dosis_computada <- compute(resumen_dosis_muni_online)

# collapse() crea un subquery que podemos usar en el proceso de otro cálculo,
# Por ejemplo, unificar el resultado del collapse con un join

registro_civil_resumido  <- registro_civil_online %>% 
  select(ID, cod_departamento, departamento_res_mad) %>% 
  collapse()
# collapse no ha ejecutado nada del lado del servidor, solo creó el query
show_query(registro_civil_resumido)
# modifiquemos el query original para que utilice la versión colapsada de la tabla
# de registro civil, esto evitará que se formen tantas variables .y en la tabla
resumen_dosis_muni_online2 <- rnve_online %>% 
  filter(dosis == "Primera") %>% 
  left_join(registro_civil_resumido, by="ID") %>% 
  group_by(cod_departamento, departamento_res_mad) %>% 
  tally()

show_query(resumen_dosis_muni_online2)

resumen_local <- collect(resumen_dosis_muni_online2)

# Profiler
# Evaluar cuanto tiempo toma ejecutar la consulta del lado de R Studio y cuanto 
# tiempo toma ejecuarla en el DBMS
# colocamos lo que queremos evaluar en funciones que deben devolver lo mismo
# para facilitarlo podemos poner el proceso completo y decir que regrese TRUE
# esto nos evita las conversiones de tipos de datos equivalentes como tibble, 
# data.table y otros.

offline_done <- function(){
  
  rnve <- read.csv("")
  registro_civil <- dbReadTable(con,"registro-civil")
  
  resumen_dosis_muni <- rnve %>% 
    filter(dosis == "Primera") %>% 
    left_join(registro_civil, by="ID") %>% 
    group_by(cod_departamento, departamento_res_mad) %>% 
    tally()
  return(TRUE)
} 

offline_done <- function(){
  
  rnve <- dbReadTable(con, "rnve")
  registro_civil <- dbReadTable(con,"registro-civil")
  
  resumen_dosis_muni <- rnve %>% 
    filter(dosis == "Primera") %>% 
    left_join(registro_civil, by="ID") %>% 
    group_by(cod_departamento, departamento_res_mad) %>% 
    tally()
  return(TRUE)
} 

online_done <- function(){
  registro_civil_resumido  <- registro_civil_online %>% 
    select(ID, cod_departamento, departamento_res_mad) %>% 
    collapse()
  
  resumen_dosis_muni_online2 <- rnve_online %>% 
    filter(dosis == "Primera") %>% 
    left_join(registro_civil_resumido, by="ID") %>% 
    group_by(cod_departamento, departamento_res_mad) %>% 
    tally() %>% 
    collect()
  
  return(TRUE)
  
}


# utilizamos la funcion bench::mark con las dos expresiones que queremos evaluar
# esto ejecuta las funciones un par de veces y devuelve el resultado de cuanto
# tiempo le toma ejecutarlo

bnch <- bench::mark(offline_done(),online_done())

# Visualizamos el resultado como tabla
bnch

# Producimos un gráfico del resultado
ggplot2::autoplot(bnch)



# Escribir una tabla a la base de datos #### 
# ¡¡Alerta!!!: Esto no se hace en la vida real, una persona que se encarga de análisis
# de datos no crea tablas en un servidor de SQL, estas tablas se crean por parte
# de un administrador de bases de datos, aprendemos esto para propositos didácticos
# en la realidad cada tabla que se crea debe seguir un proceso establecido y 
# contar con las validaciones de estructura adecuadas.

dbWriteTable(con, "rnve-resumen-departamental", resumen_local)

# Cerrar la conexión ####
# Siempre que abrimos una conexión de SQL tenemos que cerrarla, los servidores
# de bases de datos tienen una cantidad máxima de conexiones que permiten
# definida por el administrador del servidor. Si nosotros no cerramos nuestras
# conexiones 
dbDisconnect(con)