# Run requirements.R to load the needed libraries ------------------------------
source("requirements.R")

#Cargamos bases

# Run create_registro_civil_db.R -----------------------------------------------
#source("scripts/create_registro_civil_db.R")

# Run create_rnve_db.R ---------------------------------------------------------
#source("scripts/create_rnve_db.R")
rnve <- import("data/rnve.csv")
# registro_civil <- read_csv("data/uruguay/Registro civil - Uruguay.csv")

# Leer el archivo shapefile Uruguay
shp <- st_read("data/Anterior_URYMixed/URY_ADM2_Anterior.shp")

# Lee BD con info de puntos

puntos <- registro_civil

# Crear objetos sf transformando la informacion de latitud (y) y longitud (x)
puntos_sf <- st_as_sf(puntos, # base 
                      coords = c("longitude", "latitude"), # Columnas 
                      crs = st_crs(shp) # sistema de coordeadas
                      )

# Revisar la calidad de las geometrias 

table(st_is_valid(puntos_sf)) #Revisa que las geometrias ingresadas sean validas y no se crucen 

table(st_is_valid(shp))

# Corregir las geometrias

shp <- st_make_valid(shp) # arregla el error  austando el shp en un elemnto temporal 

# Realizar la intersección espacial y unión con el shapefile
interseccion <- st_intersects(puntos_sf, shp)

# Obtener los índices de la intersección

indices_interseccion <- lapply( #aplica la funcion deseada a cada elemento de una lista especificcada
  interseccion, #Elemento tipo lista   
  function(x) # define que se aplicara la funcion en el objeto X
    ifelse(length(x) > 0, # revisa que el numero de caracteres sea mayor a 0
           x, #si cumple guarda el elemento
           NA) # si no cumple regresa NA
  )

# Asignar atributos manualmente
puntos_sf$departamento_res_mad <- sapply( #es una función en R que aplica una función a cada elemento de una lista y devuelve un vector o matriz con los resultados.
  indices_interseccion, #lista
  function(x) # define que se aplicara la funcion en el objeto X
    ifelse( #condiccional
      !is.na(x), #Busca todo lo que no sea NA
      shp$ADM1_ISON[x], # Asigna la variable de la base shp correspondiente 
      NA) # si no es NA
  )

puntos_sf$municipio_res_mad <- sapply(indices_interseccion, function(x) ifelse(!is.na(x), shp$ADM2_ISON[x], NA))

puntos_sf$cod_municipio <- sapply(indices_interseccion, function(x) ifelse(!is.na(x), shp$GIS_CODE[x], NA))

puntos_sf$cod_departamento <- sapply(indices_interseccion, function(x) ifelse(!is.na(x), shp$ADM1_ISOC[x], NA))



# se agrega la informacion de Depto y Muni a base puntos

puntos_sf <- puntos_sf %>% 
  select(ID, departamento_res_mad, municipio_res_mad, cod_municipio, cod_departamento)

puntos <- left_join(x = puntos, y = puntos_sf, by = "ID")

puntos <- puntos %>% 
  select(-c(geometry, departamento_res_mad.x, municipio_res_mad.x, cod_municipio.x, cod_departamento.x)) %>% #Selecciono mis variables de interes
  rename( # Renombro las variables
    departamento_res_mad = departamento_res_mad.y,
    municipio_res_mad = municipio_res_mad.y, 
    cod_municipio = cod_municipio.y, 
    cod_departamento = cod_departamento.y
  ) %>% 
  select(1:9, 19, 20, 18, 21, everything()) #selecciono mis columnas de interes

registro_civil_ajust <- puntos
# Export table -----------------------------------------------------------------
write_csv(registro_civil_ajust, "data/Registro civil - Uruguay_ajust.csv")

# Remove all unnecessary variables ---------------------------------------------
rm(list=setdiff(ls(), c("registro_civil", "live_births")))

