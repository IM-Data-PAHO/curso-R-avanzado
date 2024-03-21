# Run requirements.R to load the needed libraries ------------------------------
source("requirements.R")

# Cargue bases
registro_civil <- import("data/registro_civil.csv")
#source("scripts/create_rnve_db.R")
rnve <- import("data/rnve.csv")


# cargue SHP####

# Con ayudar de la libreria SF podemos añadir un archivo tipo SHP a R

mun <- read_sf("data\\Anterior_URYMixed\\URY_ADM2_Anterior.shp")

### visualizacion general ####

# Al añadir la funcion plot no permite visualizar la estructura basica del SHP

plot(mun)

# Visualizaciones ggplot ####

## Basica ####

# tambien se puede utilizar ggplot2 para generar la visualizacion del codigo shp

ggplot()+
  geom_sf(data = mun)

## Ajuste BD y union al shp ####

# Ademas podemos convinar el shp con bases de datos para generar visualizaciones simples y estaticas

poblacion <- registro_civil %>% 
  select(cod_municipio, municipio_res_mad) %>% 
  group_by(GIS_CODE = cod_municipio) %>% 
  summarise(municipio_res_mad = first(municipio_res_mad), total_pob = n())

sin_vac <- rnve %>% 
  select(municipio_res_mad) %>% 
  group_by(municipio_res_mad) %>% 
  summarise(total_vac = n())

datos_map <- full_join(poblacion, sin_vac, by = "municipio_res_mad")

## Creacion de variables en el shp ####

datos_map <- full_join(mun, datos_map,by = "GIS_CODE") %>% 
  mutate(rango_pob = case_when(total_pob >= 1 & total_pob <= 474 ~ "1 - 474", # se pueden generar variables adicionales
                               total_pob > 474  & total_pob <= 964  ~ "474 - 964",
                               total_pob > 964 & total_pob <= 2648 ~ "964 - 2648",
                               total_pob > 2648 ~ "> 2648",
                               TRUE ~ "Sin Dato"),
         rango_pob = factor(rango_pob, levels = c("1 - 474", #asignar ajuste de variables(tipo factor en este caso)
                                                  "474 - 964",
                                                  "964 - 2648",
                                                  "> 2648",
                                                  "Sin Dato")),
         rango_avance = case_when(total_vac >= 1 & total_vac <= 533 ~ "1 - 533",
                                  total_vac > 533 & total_vac <= 1080 ~ "533 - 1080",
                                  total_vac > 1080 & total_vac <= 2898 ~ "1080 - 2898",
                                  total_vac > 2898~ "> 2898",
                                  TRUE ~ "Sin Dato"),
         rango_avance = factor(rango_avance, levels = c("1 - 533",
                                                  "533 - 1080",
                                                  "1080 - 2898",
                                                  "> 2898",
                                                  "Sin Dato"))
         )

## mapa Corropletas ####

# Se puede generar un mapa estatico de corropletas para el analisis de 
# distribucion espacial (Poblacion a intervenir) ggplot2

corropletas <- ggplot()+ 
  geom_sf(data = datos_map,
          aes(fill = rango_pob,
              geometry = geometry),
          color = '#969696',
          size = .9)+
  scale_fill_manual("Numeró de habitantes", 
                    values = c("white", "lightpink", "red", "darkred", "gray")) # ajuste manual de plaeta de color

corropletas

## Creacion de centroides ####

# al generar uniones se pueden generar duplicidad de informacion o puede ser posible que existan valores duplicados en las bases previas
datos_map <- st_make_valid(datos_map)


# Hacer un centroide para cada poligono

coord_puntos <- datos_map %>% 
  st_centroid()

## Mapa de frecuencia por clouster ####

puntos <-  ggplot()+
  geom_sf(data = datos_map,
          color = 'black',
          size = .1)+
  geom_sf(data = coord_puntos,
          aes(size = rango_avance),
          color = 'red',   # Set the color to red
          alpha = 0.5)     # Set the transparency (alpha) to 0.5

puntos


# Cambiar los mapas ####


ggplot()+  # Inicia un objeto ggplot
  geom_sf(data = datos_map,   # Agrega una capa al gráfico con datos de geometría espacial (geom_sf).
          aes(fill = rango_pob), # para colorear las áreas según la variable rango_pob. 
          color = '#636363',
          size = .2)+
  geom_sf(data = coord_puntos %>% # Agrega una capa al gráfico con datos de geometría espacial (geom_sf). 
            filter(rango_avance != "Sin Dato"),
          aes(size = rango_avance), # Usa el tamaño del punto (size) según la variable rango_avance
          color = "black", alpha = 0.7)+
  scale_fill_manual("Tasa de incidencia", #Define manualmente los colores para la escala de colores de la variable de relleno. 
                    values = c("1 - 474" = "#ffffd4",
                               "474 - 964"= "#fee391",
                               "964 - 2648"= "#fe9929",
                               "> 2648"= "#d95f0e"))+
  labs(title = "Porcentaje de avance campaña vacunacion 2024",
       caption = "Fuente : Datos random")+
  theme_void()+ # Personalización adicional del tema del gráfico. 
  theme(title=element_text(face = "bold"), #Establece el estilo del título en negrita,  
        legend.position= c(.9, .3), 
        legend.justification='left', # la posición y la orientación de la leyenda,
        legend.direction='vertical',
        legend.text=element_text(size=14)) # y el tamaño del texto de la leyenda.

# Mapas Interactivos ####

datos_map2 <- full_join(poblacion, sin_vac, by = "municipio_res_mad")

## Creacion de variables en el shp ####

datos_map2 <- full_join(mun, datos_map2,by = "GIS_CODE")

set.seed(234)

# Extraer una muestra aleatoria del dataframe
rnve <- rnve[sample(nrow(rnve), 10000), ]

# Create leaflet

# Manual breaks for color bins
breaks <- quantile(datos_map2$total_vac, na.rm = T)

# para encontrar colores se puede utilizar paginas como https://r-charts.com/es/colores/

pal <- colorBin(c("#24693D","#8CCE7D", "orange" ,"#EACF65", "#BF233C"), reverse = T , domain = datos_map2$total_vac, bins = breaks)

# se agregan labels a las capas agregadas
labels_cor <- sprintf("<b>%s", paste("Avance",datos_map$ADM2_ISON, datos_map$rango_avance)) %>%
  lapply(htmltools::HTML)

labels_punt <- sprintf(paste("ID caso", rnve$ID))

map <- leaflet(datos_map2) %>% 
  setView(-55.5, -32.5, zoom = 6) %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addEasyButton(
    easyButton(
      icon = "fa-globe",
      title = "Zoom Inicial",
      onClick = JS("function(btn, map){ map.setZoom(6); }")
    )
  )

map <-map %>% 
  addPolygons(
    fillColor = ~pal(total_vac),
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    label = labels_cor,
    group = "avance" )%>% 
  addLegend(
    position = "bottomleft",
    pal = pal,
    values = ~total_vac,
    na.label = "Sin Dato",
    title = "Número de vacunados")
map <- map %>% 
  addCircles(
    data = rnve,
    lng = ~longitude,
    lat = ~latitude,
    group = "Puntos",
    label = labels_punt,
    fillOpacity = 0.4) 
map <- map %>% 
    addHeatmap(
      data = rnve,
      lng = ~longitude,
      lat = ~latitude,
      group = "calor", 
      intensity = 2,
      blur = 50) %>% addLayersControl(overlayGroups = c("avance", "Puntos", "calor") , 
                     options = layersControlOptions(collapsed = TRUE ))

map


# Mapa con filtro ---------------------------------------------------------------------

sexo <- unique(rnve$sexo)
  
sexo_list <- list()
  
for (i in 1:length(sexo)) {
    
sexo_list[[i]] <- rnve %>% dplyr::filter(sexo == sexo[i]) 
  }
  
  names(sexo_list) <- sexo
  
  map2 <- leaflet() %>% addTiles()
  
  colores <- c("pink", "blue")
  
  for (i in 1:length(sexo)) {
    map2 <- map2 %>% addCircles(data = sexo_list[[i]], 
                              lat = ~latitude,
                              lng = ~longitude,
                              fillOpacity = 0.5, 
                              label = ~ID,
                              popup = ~paste("Edad de la madre", edad_madre), 
                              group = sexo[i],
                              color = colores[i])
  }
  
  map2 <- map2 %>% addLayersControl(overlayGroups = sexo, 
                                  options = layersControlOptions(collapsed = TRUE ))
  map2

