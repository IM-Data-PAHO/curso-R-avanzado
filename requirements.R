# requirements.R ---------------------------------------------------------------
# Descripción: Contiene el listado de paquetes necesarios para la correcta
# ejecución de este proyecto.
# Creado por -------------------------------------------------------------------
# Nombre: CIM Data Team
# Creado en: 2024-02-05
# Editorial --------------------------------------------------------------------
# Sección para notas o cambios editoriales.
# ______________________________________________________________________________

# cargar pacman si no lo está
if (!require("pacman")) install.packages("pacman")

# cargar paquetes necesarios
pacman::p_load(
  tidyr,
  dplyr,
  lubridate,
  stringr,
  rio,
  ggplot2,
  bench,
  janitor,
  # reclin2,
  sf,
  readr,
  cleaner,
  plotly,
  DT,
  leaflet,
  leaflet.extras
)





