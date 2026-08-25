#====================


#==================

library(geodata)
library(sf)
library(ggplot2)
library(terra)
library(exactextractr)
library(purrr)
library(dplyr)
library(stringr)


# 1. Descargar municipios de México
mex_spat <- gadm(country = "MEX", level = 2, path = tempdir())
mex_muni <- st_as_sf(mex_spat)

# 2. Guardar en tu computadora (creará 4 archivos: .shp, .shx, .dbf, .prj)
# Úsalos juntos para subirlos a Internet
st_write(mex_muni, "mex_muni_gadm.shp", delete_layer = TRUE)



# --- Configuración ---
carpeta_rasters <- "/Users/carloscarrillolazaro/Desktop/urban_econ_01/TIF"      # donde tienes los .tif descargados de Drive
ruta_shapefile  <- "/Users/carloscarrillolazaro/Desktop/urban_econ_01/Mapa/mex_muni_gadm.shp"
ruta_panel      <- "data/panel_ndvi_mx.csv"  # output acumulable

municipios_sf <- st_read(ruta_shapefile, quiet = TRUE)

# --- Detectar qué años ya tienes descargados en la carpeta ---
archivos <- list.files(carpeta_rasters, pattern = "^MODIS_NDVI_Mexico_\\d{4}\\.tif$", full.names = TRUE)

if (length(archivos) == 0) {
  stop("No encontré archivos .tif en la carpeta. Revisa la ruta o el patrón del nombre.")
}

anos_disponibles <- as.integer(str_extract(basename(archivos), "\\d{4}"))
cat("Años encontrados en disco:", paste(sort(anos_disponibles), collapse = ", "), "\n")

# --- Si ya existe un panel previo, no reprocesar años que ya están ---
if (file.exists(ruta_panel)) {
  panel_previo <- read.csv(ruta_panel)
  anos_ya_procesados <- unique(panel_previo$year)
  cat("Años ya en el panel:", paste(sort(anos_ya_procesados), collapse = ", "), "\n")
} else {
  panel_previo <- NULL
  anos_ya_procesados <- integer(0)
}

anos_a_procesar <- setdiff(anos_disponibles, anos_ya_procesados)

if (length(anos_a_procesar) == 0) {
  cat("No hay años nuevos que procesar.\n")
} else {
  cat("Procesando:", paste(sort(anos_a_procesar), collapse = ", "), "\n")
  
  panel_nuevo <- map_dfr(anos_a_procesar, function(ano) {
    archivo <- archivos[anos_disponibles == ano]
    r <- rast(archivo)
    valores <- exact_extract(r, municipios_sf, "mean")
    
    municipios_sf %>%
      st_drop_geometry() %>%
      transmute(
        NAME_1 = NAME_1,
        NAME_2 = NAME_2,
        year = ano,
        ndvi_mean = valores
      )
  })
  
  # --- Combinar con lo que ya tenías y guardar ---
  panel_final <- bind_rows(panel_previo, panel_nuevo) %>%
    arrange(NAME_1, NAME_2, year)
  
  dir.create(dirname(ruta_panel), showWarnings = FALSE, recursive = TRUE)
  write.csv(panel_final, ruta_panel, row.names = FALSE)
  cat("Guardado:", ruta_panel, "- total de años en el panel:", 
      paste(sort(unique(panel_final$year)), collapse = ", "), "\n")
}







