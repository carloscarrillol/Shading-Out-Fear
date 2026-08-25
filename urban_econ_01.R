#====================================================================================
# Install libraries
#====================================================================================

library(stringi)
library(geodata)
library(sf)
library(ggplot2)
library(terra)
library(exactextractr)
library(purrr)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

#====================================================================================
# Load and clean data
#====================================================================================
# Shape files
mex_spat <- gadm(country = "MEX", level = 2, path = tempdir())
mex_muni <- st_as_sf(mex_spat)

st_write(mex_muni, "mex_muni_gadm.shp", delete_layer = TRUE)

carpeta_rasters <- "/Users/..."
ruta_shapefile  <- "/Users/..."
ruta_panel      <- "data/panel_ndvi_mx.csv"

# -----------------------------------------------------------------------------------
# Green spaces data (NDVI - Google Earth Engine)
municipios_sf <- st_read(ruta_shapefile, quiet = TRUE)

archivos <- list.files(carpeta_rasters, pattern = "^MODIS_NDVI_Mexico_\\d{4}\\.tif$", full.names = TRUE)

if (length(archivos) == 0) {
  stop("No encontré archivos .tif en la carpeta. Revisa la ruta o el patrón del nombre.")
}

anos_disponibles <- as.integer(str_extract(basename(archivos), "\\d{4}"))


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
  panel_final <- bind_rows(panel_previo, panel_nuevo) %>%
    arrange(NAME_1, NAME_2, year)
  dir.create(dirname(ruta_panel), showWarnings = FALSE, recursive = TRUE)
  write.csv(panel_final, ruta_panel, row.names = FALSE)
  cat("Guardado:", ruta_panel, "- total de años en el panel:", 
      paste(sort(unique(panel_final$year)), collapse = ", "), "\n")
}

# -----------------------------------------------------------------------------------
# Murder data (as danger proxy) (SESNSP - INEGI)

data_11_17 <- read_csv("IDM_MAnterior_ago2024 2.csv", locale = locale(encoding = "ISO-8859-1"))
data_15_25 <- read_csv("Municipal-Delitos-2015-2025_jul2026.csv", locale = locale(encoding = "ISO-8859-1"))


data_11_17 <- data_11_17 %>%
  rename(
    ano        = ANO,
    cve_mun    = INEGI,
    entidad    = ENTIDAD,
    municipio  = MUNICIPIO,
    modalidad  = MODALIDAD,
    tipo       = TIPO,
    subtipo    = SUBTIPO
  )

data_15_25 <- data_15_25 %>%
  rename(
    ano        = Ano,
    clave_ent  = Clave_Ent,
    entidad    = Entidad,
    cve_mun    = `Cve. Municipio`,
    municipio  = Municipio,
    bien_juridico = `Bien jurï¿½dico afectado`,
    tipo       = `Tipo de delito`,
    subtipo    = `Subtipo de delito`,
    modalidad  = Modalidad
  )

data_11_17 <- data_11_17 %>% mutate(cve_mun = str_pad(as.character(cve_mun), 5, pad = "0"))
data_15_25 <- data_15_25 %>% mutate(cve_mun = str_pad(as.character(cve_mun), 5, pad = "0"))
meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
           "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

meses_upper <- toupper(meses)

data_11_17_long <- data_11_17 %>%
  pivot_longer(cols = all_of(meses_upper), names_to = "mes", values_to = "delitos") %>%
  group_by(ano, cve_mun, entidad, municipio, tipo, subtipo, modalidad) %>%
  summarise(delitos = sum(delitos, na.rm = TRUE), .groups = "drop")

data_15_25_long <- data_15_25 %>%
  pivot_longer(cols = all_of(meses), names_to = "mes", values_to = "delitos") %>%
  group_by(ano, cve_mun, entidad, municipio, tipo, subtipo, modalidad) %>%
  summarise(delitos = sum(delitos, na.rm = TRUE), .groups = "drop")

normalizar <- function(x) stri_trans_general(str_to_upper(x), "Latin-ASCII")

homicidios_viejo <- data_11_17_long %>%
  mutate(modalidad_norm = normalizar(modalidad)) %>%
  filter(modalidad_norm == "HOMICIDIOS") %>%
  group_by(ano, cve_mun) %>%
  summarise(delitos_viejo = sum(delitos, na.rm = TRUE), .groups = "drop")

homicidios_nuevo <- data_15_25_long %>%
  mutate(tipo_norm = normalizar(tipo)) %>%
  filter(tipo_norm == "HOMICIDIO") %>%
  group_by(ano, cve_mun) %>%
  summarise(delitos_nuevo = sum(delitos, na.rm = TRUE), .groups = "drop")


traslape <- inner_join(
  homicidios_viejo %>% filter(ano %in% 2015:2017),
  homicidios_nuevo %>% filter(ano %in% 2015:2017),
  by = c("ano", "cve_mun")
)


factor_empalme <- traslape %>%
  summarise(
    factor = sum(delitos_nuevo, na.rm = TRUE) / sum(delitos_viejo, na.rm = TRUE),
    correlacion = cor(delitos_viejo, delitos_nuevo, use = "complete.obs"),
    n_municipios = n()
  )

cor_por_municipio <- traslape %>%
  group_by(cve_mun) %>%
  summarise(cor_mun = cor(delitos_viejo, delitos_nuevo), .groups = "drop")

homicidios_ajustado <- homicidios_viejo %>%
  filter(ano < 2015) %>%
  mutate(delitos = delitos_viejo * factor_empalme$factor) %>%
  select(ano, cve_mun, delitos) %>%
  bind_rows(
    homicidios_nuevo %>% filter(ano >= 2015) %>% rename(delitos = delitos_nuevo)
  ) %>%
  arrange(cve_mun, ano)

# --------------------------------------------------------------------------------
# Perception (ENSU - INEGI)





