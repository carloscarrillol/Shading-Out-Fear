#====================================================================================
# Install libraries
#====================================================================================
library(tibble)
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
library(foreign)

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
# Danger Perception (ENSU, ECOSEP - INEGI)
ruta_raiz <- "/Users/carloscarrillolazaro/Desktop/urban_econ_01/ENSU"

archivos <- list.files(ruta_raiz, pattern = "\\.dbf$",
                       recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

inventario <- tibble(ruta = archivos) %>%
  mutate(
    nombre = basename(ruta),
    encuesta = case_when(
      str_detect(str_to_upper(nombre), "^ENSU") ~ "ENSU",
      str_detect(str_to_lower(nombre), "^segpub") ~ "ECOSEP",
      TRUE ~ NA_character_
    ),
    tipo = str_extract(str_to_upper(nombre), "VIV|CB|CS")
  )

stopifnot("Hay archivos sin clasificar en encuesta" = sum(is.na(inventario$encuesta)) == 0)

leer_dbf_seguro <- possibly(~ read.dbf(.x, as.is = TRUE), otherwise = NULL)

viv_data <- inventario %>%
  filter(tipo == "VIV") %>%
  mutate(
    datos = map(ruta, leer_dbf_seguro),
    tiene_cd = map_lgl(datos, ~ !is.null(.x) && "CD" %in% colnames(.x)),
    nivel_geo = if_else(tiene_cd, "ciudad", "estado")
  )

viv_data %>%
  mutate(cols = map_chr(datos, ~ paste(colnames(.x), collapse = ", "))) %>%
  select(ruta, nivel_geo, cols) %>%
  print(n = 100)

construir_cve_mun <- function(df, ruta) {
  if (is.null(df)) {
    warning("No se pudo leer: ", ruta)
    return(character(0))
  }
  cols <- colnames(df)
  if (all(c("ENT", "MPIO") %in% cols)) {
    return(
      df %>%
        mutate(cve_mun = paste0(
          str_pad(as.character(as.integer(ENT)), 2, pad = "0"),
          str_pad(as.character(as.integer(MPIO)), 3, pad = "0")
        )) %>%
        distinct(cve_mun) %>%
        pull(cve_mun)
    )
  }
  if (all(c("CVE_ENT", "CVE_MUN") %in% cols)) {
    return(
      df %>%
        mutate(cve_mun = paste0(
          str_pad(as.character(as.integer(CVE_ENT)), 2, pad = "0"),
          str_pad(as.character(as.integer(CVE_MUN)), 3, pad = "0")
        )) %>%
        distinct(cve_mun) %>%
        pull(cve_mun)
    )
  }
  if (all(c("ENT", "MUN") %in% cols)) {
    return(
      df %>%
        mutate(cve_mun = paste0(
          str_pad(as.character(as.integer(ENT)), 2, pad = "0"),
          str_pad(as.character(as.integer(MUN)), 3, pad = "0")
        )) %>%
        distinct(cve_mun) %>%
        pull(cve_mun)
    )
  }
  if ("CVE_MUN" %in% cols) {
    return(
      df %>%
        mutate(cve_mun = str_pad(as.character(as.integer(CVE_MUN)), 5, pad = "0")) %>%
        distinct(cve_mun) %>%
        pull(cve_mun)
    )
  }
  warning("Archivo sin columnas de municipio reconocibles: ", ruta,
          " | Columnas disponibles: ", paste(cols, collapse = ", "))
  return(character(0))
}

municipios_por_trimestre <- viv_data %>%
  filter(nivel_geo == "ciudad") %>%
  mutate(cve_mun_list = map2(datos, ruta, construir_cve_mun))

listas_validas <- municipios_por_trimestre %>%
  mutate(n_mun = map_int(cve_mun_list, length)) %>%
  filter(n_mun > 0) %>%
  pull(cve_mun_list)

municipios_balanceados <- reduce(listas_validas, intersect)
municipios_union <- reduce(listas_validas, union)

cb_data <- inventario %>%
  filter(tipo == "CB") %>%
  mutate(datos = map(ruta, leer_dbf_seguro))

cb_data_ensu <- cb_data %>% filter(encuesta == "ENSU")




library(dplyr)
library(stringr)
library(purrr)
library(foreign)
library(tibble)
library(tidyr)

# ============================================================
# Requiere que ya existan en la sesión: inventario, leer_dbf_seguro,
# viv_data, cb_data (o cb_data_ensu) construidos en pasos previos.
# ============================================================

# ------------------------------------------------------------
# 1. Función: agrega cve_mun a un data frame VIV completo (no solo distinct)
#    Reutiliza la misma lógica de 4 esquemas ya validada.
# ------------------------------------------------------------

agregar_cve_mun <- function(df) {
  if (is.null(df)) return(NULL)
  cols <- colnames(df)
  
  if (all(c("ENT", "MPIO") %in% cols)) {
    return(df %>% mutate(cve_mun = paste0(
      str_pad(as.character(as.integer(ENT)), 2, pad = "0"),
      str_pad(as.character(as.integer(MPIO)), 3, pad = "0")
    )))
  }
  if (all(c("CVE_ENT", "CVE_MUN") %in% cols)) {
    return(df %>% mutate(cve_mun = paste0(
      str_pad(as.character(as.integer(CVE_ENT)), 2, pad = "0"),
      str_pad(as.character(as.integer(CVE_MUN)), 3, pad = "0")
    )))
  }
  if (all(c("ENT", "MUN") %in% cols)) {
    return(df %>% mutate(cve_mun = paste0(
      str_pad(as.character(as.integer(ENT)), 2, pad = "0"),
      str_pad(as.character(as.integer(MUN)), 3, pad = "0")
    )))
  }
  if ("CVE_MUN" %in% cols) {
    return(df %>% mutate(cve_mun = str_pad(as.character(as.integer(CVE_MUN)), 5, pad = "0")))
  }
  
  warning("VIV sin columnas de municipio reconocibles")
  return(df %>% mutate(cve_mun = NA_character_))
}

# ------------------------------------------------------------
# 2. Función: extrae percepción homologada de un archivo CB
#    Devuelve tibble con: cve_mun (si está disponible directo),
#    llave_hogar (para join con VIV si hace falta), percepcion (1-9)
# ------------------------------------------------------------

extraer_percepcion <- function(df, ruta) {
  if (is.null(df)) {
    warning("No se pudo leer CB: ", ruta)
    return(NULL)
  }
  cols <- colnames(df)
  
  # Esquema 2016-2020: BP1_1 + CVE_MUN ya viene directo en el CB
  if ("BP1_1" %in% cols && all(c("CVE_ENT", "CVE_MUN") %in% cols)) {
    return(
      df %>%
        transmute(
          cve_mun = paste0(
            str_pad(as.character(as.integer(CVE_ENT)), 2, pad = "0"),
            str_pad(as.character(as.integer(CVE_MUN)), 3, pad = "0")
          ),
          percepcion = as.integer(BP1_1),
          ruta_cb = ruta
        )
    )
  }
  
  # Esquema 2016 (variante): BP1_1 + ENT/MUN sin prefijo CVE_
  if ("BP1_1" %in% cols && all(c("ENT", "MUN") %in% cols)) {
    return(
      df %>%
        transmute(
          cve_mun = paste0(
            str_pad(as.character(as.integer(ENT)), 2, pad = "0"),
            str_pad(as.character(as.integer(MUN)), 3, pad = "0")
          ),
          percepcion = as.integer(BP1_1),
          ruta_cb = ruta
        )
    )
  }
  
  # Esquema 2013: P1 + sin geografía propia, necesita join con VIV
  if ("P1" %in% cols && all(c("ENT", "FOL", "CON", "V_SEL", "N_HOG") %in% cols)) {
    return(
      df %>%
        transmute(
          ENT = str_pad(as.character(as.integer(ENT)), 2, pad = "0"),
          FOL = as.character(FOL),
          CON = as.character(CON),
          V_SEL = as.character(V_SEL),
          N_HOG = as.character(N_HOG),
          percepcion = as.integer(P1),
          ruta_cb = ruta,
          necesita_join_viv = TRUE
        )
    )
  }
  
  warning("CB sin variable de percepción reconocible: ", ruta,
          " | Columnas: ", paste(cols, collapse = ", "))
  return(NULL)
}

# ------------------------------------------------------------
# 3. Aplicar a todos los CB de ENSU (excluye ECOSEP)
# ------------------------------------------------------------

cb_data_ensu <- inventario %>%
  filter(tipo == "CB", encuesta == "ENSU") %>%
  mutate(datos = map(ruta, leer_dbf_seguro))

percepcion_extraida <- cb_data_ensu %>%
  mutate(percep = map2(datos, ruta, extraer_percepcion))

# ------------------------------------------------------------
# 4. Separar en dos grupos: ya tienen cve_mun vs necesitan join con VIV
# ------------------------------------------------------------

percep_con_geo <- percepcion_extraida %>%
  mutate(tiene_geo = map_lgl(percep, ~ !is.null(.x) && "cve_mun" %in% colnames(.x))) %>%
  filter(tiene_geo) %>%
  pull(percep) %>%
  bind_rows()

percep_sin_geo <- percepcion_extraida %>%
  mutate(necesita_join = map_lgl(percep, ~ !is.null(.x) && "necesita_join_viv" %in% colnames(.x))) %>%
  filter(necesita_join)

cat("Trimestres con geografía directa en CB:", nrow(percep_con_geo %>% distinct(ruta_cb)), "\n")
cat("Trimestres que necesitan join con VIV (2013):", nrow(percep_sin_geo), "\n")

# ------------------------------------------------------------
# 5. Para los trimestres 2013: emparejar cada CB con su VIV
#    (mismo trimestre = misma carpeta padre)
# ------------------------------------------------------------

carpeta_padre <- function(ruta) dirname(ruta)

viv_2013 <- viv_data %>%
  filter(str_detect(ruta, "ensu_bd_2013")) %>%
  mutate(
    carpeta = carpeta_padre(ruta),
    datos_geo = map(datos, agregar_cve_mun)
  )

percep_2013_con_geo <- percep_sin_geo %>%
  mutate(carpeta = carpeta_padre(ruta)) %>%
  left_join(viv_2013 %>% select(carpeta, datos_geo), by = "carpeta") %>%
  mutate(
    percep_join = map2(percep, datos_geo, function(cb_df, viv_df) {
      if (is.null(cb_df) || is.null(viv_df)) return(NULL)
      viv_llaves <- viv_df %>%
        transmute(
          ENT = str_pad(as.character(as.integer(ENT)), 2, pad = "0"),
          FOL = as.character(FOL),
          CON = as.character(CON),
          V_SEL = as.character(V_SEL),
          N_HOG = as.character(N_HOG),
          cve_mun
        ) %>%
        distinct(ENT, FOL, CON, V_SEL, N_HOG, .keep_all = TRUE)
      
      cb_df %>%
        left_join(viv_llaves, by = c("ENT", "FOL", "CON", "V_SEL", "N_HOG")) %>%
        select(cve_mun, percepcion, ruta_cb)
    })
  ) %>%
  pull(percep_join) %>%
  bind_rows()

cat("Filas 2013 con cve_mun asignado tras el join:",
    sum(!is.na(percep_2013_con_geo$cve_mun)), "de", nrow(percep_2013_con_geo), "\n")

# ------------------------------------------------------------
# 6. Panel final combinado, homologado
# ------------------------------------------------------------

panel_percepcion_raw <- bind_rows(percep_con_geo, percep_2013_con_geo) %>%
  filter(!is.na(cve_mun), !is.na(percepcion))

# Extrae año y trimestre desde la ruta del archivo para agregar
panel_percepcion_raw <- panel_percepcion_raw %>%
  mutate(
    ano = as.integer(str_extract(ruta_cb, "20(1[3-9]|20)")),
    mes = case_when(
      str_detect(str_to_lower(ruta_cb), "mar") ~ 3,
      str_detect(str_to_lower(ruta_cb), "jun") ~ 6,
      str_detect(str_to_lower(ruta_cb), "sep") ~ 9,
      str_detect(str_to_lower(ruta_cb), "dic") ~ 12,
      TRUE ~ NA_real_
    )
  )

# Agregación a nivel municipio-trimestre: proporción que percibe inseguridad
# Codificación oficial: 1 = seguro, 2 = inseguro, 9 = NS/NR (se excluye del denominador)
panel_percepcion_municipio_trimestre <- panel_percepcion_raw %>%
  filter(percepcion %in% c(1, 2)) %>%
  group_by(cve_mun, ano, mes) %>%
  summarise(
    n_encuestados = n(),
    prop_inseguro = mean(percepcion == 2),
    .groups = "drop"
  )

# Colapsa a municipio-año (promedio de los 4 trimestres) para alinear con NDVI/homicidios
panel_percepcion_municipio_ano <- panel_percepcion_municipio_trimestre %>%
  group_by(cve_mun, ano) %>%
  summarise(
    n_trimestres = n(),
    n_encuestados_total = sum(n_encuestados),
    prop_inseguro = weighted.mean(prop_inseguro, w = n_encuestados),
    .groups = "drop"
  )

cat("Municipios-año en el panel final:", nrow(panel_percepcion_municipio_ano), "\n")
cat("Municipios distintos:", n_distinct(panel_percepcion_municipio_ano$cve_mun), "\n")

# Verificación Morelia
panel_percepcion_municipio_ano %>% filter(cve_mun == "16053")

# ------------------------------------------------------------
# 7. Guardar
# ------------------------------------------------------------

dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(panel_percepcion_municipio_trimestre, "data/panel_percepcion_municipio_trimestre.csv", row.names = FALSE)
write.csv(panel_percepcion_municipio_ano, "data/panel_percepcion_municipio_ano.csv", row.names = FALSE)
