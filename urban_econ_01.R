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
library(RCurl)
library(readxl)

#====================================================================================
# Load and clean data
#====================================================================================
# Shape files
mex_spat <- gadm(country = "MEX", level = 2, path = tempdir())
mex_muni <- st_as_sf(mex_spat)

st_write(mex_muni, "mex_muni_gadm.shp", delete_layer = TRUE)

carpeta_rasters <- "..."
ruta_shapefile  <- "..."
ruta_panel      <- "..."

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

extraer_indicadores <- function(df, ruta) {
  if (is.null(df)) {
    warning("Could not read CB: ", ruta)
    return(NULL)
  }
  cols <- colnames(df)
  atestiguo_cols_nueva <- paste0("BP1_4_", 1:6)
  habito_cols_nueva    <- paste0("BP1_5_", 1:5)
  if ("BP1_1" %in% cols && "BP1_3" %in% cols &&
      all(atestiguo_cols_nueva %in% cols) && all(habito_cols_nueva %in% cols)) {
    if (all(c("CVE_ENT", "CVE_MUN") %in% cols)) {
      cve_mun <- paste0(str_pad(as.character(as.integer(df$CVE_ENT)), 2, pad = "0"),
                        str_pad(as.character(as.integer(df$CVE_MUN)), 3, pad = "0"))
    } else if (all(c("ENT", "MUN") %in% cols)) {
      cve_mun <- paste0(str_pad(as.character(as.integer(df$ENT)), 2, pad = "0"),
                        str_pad(as.character(as.integer(df$MUN)), 3, pad = "0"))
    } else {
      warning("Nueva era CB without recognizable geography: ", ruta)
      return(NULL)
    }
    return(tibble(
      cve_mun     = cve_mun,
      percepcion  = as.integer(df[["BP1_1"]]),
      expectativa = as.integer(df[["BP1_3"]]),
      atestiguo_n = rowSums(sapply(df[atestiguo_cols_nueva], function(x) as.integer(x) == 1), na.rm = TRUE),
      atestiguo_valido = rowSums(sapply(df[atestiguo_cols_nueva], function(x) as.integer(x) %in% c(1, 2)), na.rm = TRUE) > 0,
      habito_n    = rowSums(sapply(df[habito_cols_nueva], function(x) as.integer(x) == 1), na.rm = TRUE),
      habito_valido = rowSums(sapply(df[habito_cols_nueva], function(x) as.integer(x) %in% c(1, 2)), na.rm = TRUE) > 0,
      ruta_cb     = ruta
    ))
  }
  atestiguo_cols_vieja <- paste0("P3_", 1:6)
  habito_cols_vieja    <- paste0("P4_", 1:5)
  if ("P1" %in% cols && "P2" %in% cols &&
      all(atestiguo_cols_vieja %in% cols) && all(habito_cols_vieja %in% cols) &&
      all(c("ENT", "FOL", "CON", "V_SEL", "N_HOG") %in% cols)) {
    return(tibble(
      ENT   = str_pad(as.character(as.integer(df$ENT)), 2, pad = "0"),
      FOL   = as.character(df$FOL),
      CON   = as.character(df$CON),
      V_SEL = as.character(df$V_SEL),
      N_HOG = as.character(df$N_HOG),
      percepcion  = as.integer(df[["P1"]]),
      expectativa = as.integer(df[["P2"]]),
      atestiguo_n = rowSums(sapply(df[atestiguo_cols_vieja], function(x) as.integer(x) == 1), na.rm = TRUE),
      atestiguo_valido = rowSums(sapply(df[atestiguo_cols_vieja], function(x) as.integer(x) %in% c(1, 2)), na.rm = TRUE) > 0,
      habito_n    = rowSums(sapply(df[habito_cols_vieja], function(x) as.integer(x) == 1), na.rm = TRUE),
      habito_valido = rowSums(sapply(df[habito_cols_vieja], function(x) as.integer(x) %in% c(1, 2)), na.rm = TRUE) > 0,
      ruta_cb = ruta,
      necesita_join_viv = TRUE
    ))
  }
  warning("CB matches neither era pattern: ", ruta, " | Columns: ", paste(cols, collapse = ", "))
  return(NULL)
}

cb_data_ensu <- inventario %>%
  filter(tipo == "CB", encuesta == "ENSU") %>%
  mutate(datos = map(ruta, leer_dbf_seguro))

indicadores_extraidos <- cb_data_ensu %>%
  mutate(ind = map2(datos, ruta, extraer_indicadores))

ind_con_geo <- indicadores_extraidos %>%
  mutate(tiene_geo = map_lgl(ind, ~ !is.null(.x) && "cve_mun" %in% colnames(.x))) %>%
  filter(tiene_geo) %>%
  pull(ind) %>%
  bind_rows()

ind_sin_geo <- indicadores_extraidos %>%
  mutate(necesita_join = map_lgl(ind, ~ !is.null(.x) && "necesita_join_viv" %in% colnames(.x))) %>%
  filter(necesita_join)

carpeta_padre <- function(ruta) dirname(ruta)

viv_2013_2015 <- viv_data %>%
  filter(str_detect(ruta, "ensu_bd_2013|ensu_bd_2014|ensu_bd_2015")) %>%
  mutate(
    carpeta = carpeta_padre(ruta),
    datos_geo = map(datos, agregar_cve_mun)
  )

ind_vieja_con_geo <- ind_sin_geo %>%
  mutate(carpeta = carpeta_padre(ruta)) %>%
  left_join(viv_2013_2015 %>% select(carpeta, datos_geo), by = "carpeta") %>%
  mutate(
    ind_join = map2(ind, datos_geo, function(cb_df, viv_df) {
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
        select(cve_mun, percepcion, expectativa, atestiguo_n, atestiguo_valido,
               habito_n, habito_valido, ruta_cb)
    })
  ) %>%
  pull(ind_join) %>%
  bind_rows()

panel_indicadores_raw <- bind_rows(ind_con_geo, ind_vieja_con_geo) %>%
  filter(!is.na(cve_mun)) %>%
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

panel_inseguridad_municipio_ano <- panel_indicadores_raw %>%
  group_by(cve_mun, ano) %>%
  summarise(
    n_encuestados       = n(),
    
    prop_inseguro       = mean(percepcion[percepcion %in% c(1, 2)] == 2, na.rm = TRUE),
    n_valido_percepcion = sum(percepcion %in% c(1, 2)),
    
    prop_espera_empeorar = mean(expectativa[expectativa %in% 1:4] == 4, na.rm = TRUE),
    n_valido_expectativa = sum(expectativa %in% 1:4),
    
    prop_atestiguo_alguno = mean(atestiguo_valido & atestiguo_n > 0, na.rm = TRUE),
    indice_atestiguo      = mean(atestiguo_n[atestiguo_valido], na.rm = TRUE),  # 0-6 scale
    n_valido_atestiguo     = sum(atestiguo_valido),
    
    prop_cambio_habito     = mean(habito_valido & habito_n > 0, na.rm = TRUE),
    indice_habito          = mean(habito_n[habito_valido], na.rm = TRUE),       # 0-5 scale
    n_valido_habito         = sum(habito_valido),
    
    .groups = "drop"
  )

# Data base unification
base_url <- "https://raw.githubusercontent.com/carloscarrillol/Shading-Out-Fear/main/data/"

file_names <- c("panel_homicidios_mx_2011_2020.csv", 
                "panel_inseguiridad_mun_ano.csv", 
                "panel_ndvi_mx.csv",
                "muni.xlsx")

for (file in file_names) {
  full_url <- paste0(base_url, file)
  download.file(url = full_url, destfile = file, mode = "wb")
  message(paste("Successfully downloaded:", file))
}

ndvi <- read.csv("panel_ndvi_mx.csv")
homicidios <- read.csv("panel_homicidios_mx_2011_2020.csv")
inseguridad <- read.csv("panel_inseguiridad_mun_ano.csv")
muni_names <- read_excel("muni.xlsx")     # Data base for municipality names and codes to merge sf

colnames(muni_names) <- c("CVEGEO", "cve_ent", "NOM_ENT", "NOM_ABR",
                          "cve_mun", "NAME_2", "CVE_CAB", "NOM_CAB",
                          "pob_tot", "pob_mas", "pob_fem", 
                          "TOTAL DE VIVIENDAS HABITADAS")
muni_names <- muni_names[-c(1:3),]

ndvi <- ndvi %>%
  left_join(muni_names %>% 
              select(pob_tot, pob_mas, pob_fem, 
                     NAME_2, cve_mun, cve_ent), 
            by = "NAME_2")
ndvi <- ndvi %>%
  mutate(cve_mun = as.integer(paste0(cve_ent, cve_mun)))

panel_final <- ndvi %>%
  inner_join(homicidios, by = c("cve_mun", "year" = "ano")) %>%
  inner_join(inseguridad, by = c("cve_mun", "year" = "ano"))

# ================================================================================
# REAL ECONOMETRICS CODING COMING SOON ...
# ================================================================================