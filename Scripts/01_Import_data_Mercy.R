# 1 . Importar datos desde Survey-----------------------------------------------

# Asegurarse de que las credenciales necesarias estén disponibles
if (exists("email") && exists("password") && exists("server") && exists("formid")) {
  message("Credenciales de Survey cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Survey. Asegúrate de cargarlas desde el script maestro.")
}


data_ejemplo <-read_excel("raw_data/data_ejemplo_mercy.xlsx")
vars_needed <- c(colnames(data_ejemplo),"pull_celular_base")

## Conect to SurveyCTO ----------------------------------------------------------------

API <- paste0('https://',server,'.surveycto.com/api/v2/forms/data/wide/json/',formid,'?date=0')


## Import data -------------------------------------------------------------

max_attempts <- 10
attempt <- 1

repeat {
  # Llamada a la API
  dataset_json <- POST(
    url = API,
    config = authenticate(email, password),
    add_headers("Content-Type: application/json"),
    encode = 'json'
  )
  
  # Convertir JSON a data frame
  data <- jsonlite::fromJSON(rawToChar(dataset_json$content), flatten = TRUE)
  
  # Si df es un data frame válido, salir del ciclo
  if (is.data.frame(data)) break
  
  # Si se alcanzó el número máximo de intentos, lanzar error y salir
  if (attempt >= max_attempts) {
    stop("Se alcanzó el número máximo de intentos sin obtener un data frame válido.")
  }
  
  # Esperar antes de reintentar
  Sys.sleep(300)
  attempt <- attempt + 1
}

# Transformar base de datos ----------------------------------------------------


for (v in vars_needed) {
  if (!(v %in% names(data))) {
    data[[v]] <- rep(NA, nrow(data))
  }
}

# Organizar variables

# Reordenar y dejar las demás al final
otras_vars <- setdiff(names(data), vars_needed)
data <- data[ , c(vars_needed, otras_vars)]

# Filtrar pilotos --------------------------------------------------------------

data <- data %>%
  filter(nchar(ID) > 5 & username != "jlopez@equilibriumbdc.com")

# Nombres de las variables de opción múltiple
multi_vars <- c("cuidado_hogar",
                "nb_no_satisfechas",
                "inseguridad",
                "discriminacion",
                "corregir_cual"
)

for (var in multi_vars) {
  var_cols <- names(data)[startsWith(names(data), paste0(var, "_")) & !grepl("_o$", names(data))]
  
  if (length(var_cols) > 0) {
    data <- data %>%
      rowwise() %>%
      mutate(!!var := {
        vals <- c_across(all_of(var_cols))
        
        if (all(is.na(vals))) {
          NA_character_
        } else {
          activos <- which(vals == 1)
          if (length(activos) == 0) NA_character_ else {
            seleccionados <- gsub(paste0("^", var, "_"), "", var_cols[activos])
            paste(seleccionados, collapse = ",")
          }
        }
      }) %>%
      ungroup()
  }
}

# Transformar variables numéricas ----------------------------------------------


data <- data %>%
  mutate(
    across(starts_with("fcs"), as.numeric),
    across(starts_with("rcsi"), as.numeric),
    across(starts_with("hhs"), as.numeric),
    SubmissionDate = mdy_hms(SubmissionDate, tz = "UTC"),
    SubmissionDate_COL = SubmissionDate - hours(5)
  ) %>%
  filter(SubmissionDate_COL >= ymd("2026-02-27"))


# Correcciones valores numéricos

data <- data %>%
  mutate(ingresos_mes = case_when(
    ID == "a1e57ee9-b441-4a81-84c2-06ca683c9f68" & ingresos_mes == "20000000" ~ "2000000",
    ID == "3877173c-a2be-42d7-8431-27f0ee9d4606" & ingresos_mes == "8500000" ~ "850000",
    TRUE ~ ingresos_mes),
    gasto_inversion = case_when(
      ID == "1f4253a8-a6ef-478b-b5d3-f594d4354e16" & gasto_inversion == "5670000" ~ "567000",
      TRUE ~ gasto_inversion))

var_gastos_1 <- names(data %>%
                      select(!contains("number") & contains("gasto") &  !contains("validacion") & !contains("umbral") 
                             & !contains("totales")))
var_gastos_1 <- c(var_gastos_1,"ingresos_mes")


data <- data %>%
  mutate(across(all_of(var_gastos_1),
                ~ if_else(.x %in% c("88","99"), "0", .x))) %>%
  mutate(across(all_of(var_gastos_1),
                ~ as.numeric(.x),
                .names = "{.col}_number"))







