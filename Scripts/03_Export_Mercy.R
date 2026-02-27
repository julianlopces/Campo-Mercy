# Exportar alertas ---------------------------------------------------------------

# Verificar si las credenciales están disponibles
if (!exists("temp_creds_file") || !file.exists(temp_creds_file)) {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

sheet <- tryCatch({
  gs4_get(id_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# Helper genérico para exportar a Google Sheets con manejo de errores y pausa opcional
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("Exportando %s...", label))
  tryCatch({
    sheet_write(df, ss = ss, sheet = sheet_name)
    message(sprintf("Datos de %s exportados correctamente.", label))
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
}

# Llamadas usando el wrapper
export_sheet(alertas,             sheet, "alertas_mercy",  label = "alertas",                 pause = 5)


# Exportat audios para Supervisores

vars_finanzas <- c(
  "ingresos_semana",
  "ingresos_mes",
  "gasto_alimentos",
  "gasto_educacion",
  "gasto_transporte",
  "gasto_salud",
  "gasto_arriendo",
  "gasto_agua",
  "gasto_energia",
  "gasto_gas_gasolina",
  "gasto_carbon_lena",
  "gasto_internet",
  "gasto_no_alim",
  "gasto_prestamos",
  "gasto_inversion",
  "gasto_remesas_ven",
  "gasto_ahorros",
  "gasto_otros",
  "corr_alim",
  "corr_edu",
  "corr_trans",
  "corr_salud",
  "corr_arriendo",
  "corr_agua",
  "corr_energia",
  "corr_gas",
  "corr_carbon",
  "corr_internet",
  "corr_no_alim",
  "corr_prestamos",
  "corr_inversion",
  "corr_remesas",
  "corr_ahorros",
  "corr_otros"
)


audios_supervisores <- alertas %>%
  filter(push_estado == "Éxito")%>%
  arrange(SubmissionDate_COL)%>%
  mutate(SubmissionDate = as.character(SubmissionDate_COL))%>%
  select(Fecha = SubmissionDate,Encuestador = username, Encuestado = pull_name, Celular = pull_celular,
         audit, audit_2, audit_3, all_of(vars_finanzas))


sheet2 <- tryCatch({
  gs4_get("16-oq125y0fLtAyJ19ImRNnSQ9H-nSNcI77yILvQRgjY")
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

export_sheet(audios_supervisores,sheet2, "base_audios_supervisores",  label = "audios", pause = 5)


message("✅ Todos los datos fueron exportados exitosamente.")