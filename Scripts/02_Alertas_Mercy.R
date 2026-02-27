#### Alertas -------------------------------------------------------------------

alertas <- data %>%
  group_by(ID)%>%
  slice_tail(n=1)%>%
  ungroup()

# Crear duración y alerta

alertas <- alertas %>% 
  mutate(duration_minutes = round(as.numeric(time_encuesta_sec)/60,2))%>%
  mutate(part_valido = if_else(consentimiento == 1 & (informante_id == 1 | informante_id_2 == 1),1,0),
    flag_duracion = if_else(part_valido == 1 & duration_minutes < 20, 1,0,missing = 0))

# Alertas de saltos

### Missings y saltos


ODK_filtrado <- odkmissing::import_odk_propagate_required("raw_data/Instrumento_mercy_ODK.xlsx", required_value = "yes")


ODK_procesado <- odkmissing::build_spec_for_flags(datos = alertas, ODK_filtrado = ODK_filtrado)


spec_for_flags <- ODK_procesado$spec_for_flags
datos_tokens   <- ODK_procesado$datos_tokens

# Levantar missings


alertas <- odkmissing::flags_missing_por_variable(
  data          = datos_tokens,
  spec          = spec_for_flags,
  prefix        = "m",
  numeric_conds = TRUE,
  coerce_target = FALSE
)

variables_missing <- names(alertas)[grepl("^m_", names(alertas))]

alertas <- alertas |>
  dplyr::mutate(
    total_missing = rowSums(dplyr::pick(dplyr::all_of(variables_missing)), na.rm = TRUE),
    flag_missing  = ifelse(total_missing > 0, 1, 0)
  )


# Levantar saltos

alertas <- odkmissing::create_skip_vars(
  data          = alertas,
  spec          = spec_for_flags,
  prefix        = "s",
  numeric_conds = TRUE
)


# Alertas de gastos superiores a ingresos --------------------------------------

var_gastos <- names(data %>%
  select(contains("number") & contains("gasto")))

var_ingreso <- names(data %>%
                      select(contains("number") & contains("ingresos")))


alertas <- alertas %>%
  mutate(
    across(all_of(c(var_gastos,var_ingreso)), as.numeric),
    total_gastos = rowSums(across(all_of(var_gastos)), na.rm = TRUE),
    ingreso_mes  = .data[[var_ingreso]],
    flag_gastos  = if_else(
      total_gastos > ingreso_mes * 1.40,
      1,
      0,
      missing = 0
    )
  )


# Consolidar alertas -----------------------------------------------------------


alertas <- alertas %>%
  mutate(total_encuestas = n(),
         Exitos = if_else(flag_duracion== 0 & flag_missing == 0 & flag_skips == 0 &  
                            flag_gastos == 0 & part_valido == 1,1,0),
         Alertas = if_else(flag_duracion == 1 | flag_missing == 1 | flag_skips == 1 |   
                             flag_gastos == 1,1,0),
         Rechazo = if_else(no_acepta == 5 | consentimiento == 0,1,0,missing = NA),
         no_contesta = if_else(no_acepta == 1,1,0,missing = NA),
         num_equiv = if_else(no_acepta == 2,1,0,missing = NA),
         num_fuera = if_else(no_acepta == 3,1,0,missing = NA),
         reagendam = if_else(no_acepta == 4,1,0,missing = NA),
         otro_numeroco = if_else(no_acepta == 6,1,0,missing = NA)
  )

