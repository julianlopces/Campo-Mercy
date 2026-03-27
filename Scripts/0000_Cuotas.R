cases <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1jISKknSOXNigJNBygxGPEYNbTlZvmL835nEHie7qe6Q/edit?gid=2100601727#gid=2100601727")

cuotas_survey <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18okqNnbRi8fXqDZC5cDP3dG8G2PuYF00AGFPVls9omQ/edit?gid=2117072521#gid=2117072521")


cuotas_abiertas <- cuotas_survey %>%
  filter(encuestas_levantadas < meta)%>%
  mutate(tratamiento = str_sub(cuota,-1))


cuotas_cerradas <- cuotas_survey %>%
  filter(encuestas_levantadas >= meta)%>%
  mutate(tratamiento = str_sub(cuota,-1))


casos_abiertos_detalle <- cases %>%
  filter(
    numero_contactos <= 3 & !estado %in% c("Éxito","Número equivocado",
                                           "No desea participar") &
      cuota %in% cuotas_abiertas$cuota & !str_detect(users,"Cerrado") &
      !str_detect(users,"Cierre") & !str_detect(users,"Cuota")
  )
   

casos_abiertos <- cases %>%
  filter(
    numero_contactos <= 3 & !estado %in% c("Éxito","Número equivocado",
                                           "No desea participar") &
      cuota %in% cuotas_abiertas$cuota
  )%>%
  group_by(cuota, Tratamiento)%>%
  summarise(asignados_disponibles = n())%>%
  ungroup()%>%
  mutate(
    exitos_asignados_estimados =
      case_when(
        Tratamiento == "T" ~ round(asignados_disponibles * 0.51),
        Tratamiento == "C" ~ round(asignados_disponibles * 0.41),
        TRUE ~ NA
      )
  )%>% select(-Tratamiento)


analisis_cuotas <- cuotas_abiertas %>%
  left_join(casos_abiertos, by = "cuota")%>%
  select(-disponibles_en_base)%>%
  mutate(
    exitos_estimados =  coalesce(encuestas_levantadas,0) + coalesce(exitos_asignados_estimados,0),
    alerta_cuota = if_else(exitos_estimados <= meta, "Cuota en riesgo", "Ok", missing = "No se han asignado"),
    margen = exitos_estimados - meta)%>%
  arrange(margen)
  

cases_totales_control <- googlesheets4::read_sheet("1z4j7eRhUtzw1Fgp8YOftqkCOLWJFS5GSvCFYYAJzG7Y")
cases_sobremuestra_tratamiento <- googlesheets4::read_sheet("1lvEJWi9-uXY8x4ogOMM4sSti7XATB-3-da0Fu7i0X90")

cuotas_por_asignar_control <- cases_totales_control %>%
  filter(!id %in% cases$id & cuota %in% cuotas_abiertas$cuota)

cuotas_por_asignar_tratamiento <- cases_sobremuestra_tratamiento %>%
  filter(!id %in% cases$id & cuota %in% cuotas_abiertas$cuota)
  

table(cuotas_por_asignar_control$prioridad)
table(cuotas_por_asignar_tratamiento$prioridad)

cuotas_por_asignar_total_detalle <- bind_rows(cuotas_por_asignar_control,cuotas_por_asignar_tratamiento)
cuotas_por_asignar_total <- bind_rows(cuotas_por_asignar_control,cuotas_por_asignar_tratamiento)

cuotas_por_asignar_total <- cuotas_por_asignar_total %>%   
  group_by(cuota, Tratamiento)%>%
  summarise(por_asignar_disponibles = n())%>%
  ungroup()%>%
  mutate(
    exitos_por_asignar_estimados =
      case_when(
        Tratamiento == "T" ~ round(por_asignar_disponibles * 0.51),
        Tratamiento == "C" ~ round(por_asignar_disponibles * 0.41),
        TRUE ~ NA
      )
  )%>% select(-Tratamiento)


# Analisis final

analisis_cuotas_final <- analisis_cuotas %>%
  left_join(cuotas_por_asignar_total, by = "cuota") %>%
  mutate(exitos_estimados_final = coalesce(exitos_estimados, 0) + 
           coalesce(exitos_por_asignar_estimados, 0),
         alerta_cuota_final = if_else(exitos_estimados_final <= meta,
                                      "Cuota en riesgo",
                                      "Ok"),
         margen = exitos_estimados_final - meta)%>%
  arrange(margen)

analisis_cuotas_final_resumen <- analisis_cuotas_final %>%
  select(cuota,meta,encuestas_levantadas,exitos_estimados_final,alerta_cuota_final,margen)



# Exitos  ----------------------------------------------------------------

cases_exitos <- cases %>%
  filter(estado == "Éxito")

## Casos abiertos por encuestador

casos_abiertos_detalle_encuestador <- casos_abiertos_detalle %>%
  mutate(users =trimws(str_remove(users,fixed(", jlopez@equilibriumbdc.com"))),
         enumerators = users)


googlesheets4::write_sheet(casos_abiertos_detalle_encuestador,ss = "1r954FJkk5OS_hLGCU8Xkw39NBHPHCAe__BeWe8MOMJQ",
                           sheet = "casos_abiertos")

googlesheets4::write_sheet(analisis_cuotas,ss = "1r954FJkk5OS_hLGCU8Xkw39NBHPHCAe__BeWe8MOMJQ",
                           sheet = "Proyeccion_cuotas_asignadas")

googlesheets4::write_sheet(analisis_cuotas_final,ss = "1r954FJkk5OS_hLGCU8Xkw39NBHPHCAe__BeWe8MOMJQ",
                           sheet = "Proyeccion_cuotas_por_asignar")

casos_abiertos_encuestador <- data.frame(table(casos_abiertos_detalle_encuestador$users,casos_abiertos_detalle$Tratamiento))

casos_abiertos_encuestador <- casos_abiertos_encuestador %>%
  rename(
    encuestador = Var1,
    tratamiento = Var2,
    Casos_abiertos = Freq
  ) %>%
  # Esto asegura que todas las combinaciones de encuestador y (C, T) existan
  tidyr::complete(encuestador, tratamiento = c("C", "T"), fill = list(Casos_abiertos = 0)) %>%
  pivot_wider(names_from = tratamiento,
              values_from = Casos_abiertos) %>%
  mutate(
    casos_abiertos_totales = C + T
  ) %>%
  arrange(desc(casos_abiertos_totales))

googlesheets4::write_sheet(casos_abiertos_encuestador,ss = "1r954FJkk5OS_hLGCU8Xkw39NBHPHCAe__BeWe8MOMJQ",
                           sheet = "Casos abiertos por encuestador")




# cases_cerrados <- cases %>%
#   filter(cuota %in% cuotas_survey$cuota & estado != "Éxito"
#          & !str_detect(users,"Cerrado"))%>%
#   select(id,users,enumerators)%>%
#   mutate(users = "Cuota completa",
#          enumerators = "Cuota completa")
# 
# 
# write_xlsx(cases_cerrados, "bases_case_management_clean/base_para_cerrar_cuotas.xlsx")
# 
# 
# cases_filtrado <- cases %>%
#   filter(users %in% asignar_controles & numero_contactos == 0 &
#            !cuota %in% cuotas_survey$cuota & Tratamiento == "C")