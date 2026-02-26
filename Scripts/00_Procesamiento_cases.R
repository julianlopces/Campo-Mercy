## Procesamiento de base de datos para creación de cases #######################
## Proyecto: Mercy Corps - VenEsperanza ########################################
## Descripción: Este script toma la base de datos cruda de las personas a 
## encuestar y la transformar en una base de cases para Survey Cto

# Cargar paquetes y base de datos ----------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse,readxl,writexl)

Muestra_completa_23_02_26 <- read_excel("bases_case_management_raw/Muestra_operativo_23_02_26.xlsx",
                                        sheet = "Listado_Campo")
Cuotas <- read_excel("bases_case_management_raw/Muestra_operativo_23_02_26.xlsx", 
                     sheet = "Matriz_Cuotas")%>%
  filter(Meta_Tratados > 0)
Base_miembros_17_02_26 <- read_excel("bases_case_management_raw/Base_miembros_17_02_26.xlsx")

# Limpieza y ajustes de la base ------------------------------------------------

## Filtrado sólo tratamiento de muestra principal, por ahora

muestra_operativo <- Muestra_completa_23_02_26 %>%
  mutate(muestra = trimws(muestra))

## Selección columnas para cases

cases_totales <- muestra_operativo %>%
  select(id = caseid,
         label = case_name,
         sortby = nivel_prioridad,
         celular =numero_contacto_colombiano,
         celular_2 = numero_contacto_alternativo,
         grupo = tipo_intervencion,
         socio_name = socio_name,
         fecha_deposito_1 = fecha_deposito_1,
         Dpto = Departamento_DIVIPOLA,
         Mpio = Municipio_DIVIPOLA,
         estatus_campo,
         prioridad_muestreo,
         prioridad_control
         )%>%
  mutate(
    Tratamiento = case_when(
      grupo == "Control (Solo Asistencia)" ~ "C",
      grupo == "Ruta Mixta (Ahorro + Emprendimiento)" ~ "T",
      TRUE ~ NA_character_
    ),
    prioridad = coalesce(prioridad_muestreo,prioridad_control),
    Dpto = trimws(Dpto),
    Mpio = trimws(Mpio),
    fecha_deposito_1 = format(fecha_deposito_1, "%d-%m-%Y"),
    formids = "Mercy_seguimiento",
    users = NA,
    roles = NA,
    enumerators = NA,
    numero_contactos = 0,
    estado = "Sin contactar",
    label = str_to_title(label)
  )%>%
  relocate(
    c(formids,users,enumerators,roles),.before = sortby
  )%>%
  select(-c(prioridad_muestreo,prioridad_control))%>%
  arrange(estatus_campo,prioridad)%>%
  mutate(
    sortby = row_number(),
    nuevo_celular = NA,
    fecha_reagendamiento = NA
  )

## Crear cuotas ----------------------------------------------------------------

Cuotas_procesado <- Cuotas %>%
  mutate(dep_mun = paste(Departamento,Municipio, sep = "_"))


cases_totales <- cases_totales %>%
  mutate(
    dep_mun = paste(Dpto,Mpio, sep = "_"),
    dep_mun_dummy = if_else(dep_mun %in% Cuotas_procesado$dep_mun,1,0),
    cuota = if_else(dep_mun_dummy == 1, paste(dep_mun,Tratamiento,sep = "_"),
                    paste(Dpto,Tratamiento,sep="_"))
  )%>%
  filter(!cuota %in% c("ARAUCA_C","ARAUCA_T"))%>%
  select(-c(dep_mun,dep_mun_dummy))


tabla_cuotas <- cases_totales %>%
  group_by(Dpto,Mpio,cuota)%>%
  summarise(disponibles_en_base = n())%>%
  ungroup()%>%
  filter(!duplicated(cuota))%>%
  left_join(Cuotas_procesado%>%select(Municipio,Meta_Tratados), by = c("Mpio"="Municipio"))%>%
  mutate(Meta_Tratados = case_when(
    Dpto == "ANTIOQUIA" & is.na(Meta_Tratados) ~ 2,
    Dpto == "BOLÍVAR" & is.na(Meta_Tratados) ~ 11,
    Dpto == "LA GUAJIRA" & is.na(Meta_Tratados) ~ 3,
    Dpto == "NORTE DE SANTANDER" & is.na(Meta_Tratados) ~ 1,
    TRUE ~ Meta_Tratados
  ))%>%
  select(cuota,Meta_Tratados)


cuotas_disponibles_en_base <- cases_totales %>%
  group_by(cuota)%>%
  summarise(disponibles_en_base = n())

tabla_cuotas <- tabla_cuotas %>%
  left_join(cuotas_disponibles_en_base, by = "cuota")%>%
  mutate(encuestas_levantadas = 0)%>%
  rename(meta = Meta_Tratados)%>%
  arrange(desc(meta))


# Procesar miembros del hogar --------------------------------------------------

miembros_hogar <- Base_miembros_17_02_26 %>%
  filter(parentesco_titular_hogar != "cabeza_de_hogar" )

## Conteo de tamaño de hogar y añadir a casos

hh_size_total <- miembros_hogar %>%
  group_by(caseid)%>%
  summarise(miembros_hogar = n())

cases_totales <- cases_totales %>%
  left_join(hh_size_total, by = c("id" = "caseid"))%>%
  mutate(miembros_hogar = if_else(is.na(miembros_hogar),0,miembros_hogar))


## Crear base de miembros

miembros_hogar_procesada <- miembros_hogar %>%
  select(house_id = caseid, caseid_individuo, nombre_miembro = nombre_completo_miembro_de_famil, relacion_jefe = parentesco_titular_hogar)%>%
  group_by(house_id)%>%
  mutate(number = row_number(),
         member_id = paste(house_id,number,sep = "_"),
         nombre_miembro = str_to_title(nombre_miembro))%>%
  ungroup()%>%
  relocate(member_id,.after = house_id)%>%
  select(-number)

# crear encuestadores

#enumerators <- tibble(id = "jlopez@equilibriumbdc.com",name = "JulianL", users="jlopez@equilibriumbdc.com" )

# Exportar bases ---------------------------------------------------------------

## Separar por estatus de campo ------------------------------------------------

cases_totales_titular <- cases_totales %>%
  filter(estatus_campo == "1. TITULAR (MUESTRA)")

# 1. Lista de encuestadores (24 correos)
encuestadores <- c(
  "mabecabo70@hotmail.com", "almitarojast@gmail.com", "rosnell0507@hotmail.com",
  "mar.zarate@hotmail.com", "sancue25@gmail.com", "jmrivera183@gmail.com",
  "drmoniy9@gmail.com", "icardenasyrodriguez2024@gmail.com", "ivonneyangeles1@gmail.com",
  "infoberlynestrada28@gmail.com", "linah9708@gmail.com", "mari020611@gmail.com",
  "adriana124ramirez@gmail.com", "maryciebarrios@gmail.com", "macda061@gmail.com",
  "machelitagiraldo@gmail.com", "mabeliugc@gmail.com", "stella.moralesmartinez1@gmail.com",
  "caballerodiana0809@gmail.com", "velasquezofelia305@gmail.com", "chvenegasr@gmail.com",
  "danitriana47@gmail.com", "pgutierrezlopez7@gmail.com", "bifandino@hotmail.com"
)

# 2. Correo del supervisor
supervisor <- "jlopez@equilibriumbdc.com"

# 3. Ordenar la base de datos por cuota
# Esto garantiza que al aplicar la secuencia, las cuotas grandes se fragmenten proporcionalmente
cases_totales_titular <- cases_totales_titular[order(cases_totales_titular$cuota), ]

# 4. Generar la secuencia de asignación
# Calculamos cuántas filas hay y repetimos la lista de correos hasta cubrir el total
n_total <- nrow(cases_totales_titular)
secuencia_encuestadores <- rep(encuestadores, length.out = n_total)

# 5. Crear las columnas 'users' y 'enumerators' con el formato solicitado
# Formato: "correo_encuestador, jlopez@equilibriumbdc.com"
cases_totales_titular$users <- paste(secuencia_encuestadores, supervisor, sep = ", ")
cases_totales_titular$enumerators <- cases_totales_titular$users

# 6. Verificación de la repartición (Opcional)
# Esto te mostrará cuántos casos recibió cada uno (debería ser ~33 o 34 casos por persona)
table(secuencia_encuestadores)

cases_totales_sobremuestra <- cases_totales %>%
  filter(estatus_campo == "2. REEMPLAZO (SOBREMUESTRA)")
cases_totales_control <- cases_totales %>%
  filter(estatus_campo == "3. CONTROL (ESPEJO)")

write_xlsx(cases_totales_titular, "bases_case_management_clean/cases_tratamiento_titular_23_02_26.xlsx")
write_xlsx(cases_totales_sobremuestra, "bases_case_management_clean/cases_tratamiento_sobremuestra_23_02_26.xlsx")
write_xlsx(cases_totales_control, "bases_case_management_clean/cases_control_23_02_26.xlsx")
write_xlsx(miembros_hogar_procesada, "bases_case_management_clean/miembros_hogar_17_02_26.xlsx")
write_xlsx(tabla_cuotas, "bases_case_management_clean/cuotas_23_02_26.xlsx")

#write_xlsx(enumerators, "bases_case_management_clean/enumerators_17_02_26.xlsx")










