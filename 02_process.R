# Limpieza de raw_trips_2023

#### Creo un nuevo data frame sobre el que voy a trabajar
#### Elimino de raw_trips_2023 las columnas que no necesito y las que presentan inconsistencias o falta de información. 
#### Elimino los n/a
#### Convierto las columnas de fecha_origen y fecha_destino a date time, agrego una nueva columna calculando el tiempo de viaje en minutos y agrego una para indicar el día de la semana en el que comenzó el viaje

trips_2023 <- raw_trips_2023 %>% 
  select(-X, -duracion_recorrido, -género) %>% 
  drop_na() %>% 
  mutate_at(vars(nombre_estacion_origen, nombre_estacion_destino), toupper) %>% 
  mutate(fecha_origen_recorrido = as.POSIXct(fecha_origen_recorrido, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(fecha_destino_recorrido = as.POSIXct(fecha_destino_recorrido, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(minutos_viaje = floor(as.numeric(difftime(fecha_destino_recorrido, fecha_origen_recorrido, units = "mins")))) %>% 
  mutate(dia_semana_viaje = weekdays(fecha_origen_recorrido))

glimpse(trips_2023)



###############################################################################

# Limpieza de raw_users_2023

#### Creo un nuevo data frame sobre el que voy a trabajar
#### Renombro ID_usuarios del data frame users_2023 para que coincidan los nombre de las columnas en ambos data frames. 
#### Borro los n/a
#### Cambio los formatos de fecha_alta a date
#### Filtro las edades para que solo aparezcan del rango 0 a 100
#### Filtro las edades para dejar solo valores finitos
#### Elimino las columnas hora_alta y customer.has.dni ya que no las necesito para este análisis.


users_2023 <- raw_users_2023 %>% 
  rename_all(tolower) %>% 
  drop_na() %>% 
  mutate(fecha_alta = as.Date(fecha_alta)) %>% 
  filter(edad_usuario >= 0 & edad_usuario <= 100) %>%
  filter(is.finite(edad_usuario)) %>% 
  select (-hora_alta, -customer.has.dni..yes...no.) %>% 
  mutate(dia_semana_registro = weekdays(fecha_alta))


#### Convierto los datos de la columna id_usuarios de chr a numeric, eliminando las letras.

trips_2023$id_usuario <-  as.integer(gsub("[^0-9]", "", trips_2023$id_usuario))

# Creo nuevo data frame con nuevos usuarios y sus viajes

#### Uno el data frame users_2023 al de new_trips_2023 a través de un inner join de sus usuarios. 
#### Elimino todas las columnas que finalizan con ".y"

user_trips_2023 <- trips_2023 %>% 
  inner_join(users_2023, by = "id_usuario") %>% 
  rename_with(~str_replace(., "\\.x$", "")) %>% 
  select(-ends_with(".x"), -ends_with(".y"))


#### Voy a agregar una columna "decada_edad" para indicar el rango de edad al que pertenece el usuario y voy a convertirlo al tipo "chr"

# Definir los límites de los rangos de edad
limites_edades <- c(15, 25, 35, 45, 55, 65, 75, 85, Inf)

# Etiquetas para los rangos de edad
etiquetas_edades <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", ">=85")

# Agregar la columna "rango_edad" al data frame
user_trips_2023 <- user_trips_2023 %>%
  mutate(rango_edad = cut(edad_usuario, breaks = limites_edades, labels = etiquetas_edades, include.lowest = TRUE))


