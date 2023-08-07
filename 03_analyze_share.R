# Cantidad

### ¿Cuántos usuarios nuevos se registraron?

nuevos_usuarios_total <- user_trips_2023 %>% 
  filter(!duplicated(id_usuario))

cantidad_nuevos_usuarios = nrow(nuevos_usuarios_total)
cantidad_nuevos_usuarios

### ¿Qué día de la semana tuvo mayor cantidad de nuevos usuarios?

orden_dias_semana <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
nuevos_usuarios_total$dia_semana_registro <- factor(nuevos_usuarios_total$dia_semana_registro, levels = orden_dias_semana)

# Crear el gráfico con el orden de los días de la semana especificado
nuevos_usuarios_x_dia <- nuevos_usuarios_total %>% 
  mutate(dia_semana_registro = factor(nuevos_usuarios_total$dia_semana_registro, levels = orden_dias_semana)) %>%   ggplot() +
    geom_bar(mapping = aes(x = dia_semana_registro, fill = dia_semana_registro)) +
    geom_text(aes(x = dia_semana_registro, label = after_stat(count)), 
              stat = "count", 
              vjust = 1.5,
              size = 3) +
    labs(title = "Nuevos usuarios por cada día de la semana", x = "Día de la semana", y = "Usuarios") +
    guides(fill = FALSE) +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral")

nuevos_usuarios_x_dia


### ¿Qué mes tuvo mayor cantidad de nuevos usuarios?

meses_alta = month(nuevos_usuarios_total$fecha_alta, label = TRUE)

nuevos_usuarios_x_mes <- ggplot(nuevos_usuarios_total) +
  geom_bar(mapping = aes(x=meses_alta, fill=meses_alta)) +
  geom_text(aes(x = meses_alta, label = after_stat(count)), 
            stat = "count", 
            vjust = 1.5,
            size=3) +
  labs(title = "Cantidad de nuevos usuarios por cada mes", y="Usuarios", x="Mes") +
  guides(fill = FALSE) +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral")

nuevos_usuarios_x_mes


# EDAD

### ¿Cuántos nuevos usuarios hay para cada rango de edad?
### ¿Cuál es el rango de edad con mayor cantidad de nuevos usuarios?
### ¿Cuál fue el promedio de edad de los nuevos usuarios?

edad_promedio <- round(mean(nuevos_usuarios_total$edad_usuario, na.rm = TRUE))

nuevos_usuarios_x_edad <- ggplot(nuevos_usuarios_total) +
  geom_histogram(mapping = aes(x=edad_usuario, fill=rango_edad), binwidth = 1, color="white", position = "identity") +
  labs(title = "Edad nuevos usarios", x = "Edad", y = "Usuarios", fill="Rango edad") +
  geom_vline(xintercept = edad_promedio, linetype = "dashed", color = "blue", size = .6) +
  annotate("text", x = edad_promedio + 1, y = 2500, label = paste("Edad promedio:", round(edad_promedio)), color = "blue", hjust = 0, size=3) +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral")

nuevos_usuarios_x_edad

# GENERO

### ¿Qué porcentaje de nuevos usuarios representa cada género?
genre_data <- nuevos_usuarios_total %>%
  group_by(genero_usuario) %>% 
  summarise(cantidad = n())

genre_data <- genre_data %>% 
  arrange(desc(genero_usuario)) %>%
  # mutate(prop = cantidad / sum(cantidad) *100) %>%
  mutate(prop = cantidad / sum(cantidad), porcentaje = scales::percent(prop, accuracy = 0.1)) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

nuevos_registros_x_genero <- ggplot(nuevos_usuarios_total) +
  geom_bar(mapping = aes(x=genero_usuario, fill=genero_usuario)) +
  geom_text(aes(x = genero_usuario, label = after_stat(count)), 
            stat = "count", 
            vjust = 1.5,
            size=3) +
  labs(x="Género", y="Cantidad") +
  theme_minimal() +
  guides(fill = FALSE) +
  scale_fill_brewer(palette = "Spectral")

nuevos_usuarios_x_genero <- ggplot(genre_data, aes(x="", y=prop, fill=genero_usuario)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(fill="Género")+
  geom_text(aes(y = ypos, label = porcentaje), color = "black", size=3) + 
  theme_void() +
  scale_fill_brewer(palette = "Spectral")

nuevos_registros_x_genero + nuevos_usuarios_x_genero +
  plot_annotation(title = "Género nuevos usuarios", theme = theme(plot.title = element_text(hjust = 0.5)))

# DURACIÓN VIAJES

### ¿Cuál fue el promedio de duración de viajes por día?

promedio_duracion_viajes <- mean(user_trips_2023$minutos_viaje, na.rm = TRUE)

duracion_viajes_total <- user_trips_2023 %>%
  filter((minutos_viaje/60) < 16) %>% 
  ggplot() +
  geom_line(mapping = aes(x=fecha_origen_recorrido, y=(minutos_viaje/60)), color='blue', alpha=0.5) +
  geom_hline(yintercept = (promedio_duracion_viajes/60), linetype = "solid", color = "red", size = .5) +
  labs(title="Duración de los viajes", 
       caption = "*Se omiten viajes superiores a 16 horas", 
       x="Fecha", 
       y="Horas") +
  scale_y_continuous(breaks = seq(0, 16, by = 4)) +
  theme_minimal()

duracion_viajes_total

### Repito el gráfico pero resalto la franja de horas con mayor frecuencia de viajes

duracion_viajes_total +
  annotate("rect", xmin = min(user_trips_2023$fecha_origen_recorrido), xmax = max(user_trips_2023$fecha_origen_recorrido),
           ymin = 0, ymax = 2, fill = "red", alpha = 0.2)


### Ahora grafico sobre la franja de dos horas que cuenta con la mayor frecuencia de viajes

duracion_viajes_dos_horas <- user_trips_2023 %>%
  filter(minutos_viaje < 120) %>% 
  ggplot() +
  geom_line(mapping = aes(x=fecha_origen_recorrido, y=minutos_viaje), color='blue', alpha=0.5) +
  geom_hline(yintercept = promedio_duracion_viajes, linetype = "solid", color = "red", size = .5) +
  annotate("text", 
           x = max(user_trips_2023$fecha_origen_recorrido), 
           y = promedio_duracion_viajes,
           label = paste("Viaje promedio:", round(promedio_duracion_viajes), "min"), 
           hjust = 1, 
           vjust = -1, 
           color = "red",
           size=3) +
  labs(title="Duración de los viajes", 
       x="Fecha", 
       y="Minutos") +
  scale_y_continuous(breaks = seq(0, 120, by = 20))

duracion_viajes_dos_horas


# UBICACIÓN

barrios <- st_read('https://bitsandbricks.github.io/data/CABA_barrios.geojson')

### ¿Dónde están ubicadas las estaciones?

estaciones <- user_trips_2023 %>% 
  distinct(nombre_estacion_origen, .keep_all = TRUE) %>% 
  select(nombre_estacion_origen, long_estacion_origen, lat_estacion_origen)

mapa_estaciones <- ggplot() +
  geom_sf(data=barrios, fill="white") +
  geom_point(data=estaciones, mapping = aes(x = long_estacion_origen, y = lat_estacion_origen), color='blue', alpha=.5) +
  labs(title="Ubicación estaciones", x=NULL, y=NULL) +
  guides(color = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

mapa_estaciones


### ¿Cuál fue la estación de inicio más elegida para el primer viaje por lo nuevos usuarios?

ranking_estaciones_origen <- nuevos_usuarios_total %>% 
  group_by(nombre_estacion_origen, long_estacion_origen, lat_estacion_origen) %>% 
  summarise(cantidad_viajes = n()) %>% 
  arrange(-cantidad_viajes) %>% 
  ungroup() %>% 
  slice(1:10)

barrios_ranking_origen <- cross_join(barrios, ranking_estaciones_origen)

View(barrios_ranking_origen)

lista_ranking_origen <- ggplot(ranking_estaciones_origen) +
  geom_col(mapping = aes(x = reorder(nombre_estacion_origen, cantidad_viajes), y = cantidad_viajes, fill=reorder(nombre_estacion_origen, -cantidad_viajes))) +
  geom_text(mapping = aes(x = reorder(nombre_estacion_origen, cantidad_viajes), y = cantidad_viajes, label = cantidad_viajes), 
            vjust = 0.33, 
            hjust=1.25, 
            size=3) +
  labs(x=NULL, y="viajes") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  scale_fill_brewer(palette="Spectral")

mapa_ranking_origen <- ggplot(barrios_ranking_origen) +
  geom_sf(fill="white") +
  geom_point(aes(x = long_estacion_origen, 
                 y = lat_estacion_origen, 
                 color = reorder(nombre_estacion_origen, -cantidad_viajes), 
                 size = cantidad_viajes),
             alpha=0.5) +
  scale_size_continuous(range = c(3, 15)) +
  labs(x=NULL, y=NULL, color="Estación") +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 5, alpha=1)), size=FALSE) +
  scale_color_brewer(palette = "Spectral")  


lista_ranking_origen
mapa_ranking_origen


### ¿Cuál fue la estación de destino más elegida para el primer viaje por los nuevos usuarios?

ranking_estaciones_destino <- nuevos_usuarios_total %>% 
  group_by(nombre_estacion_destino, long_estacion_destino, lat_estacion_destino) %>% 
  summarise(cantidad_viajes = n()) %>% 
  arrange(-cantidad_viajes) %>% 
  ungroup() %>% 
  slice(1:10)

barrios_ranking_destino <- cross_join(barrios, ranking_estaciones_destino)

lista_ranking_destino <- ggplot(ranking_estaciones_destino) +
  geom_col(mapping = aes(x = reorder(nombre_estacion_destino, cantidad_viajes), y = cantidad_viajes, fill=reorder(nombre_estacion_destino, -cantidad_viajes))) +
  geom_text(mapping = aes(x = reorder(nombre_estacion_destino, cantidad_viajes), y = cantidad_viajes, label = cantidad_viajes), 
            vjust = 0.33, 
            hjust=1.25, 
            size=3) +
  labs(x=NULL, y="viajes") +
  coord_flip() +
  theme_minimal() +
  guides(fill = FALSE) +
  scale_fill_brewer(palette="Spectral")

mapa_ranking_destino <- ggplot(barrios_ranking_destino) +
  geom_sf(fill="white") +
  geom_point(aes(x = long_estacion_destino, 
                 y = lat_estacion_destino, 
                 color = reorder(nombre_estacion_destino, -cantidad_viajes), 
                 size = cantidad_viajes),
             alpha=0.5) +
  scale_size_continuous(range = c(3, 15)) +
  labs(x=NULL, y=NULL, color="Estación") +
  theme_void() +
  guides(color = guide_legend(override.aes = list(size = 5, alpha=1)), size=FALSE) +
  scale_color_brewer(palette = "Spectral")

lista_ranking_destino
mapa_ranking_destino
