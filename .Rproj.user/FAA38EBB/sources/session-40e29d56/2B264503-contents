# Instalo R Markdown
install.packages('rmarkdown')

# Instalo tidyverse y abro readr para abrir los archivos
install.packages('tidyverse')
library(tidyverse)
library(readr)
library(stringr)

# Instalo y cargo dplyr
install.packages('dplyr')
library(dplyr)

# Cargo ggplot2
library(ggplot2)

# Instalo y cargo patchwork para crear composiciones con las visualizaciones
install.packages('patchwork')
library(patchwork)

# Instalo y cargo sf para el tratamiento de datos espaciales
install.packages("sf")
library(sf)

# Importo los datasets y los almaceno en variables

# trips_2023.csv debe ser descargado desde https://data.buenosaires.gob.ar/dataset/bicicletas-publicas/resource/ff671909-6860-4398-8d0a-8f2389cb2780
# usuarios_ecobici_2023.csv debe ser descargado desde https://data.buenosaires.gob.ar/dataset/bicicletas-publicas/resource/edb5ed27-8506-43f0-88ce-3e5c0aa1a406

raw_trips_2023 <- read.csv('datasets/trips_2023.csv', fileEncoding = "UTF-8")
raw_users_2023 <- read.csv('datasets/usuarios_ecobici_2023.csv', fileEncoding = "UTF-8")

# Previsualizo y genero un resumen de los datasets
glimpse(raw_trips_2023)
glimpse(raw_users_2023)
