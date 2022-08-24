# Datos en línea para la investigación: conectar, bajar, clasificar, visualizar

## Ordenar y Clasificar (Parte I)

### Ordenar y reordenar datos con dplyr y tidyr

# **dplyr** es una gramática de manipulación de datos que proporciona un conjunto consistente de verbos que lo ayudan a resolver los desafíos más comunes de manipulación de datos:

#      -> mutate() agrega nuevas variables que son funciones de variables existentes

#      -> select() elige variables en función de sus nombres.

#      -> filter() elige casos en función de sus valores.

#      -> summarise() reduce varios valores a un solo resumen.

#      -> arrange() cambia el orden de las filas.

# El objetivo de tidyr es crear datos ordenados. Los datos ordenados son datos donde:

#      -> Cada columna es variable.

#      -> Cada fila es una observación.

#      -> Cada celda es un valor único.

### Trabajar con dplyr

##### Bajamos y cargamos los datos con funciones las funciones 'download.file' y 'read.table' del paquete 'utils', paquete base de r

# Los paquetes que vamos a usar en este fragmento de código no requieren ser cargados.

# Descarga del registro de femicidio de datos.jus.gob.ar

# creamos el objeto 'url' con la ubicación del archivo que queremos bajar

url <- "http://datos.jus.gob.ar/dataset/27bb9b2c-521b-406c-bdf9-98110ef73f34/resource/583cec9a-3022-4234-8b32-17692a267aac/download/registro-de-femicidios-20200109.csv"

url

# creamos el objeto 'file' para definir el lugar donde guardar el documento a bajar, con funciones base de r

file <- 
  base::file.path(
    ".", 
    base::basename(
      url))

file

# bajamos el documento que nos interesa

utils::download.file(
  url, 
  file)

# cargamos el documento en el objeto 'femicidios'

femicidios <- 
    read.table(file, 
               header = TRUE, 
               sep = ',', 
               stringsAsFactors = FALSE,
               encoding = "UTF-8")

femicidios |> head()

# Estructuras con dplyr

# Inspeccionamos la estructura de los datos con la función 'glimpse' de 'dplyr'

# Cargamos dplyr

require(dplyr)

# Vemos los datos a partir de 'glimpse' es como una versión transpuesta de print(): las columnas corren por la página y los datos se cruzan

glimpse(femicidios)

# Tabla de frecuencias con dplyr

# Con la función 'count' del paquete 'dplyr' creamos una tabla con la frecuencia de femicidios por provincia

# creamos una tabla con los datos de la columna 'hecho_provincia' con la función count de dplyr

tablafemicidios <- femicidios %>% count(hecho_provincia)

tablafemicidios

# Renombrar con dplyr

# Renombramos valores de la tabla con la función 'mutate' de 'dplyr'

tablafemicidios <- tablafemicidios |> mutate(
    hecho_provincia = case_when(
        hecho_provincia == "Ciudad Autónoma de Bs.As." ~ "CABA",
        hecho_provincia == "" ~ "s-d",
        TRUE ~ as.character(hecho_provincia)))

tablafemicidios

# Frecuencias con dplyr

# Ahora ordenamos la frecuencia con la funcion arrange del paquete dplyr

tablafemicidios <- tablafemicidios |> arrange(desc(n))

tablafemicidios

# Filtrar con dplyr

# Con la función 'filter' de 'dplyr' filtramos el valor 's-d' de la columna 'hecho_provincia' 

tablafemicidios <- tablafemicidios |> filter(hecho_provincia != 's-d')

tablafemicidios

# Top

# Usamos la función top_n del paquete dplyr para quedarnos con los primeros cinco registros de la tabla

tablafemicidios |> top_n(5)

# Renombramos

# Renombramos las variables con la función rename del paquete dplyr

tablafemicidios <- tablafemicidios |> rename(Provincias = hecho_provincia, Femicidios = n)

tablafemicidios

# Tablas con knitr

# Tuneamos la tabla con formattable, knitr y kableExtra


# Instalamos y cargamos formattable, knitr y kableExtra

# install.packages(c('formattable','kableExtra'))

require(formattable)
require(knitr)
require(kableExtra)

# Con los paquetes formattable, knitr y kableExtra creamos una tabla con css bootstrap y gráfico de barras incluido

head(tablafemicidios, 5) %>% 
  mutate(
    Provincias = Provincias,
    Femicidios = color_bar("lightgreen")(Femicidios)) %>%
  select(Provincias, everything()) %>%
  kable(escape = F,
    caption = "<center><span style='font-size:30px'>Femicidios en Argentina</span></center>") %>%
  kable_styling("hover", full_width = F, font_size = 16) %>%
  column_spec(2, width = "10cm")

# Ordenar y Clasificar (Parte 2)

# Trabajar con tidyr

# Bajamos y cargamos los datos con las funciones 'download.file' y 
# 'read.table' del paquete 'utils', paquete base de r

# Descarga datos población mundial

url <- "https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder_wide.csv"

file <- file.path(".", basename(url))

download.file(url, file)

poblacion <- read.table(file, 
                        header = TRUE, 
                        sep = ',', 
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8")

head(poblacion)

# Vemos su estructura de datos

utils::str(poblacion)

# Transformamos la base a formato largo

# Transformamos la base de datos a formato largo con las funciones 'gather' del paquete tidyr.

# Cargamos tidyr

require(tidyr)

poblacion_long <- poblacion %>% 
  gather(obs_anio,                 # nombramos la nueva columna para la nueva variable
         valor,                    # nombre de la nueva variable con los valores de las viejas variables 
         -continent,               # (-) identificamos qué variables queremos excluir del proceso 'gather'
         -country)                 # (-) identificamos qué variables queremos excluir del proceso 'gather'

poblacion_long <- poblacion %>% 
    pivot_longer(c(-continent, -country), 
                 names_to = "obs_anio", 
                 values_to = "valor")

head(poblacion_long)

# Vemos su estructura de datos

utils::str(poblacion_long)

# Separar para crear nuevas columnas

# Separamos la observacion del año con la función separate del paquete tidyr

# Separamos la observacion del año
gap_long <- 
    poblacion_long %>% 
    separate(obs_anio, 
             into = c("obs", "anio"), 
             sep = "_") %>% 
    mutate(anio = as.integer(anio))

head(gap_long)

# Vemos su estructura de datos

utils::str(gap_long)

# Separamos para darle la estrcutura deseada

# Usamos la función spread del paquete tidyr

gap_normal <- gap_long %>% spread(obs, valor)

gap_normal <- gap_long %>% pivot_wider(names_from = obs, values_from = valor)

head(gap_normal)

# Vemos su estructura de datos

utils::str(gap_normal)

# Algo de tablas para visualización y consultas... """

# install.packages(c('DT','gapminder'))
require(gapminder)
require(DT)

data("gapminder")
tabla02 <- datatable(gapminder)
html <- "tabla02.html"
saveWidget(tabla02, html)

# EPH: Encuesta Permanante de Hogares

# El paquete 'eph' en un conjunto de herramientas para descargar y manipular la Encuesta Permanente de Hogares de Argentina. Podemos descargar los conjuntos de datos, descargar los datos sobre pobreza, podemos calcular si un hogar es pobre o no, siguiendo la metodología oficial. Los métodos implementados se basan en INDEC (2016).

# Vamos a ver cómo trabajar con el paquete 'eph'

# install.packages('eph')

require(eph)

print('Base de datos EPH - Primer trimestre de 2020')

# Obtenemos la base de microdatos de individuos 
# para el primer trimestre de 2020:

(ind_01_20 <-           # nombre del objeto a crear
  get_microdata(        # función para cargar los datos de la eph
    year = 2020,        # año
    trimester = 1,      # trimestre
    type = 'individual' # tipo: 'individual' / 'hogar'
    )) |> head()

# Veamos que consultas podemos hacer"""

print('Diccionario aglomerados')

eph::diccionario_aglomerados |> head()

print('Filtramos para MdP')

(mdp_ind_01_20 <- 
  ind_01_20 %>% filter(AGLOMERADO == '34') %>%
  organize_labels(., type='individual')) |> head()

print('Armamos la tabla de condición de ocupación según sexo')

(tabla <- 
  mdp_ind_01_20 %>% 
  calculate_tabulates(x='ESTADO', 
                      y='CH04',
                      weights = 'PONDIH', 
                      add.totals='row', 
                      add.percentage='col')) |> head()

# Mejoremos el resultado obtenido"""

print("Tabla 3")

(Tabla_Ocu1 <- 
    calculate_tabulates(
      base=mdp_ind_01_20, 
      x = 'ESTADO', 
      y = 'CH04', 
      weights = 'PONDIH',
      add.totals = 'row', 
      add.percentage = 'col') %>% 
    data.frame() %>% 
    as_tibble() %>% 
    rename(Condicion = ESTADO.CH04) %>% 
    filter(!stringr::str_detect
           (Condicion,
             "Entrevista")) %>%
    rename(Varon_P = Varon) %>%
    rename(Mujer_P = Mujer) %>%
    mutate(Condicion = case_when(
      Condicion == "Ocupado" ~ "Ocupadx",
      Condicion == "Desocupado" ~ "Desocupadx",
      Condicion == "Inactivo" ~ "Inactivx",
      Condicion == "Menor de 10 anios." ~ "< 10 años",
      TRUE ~ as.character(Condicion)
    )))



print("Tabla 2")

(Tabla_Ocu2 <- 
    calculate_tabulates(
      base = mdp_ind_01_20,
      x = 'ESTADO', 
      y = 'CH04', 
      weights = 'PONDIH',
      add.totals = 'row') %>% 
    data.frame() %>% 
    as_tibble() %>% 
    rename(Condicion = ESTADO.CH04) %>% 
    rename(Varon_N = Varon) %>%
    rename(Mujer_N = Mujer) %>%
    filter(!stringr::str_detect(
      Condicion,
      "Entrevista")) %>% 
    mutate(Condicion = case_when(
      Condicion == "Ocupado" ~ "Ocupadx",
      Condicion == "Desocupado" ~ "Desocupadx",
      Condicion == "Inactivo" ~ "Inactivx",
      Condicion == "Menor de 10 anios." ~ "< 10 años",
      TRUE ~ as.character(Condicion)
    )))

print("Tabla Unificada")

(Tabla_unificada <- 
  full_join(
    Tabla_Ocu2, 
    Tabla_Ocu1, 
    by = "Condicion"))
