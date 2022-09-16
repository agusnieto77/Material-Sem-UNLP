
# Parte I - Cargamos los paquetes -----------------------------------------

# Gestión de datos de uso general
library(tidyverse)
# Análisis de archivos HTML/XML
library(rvest)
# Facilita la manipulación de vectores date-time
library(lubridate) 
# Formato JSON 
library(jsonlite)


# Parte II - Documentos de texto, imágenes, etc. --------------------------

# Creamos el objeto 'url' en base a un link del CEDINCI
url <- 'https://americalee.cedinci.org/portfolio-items/la-ciudad-futura/'

# Selección de los nodos y extraemos los links
links <- read_html(url) |> html_elements('.fusion-no-lightbox') |> html_attr('href')

# Nombramos un nuevo directorio
nuevodir <- 'scripts/U03/pdfs/'

# Creamos un nuevo directorio
dir.create(nuevodir)

# Ejecutamos un for para extraer todos los pdfs
for (i in seq_along(links)) {
  message(paste0('Bajando ', i, '/', length(links), ' documentos...'))
  download.file(url = links[i], destfile = paste0(nuevodir, basename(links[i])), mode="wb")
  cat('\014')
}

# Parte III - Datos no estructurados --------------------------------------

# Creamos el objeto 'url' en base a un link de La Nación
url <- 'https://www.lanacion.com.ar/politica/'

# Cargar y analizar el DOM (Modelo de Objetos del Documento) del archivo HTML 
html <- read_html(url)

# Imprimir 
html

# Selección de nodos de fecha 
fechas <- html |> html_elements(xpath='//*[contains(@class, "com-date")]') |> html_text2()

# Imprimir 
fechas

# Transformamos el vector fechas en un vector de clase data-time
fechas <- as_date(fechas, format = '%d de %B de %Y')

# Imprimir 
fechas

# Seleccionar los nodos de los titulares
titulares <- html |> html_elements(xpath='//*[contains(@class, "mod-description")]/h2') |> html_text2()

# Imprimir 
titulares

# Selección de los nodos de los links
links <- html |> html_elements(xpath='//*[contains(@class, "mod-description")]/h2/a') |> html_attr("href") |> url_absolute(url)

# Imprimir 
links

# Crear una estructura de datos compuesta (lista)

# Creamos un objeto vacío
datos <- list()

# Corremos el for
for (i in 1:length(titulares)) {
  datos <- append(datos, list(list(fecha=fechas[i], titular=titulares[i], link=links[i])))
}

# Imprimir 
datos

# Formatear los resultados de la minería en formato JSON
json <- toJSON(datos)

# Imprimir 
json

# Formatear los resultados de la minería en formato tibble
df <- unnest(fromJSON(json), cols = c(fecha, titular, link))

# Imprimir 
df

# Borramos todos los objetos creados
rm(list = ls())

# Limpiamos la consola
cat("\014")

# Parte II - Datos estructurados ------------------------------------------

# Creamos el objeto 'url' en base a un link de Wikipedia
url <- 'https://es.wikipedia.org/wiki/Demograf%C3%ADa_de_Argentina'

# Cargar y analizar el DOM (Modelo de Objetos del Documento) del archivo HTML 
html <- read_html(url)

# Imprimir 
html

# Selección de tablas
tablas <- html |> html_nodes(".wikitable") |> html_table()

# Imprimir todas las tablas
tablas

# Imprimir solo la primera
print(tablas[[1]], n = 24)

# Imprimir la 3ª y la 1ª
tablas[c(3,1)]

# Imprimir solo la 2ª
print(tablas[[2]], n = 37)

# Borramos todos los objetos creados
rm(list = ls())

# Limpiamos la consola
cat("\014")
