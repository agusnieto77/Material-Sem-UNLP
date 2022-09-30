library(tidyverse)
library(rvest)
library(lubridate) 
library(jsonlite)

url2 <- 'https://www.losandes.com.ar/politica/545/'

# Cargar y analizar el DOM (Modelo de Objetos del Documento) del archivo HTML 


#losandes
html2 <- read_html(url2)

# Imprimir parte simplificada

#losandes
html2

# Selección de nodos 

titulo <-html2 |> html_elements(xpath='//*[contains (@class, "article-title")]') |> html_text2()

sección <-html2 |> html_elements(xpath='//*[contains (@class, "article-section")]') |> html_text2()

fechas <- html2 |> html_elements(xpath='//*[contains (@class, "uppercase")]') |> html_text2()

links <- html2 |> html_elements(xpath='//*[contains (@class, "article-title")]//a') |> html_attr('href') |> url_absolute(url2)

notas <- c()

for (i in links) {
  html3 <- read_html(i)
  notas <- append(notas, paste0(html_elements(html3, xpath='//*[contains (@class, "clearfix story mxn0 flex flex-wrap mb1")]//p') |> html_text2(), collapse = '\\n'))
}

basedatos <- tibble()

for (i in 1:length (titulo)) {basedatos <- rbind(basedatos, tibble(fechas= fechas[i], sección= sección[i], titulo=titulo[i], notas= notas[i], links= links[i]))}

# Imprimir 
basedatos
