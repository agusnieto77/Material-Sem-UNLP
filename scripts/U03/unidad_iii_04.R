
# Parte I - Paquete -------------------------------------------------------

require(rvest)

# Parte II - La Nación ----------------------------------------------------

# Definimos una url

url <- 'https://www.lanacion.com.ar/politica/'

# Creamos un set de objetos vacíos

# Links
links <- c()

# Variables
fechas    <- c()
titulares <- c()
notas     <- c()
hipers    <- c()

# DataFrame
la_nacion <- data.frame()

# Corremos el for para extraer los links de la url
links <- append(links, read_html(url) |> html_elements('.com-title.--xs a') |> html_attr('href')) |> url_absolute(url)

# Corremos el for para extraer el contenido de los primeros 9 links escrapeados con el for anterior
for(link in links[1:9]){
  fechas    <- append(fechas,    read_html(link) |> html_elements('.com-date.--twoxs') |> html_text2())
  titulares <- append(titulares, read_html(link) |> html_elements('.com-title.--threexl') |> html_text2())
  notas     <- append(notas,     read_html(link) |> html_elements('.com-paragraph') |> html_text2() |> list())
  hipers    <- append(hipers, link)
}

# Corremos el for para extraer el contenido de los primeros 9 links escrapeados con  el for anterior y crear un data.frame
for(i in 1:length(links[1:9])) {la_nacion <- rbind(la_nacion, data.frame(fecha=fechas[i], titular=titulares[i], nota=paste0(notas[[i]], collapse = ' | '), hiper=hipers[i]))}

# Imprimir
la_nacion
