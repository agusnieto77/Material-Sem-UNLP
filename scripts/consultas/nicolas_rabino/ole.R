
# Parte I - Paquete -------------------------------------------------------

require(rvest)

# Parte II - La Nación ----------------------------------------------------

# Definimos una url

url <- 'https://www.ole.com.ar/seleccion'

# Creamos un set de objetos vacíos

# Links
links <- c()

# Variables
fechas    <- c()
titulares <- c()
notas     <- c()
hipers    <- c()

# DataFrame
ole <- data.frame()

# Corremos el for para extraer los links de la url
# links <- append(links, read_html(url) |> html_elements('.entry-title') |> html_attr('.a.href')) |> url_absolute(url)
links <- read_html(url) |> html_elements('.entry-title a') |> html_attr('href') |> url_absolute(url)
links <- links[!links %in% c("https://www.ole.com.ar/seleccion#")]

# Corremos el for para extraer el contenido de los primeros 9 links escrapeados con el for anterior
for(link in links){
  fechas    <- append(fechas,    read_html(link) |> html_elements('.publishedDate') |> html_text2())
 #titulares <- append(titulares, read_html(link) |> html_elements('.title') |> html_text2())
  titulares <- append(titulares, read_html(link) |> html_elements('#title') |> html_text2())
 #notas     <- append(notas,     read_html(link) |> html_elements('.body-nota') |> html_text2() |> list())
  notas     <- append(notas,     read_html(link) |> html_elements('p') |> html_text2() |> list())
  hipers    <- append(hipers, link)
}

# Corremos el for para extraer el contenido de los primeros 9 links escrapeados con  el for anterior y crear un data.frame
for(i in 1:length(links)) {ole <- rbind(ole, data.frame(fecha=fechas[i], titular=titulares[i], nota=paste0(notas[[i]], collapse = ' | '), hiper=hipers[i]))}

# Imprimir
ole |> tibble::tibble()
