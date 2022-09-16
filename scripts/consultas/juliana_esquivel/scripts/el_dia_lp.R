
# Parte I - Paquete -------------------------------------------------------

require(rvest)

# Parte II - La Nación ----------------------------------------------------

# Definimos una url

url <- 'https://www.eldia.com/seccion/la-ciudad/'

# Creamos un set de objetos vacíos

# Links
links <- c()

# Variables
fechas    <- c()
titulares <- c()
notas     <- c()
hipers    <- c()

# DataFrame
el_dia <- data.frame()

# Corremos el for para extraer los links de la url
links <- append(links, read_html(url) |> html_elements('.nota_titulo_arriba a') |> html_attr('href')) |> url_absolute(url)

# Corremos el for para extraer el contenido de los primeros 9 links escrapeados con el for anterior
for(link in links){
  fechas    <- append(fechas,    read_html(link) |> html_element('.fecha') |> html_text2())
  titulares <- append(titulares, read_html(link) |> html_element('.titulo_nota') |> html_text2())
  notas     <- append(notas,     read_html(link) |> html_element('.body.text-resizable.cuerpo_nota') |> html_text2())
  hipers    <- append(hipers, link)
}

# Corremos el for para extraer el contenido de los primeros 9 links escrapeados con  el for anterior y crear un data.frame
for(i in 1:length(links)) {el_dia <- rbind(el_dia, data.frame(fecha=fechas[i], titular=titulares[i], nota=notas[i], hiper=hipers[i]))}

# Imprimir
el_dia |> tibble()
