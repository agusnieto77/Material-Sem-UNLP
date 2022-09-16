
# Parte I - Paquetes ------------------------------------------------------

require(polite)
require(rvest)

# Parte II - Mundo Gremial ------------------------------------------------

# Definimos una url
url <- 'https://mundogremial.com/seccion/'

# Creamos el listado de páginas que queremos escrapear
urls <- c(paste0(url,'informacion-general/page/',1:1667,'/'),
          paste0(url,'paritarias/page/',1:123,'/'))

# Imprimir los últimos 200 links
urls[1591:1790]

# Creamos un set de objetos vacíos

# Links
links <- c()

# Variables
fechas    <- c()
titulares <- c()
notas     <- c()
hipers    <- c()

# DataFrame
mundo_gremial <- data.frame()

# Corremos el for para extraer los links de las tres primeras urls
for(url in urls[1:3]){
  links <- append(links, scrape(bow(url)) |> 
                    html_elements('.mvp-blog-story-wrap a, .mvp-widget-feat2-left a,
                                  .mvp-widget-feat2-right a') |> 
                    html_attr('href'))
}

# Imprimir 
links

# Corremos el for para extraer el contenido de los primeros 9 links escrapeados con  el for anterior
for(link in links[1:9]){
  fechas    <- append(fechas,    scrape(bow(link)) |> html_elements('.post-date') |>                      
                        html_text2())
  titulares <- append(titulares, scrape(bow(link)) |> html_elements('.mvp-post-title') |>                 
                        html_text2())
  notas     <- append(notas,     scrape(bow(link)) |> html_elements('.theiaPostSlider_preloadedSlide') |> 
                        html_text2())
  hipers    <- append(hipers, link)
}

# Imprimir 
fechas

# Imprimir 
titulares

# Imprimir 
notas

# Imprimir 
hipers

# Corremos el for para extraer el contenido de los primeros 9 links 
# escrapeados con  el for anterior y crear un data.frame
for (i in 1:length(links[1:9])) {
  mundo_gremial <- rbind(mundo_gremial, data.frame(
    fecha   = fechas[i], 
    titular = titulares[i], 
    nota    = notas[i], 
    hiper   = hipers[i])
    )
}

# Imprimir
mundo_gremial |> tibble()
