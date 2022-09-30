library(tidyverse)
library(rvest)

url <- 'https://www.diarionorte.com/notas/politica/'

html <- read_html(url)

links <- html |> html_elements(css='a.floatFix') |> html_attr('href') |> url_absolute(url)

fechas <- c()
titulos <- c()
bajada <- c()
nota <- c()
urls <- c()

for (i in links) {
  html <- read_html(i)
  fechas <- append(fechas, html_element(html, 'time') |> html_text2())
  titulos <- append(titulos, html_element(html, 'h1') |> html_text2())
  bajada <- append(bajada, html_element(html, 'summary') |> html_text2())
  nota <- append(nota, html_element(html, '.main-text') |> html_text2())
  urls <- append(urls, i)
  
}
