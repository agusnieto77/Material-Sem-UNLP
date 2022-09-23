# Cargamos la librería 
require(rvest)

# Definimos una url
urls <- 'https://www.lanacion.com.ar/politica/'

# Creamos una base de datos con el contenido de www.lanacion.com.ar/politica/
ln <- tibble::tibble(
  fechas    = read_html(urls) |> html_elements('.com-date')           |> html_text2(),
 #tags      = gsub('^<section.+',NA,gsub('^<section.+.com.link...tags...|^<section.+bullet\\">|</a>.+.section>$','',as.character(read_html(urls) |> html_elements('.row-gap-tablet-3.hlp-degrade .mod-description')))),
  tags      = str_extract_all(as.character(read_html(urls) |> html_elements('.row-gap-tablet-3.hlp-degrade .mod-description')), '(?<=com.link...tags\\s.>|com.link...tags\\s--bullet.>)(.*?)(?=./a.)', simplify = T) |> as.vector(),
  titulares = read_html(urls) |> html_elements('h2.com-title.--xs')   |> html_text2(),
  hipers    = read_html(urls) |> html_elements('h2.com-title.--xs a') |> html_attr('href') |> url_absolute(urls)
)

# Limpiar la consola
cat('\014')

# Imprimir
print(ln, n = 30)
