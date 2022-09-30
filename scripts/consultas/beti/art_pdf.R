require(rvest)
require(stringr)

url_relmecs <- 'https://www.relmecs.fahce.unlp.edu.ar/issue/archive'

links_rev <- read_html(url_relmecs) |> html_elements('a.title') |> html_attr('href')

links_art <- c()

for (i in links_rev) {
  links_art <- append(links_art, read_html(i) |> 
                        html_elements('a.galley-link.btn.btn-primary.pdf') |> html_attr('href'))
}

links_art <- links_art |> str_replace_all('view', 'download')

dir.create('./scripts/consultas/beti/art_relmecs')

for (i in links_art) {
  download.file(url = i, 
                destfile = paste0('./scripts/consultas/beti/art_relmecs/',
                str_remove_all(str_extract(i,'(?<=download)(.*?)(?=$)'), '/'),'.pdf'))
}


