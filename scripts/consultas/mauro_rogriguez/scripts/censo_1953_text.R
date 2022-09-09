require(tidyverse)
require(pdftools)

p17b <- pdf_ocr_text('scripts/consultas/mauro_rogriguez/pdfs/1c1954t3p17.pdf', language = "spa")

p17b_tibble <- tibble(x = unlist(str_split(p17b, '[\\r\\n]+')))
p17b_tibble <- p17b_tibble[26:42, ]
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\.')

p17b_tibble <- p17b_tibble |> mutate(prop = str_extract(x, '\\w*[,\\d].$|\\w*[,\\d]\\d.$|\\w*[0-9]$'))

p17b_tibble$x <- str_remove(p17b_tibble$x, '\\w*[,\\d].$|\\w*[,\\d]\\d.$|\\w*[0-9]$')
p17b_tibble$x <- str_remove(p17b_tibble$x, '\\s$')
p17b_tibble$x <- str_remove(p17b_tibble$x, ',')

p17b_tibble <- p17b_tibble |> mutate(cantidad = str_extract(x, '\\w*[0-9]$|\\w*[0-9].$'), .before = prop)

p17b_tibble$x <- str_remove_all(p17b_tibble$x, '[0-9]|[:punct:]|\\+|>|<|\\$|£')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][a-zA-Z][a-zA-Z][^del]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][a-zA-Z][a-zA-Z][a-zA-Z]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][^y|la]\\s|\\s[a-zA-Z][a-zA-Z][^la]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z]$|\\s[a-zA-Z][a-zA-Z]$|éé')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z]\\s$|\\s[a-zA-Z]$')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][^y|la]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z]\\s$|\\s[a-zA-Z]$')

p17b_tibble <- p17b_tibble |> 
  mutate(grupo_ind_manu = case_when(
    str_detect(x, 'T  T ') ~ 'Total',
    str_detect(x, 'TUXTLLOS') ~ 'Textiles',
    str_detect(x, 'MAdOora') ~ 'Madera',
    str_detect(x, 'GUSTO ') ~ 'Cuero',
    str_detect(x, 'TRTTOD') ~ 'Varios',
    str_detect(x, 'PADEL') ~ 'Papel y cartón',
    str_detect(x, 'aESP') ~ 'Tabaco',
    TRUE ~ as.character(x)), .after = x) |> select(-x)

p17b_tibble$grupo_ind_manu <- str_trim(p17b_tibble$grupo_ind_manu)

p17b_tibble <- p17b_tibble |> 
  mutate(prop = case_when(
    str_detect(grupo_ind_manu, 'Vehículos y maquinaria') ~ '14,4',
    str_detect(grupo_ind_manu, 'Maquinaria y aparatos eléctricos') ~ '3,4',
    str_detect(grupo_ind_manu, 'Imprenta y publicaciones') ~ '2,5',
    str_detect(grupo_ind_manu, 'Caucho') ~ '1,4',
    TRUE ~ as.character(prop)))

p17b_tibble$prop <- as.numeric(str_replace(p17b_tibble$prop, ',', '.'))

p17b_tibble <- p17b_tibble |> 
  mutate(cantidad = case_when(
    str_detect(grupo_ind_manu, 'Productos químicos') ~ '44360',
    TRUE ~ as.character(cantidad)))

p17b_tibble$cantidad <- as.integer(str_replace(p17b_tibble$cantidad, '%', '4'))

sum(p17b_tibble$cantidad)-p17b_tibble$cantidad[1]

sum(p17b_tibble$prop)-p17b_tibble$prop[1]
