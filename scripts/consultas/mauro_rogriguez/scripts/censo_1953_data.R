require(tidyverse)
require(pdftools)

p17t <- pdf_ocr_data('scripts/consultas/mauro_rogriguez/pdfs/1c1954t3p17_tabla.pdf', language = "spa")[[1]] |> select(1)

p17t <- p17t |> mutate(dato = rep(c('cantidad','prop'),16),
  id = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15,16,16))

p17b_tabla <- p17t |> pivot_wider(names_from = dato, values_from = word)

p17b_tabla$cantidad <- str_remove(p17b_tabla$cantidad, '\\.')
p17b_tabla$cantidad <- as.integer(str_replace(p17b_tabla$cantidad, '%', '4'))
p17b_tabla$prop <- as.numeric(str_replace(p17b_tabla$prop, ',', '.'))

p17b_tabla <- p17b_tabla |> mutate(grupo_ind_manu = p17b_tibble$grupo_ind_manu[2:17], .after =id)

sum(p17b_tabla$cantidad)

sum(p17b_tabla$prop)