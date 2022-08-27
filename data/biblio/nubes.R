require(tidyverse)
require(tidytext)
require(forcats)
require(pdftools)
require(ggwordcloud)

textos <- data.frame(texto = c(paste(pdf_text('./data/biblio/U01/gualda.pdf'), collapse = ' '),
                                paste(pdf_text('./data/biblio/U01/milligan.pdf'), collapse = ' '),
                                paste(pdf_text('./data/biblio/U01/pons.pdf'), collapse = ' '),
                                paste(pdf_text('./data/biblio/U01/putnam.pdf'), collapse = ' ')),
                     autore = c('galda','milligan','pons','putnam'))

textos$texto <- str_remove_all(textos$texto, '[:punct:]')
textos$texto <- str_remove_all(textos$texto, '[:digit:]')
textos$texto <- str_remove_all(textos$texto, 'http.*\\s')

textos_tk <- textos |> unnest_tokens(palabra, texto) |> count(autore,palabra)
textos_tk <- textos_tk |> anti_join(tibble(palabra = tm::stopwords('es')))
textos_tk <- textos_tk |> anti_join(tibble(palabra = tm::stopwords('en')))
textos_tk <- textos_tk |> anti_join(tibble(palabra = c('solo','allí','cualquier','dos','ello',
                                                       'sino','pues','decir','siempre','año',
                                                       'et','ción','p','etc','tos','mos','histmem',
                                                       'pp','nº','si')))
textos_tk <- textos_tk |> filter(n > 9)
textos_tk <- textos_tk |> group_by(autore) |> mutate(prop = n/sum(n))

set.seed(4444)
ggplot(textos_tk, aes(label = palabra, size = prop, color = autore)) +
  geom_text_wordcloud() +
  facet_wrap(~autore) +
  scale_size_area(max_size = 14) +
  theme_minimal() +
  theme(strip.text = element_blank())

# IDF

palabras_totales <- textos_tk |>
  group_by(autore) |>
  summarize(total = sum(n))

textos_tk_idf <- left_join(textos_tk, palabras_totales)

textos_tk_idf <- textos_tk_idf  |> bind_tf_idf(palabra, autore, n)

textos_tk_idf |> select(-total) |> arrange(desc(tf_idf))

textos_tk_idf |>
  group_by(autore) |>
  slice_max(tf_idf, n = 12) |>
  ungroup() |>
  ggplot(aes(tf_idf, fct_reorder(palabra, tf_idf), fill = autore)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~autore, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_bw() +
  theme(strip.text = element_blank())

