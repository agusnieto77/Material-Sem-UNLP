#install.packages("tesseract")
#install.packages("magick")

require(tidyverse)
#require(pdftools) # Esta librería es para PDFs
require(tesseract)
require(magick)
require(tidytext)

#notas <- pdf_ocr_text('seminario Agustin/6-Junio/1997-06-05.jpg', 
                      #language = "spa") # Especificamos el idioma para el OCR

tesseract_download('spa', datapath = NULL, progress = interactive())

(titulo <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/titulos.png"), 
            engine = tesseract('spa')))

(titulob <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/titulosb.png"), 
               engine = tesseract('spa')))

(bajada <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/bajada.png"), 
                 engine = tesseract('spa')))

(bajadab <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/bajadab.png"), 
                  engine = tesseract('spa')))

(titulobajada <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/titulo y bajada.png"), 
              engine = tesseract('spa')))

(titulobajadab <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/titulo y bajadab.png"), 
              engine = tesseract('spa')))

(notabreve <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/nota_breve.png"), 
              engine = tesseract('spa')))

(notabreveb <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/nota_breveb.png"), 
               engine = tesseract('spa')))

tabla_comparada <- tibble(
  tipo_contenido = c('titulo', 'bajada', 'titulo y bajada', 'nota breve'),
  ocr_imagen_original = c(titulo, bajada, titulobajada, notabreve),
  ocr_imagen_procesada = c(titulob, bajadab, titulobajadab, notabreveb)
    )

tabla_comparada$ocr_imagen_original[1]
tabla_comparada$ocr_imagen_procesada[1]

str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_original[1], '\n', ' ')), '- |: ')
str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_procesada[1], '\n', ' ')), '- |: ')

str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_original[2], '\n', ' ')), '- |: ')
str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_procesada[2], '\n', ' ')), '- |: ')

str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_original[3], '\n', ' ')), '- |: ')
str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_procesada[3], '\n', ' ')), '- |: ')

str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_original[4], '\n', ' ')), '- |: ')
str_remove_all(str_trim(str_replace_all(tabla_comparada$ocr_imagen_procesada[4], '\n', ' ')), '- |: ')

tabla_comparada_mejorada <- tabla_comparada |> mutate(
  ocr_imagen_original = str_remove_all(str_trim(str_replace_all(ocr_imagen_original, '\n', ' ')), '- |: '),
  ocr_imagen_procesada = str_remove_all(str_trim(str_replace_all(ocr_imagen_procesada, '\n', ' ')), '- |: ')
)

tabla_comparada_mejorada$ocr_imagen_procesada

# Con imágenes de 2005

(DSC00326 <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/DSC00326.JPG"), 
               engine = tesseract('spa')))

(DSC00326A <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/DSC00326A.JPG"), 
                engine = tesseract('spa')))

(DSC00326B <- ocr(image_read("scripts/consultas/nicolas_rabino/fotos/DSC00326B.JPG"), 
               engine = tesseract('spa')))

tabla <- tibble(nota = c(DSC00326A, DSC00326B))

tabla <- tabla |> unnest_tokens(output = palabras, input = nota, token = 'words')

tabla |> count(palabras) |> filter(nchar(palabras) > 2) |> arrange(desc(n)) |> 
  filter(!palabras %in% c(tm::stopwords('es'), 'dela','dores','horas','mar')) |> 
  head(20) |> 
  ggplot() +
  geom_bar(aes(x = fct_reorder(palabras, n), y = n, fill = n), stat = 'identity') +
  coord_flip() +
  scale_fill_gradient2() +
  labs(x = 'Palabras', y = 'Frecuencia') +
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size = 14))

tablab <- tibble(nota = DSC00326)

tablab <- tablab |> unnest_tokens(output = palabras, input = nota, token = 'words')

tablab |> count(palabras) |> filter(nchar(palabras) > 2) |> arrange(desc(n)) |> 
  filter(!palabras %in% c(tm::stopwords('es'), 'dela','dores','horas','mar','aaa','ayer')) |> 
  head(20) |> 
  ggplot() +
  geom_bar(aes(x = fct_reorder(palabras, n), y = n, fill = n), stat = 'identity') +
  coord_flip() +
  scale_fill_gradient2() +
  labs(x = 'Palabras', y = 'Frecuencia') +
  theme_bw() +
  theme(legend.title = element_blank(),
        text = element_text(size = 14))
         