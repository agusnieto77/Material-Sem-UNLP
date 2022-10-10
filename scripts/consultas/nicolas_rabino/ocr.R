#install.packages("tesseract")
#install.packages("magick")

require(tidyverse)
#require(pdftools) # Esta librería es para PDFs
require(tesseract)
require(magick)

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
