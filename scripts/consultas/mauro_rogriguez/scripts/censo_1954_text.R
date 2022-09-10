require(tidyverse)
require(pdftools) # Esta librería usa para OCR el paquete tesseract

p17b <- pdf_ocr_text('scripts/consultas/mauro_rogriguez/pdfs/1c1954t3p17.pdf', 
                     language = "spa") # Especificamos el idioma para el OCR

# Imprimimos el objeto p17b
p17b # Es una cadena de caracteres

# Creamos una tibble con una observación por salto de línea [\\r\\n]+
p17b_tibble <- tibble(x = unlist(str_split(p17b, '[\\r\\n]+')))

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 43) # Es una tibble

# Nos quedamos con las líneas de la tabla que nos interesa
p17b_tibble <- p17b_tibble[26:42, ]

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Removemos los puntos (.)
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\.')

# Extraemos la información de la tercera columna de la tabla (el porcentaje)
p17b_tibble <- p17b_tibble |> mutate(prop = str_extract(x, '\\w*[,\\d].$|\\w*[,\\d]\\d.$|\\w*[0-9]$'))

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Removemos la información referente a la tercera columna en la variable x
p17b_tibble$x <- str_remove(p17b_tibble$x, '\\w*[,\\d].$|\\w*[,\\d]\\d.$|\\w*[0-9]$')
p17b_tibble$x <- str_remove(p17b_tibble$x, '\\s$')
p17b_tibble$x <- str_remove(p17b_tibble$x, ',')

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Extraemos la información de la segunda columna de la tabla (la cantidad)
p17b_tibble <- p17b_tibble |> mutate(cantidad = str_extract(x, '\\w*[0-9]$|\\w*[0-9].$'), .before = prop)

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Removemos la información que no refiera a la primera columna de la tabla en la variable x
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '[0-9]|[:punct:]|\\+|>|<|\\$|£')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][a-zA-Z][a-zA-Z][^del]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][a-zA-Z][a-zA-Z][a-zA-Z]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][^y|la]\\s|\\s[a-zA-Z][a-zA-Z][^la]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z]$|\\s[a-zA-Z][a-zA-Z]$|éé')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z]\\s$|\\s[a-zA-Z]$')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z][^y|la]\\s')
p17b_tibble$x <- str_remove_all(p17b_tibble$x, '\\s[a-zA-Z]\\s$|\\s[a-zA-Z]$')

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Creamos una nueva variable con el contenido de la variable x 
# para reconstruir los grupos de industria manufacturera de la tabla
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

# Removemos los espacios en blanco sobrantes 
p17b_tibble$grupo_ind_manu <- str_trim(p17b_tibble$grupo_ind_manu)

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Corregimos los errores restantes en la columna de porcentajes 
p17b_tibble <- p17b_tibble |> 
  mutate(prop = case_when(
    str_detect(grupo_ind_manu, 'Vehículos y maquinaria') ~ '14,4',
    str_detect(grupo_ind_manu, 'Maquinaria y aparatos eléctricos') ~ '3,4',
    str_detect(grupo_ind_manu, 'Imprenta y publicaciones') ~ '2,5',
    str_detect(grupo_ind_manu, 'Caucho') ~ '1,4',
    TRUE ~ as.character(prop)))

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Cambiamos las comas (,) por puntos (.) como separador decimal 
# y la pasamos a formato numérico
p17b_tibble$prop <- as.numeric(str_replace(p17b_tibble$prop, ',', '.'))

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Corregimos los errores restantes en la columna de cantidades 
p17b_tibble <- p17b_tibble |> 
  mutate(cantidad = case_when(
    str_detect(grupo_ind_manu, 'Productos químicos') ~ '44360',
    TRUE ~ as.character(cantidad)))

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Corregimos los errores restantes en la columna de cantidades 
# y la pasamos a formato numérico
p17b_tibble$cantidad <- as.integer(str_replace(p17b_tibble$cantidad, '%', '4'))

# Imprimimos el objeto p17b_tibble
print(p17b_tibble, n = 17)

# Constatamos que la suma de los totales correctos en ambas columnas
sum(p17b_tibble$cantidad)-p17b_tibble$cantidad[1]
sum(p17b_tibble$prop)-p17b_tibble$prop[1]

# Creamos y guardamos el vector de nombres de los grupos de industrias manufactureras
nombres_grupos_ind_manu <- p17b_tibble$grupo_ind_manu[2:17]

saveRDS(nombres_grupos_ind_manu, 
        'scripts/consultas/mauro_rogriguez/outputs/nombres_grupos_ind_manu.rds')
