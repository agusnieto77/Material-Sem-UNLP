require(tidyverse)
require(pdftools)

# Leemos los datos numéricos de la tabla de la página 17 del censo industrial de 1954
# Especificamos el idioma y seleccionamos solo una columna, la de los datos recuperados.
p17t <- pdf_ocr_data('scripts/consultas/mauro_rogriguez/pdfs/1c1954t3p17_tabla.pdf', 
                     language = "spa")[[1]] |> select(1) 

# Imprimimos el objeto p17t
print(p17t, n = 32)

# creamos un vector vacío
id <- c()
# Lo llenamos con 32 números que van de 1 al 16 y se repinten
# una vez en orden creciente y consecutivo 
for(i in 1:16){id <- append(id, rep(i,2))}

# Imprimimos el objeto id
id

# Creamos dos nuevas variables en la tabla
p17t <- p17t |> mutate(
  dato = rep(c('cantidad','prop'),16),
  id = id)

# Imprimimos el objeto p17t
print(p17t, n = 32)

# Pasamos el formato de la tabla de largo a ancho
p17b_tabla <- p17t |> pivot_wider(
  # los valores de la variable 'dato' pasan a ser los nombres de las nuevas variables
  names_from = dato, 
  # Los valores de la variable 'word' se desdoblan como valores de las nuevas variables
  values_from = word)

# Imprimimos el objeto p17b_tabla
print(p17b_tabla, n = 16)

# Removemos los puntos (.)
p17b_tabla$cantidad <- str_remove(p17b_tabla$cantidad, '\\.')

# Reemplazamos el símbolo '%' por el número '4'
# y convertimos la variable a formato numérico
p17b_tabla$cantidad <- as.integer(str_replace(p17b_tabla$cantidad, '%', '4'))

# Cambiamos las comas (,) por puntos (.) como separador decimal 
# y convertimos la variable a formato numérico
p17b_tabla$prop <- as.numeric(str_replace(p17b_tabla$prop, ',', '.'))

# Cargamos el vector de nombres de los grupos de industria
grupo_ind_manu <- readRDS('scripts/consultas/mauro_rogriguez/outputs/nombres_grupos_ind_manu.rds')

# Imprimimos el objeto grupo_ind_manu
grupo_ind_manu

# Creamos una nueva variable con el nombre de las industrias
p17b_tabla <- p17b_tabla |> mutate(grupo_ind_manu = grupo_ind_manu, .after =id)

# Imprimimos el objeto p17b_tabla
print(p17b_tabla, n = 16)

# Constatamos que la suma de los totales correctos en ambas columnas
sum(p17b_tabla$cantidad)
sum(p17b_tabla$prop)

# Guardamos la tabla
saveRDS(p17b_tabla, 'scripts/consultas/mauro_rogriguez/outputs/tabla_p_17.rds')

# Imprimimos la nueva tabla en consola
readRDS('scripts/consultas/mauro_rogriguez/outputs/tabla_p_17.rds')
