
# Una introducción al trabajo con diccionarios ----------------------------

# PAQUETES ----------------------------------------------------------------

# Paquetes a instalar
install.packages('ACEP')

# Paquetes a cargar
require(ACEP)
require(dplyr)
require(ggplot2)
require(stringr)
require(lubridate)

# Limpiamos la consola
cat("\014")
#

# BASES -------------------------------------------------------------------

# Cargamos una base de datos de notas de La Capital de MDP
# Puede tardar unos minutos porque es un archivo de 28MB
# Aumentamos el valor de 'timeout' que por defecto es de 60 segundos
# 600 es un poco exagerado
# Mi abuela siempre decía: 'Mejor que sobre...'
options(timeout = 600) 

# Creamos el objeto 'la_capital'
la_capital <- acep_load_base(acep_bases$lc_mdp) # F1 en acep_load_base() para ver de qué se trata

# Limpiamos la consola
cat("\014")

# Lo inspeccionamos con la función str() 
str(la_capital) # No muy visual ¿verdad?

# Limpiamos la consola
cat("\014")

# Lo inspeccionamos con la función glimpse() del paquete tibble
glimpse(la_capital) # ¡¡¡Mucho mejor!!!

# Limpiamos la consola
cat("\014")
#

# SUB_BASES_PIQUETES ------------------------------------------------------

# Títulos -----------------------------------------------------------------

# Vamos a crear una sub-base con las variables 'fecha' y 'titulo'
titulos_la_capital <- la_capital[ , c('fecha','titulo')]

# Inspeccionamos la base
glimpse(titulos_la_capital)

# Borramos
rm(titulos_la_capital)

# También se puede crear así
titulos_la_capital <- subset(la_capital, select = c('fecha','titulo'))

# Limpiamos la consola
cat("\014")

# Inspeccionamos la base
glimpse(titulos_la_capital)

# Con esta sub-base vamos a generar una nueva columna 
# con la frecuencia de menciones de un diccionario
titulos_la_capital$piquetes <- acep_men(   # F1 para ver de qué se trata
  x       = titulos_la_capital$titulo,     # Vector de texto
  y       = c('piquete', 'corte de ruta'), # Vector de palabras
  tolower = TRUE)                          # Pasar todo a minúsculas

# Limpiamos la consola
cat("\014")

# Inspeccionamos la base
glimpse(titulos_la_capital)

# Vemos cuantos piquetes se contabilizaron
sum(titulos_la_capital$piquetes, na.rm = TRUE) # na.rm = TRUE remueve los valores pedidos

# Filtramos los título que contabilizaron al menos una mención
piquetes_titulos <- subset(
  titulos_la_capital, # Base de datos
  piquetes > 0        # Variable (piquetes) que conserva los valores (números) mayores a 0 (> 0)
  )       

# Limpiamos la consola
cat("\014")

# Imprimimos los 19 títulos
print(piquetes_titulos,             # Base de datos
      n = nrow(piquetes_titulos))   # Número de filas

# Agrupamos por año y contamos piquetes
tabla_piquetes_anio_titulos <- piquetes_titulos |> # Base de datos
  mutate(año = year(fecha)) |>                     # Nueva columna numérica con el año
  count(año, name = 'frecuencia')                  # Frecuencia de los valores de la columna año

# Limpiamos la consola
cat("\014")

# imprimimos la base
tabla_piquetes_anio_titulos

# Visualizamos
barplot(frecuencia ~ año,                          # Variables
        data = tabla_piquetes_anio_titulos,        # Base de datos
        main = "Piquetes [dicc ad hoc - titulos]", # Título
        col  = rgb(0.8,0.1,0.1,0.6)                # Color
        )

# Limpiamos la consola
cat("\014")
# 

# Bajadas -----------------------------------------------------------------

# Vamos a crear una sub-base con las variables 'fecha' y 'bajada'
bajadas_la_capital <- la_capital[ , c('fecha','bajada')]

# Con esta sub-base vamos a generar una nueva columna 
# con la frecuencia de menciones de un diccionario
bajadas_la_capital$piquetes <- acep_men(x = bajadas_la_capital$bajada, 
                                        y = c('piquete', 'corte de ruta'),
                                        tolower = TRUE)

# Inspeccionamos la base
glimpse(bajadas_la_capital)

# Vemos cuantos piquetes se contabilizaron
sum(bajadas_la_capital$piquetes, na.rm = TRUE)

# Filtramos los título que contabilizaron al menos una mención
piquetes_bajadas <- subset(bajadas_la_capital, piquetes > 0)

# Limpiamos la consola
cat("\014")

# Imprimimos las 21 filas
print(piquetes_bajadas, n = nrow(piquetes_bajadas))

# Limpiamos la consola
cat("\014")

# Agrupamos por año y contamos piquetes
tabla_piquetes_anio_bajadas <- piquetes_bajadas |>
  mutate(año = year(fecha)) |> 
  count(año, name = 'frecuencia')

# Visualizamos
barplot(frecuencia ~ año, 
        data = tabla_piquetes_anio_bajadas,
        main="Piquetes [dicc ad hoc - bajadas]",
        col=rgb(0.8,0.1,0.1,0.6))

# Limpiamos la consola
cat("\014")
#

# Títulos y Bajadas -------------------------------------------------------

# Vamos a crear una sub-base con las variables 'fecha', 'titulo' y 'bajada'
t_b_la_capital <- la_capital[ , c('fecha', 'titulo', 'bajada')]

# Creamos una nueva variables pegando titulo y bajada en una misma celda
t_b_la_capital$t_b <- paste(
  t_b_la_capital$titulo, # Variable 'titulo'
  t_b_la_capital$bajada, # Variable 'bajada'
  sep = ' || ')          # Especifica el separador entre los elementos pegados

# Inspeccionamos la base
glimpse(t_b_la_capital)

# Limpiamos la consola
cat("\014")

# Nos quedamos solo con las variables 'fecha' y 't_b'
t_b_la_capital <- t_b_la_capital[ , c('fecha', 't_b')]

# Inspeccionamos la base
glimpse(t_b_la_capital)

# Con esta sub-base vamos a generar una nueva columna 
# con la frecuencia de menciones de un diccionario
t_b_la_capital$piquetes <- acep_men(x = t_b_la_capital$t_b, 
                                        y = c('piquete', 'corte de ruta'),
                                        tolower = TRUE)

# Inspeccionamos la base
glimpse(t_b_la_capital)

# Vemos cuantos piquetes se contabilizaron
sum(t_b_la_capital$piquetes, na.rm = TRUE)

# Filtramos los título que contabilizaron al menos una mención
piquetes_t_b <- subset(t_b_la_capital, piquetes > 0)

# Limpiamos la consola
cat("\014")

# Imprimimos las 40 filas
print(piquetes_t_b, n = nrow(piquetes_t_b))

# Limpiamos la consola
cat("\014")

# Agrupamos por año y contamos piquetes
tabla_piquetes_anio_t_b <- piquetes_t_b |>
  mutate(año = year(fecha)) |> 
  count(año, name = 'frecuencia')

# Imprimimos
tabla_piquetes_anio_t_b

# Visualizamos
barplot(frecuencia ~ año, 
        data = tabla_piquetes_anio_t_b,
        main="Piquetes [dicc ad hoc - titulos + bajadas]",
        col=rgb(0.8,0.1,0.1,0.6))

# Limpiamos la consola
cat("\014")
#

# Títulos, Bajadas y Notas ------------------------------------------------

# Vamos a crear una sub-base con las variables 'fecha', 'bajada' y 'nota'
t_b_n_la_capital <- la_capital[ , c('fecha', 'titulo', 'bajada', 'nota')]

# Creamos una nueva variables pegando titulo, bajada y nota en una misma celda
t_b_n_la_capital$t_b_n <- paste(t_b_n_la_capital$titulo, 
                                t_b_n_la_capital$bajada, 
                                t_b_n_la_capital$nota,
                                sep = ' || ')

# Nos quedamos solo con las variables 'fecha' y 't_b'
t_b_n_la_capital <- t_b_n_la_capital[ , c('fecha', 't_b_n')]

# Con esta sub-base vamos a generar una nueva columna 
# con la frecuencia de menciones de un diccionario
t_b_n_la_capital$piquetes <- acep_men(x = t_b_n_la_capital$t_b_n, 
                                    y = c('piquete', 'corte de ruta'),
                                    tolower = TRUE)

# Limpiamos la consola
cat("\014")

# Inspeccionamos la base
glimpse(t_b_n_la_capital)

# Vemos cuantos piquetes se contabilizaron
sum(t_b_n_la_capital$piquetes, na.rm = TRUE)

# Filtramos los título que contabilizaron al menos una mención
piquetes_t_b_n <- subset(t_b_n_la_capital, piquetes > 0)

# Imprimimos las 131 filas
print(piquetes_t_b_n, n = nrow(piquetes_t_b_n))

# Limpiamos la consola
cat("\014")

# Agrupamos por año y contamos piquetes
tabla_piquetes_anio_t_b_n <- piquetes_t_b_n |>
  mutate(año = year(fecha)) |> 
  count(año, name = 'frecuencia')

# Imprimimos
tabla_piquetes_anio_t_b_n

# Visualizamos
barplot(frecuencia ~ año, 
        data = tabla_piquetes_anio_t_b_n,
        main="Piquetes [dicc ad hoc - titulos + bajadas + notas]",
        col=rgb(0.8,0.1,0.1,0.6))

# Limpiamos la consola
cat("\014")
#

# Diccionario de SISMOS ---------------------------------------------------

# Vamos a crear una nueva sub-base con las variables 'fecha', 'bajada' y 'nota'
la_capital_sismos <- la_capital[ , c('fecha','titulo','bajada','nota')]

# Con esta sub-base vamos a generar una nueva columna con la 
# frecuencia de menciones del diccionario de SISMOS en el título
la_capital_sismos$conf_tit <- acep_men(x = la_capital$titulo, 
                                        y = acep_diccionarios$dicc_confl_sismos,
                                        tolower = TRUE)

# Con esta sub-base vamos a generar una nueva columna con la 
# frecuencia de menciones del diccionario de SISMOS en la bajada
la_capital_sismos$conf_baj <- acep_men(x = la_capital$bajada, 
                                       y = acep_diccionarios$dicc_confl_sismos,
                                       tolower = TRUE)

# Con esta sub-base vamos a generar una nueva columna con la 
# frecuencia de menciones del diccionario de SISMOS en la nota
la_capital_sismos$conf_not <- acep_men(x = la_capital$nota, 
                                       y = acep_diccionarios$dicc_confl_sismos,
                                       tolower = TRUE)

# Reemplazamos los valores perdidos en las columnas numéricas por el valor 0
la_capital_sismos[ , c('conf_tit','conf_baj','conf_not')][is.na(
  la_capital_sismos[ , c('conf_tit','conf_baj','conf_not')])] = 0

# Creamos una nueva columna con la suma de las menciones 
# en los títulos, las bajadas y las notas
la_capital_sismos$conflictos <- 
  la_capital_sismos$conf_tit + 
  la_capital_sismos$conf_baj + 
  la_capital_sismos$conf_not

# Limpiamos la consola
cat("\014")

# Inspeccionamos la base
glimpse(la_capital_sismos)

# Vemos cuantos piquetes (menciones) se contabilizaron
sum(la_capital_sismos$conflictos)

# Filtramos los título que contabilizaron al menos una mención
conflictos_lc <- subset(la_capital_sismos, 
                        conflictos > 0) # Podemos cambiar el umbral

# Limpiamos la consola
cat("\014")

# Imprimimos solo 20 filas
print(conflictos_lc, n = 20)

# Limpiamos la consola
cat("\014")

# Agrupamos por año y contamos piquetes
tabla_conflictos_lc_anio <- conflictos_lc |>
  mutate(año = year(fecha)) |> 
  count(año, name = 'frecuencia')

# Visualizamos
barplot(frecuencia ~ año, 
        data = tabla_conflictos_lc_anio,
        main="Conflictividad [dicc. SISMOS - titulos + bajadas + notas]",
        col=rgb(0.1,0.6,0.1,0.6))

# Limpiamos la consola
cat("\014")
#

# STRINGR -----------------------------------------------------------------

# Vamos a crear una nueva sub-base con las variables 'fecha' y 'titulo'
titulos_lc <- la_capital[ , c('fecha','titulo')]

# Con esta sub-base vamos a generar una nueva columna 
# con la frecuencia de menciones de un diccionario
titulos_lc$piquetes <- str_count(
  string = str_to_lower(titulos_lc$titulo), # vector de texto en minúsculas
  pattern = 'piquete|corte de ruta'         # vector de palabras
)

# Inspeccionamos la base
glimpse(titulos_lc)

# Vemos cuantos piquetes se contabilizaron
sum(titulos_lc$piquetes, na.rm = TRUE)

# Limpiamos la consola
cat("\014")
#

# Diccionario de G. Palazzo -----------------------------------------------

# Vamos a crear una nueva sub-base con las variables 'fecha', 'bajada' y 'nota'
la_capital_gp <- la_capital[ , c('fecha','titulo','bajada','nota')]

# Con esta sub-base vamos a generar una nueva columna con la 
# frecuencia de menciones del diccionario de GP en el título
la_capital_gp$conf_tit <- str_count(str_to_lower(la_capital_gp$titulo), 
                                    paste0(acep_diccionarios$dicc_confl_gp, collapse = '|'))

# Con esta sub-base vamos a generar una nueva columna con la 
# frecuencia de menciones del diccionario de GP en la bajada
la_capital_gp$conf_baj <- str_count(str_to_lower(la_capital_gp$bajada), 
                                    paste0(acep_diccionarios$dicc_confl_gp, collapse = '|'))

# Con esta sub-base vamos a generar una nueva columna con la 
# frecuencia de menciones del diccionario de GP en la nota
# Demora entre 10 y 15 minutos aprox
la_capital_gp$conf_not <- str_count(str_to_lower(la_capital_gp$nota), 
                                    paste0(acep_diccionarios$dicc_confl_gp, collapse = '|'))

# Reemplazamos los valore sperdidos en las columnas numéricas por 0
la_capital_gp[ , c('conf_tit','conf_baj','conf_not')][is.na(
  la_capital_gp[ , c('conf_tit','conf_baj','conf_not')])] = 0

# Creamos una nueva columna con la suma de las menciones 
# en los títulos, las bajadas y las notas
la_capital_gp$conflictos <- 
  la_capital_gp$conf_tit + 
  la_capital_gp$conf_baj + 
  la_capital_gp$conf_not

# Limpiamos la consola
cat("\014")

# Inspeccionamos la base
glimpse(la_capital_gp)

# Vemos cuantos piquetes se contabilizaron
sum(la_capital_gp$conflictos)

# Filtramos los título que contabilizaron al menos una mención
la_capital_gp <- subset(la_capital_gp, conflictos > 1)

# Agrupamos por año y contamos piquetes
tabla_conf_gp_anio <- la_capital_gp |>
  mutate(año = year(fecha)) |> 
  count(año, name = 'frecuencia')

# Imprimimos
tabla_conf_gp_anio

# Visualizamos
barplot(frecuencia ~ año, 
        data = tabla_conf_gp_anio,
        main="Hostilidad [dicc. Palazzo | titulos + bajadas + notas]",
        col=rgb(0.1,0.9,0.1,0.6))

# Vizualizamos con ggplot2
ggplot(tabla_conf_gp_anio) +
  geom_bar(aes(x=año,y=frecuencia), 
           fill = 'skyblue',
           color='grey40', 
           size= 0.85,
           stat = 'identity') +
  labs(title = 'Hostilidad [dicc. Palazzo | titulos + bajadas + notas]') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Limpiamos la consola
cat("\014")
#
  