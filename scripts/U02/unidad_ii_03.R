
# Primera parte -----------------------------------------------------------

# Cargamos las librerías
require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)
require(lubridate)

# Descargamos la base de notas de la Revista Puerto
# options(timeout = 900)
download.file(url="https://zenodo.org/record/6800637/files/rp_mdp.rds",
              destfile = './scripts/U02/rev_puerto.rds')

# Cargamos la base de notas de la Revista Puerto
rev_puerto <- readRDS('./scripts/U02/rev_puerto.rds')

# Cargamos el diccionario de conflictos de SISMOS
dicc_confl_sismos <- read.csv2('scripts/U02/dicc_confl_sismos.csv')[ , 2] |> as.vector()

# Contamos la frecuencia de palabras de cada nota y creamos una
# nueva columna llamada  n_palabras
rev_puerto$n_palabras_m1  <- str_count(rev_puerto$nota, '[:graph:]+\\b')
rev_puerto$n_palabras_m2  <- str_count(
  str_remove_all(rev_puerto$nota, '[:punct:]|°|\\$|´|=|`|\\+|×|¬|¨'), '\\w+\\b|\\w+​')
rev_puerto$n_palabras_dif <- rev_puerto$n_palabras_m1-rev_puerto$n_palabras_m2
rev_puerto$n_palabras_dif <- NULL
rev_puerto$n_palabras_m2  <- NULL
colnames(rev_puerto)[7] <- 'n_palabras'

# Imprimimos en pantalla la base con la nueva columna de frecuencia de palabras
rev_puerto |> head()

# Ahora contamos la frecuencia de menciones de términos del diccionario de 
# conflictividad de SISMOS de cada nota y creamos una nueva columna llamada  conflictos
rev_puerto <- rev_puerto[1:100,] # elaboramos un corpus más pequeño para el ejemplo
rev_puerto <- rev_puerto |> mutate(
  conflictos = str_count(tolower(nota), paste0(dicc_confl_sismos, collapse = '|'))
)

# Imprimimos en pantalla la base con la nueva columna de menciones del diccionario de conflictividad
rev_puerto  |> head()

# Ahora calculamos un índice de intensidad de la conflictividad y creamos una
# nueva columna llamada  intensidad
rev_puerto <- rev_puerto |> mutate(
  intensidad = round(conflictos/n_palabras, 3)
)

# Imprimimos en pantalla la base con la nueva columna de intensidad
rev_puerto  |> head()

# Segunda parte -----------------------------------------------------------

# Volvemos a cargar la base de notas de la Revista Puerto sin procesar
rev_puerto <- readRDS('./scripts/U02/rev_puerto.rds')

# Creamos un subset
subset_rp <- rev_puerto[1:100,]

# Ahora creamos las tres variables en un paso
rp_procesada <- subset_rp |> mutate(
  n_palabras = str_count(nota, '[:graph:]+\\b'),
  conflictos = str_count(tolower(nota), paste0(dicc_confl_sismos, collapse = '|')),
  intensidad = round(conflictos/n_palabras, 3)
)

# Imprimimos en pantalla la base con las tres columna creadas
rp_procesada |> head()

# Tercera parte -----------------------------------------------------------

# Volvemos a cargar la base de notas de la Revista Puerto sin procesar
rev_puerto <- readRDS('./scripts/U02/rev_puerto.rds')

# Ahora creamos las tres variables en un paso para la base completa
rp_procesada <- rev_puerto |> mutate(
  n_palabras = str_count(nota, '[:graph:]+\\b'),
  conflictos = str_count(tolower(nota), paste0(dicc_confl_sismos, collapse = '|')),
  intensidad = round(conflictos/n_palabras, 3)
)

# Ahora elaboramos un resumen estadístico
rp_resumen <- rp_procesada |> mutate(
  st = year(fecha),
  csn = ifelse(conflictos > 4, 1, 0)
  ) |> 
  group_by(st) |> 
  summarise(
    frecn           = n(),
    csn             = sum(csn),
    frecp           = sum(n_palabras),
    frecm           = sum(conflictos),
    intac           = sum(intensidad, na.rm = T),
    intensidad      = round(frecm/frecp,4),
    int_notas_confl = round(csn/n(), 4)
  )

# Imprimimos en pantalla la base con las métricas de conflictividad
rp_resumen |> head()

ACEP::acep_rst(rp_procesada, rp_procesada$fecha, rp_procesada$n_palabras, 
               rp_procesada$conflictos, st = 'anio', u = 4) |> as_tibble() |> head()

# Cuarta parte ------------------------------------------------------------

# Elaboramos un gráfico de barras con menciones del diccionario de conflictividad
ggplot(rp_resumen) +
  geom_bar(aes(x = as.factor(st), y = frecm), 
           color = 'grey40', 
           fill = 'pink',
           stat = 'identity') +
  labs(title = 'Evolución de la conflictividad en el sector pesquero argentino',
       x = 'Años analizados',
       y = 'Menciones del diccionario de conflictos') +
  theme_bw()

# Ahora elaboramos una visualización resumen con cuatro gráficos de barras 
rp_resumen |> select(-frecn, -csn, -frecp) |> 
  pivot_longer(cols = frecm:int_notas_confl) |> 
  mutate(name = factor(name, 
                          levels=c('int_notas_confl','frecm','intensidad','intac'))) |> 
  ggplot() +
    geom_bar(aes(x = as.factor(st), y = value), 
             color = 'grey40', 
             fill = 'pink',
             stat = 'identity') +
  facet_wrap(~name, scales = "free_y",
             labeller = labeller(name = 
                                   c(
                                     "int_notas_confl" = "Eventos de protesta",
                                     "frecm" = "Acciones de protesta",
                                     "intensidad" = "Intensidad de la protesta",
                                     "intac" = "Intensidad acumulada de la protesta")
                                     
             )) +
  labs(x = 'Años analizados',
       y = 'Menciones del diccionario de conflictos') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
