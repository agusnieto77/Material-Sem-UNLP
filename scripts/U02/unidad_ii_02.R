
# Primera parte -----------------------------------------------------------

# Cargamos la librería
require(ACEP)

# Función acep_load_base()
View(acep_load_base)

# Cargamos la base de notas de la Revista Puerto con la función acep_load_base()
rev_puerto <- acep_load_base(acep_bases$rp_mdp)

# Cargamos la etiqueta de la base a descargar
rp_mdp <- acep_bases$rp_mdp

# Cargamos la base de notas de la Revista Puerto
revista_puerto <- acep_load_base(rp_mdp)

# Objeto acep_diccionarios
View(acep_diccionarios)

# Cargamos el diccionario de conflictos de SISMOS
dicc_confl_sismos <- acep_diccionarios$dicc_confl_sismos

# Función acep_frec()
View(acep_frec)

# Con la función acep_frec() contamos la frecuencia de palabras de cada nota y creamos una
# nueva columna llamada  n_palabras
revista_puerto$n_palabras <- acep_frec(revista_puerto$nota)

# Imprimimos en pantalla la base con la nueva columna de frecuencia de palabras
revista_puerto |> head()

# Función acep_men()
View(acep_men)

# Ahora con la función acep_men() contamos la frecuencia de menciones de términos del
# diccionario de conflictividad de SISMOS de cada nota y creamos una nueva columna llamada  conflictos
revista_puerto <- revista_puerto[1:100,] # elaboramos un corpus más pequeño para el ejemplo
revista_puerto$conflictos <- acep_men(revista_puerto$nota, dicc_confl_sismos)

# Imprimimos en pantalla la base con la nueva columna de menciones del diccionario de conflictividad
revista_puerto  |> head()

# Función acep_int()
View(acep_int)

# Ahora con la función acep_int() calculamos un índice de intensidad de la conflictividad y creamos una
# nueva columna llamada  intensidad
revista_puerto$intensidad <- acep_int(revista_puerto$conflictos, revista_puerto$n_palabras, 3)

# Imprimimos en pantalla la base con la nueva columna de intensidad
revista_puerto  |> head()

# Segunda parte -----------------------------------------------------------

# Volvemos a cargar la base de notas de la Revista Puerto sin procesar
revista_puerto <- acep_load_base(rp_mdp)

# Creamos un subset
subset_rp <- revista_puerto[1:100,]

# Cargamos el diccionario de conflictos de SISMOS
dicc_confl_sismos <- acep_diccionarios$dicc_confl_sismos

# Función acep_db()
View(acep_db)

# Ahora con la función acep_db() aplicamos las tres funciones en un solo paso
rp_procesada <- acep_db(subset_rp, subset_rp$nota, dicc_confl_sismos, 3)

# Imprimimos en pantalla la base con las tres columna creadas
rp_procesada |> head()

# Tercera parte -----------------------------------------------------------

# Cargamos los datos procesados
rp_procesada <- acep_bases$rp_procesada

# Función acep_rst()
View(acep_rst)

# Ahora con la función acep_rst() elaboramos un resumen estadístico
rp_procesada <- acep_rst(rp_procesada, rp_procesada$fecha, rp_procesada$n_palabras, 
                         rp_procesada$conflictos, st = 'anio', u = 4)

# Imprimimos en pantalla la base con las métricas de conflictividad
rp_procesada |> head()

# Función acep_plot_st()
View(acep_plot_st)

# Ahora con la función acep_plot_st() elaboramos un gráfico de barras 
# con menciones del diccionario de conflictividad
acep_plot_st(rp_procesada$st, rp_procesada$frecm,
             t = 'Evolución de la conflictividad en el sector pesquero argentino',
             ejex = 'Años analizados',
             ejey = 'Menciones del diccionario de conflictos',
             etiquetax = 'horizontal')

# Cuarta parte ------------------------------------------------------------

# Función acep_plot_rst()
View(acep_plot_rst)

# Ahora con la función acep_plot_rst() elaboramos una visualización resumen
# con cuatro gráficos de barras 
acep_plot_rst(rp_procesada, tagx = 'vertical')

# Fuente ------------------------------------------------------------------

# https://agusnieto77.github.io/ACEP/
