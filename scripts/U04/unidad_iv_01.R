# Instalación y activación de paquetes ------------------------------------
#
# instalamos los paquetes solo si no los tenemos instalados 
# lista de paquetes requeridos para correr el script
paquetes_a_instalar <- c("tidyverse", "quanteda", "textreuse", "lubridate", "caret","seededlda","e1071",
                         "tidytext", "udpipe", "spacyr", "tm", "quanteda.textmodels","randomForest","ROCR")

# lista de paquetes faltantes
paquetes_faltantes <- paquetes_a_instalar[!(paquetes_a_instalar %in% installed.packages()[,"Package"])]

# orden para instalar solo los paquetes faltantes 
if(length(paquetes_faltantes)) install.packages(paquetes_faltantes)

# activamos los paquetes
require(tidyverse)           # para activar varios paquetes (gglopt2, dplyr, etc.)
require(quanteda)            # para corpus y matrices texto~documento
require(quanteda.textmodels) # para modelos de escala y clasificadores sobre objetos matriciales dispersos que representan datos textuales en forma de una matriz de características de documento
require(seededlda)           # para implementar el seeded-LDA en el modelado de temas semi-supervisados usando quanteda
require(randomForest)        # para clasificación y regresión basadas en un bosque de árboles con entradas aleatorias
require(caret)               # para agilizar el proceso de entrenamiento del modelo para problemas complejos de regresión y clasificación
require(textreuse)           # para comparar documentos
require(lubridate)           # para series temporales
require(tidytext)            # para tokenizar
require(udpipe)              # para lematizar
require(spacyr)              # para lematizary detectar entidades
require(ROCR)                # para curva ROC
require(tm)                  # para corpus y matrices texto~documento
require(e1071)               # para algoritmos de clasificación y test de modelos

# Cargamos las notas ------------------------------------------------------

# Cargamos la base 1
base_lc_notas01 <- read.csv("https://estudiosmaritimossociales.org/Data_TalleR/muestra_etiquetada.csv", 
                            sep = ';', fileEncoding="UTF-8-BOM") %>% 
  mutate(id = row_number(), .before = fecha)

# Cargamos la base 2
base_lc_notas02 <- read.csv("https://estudiosmaritimossociales.org/Data_TalleR/muestra_etiquetada.csv", 
                            sep = ';', fileEncoding="UTF-8-BOM") %>% 
  mutate(id = c(1:100,201:248), .before = fecha)

# Vemos su estructura
glimpse(base_lc_notas02) 

# Unificamos las dos bases
(base_lc_notas <- rbind(base_lc_notas01,base_lc_notas02) %>% mutate(fecha = as.Date(fecha)) %>% arrange(desc(fecha)))

# Eliminamos duplicados y resolvemos redundancias -------------------------

# Dejamos solo los registros únicos según id
(base_lc_notas <- base_lc_notas %>% distinct(id, .keep_all = T))

# Dejamos solo los registros únicos según fecha y nota
(notas_distinct <- base_lc_notas %>% distinct(fecha, nota, .keep_all = T))

# A partir del id vemos qué notas fueron eliminadas 
(notas_iguales <- anti_join(base_lc_notas,notas_distinct,by='id') %>% mutate(clase = 'eliminadas'))

# Vemos id + nota
(notas_comparadas <- notas_iguales[,c(1,4)] %>% rename(id_i=id) %>% 
    left_join(base_lc_notas[,c(1,4)], by='nota') %>% filter(id_i != id))

# Eliminamos las notas que tienen 450 caracteres o menos
# Primero vemos qué notas tienen menos de 451 caracteres
notas_distinct %>% filter(nchar(nota) <= 450)

# Las imprimimos en pantalla
notas_distinct %>% filter(id_duplicadas == 34 | id_duplicadas == 61) %>% .[,4]

# Luego las eliminamos y nos quedamos con las que tienen 451 caracteres o más
(base_lc_notas <- notas_distinct %>% filter(nchar(nota) >= 450))

# Eliminamos los objetos que no usamos 
rm(base_lc_notas01,base_lc_notas02,notas_distinct,notas_iguales, notas_comparadas,paquetes_a_instalar,paquetes_faltantes)

# Transformamos el data.frame a tibble
base_lc_notas <- as_tibble(base_lc_notas)

# Modificamos los id
(base_lc_notas <- base_lc_notas %>% mutate(id = row_number()))

# Comparamos las notas para detectar similitudes con el objetivo de borrar las notas repetidas
# Generar una función MinHash 
# https://es.wikipedia.org/wiki/MinHash
# https://towardsdatascience.com/understanding-locality-sensitive-hashing-49f6d1f6134
# https://skeptric.com/minhash-lsh/
# http://snap.stanford.edu/class/cs246-2017/slides/LSH-1.pdf
(minhash <- minhash_generator(n = 120, seed = 9234))

# Creamos un corpus_minhash con la función TextReuseCorpus()
# Esta clase (TextReuseCorpus) contiene el texto de un documento y sus metadatos. 
# Cuando se carga el documento, el texto también se tokeniza. 
# A continuación, esos tokens se procesan mediante una función hash. 
# De forma predeterminada, los hashes se conservan y los tokens se descartan, 
# ya que usar solo hashes da como resultado un ahorro de memoria significativo
(corpus_minhash <- TextReuseCorpus(text = base_lc_notas$nota,
                                   tokenizer = tokenize_ngrams, n = 7,
                                   minhash_func = minhash))

# Ahora identificamos las coincidencias con la función lsh()
# Locality Sensitive Hashing (LSH) descubre rápidamente las coincidencias 
# potenciales entre un corpus de documentos, 
# de modo que sólo se pueden comparar los pares probables.
# Calculamos las coincidencias potenciales,
(cubo_lsh <- lsh(x = corpus_minhash, bands = 20, progress = FALSE))

# Extraemos los candidatos con la función lsh_candidates(),
(pares_candidatxs_lsh <- lsh_candidates(cubo_lsh))

# Aplicamos una función de comparación sobre esos candidatos.
(scores <- lsh_compare(pares_candidatxs_lsh, corpus_minhash, jaccard_similarity, progress = FALSE))

# Identificamos los artículos que teniendo un id único contienen contenido repetido
(scores_rep <- scores %>% 
    mutate(a2 = as.integer(str_remove(a,'doc-')), 
           b2 = as.integer(str_remove(b,'doc-')),
           dif = sqrt((a2-b2)^2)) %>% arrange(desc(score)))

# Generamos una tabla con pares de id's 
(scores_rep_for_anti_join <- scores_rep %>% select(b2,score,a2) %>% rename(id = b2, id_par = a2))

# Seleccionamos los id's de artículos con contenido repetido que aparecen en 
# segundo lugar para hacer un anti_join
(notas_id_out <- scores_rep_for_anti_join %>% select(id) %>% distinct())

# Hacemos una tabla de notas con contenido repetido según el algoritmo mishash
(par_notas_rep <- scores_rep_for_anti_join %>% 
    left_join(base_lc_notas[,c(1,4)], by = c('id' = 'id')) %>% rename(nota_id = nota) %>% 
    left_join(base_lc_notas[,c(1,4)], by = c('id_par' = 'id')) %>% rename(nota_id_par = nota))

# Nos quedamos con notas únicas 
(base_lc_notas <-  base_lc_notas %>% anti_join(notas_id_out))

# Modificamos los id's
(base_lc_notas <- base_lc_notas %>% mutate(id = row_number()))

# Eliminamos los objetos que no usamos 
rm(minhash,scores_rep,scores,pares_candidatxs_lsh,notas_id_out,cubo_lsh,
   corpus_minhash,par_notas_rep,scores_rep_for_anti_join)

# Ahora nos vamos a ocupar de las notas que no son duplicadas pero reiteran información 
# sobre un mismo evento
# volvemos a aplicar la función minhash para detectar similitudes,
(minhash <- minhash_generator(n = 120, seed = 9234))

# Creamos un corpus minhash,
(corpus_minhash <- TextReuseCorpus(text = base_lc_notas$nota,
                                   tokenizer = tokenize_ngrams, 
                                   n = 4, # cambiamos el n_grams
                                   minhash_func = minhash))

# Calculamos las coincidencias potenciales con la función Locality Sensitive Hashing (LSH),
(cubo_lsh <- lsh(x = corpus_minhash, bands = 120, progress = FALSE)) # cambiamos el n de bands
# El número de bands (filas) que se utilizará para el hashing sensible a la localidad. 
# El número de hashes (120) en los documentos del corpus debe ser divisible por el número de filas (120).

# Extraemos los candidatos con la función lsh_candidates(),
(pares_candidatxs_lsh <- lsh_candidates(cubo_lsh))

# Aplicamos una función de comparación sobre esos candidatos.
(scores <- lsh_compare(pares_candidatxs_lsh, corpus_minhash, jaccard_similarity, progress = FALSE))

# Identificamos los artículos que teniendo un id único contienen contenido repetido
(scores_rep <- scores %>% 
    mutate(a2 = as.integer(str_remove(a,'doc-')), 
           b2 = as.integer(str_remove(b,'doc-')),
           dif = sqrt((a2-b2)^2)) %>% arrange(desc(score)) %>% 
    filter(dif < 2))

# Generamos una tabla con pares de id's
(scores_rep_join <- scores_rep %>% select(b2,score,a2) %>% rename(id = b2, id_par = a2))

# Incorporamos las fechas para calcular la distancia en días entre nota y nota
(scores_rep_join_fechas <- scores_rep_join %>% left_join(base_lc_notas[,1:2], by = c('id' = 'id')) %>% 
    rename(fecha_id = fecha) %>% left_join(base_lc_notas[,1:2], by = c('id_par' = 'id')) %>% 
    rename(fecha_id_par = fecha) %>% mutate(dif_fecha = fecha_id-fecha_id_par) %>% 
    mutate(dif_fecha = as.numeric(dif_fecha)) %>% mutate(dif_fecha = sqrt(dif_fecha^2)))

# Nos quedamos con aquellos pares de notas que no tienen más de 1 día de distancia
(scores_rep_id <- scores_rep_join_fechas %>% filter(dif_fecha < 2))

# Hacemos una tabla de notas con contenido compartido según el algoritmo mishash
(par_notas_red <- scores_rep_id %>% 
    left_join(base_lc_notas[,c(1,4)], by = c('id' = 'id')) %>% rename(nota_id = nota) %>% 
    left_join(base_lc_notas[,c(1,4)], by = c('id_par' = 'id')) %>% rename(nota_id_par = nota))

# Vemos el contenido de los pares de notas
par_notas_red[,7:8]

# Seleccionamos los id's de artículos con contenido compartido que aparecen en segundo lugar
(notas_id_red <- par_notas_red %>% select(id) %>% distinct())

# Nos quedamos con notas únicas 
(base_lc_notas_unicas <-  base_lc_notas %>% select(-duplicada) %>% anti_join(notas_id_red))

# Comprobamos que no hay duplicas según la variable id_duplicadas
as.vector(as_vector(base_lc_notas_unicas %>% count(id_duplicadas) %>% 
                      summarise(sum(n)))) == length(base_lc_notas_unicas$id_duplicadas)

# Creamos una nueva columna con las notas de contenido similar que fueron eliminadas 
(base_lc_notas_unicas <- base_lc_notas_unicas %>% 
    left_join(par_notas_red[,c(3,7)], by =c('id' = 'id_par')) %>% 
    rename(nota_dupli = nota_id) %>% select(id,fecha,portal,nota,nota_dupli))

# Transformamos los NA's en una cadena de caracteres vacía
base_lc_notas_unicas[is.na(base_lc_notas_unicas)] <- ''

# Vemos el resultado
base_lc_notas_unicas

# Ahora creamos una nueva columna que una el contenido de las notas de ambas columnas
(base_lc_notas_unicas <- base_lc_notas_unicas %>% 
    unite(mas_notas, nota:nota_dupli, sep = ' [NUEVA NOTA]> ', remove = FALSE) %>% 
    mutate(mas_notas = str_replace_all(mas_notas,'..NUEVA NOTA...$','')) %>% 
    left_join(base_lc_notas[,c(1,5)]) %>% select(id,fecha,portal,nota,mas_notas,clase))

# Eliminamos los objetos que no usamos 
rm(minhash,scores_rep,scores,pares_candidatxs_lsh,notas_id_red,cubo_lsh,corpus_minhash,
   par_notas_red,scores_rep_id,base_lc_notas,scores_rep_join,scores_rep_join_fechas)

# Actualizamos el id de notas y contamos las palabras
(base_lc_notas_unicas <- base_lc_notas_unicas %>% arrange(desc(fecha)) %>% 
    mutate(id          = row_number(),
           n_words_n   = sapply(strsplit(nota, " "), length),
           n_words_m_n = sapply(strsplit(mas_notas, " "), length),
           más_info    = ifelse((n_words_m_n-n_words_n) != 0, 'SI', 'NO')))

# Resumen del contenido de las notas --------------------------------------

# Nos quedamos con las columnas id y notas
(solo_id_y_notas <- base_lc_notas_unicas %>% select(id,mas_notas))

# Tokenizamos y normalizamos las notas
(notas_norm <- solo_id_y_notas %>% tidytext::unnest_tokens(palabras,mas_notas) %>% 
    filter(!str_detect(palabras, '[[:punct:]]')) %>% filter(!str_detect(palabras, '[[0-9]]')) %>% 
    anti_join(tibble(palabras = c(tm::stopwords(kind='es'),'mar','plata','buenos','aires','general',
                                  'pueyrredon','pueyrredón','años','año','anos','ano','ciudad',
                                  'dos','además','provincia','así','tres','sólo','mismo','mismos',
                                  'parte','ser','según','días','vez','luego','lugar','ayer','hoy','dijo',
                                  'tras','tres','mientras','después','cada','mismo','hora','horas',
                                  'día','pasado','hacer','local','explicó','semana','mañana','junto',
                                  'solo','puede','través','momento','mientras','aunque','sino','señaló',
                                  'aseguró','julio','carlos','último','ver','meses','agregó','respecto',
                                  'primer','misma','hacia','siempre'
    ))) %>% 
    filter(nchar(palabras) > 2) %>% group_by(id) %>% 
    summarise(nota_norm = paste0(palabras, collapse = ' ')) %>% 
    ungroup() %>% full_join(base_lc_notas_unicas) %>% 
    select(id, fecha, portal, nota, mas_notas, nota_norm, clase))

# TOKENS
# Ahora realizaremos una selección de palabras clave por nota
(notas_norm_keywords <- notas_norm %>% select(id,nota,nota_norm) %>% 
    unnest_tokens(palabras,nota_norm, drop = FALSE) %>% group_by(id,nota_norm) %>% count(palabras) %>% 
    arrange(id,desc(n)) %>% filter(n > 1) %>% slice_max(order_by = n, n = 10) %>% 
    summarise(keywords = paste0('| ', palabras, collapse = ' ')) %>% select(id,keywords) %>% 
    ungroup() %>% full_join(notas_norm, by = 'id') %>% 
    select(id, fecha, portal, nota, mas_notas, nota_norm, keywords,clase))

# LEMAS
# Lematizamos con udpipe
# Descargamos el modelo en español
es_model <- udpipe_download_model(language = "spanish")

# Lo cargamos
es_model <- udpipe_load_model(es_model$file_model)

# Lematizamos el listado de palabras
lemmas_udpipe <- udpipe_annotate(es_model, x = iconv(notas_norm_keywords$nota, to = 'UTF-8')) # lematización con tildes

# Lo transformamos en data.frame
(lemmas_udpipe <- as.data.frame(lemmas_udpipe))

# Hacemos una selección de variables 
(lemmas_udpipe <- lemmas_udpipe %>% select(doc_id,paragraph_id,sentence_id,token_id, token,lemma,upos,feats) %>% 
    mutate(id = as.integer(str_remove_all(doc_id,'doc')), .before = doc_id) %>% select(-doc_id) %>% as_tibble())

# Guardamos las notas lemmatizadas
#saveRDS(lemmas_udpipe,'./data/notas_lemmas.rds')

# Ahora realizaremos una selección de lemmas clave por nota
(notas_norm_key_words_lemmas <- lemmas_udpipe %>% select(id,lemma) %>% 
    filter(!str_detect(lemma, '[[:punct:]]')) %>% filter(!str_detect(lemma, '[[0-9]]')) %>% 
    anti_join(tibble(lemma = c(tm::stopwords(kind='es'),'mar','plata','buenos','aires','general',
                               'pueyrredon','pueyrredón','años','año','anos','ano','ciudad',
                               'dos','además','provincia','así','tres','sólo','mismo','mismos',
                               'parte','ser','según','días','vez','luego','lugar','ayer','hoy','dijo',
                               'tras','tres','mientras','después','cada','mismo','hora','horas',
                               'día','pasado','hacer','local','explicó','semana','mañana','junto',
                               'solo','puede','través','momento','mientras','aunque','sino','señaló',
                               'aseguró','julio','carlos','último','ver','meses','agregó','respecto',
                               'primer','misma','hacia','siempre','|'))) %>% 
    group_by(id) %>% count(lemma) %>% 
    arrange(id,desc(n)) %>% filter(n > 1) %>% slice_max(order_by = n, n = 10) %>% 
    summarise(keylemmas = paste0('| ', lemma, collapse = ' ')) %>% select(id,keylemmas) %>% 
    ungroup() %>% full_join(notas_norm_keywords, by = 'id') %>% 
    select(id, fecha, portal, nota, mas_notas, nota_norm, keywords, keylemmas,clase) %>% 
    mutate(keylemmas = str_to_lower(keylemmas))
)

# ENTITY
# Con spacyR identificamos entidades
# Creamos el corpus de textos
(corpus_notas <- corpus(notas_norm_key_words_lemmas$nota,
                        docnames = notas_norm_key_words_lemmas$id))

# spacy_install()
# spacy_finalize()

# Bajamos el modelo en español
# spacy_download_langmodel("es_core_news_md")

# Cargamos el modelo es
spacy_initialize(model = "es_core_news_md")

# Analizamos con spacyR las notas
(corpus_notas_spacyR <- spacy_parse(corpus_notas, 
                                    pos = TRUE,
                                    tag = FALSE,
                                    lemma = TRUE,
                                    entity = TRUE,
                                    dependency = TRUE,
                                    nounphrase = FALSE,
                                    multithread = FALSE) %>% 
    as_tibble() %>% select(-head_token_id))

# Guardamos la base de datos devuelta por spacyR
#saveRDS(corpus_notas_spacyR,'./data/corpus_notas_spacyR.rds')

# Ahora realizaremos una selección de entidades por nota
(notas_norm_key_words_lemmas_entities <- corpus_notas_spacyR %>% select(doc_id,token,entity) %>% 
    mutate(id = as.integer(doc_id), .before = doc_id) %>% select(-doc_id) %>% 
    filter(entity != "" & 
             token != "El" & token != "Los" & token != "La" & 
             token != "Las" & token != "De" & token != "LA" & token != "En" &
             token != "Capital" & token != "CAPITAL" & token != "Mar" & 
             token != "Plata" & token != "Por" & token != "Que" & token != "Con" &
             token != "Este" & token != "Nº" & token != "Según" & token != "Hay" &
             token != "Para" & token != "A" & token != "Al" & token != "No" &
             token != "Una" & token != "Si" & token != "Hoy" & token != "Pero" &
             token != "Allí" & token != "Ante" & token != "Ayer" & token != "Está" &
             token != "Esta" & token != "Lo" & token != "Uno" & token != "Así" &
             token != "Esa" & token != "Eso" & token != "Desde" & token != "MdP" &
             token != "Paro" & token != ",Según" & token != "“Habrá" & token != "2016).Por" &
             token != "2017.Desde" & token != "5º.En" & token != "Acá" & 
             token != "AgradecimientosLuego" & token != "Ahora" & token != "Amparo-confl" & 
             token != "AnsesManifestantes" & token != "actividades.-Judiciales" &
             token != "Añade" & token != "añoPor" & token != "Bomberos).Además" & 
             token != "Boquerón”" & token != "SMN)anunció" & token != "Corresponsal).-" &
             token != "Brasil).Buenos" & token != "C" & token != "“Cada" & token != "Cambios" &
             token != "Cómo" & token != "DAC.\"El" & token != "“EN" & token != "IAMC.Los" & 
             token != "Martín.#NoALaReformaPrevisionalMañana" & token != "LU6.Según" &
             token != "Bomberos).Además") %>% 
    filter(str_detect(token,'[[A-Z]]')) %>% 
    group_by(id) %>% summarise(entities = paste0('| ', token, collapse = ' ')) %>% select(id,entities) %>% 
    ungroup() %>% full_join(notas_norm_key_words_lemmas, by = 'id') %>% 
    select(id, fecha, portal, nota, mas_notas, nota_norm, keywords, keylemmas, entities, clase)
)

# Ahora realizaremos una selección de acciones por nota (Verbos + Sustantivos)
(notas_norm_key_words_lemmas_entities_acc <- corpus_notas_spacyR %>% select(doc_id,token,pos) %>% 
    mutate(id = as.integer(doc_id), .before = doc_id) %>% select(-doc_id) %>% filter(pos == 'VERB' | pos == 'NOUN') %>% 
    group_by(id) %>% summarise(acciones = paste0('| ', token, collapse = ' ')) %>% select(id,acciones) %>% 
    ungroup() %>% full_join(notas_norm_key_words_lemmas_entities, by = 'id') %>% 
    select(id, fecha, portal, nota, mas_notas, nota_norm, keywords, keylemmas, entities, acciones, clase)
)

# Eliminamos los objetos que no usamos 
rm(base_lc_notas_unicas,corpus_notas_spacyR,es_model,lemmas_udpipe,notas_norm,notas_norm_key_words_lemmas,
   notas_norm_key_words_lemmas_entities,notas_norm_keywords,solo_id_y_notas,corpus_notas)

# Clasificación de las notas según refieran o no a conflictos -------------

# Clasificación haciendo uso de diccionarios
# Creamos un diccionario con palabras referidas a conflictos
(dicc_breve <- c('protesta','conflict','huelg','corte de ca','corte de ru','plan de lucha','movilización','piqueter','medida de fuerza',
                 'piquete','paro de','medidas de fuerza','reclaman','reclamo','manifestantes','asambleas','paralizar','manifestación'))

# Clasificamos las notas en función de la presencia o ausencia de palabras del diccionario 
(notas_clas <- notas_norm_key_words_lemmas_entities_acc %>% 
    mutate(clase_nota_dicc = ifelse(str_detect(str_to_lower(nota), paste0(dicc_breve, collapse = '|')), 'conflicto', 'no_conflicto'),
           correspondencia = ifelse(clase == clase_nota_dicc, 'SI', 'NO')))
# Vemos su rendimiento
# Absoluto
table(notas_clas$correspondencia)
# Porcentaje
prop.table(table(notas_clas$correspondencia))
# Matriz
table(notas_clas$clase, notas_clas$clase_nota_dicc, dnn = c("Etiq Manual", "Etiq Dicc"))

# Clasificamos las notas en función de la cantidad de palabras del diccionario en las notas (>1)
(notas_clas_2 <- notas_norm_key_words_lemmas_entities_acc %>% 
    mutate(frec_pal_confli = str_count(str_to_lower(nota), paste0(dicc_breve, collapse = '|')),
           clase_nota_dicc = ifelse(frec_pal_confli > 1, 'conflicto', 'no_conflicto'),
           correspondencia = ifelse(clase == clase_nota_dicc, 'SI', 'NO')))

# Vemos su rendimiento
# Absoluto
table(notas_clas_2$correspondencia)
# Porcentaje
prop.table(table(notas_clas_2$correspondencia))
# Matriz
table(notas_clas_2$clase, notas_clas_2$clase_nota_dicc, dnn = c("Etiq Manual", "Etiq Dicc"))

### Clasificador bayesiano ingenuo (supervisado) ####----------------------------------------------------
# Naive Bayes es un modelo supervisado que se suele utilizar para clasificar documentos en dos o 
# más categorías. Una ventaja de este modelo es que solo se requiere una pequeña cantidad de datos 
# de entrenamiento para estimar los parámetros (las medias y las varianzas de las variables) 
# necesarias para la clasificación. Como las variables independientes se asumen, solo es necesario 
# determinar las varianzas de las variables de cada clase y no toda la matriz de covarianza.
# Entrenamos al clasificador usando etiquetas de clase adjuntas a los documentos y 
# predecimos la(s) clase(s) más probables de nuevos documentos sin etiquetar.

# Creamos el corpus
(corpus_notas_norm <- corpus(notas_norm_key_words_lemmas_entities_acc$nota_norm,
                             docnames = notas_norm_key_words_lemmas_entities_acc$id))

# Creamos la variable id
corpus_notas_norm$id    <- notas_norm_key_words_lemmas_entities_acc$id

# Creamos la variable clase
corpus_notas_norm$clase <- notas_norm_key_words_lemmas_entities_acc$clase

# Imprimimos las variables creadas
docvars(corpus_notas_norm)

# Imprimimos un resumen del contenido del corpus
summary(corpus_notas_norm, 5)

# Generamos 90 números aleatorios sin reemplazo
set.seed(123)
(id_entrenamiento <- sample(1:134, 90, replace = FALSE))

# Tokenizamos las notas
(tokens_notas <- quanteda::tokens(corpus_notas_norm, remove_punct = TRUE, remove_number = TRUE) %>% 
    tokens_remove(pattern = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')) %>% 
    tokens_wordstem())

# Creamos una matriz documento-términos
(dfm_notas <- dfm(tokens_notas))

# Creamos el conjunto de entrenamiento
(dfm_entrenamiento <- dfm_subset(dfm_notas, id %in% id_entrenamiento))

# Creamos el conjunto de testeo
(dfm_testeo <- dfm_subset(dfm_notas, !id %in% id_entrenamiento))

# Entrenamos al clasificador bayesiano ingenuo usando textmodel_nb()
# Esta función ajusta el modelo Bernoulli Naive Bayes en base a dos parámetros: 
# 1) un dfm; 2) etiquetas de entrenamiento.
(entr_mod_nb <- textmodel_nb(dfm_entrenamiento, dfm_entrenamiento$clase))

# Imprimimos un resumen del modelo nb
summary(entr_mod_nb)

# Naive Bayes solo puede tener en cuenta las características que ocurren tanto en el 
# conjunto de entrenamiento como en el conjunto de prueba, por eso usamos la función dfm_match()
# Esta función hace coincidir el conjunto de características de un dfm con un vector especificado 
# de nombres de características. Se incluirán las características existentes en x para las que haya 
# una coincidencia exacta con un elemento de features. Cualquier característica en x que no sea features 
# se descartará, y cualquier nombre de característica especificado en features pero que no se encuentre 
# en x se añadirá con todos los recuentos de cero.
(dfm_matched <- dfm_match(dfm_testeo, features = featnames(dfm_entrenamiento)))

# Ahora inspeccionamos el rendimiento del moldeo entrenado 
# Creamos un vector con las clases del etiquetado manual
(etiq_manual <- dfm_matched$clase)

# Ahora aplicamos el modelo entrenado sobre los datos de testeo
(etiq_nb     <- predict(entr_mod_nb, newdata = dfm_matched))

# Creamos la tabla de clasificación 
(tabla_clasificacion <- table(etiq_manual, etiq_nb)) # solo tres falsos positivos

# Utilizamos la función confusionMatrix() del paquete caret para evaluar el rendimiento de la clasificación.
caret::confusionMatrix(tabla_clasificacion, mode = "everything")

# Repetimos los paso para recuperar las 134 etiquetas
# Aplicamos dfm_match()
(dfm_matched_2 <- dfm_match(dfm_notas, features = featnames(dfm_entrenamiento)))

# Creamos un vector con las clases del etiquetado manual
(etiq_manual <- dfm_matched_2$clase)

# Ahora aplicamos el modelo entrenado sobre todos los datos
(etiq_nb_2     <- predict(entr_mod_nb, newdata = dfm_matched_2))

# Creamos la tabla de clasificación 
(tabla_clasificacion <- table(etiq_manual, etiq_nb_2))

# Incorporamos la predicción como nueva variable en la base
(notas_clas_3 <- notas_norm_key_words_lemmas_entities_acc %>% 
    mutate(clase_nb = etiq_nb_2,
           correspondencia = ifelse(clase == clase_nb, 'SI', 'NO')))

# Dejo algunas referencias:
# Jurafsky, Daniel, and James H. Martin. 2018. Speech and Language Processing. An Introduction to Natural Language Processing, 
# Computational Linguistics, and Speech Recognition. Draft of 3rd edition, September 23, 2018 (Chapter 4).

### Ahora probamos con el algoritmo Random Forest ####--------------------------------------------------

# Random Forest es un técnica de aprendizaje automático supervisada basada en árboles de decisión. 
# Su principal ventaja es que obtiene un mejor rendimiento de generalización para un rendimiento 
# durante entrenamiento similar. Esta mejora en la generalización la consigue compensando los errores 
# de las predicciones de los distintos árboles de decisión. Para asegurarnos que los árboles sean 
# distintos, lo que hacemos es que cada uno se entrena con una muestra aleatoria de los datos de 
# entrenamiento. Esta estrategia se denomina bagging. 
# Fuente: https://www.iartificial.net/random-forest-bosque-aleatorio/

# Preparamos la base de datos
# Creamos un listado con las palabras de menor frecuencia 
(min_freq <- notas_norm_key_words_lemmas_entities_acc %>% unnest_tokens(palabras,nota_norm) %>% select(id,palabras,clase) %>%
   anti_join(tibble(palabras = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo'))) %>% 
   count(palabras) %>% arrange(desc(n)) %>% filter(n < 14) %>% select(1))

# Preparamos el corpus para aplicar la función randomForest
(corpus_rf <- notas_norm_key_words_lemmas_entities_acc %>% unnest_tokens(palabras,nota_norm) %>% select(id,palabras,clase) %>%
    anti_join(tibble(palabras = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo'))) %>% 
    anti_join(min_freq) %>% group_by(id,clase) %>% count(palabras) %>% spread(palabras,n) %>% ungroup() %>% as.data.frame())

# Reemplazamos los NA's por 0
corpus_rf[is.na(corpus_rf)] <- 0

# Transformamos en factor la variable clase
corpus_rf$clase <- as.factor(corpus_rf$clase)

# Definimos una semilla y extraemos los id de entrenamiento 
set.seed(2021)
id_entrenamiento <- createDataPartition(corpus_rf$clase, p = 0.7, list = F)

# Entrenamos el modelo
(mod <- randomForest(clase ~ ., corpus_rf[id_entrenamiento,],
                     ntree = 500,
                     keep.forest = TRUE))

# Hacemos la predicción 
pred <- predict(mod, corpus_rf[-id_entrenamiento,], type = "class")

# Vemos su rendimiento
table(corpus_rf[-id_entrenamiento,"clase"] %>% as_vector(), pred, dnn= c("Actual", "Predicho"))

# Visualizamos su rendimiento con un curva roc
probs <- predict(mod, corpus_rf[-id_entrenamiento,], type = "prob")

pred <- prediction(probs[,2], corpus_rf[-id_entrenamiento,"clase"])

perf <- performance(pred, "tpr", "fpr")

# Imprimimos el plot
plot(perf, xlab = 'Razón de verosimilitud negativa', ylab = 'Razón de verosimilitud positiva',
     main = 'Random-Forest', col = 'red')

# Clasificamos con modelos no supervisado -------------------

### Agrupamiento Jerárquico

# La agrupación en clústeres es la forma más común de aprendizaje no supervisado, un tipo de algoritmo de 
# aprendizaje automático que se utiliza para extraer inferencias a partir de datos no etiquetados.

# El objetivo de los algoritmos es crear grupos que sean coherentes internamente, pero claramente 
# diferentes entre sí externamente. En otras palabras, las entidades dentro de un clúster deben ser lo más 
# similares posible y las entidades de un clúster deben ser lo más diferentes posible de las entidades de 
# otro. En términos generales, hay dos formas de agrupar puntos de datos en función de la estructura y 
# operación algorítmica, a saber, aglomerativa y divisiva.

# Aglomerativo ('de abajo hacia arriba'): un enfoque aglomerativo comienza con cada observación en un grupo 
# distinto (singleton) y fusiona grupos juntos hasta que se satisface un criterio de parada.

# Divisivo ('de arriba hacia abajo'): un método divisivo comienza con todos los patrones en un solo grupo y 
# realiza la división hasta que se cumple un criterio de detención.

# Creamos el corpus
(corpus_us <- Corpus(VectorSource(notas_norm_key_words_lemmas_entities_acc$acciones)))

# Lo normalizamos
(corpus_us <- tm_map(corpus_us,removeWords,c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')))

# Quitamos los espacios en blanco
(corpus_us <- tm_map(corpus_us,stripWhitespace))

# Lo transformamos en una matriz documento-término
(dtm <- DocumentTermMatrix(corpus_us))

# Estimamos la TF-IDF (Term Frequency - Inverse Document Frequency)
(dtm_tfi <- weightTfIdf(dtm))

# Removemos una parte de los términos
# Una matriz término-documento en la que se eliminan los términos que aparecen 0 veces en un documento. 
# Es decir, la matriz resultante sólo contiene términos con un factor de dispersión menor que el de dispersión.
(dtm_tfi <- removeSparseTerms(dtm_tfi, 0.80)) # con el valor 0.999 dejamos todos los términos.

# La transformamos en matriz común
(dtm_tfi_matrix <- as.matrix(dtm_tfi))

##### Estimamos la similitud coseno

# La similitud coseno es una medida de la similitud existente entre dos vectores en un espacio que 
# posee un producto interior con el que se evalúa el valor del coseno del ángulo comprendido entre ellos. 
# Esta función trigonométrica proporciona un valor igual a 1 si el ángulo comprendido es cero, es decir 
# si ambos vectores apuntan a un mismo lugar.

# La función dist() calcula y devuelve la matriz de similitud entre filas o columnas de una matriz,
# así como la matriz de distancia cruzada entre dos matrices/marcos de datos/listas.

(dist_cos <- proxy::dist(dtm_tfi_matrix, method = "cosine"))

# Aplicamos la función hclust() para análisis jerárquico de conglomerados.
# Esta función hace el análisis jerárquico de conglomerados sobre un conjunto de 
# disimilitudes en base a distintos métodos.
# Para la función ‘hclust’, se requieren los valores de distancia que se pueden calcular en R 
# utilizando la función ‘dist’, como hicimos arriba. 
# La medida predeterminada para la función dist es ‘Euclidiana’, sin embargo, puede cambiarla 
# con el argumento del método. Con esto, también necesitamos especificar el método de 
# vinculación que queremos usar (es decir, “complete”, “promedio”, “single”, “ward.D”)
# En este caso usamos el método "ward.D2".

# El método de varianza mínima de Ward ("ward.D2") puede ser definido e implementado recursivamente por 
# el algoritmo de Lance-Williams. El algoritmo de Lance-Williams consiste en una familia infinita de 
# aglomeración de algoritmos jerárquicos de clúster, los cuales son representados mediante una 
# forma recursiva para actualizar la distancia de clúster en cada paso (cada vez se mezcla un par 
# de clúster).En cada paso es necesario la optimización de la función objetivo (encontrar el par 
# de clúster óptimo a mezclar). La fórmula recursiva simplifica la búsqueda del par óptimo.

# La diferencia entre ward.D y ward.D2 es la diferencia entre los dos criterios de agrupación.
# El algoritmo Ward se implementa directamente de forma correcta solo en ward.D2, pero también 
# se puede usar ward.D si las distancias euclidianas (desde dist()) se elevan al cuadrado antes 
# de ingresarlas en el hclust() utilizando ward.D como método.

(Hierarchical_Clustering <- hclust(dist_cos, method = "ward.D2"))

# Determinamos dos grupos de ramificaciones con la función cutree().
# Corta un árbol en varios grupos, ya sea especificando 
# el número de grupos deseados o la altura de corte.
(Hierarchical_Clustering_2 <- cutree(Hierarchical_Clustering, k = 2))

# Escalamos los datos como putos en un plano.
# Escalamiento Multidimensional Clásico (MDS).
# MDS devuelve una solución óptima para representar los datos en un espacio de dimensiones inferiores, 
# donde se especifica el número k. Por ejemplo, elegir k = 2 optimiza las ubicaciones de los objetos 
# para un diagrama de dispersión bidimensional.
(puntos <- cmdscale(dist_cos, k = 2))

# Visualizamos
plot(puntos, main = 'Agrupamiento Jerárquico', col = as.factor(Hierarchical_Clustering_2), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')

# Incorporamos la predicción como nueva variable en la base
(notas_clas_4 <- notas_norm_key_words_lemmas_entities_acc %>% 
    mutate(clase_sh = as.vector(Hierarchical_Clustering_2),
           clase_sh = ifelse(clase_sh == 2, 'no_conflicto', 'conflicto'),
           correspondencia = ifelse(clase == clase_sh, 'SI', 'NO')))

# Vemos su rendimiento
# Absoluto
table(notas_clas_4$correspondencia)
# Porcentaje
prop.table(table(notas_clas_4$correspondencia))
# Matriz 
table(notas_clas_4$clase, notas_clas_4$clase_sh, dnn = c("Etiq Manual", "Etiq cluster"))

### Modelado de tópicos ####--------------------------------------------------------------------------

# El MT descubre tópicos que ocurren en una colección de documentos (corpus) utilizando un modelo probabilístico. 
# Permite revelar estructuras semánticas dentro de un cuerpo de texto. Un documento sobre un tema específico 
# tendrá ciertas palabras que aparecerán con más frecuencia que otras. Los términos que exhiben similitud 
# se agrupan y el tema se determina en función de la probabilidad estadística de ocurrencia de esas palabras.
# Los documentos no se limitan a discutir un solo tema. Frecuentemente abordan múltiples temas. Los modelos 
# temáticos revelan estructuras semánticas latentes y ofrecen información sobre datos no estructurados, 
# el tipo de datos que impregna Internet. Algunos modelos de temas populares incluyen 
# LDA (asignación latente de Dirichlet), LSA (análisis semántico latente, usa la puntuación tf-idf). 
# Y hay más: pLSA, NMF, BERTopic, Top2Vec, BTM, STM y otros

# Fuente: https://blog.marketmuse.com/glossary/topic-modeling-definition/
# Fuente: https://towardsdatascience.com/topic-modeling-with-lsa-plsa-lda-nmf-bertopic-top2vec-a-comparison-5e6ce4b1e4a5
# Fuente: https://xiaohuiyan.github.io/paper/BTM-TKDE.pdf
# Fuente: https://warin.ca/shiny/stm/
# Fuente: https://towardsdatascience.com/introduction-to-the-structural-topic-model-stm-34ec4bd5383
# Fuente: https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
# Fuente: https://rpubs.com/jmhardaw/eci588-final-project

# Creamos un corpus
(corpus_notas_norm <- corpus(notas_norm_key_words_lemmas_entities_acc$nota_norm,
                             docnames = notas_norm_key_words_lemmas_entities_acc$id))

# Creamos la variable id
corpus_notas_norm$id    <- notas_norm_key_words_lemmas_entities_acc$id

# Creamos la variable clase
corpus_notas_norm$clase <- notas_norm_key_words_lemmas_entities_acc$clase

# Imprimimos las variables creadas
docvars(corpus_notas_norm)

# Imprimimos un resumen del contenido del corpus
summary(corpus_notas_norm, 5)

# Tokenizamos las notas y, después de eliminar las palabras vacías y la puntuación, nos quedamos con el 5% de los términos más frecuentes y
# nos quedamos con los términos que aparecen en menos del 10% de todos los documentos,
# esto para centrarnos en los rasgos comunes pero distintivos.
(tokens_notas <- quanteda::tokens(corpus_notas_norm, remove_punct = TRUE, remove_number = TRUE, remove_symbol = TRUE) %>% 
    tokens_remove(pattern = c('lunes','martes','miércoles','jueves','viernes','sábado','domingo')))

# Creamos la matriz documento-término
(dfm_notas <- dfm(tokens_notas) %>% 
    dfm_trim(min_termfreq = 0.7, termfreq_type = "quantile",
             max_docfreq = 0.09, docfreq_type = "prop"))

# Imprimimos un resumen del contenido de la matriz
summary(dfm_notas)

# Creamos el modelo de tópicos con la LDA

# La Latent Dirichlet Allocation (LDA) es un modelo generativo que permite que conjuntos de observaciones 
# puedan ser explicados por grupos no observados que explican por qué algunas partes de los datos son similares. 
# Por ejemplo, si las observaciones son palabras en documentos, presupone que cada documento es una mezcla de un 
# pequeño número de tópicos y la aparición de cada palabra en un documento se debe a una de los tópicos a los que el documento pertenece.
# Con la LDA cada documento puede verse como una conjunción de varios tópicos. 

# textmodel_lda() devuelven una lista de parámetros del modelo. theta es la distribución 
# de los temas sobre los documentos; phi es la distribución de las palabras sobre los temas. 
# alpha y beta son la pequeña constante añadida a la frecuencia de las palabras para estimar theta y phi, 
# respectivamente, en el muestreo de Gibbs. 
set.seed(890)
(tm_lda_notas <- textmodel_lda(dfm_notas, k = 2))

# guardamos el modelo
#saveRDS(tm_lda_notas,'tm_lda_notas_seed_123.rds')
#saveRDS(tm_lda_notas,'tm_lda_notas_seed_678.rds')
#saveRDS(tm_lda_notas,'tm_lda_notas_seed_890.rds')

# Imprimimos los primeros 30 términos de los tópicos
terms(tm_lda_notas, 30)

# Asignar los tópicos como una nueva variable a nivel de documento
dfm_notas$topico <- topics(tm_lda_notas)

# Tabla de frecuencia de los tópicos
table(dfm_notas$topico)

# Incorporamos la predicción como nueva variable en la base
(notas_clas_5 <- notas_norm_key_words_lemmas_entities_acc %>% 
    mutate(clase_tm = dfm_notas$topico,
           clase_tm = ifelse(clase_tm == 'topic1', 'no_conflicto', 'conflicto'),
           correspondencia = ifelse(clase == clase_tm, 'SI', 'NO')))

# Vemos su rendimiento
# Absoluto
table(notas_clas_5$correspondencia)
# Porcentaje
prop.table(table(notas_clas_5$correspondencia))
# Matriz
table(notas_clas_5$clase, notas_clas_5$clase_tm, dnn = c("Etiq Manual", "Etiq tópico"))

# Referencias de interés --------------------------------------------------

# KNN- Vecinos más Cercanos: https://dialnet.unirioja.es/servlet/articulo?codigo=7451370
# Clustering y heatmaps: https://rpubs.com/Joaquin_AR/310338
# Agglomerative Clustering on a Directed Graph: https://sci-hub.se/10.1007/978-3-642-33718-5_31
# Conglomerados: https://www.fuenterrebollo.com/Economicas/ECONOMETRIA/SEGMENTACION/CONGLOMERADOS/conglomerados.pdf

# Cosine-SVM: https://www.naturalspublishing.com/files/published/2b29o974c204v6.pdf
# Blei, David M., Andrew Y. Ng, and Michael I. Jordan. 2003. “Latent Dirichlet Allocation.” The Journal of Machine Learning Research 3(1): 993-1022.
# Lu, B., Ott, M., Cardie, C., & Tsou, B. K. 2011. “Multi-aspect sentiment analysis with topic models". Proceeding of the 2011 IEEE 11th International Conference on Data Mining Workshops, 81–88.

# Topic Model: https://elmundodelosdatos.com/topic-modeling-gensim-similitud-textos/

# Documentación
# textreuse: https://cran.r-project.org/web/packages/textreuse/vignettes/textreuse-introduction.html
# r-for-newspaper-data: https://bookdown.org/yann_ryan/r-for-newspaper-data/detecting-text-reuse-in-newspaper-articles-.html

# Glosarios tags
# https://cs.nyu.edu/~grishman/jet/guide/PennPOS.html

# Tutorial Random Forest: https://rpubs.com/AdSan-R/RFChurnTelco
# Tutorial Random Forest: https://rpubs.com/Joaquin_AR/255596
# Tutorial Random Forest: https://rstudio-pubs-static.s3.amazonaws.com/592295_f0978d656eeb47b5a29f2cf5909e2507.html
# Tutorial Aglomerados Jerárquicos con R: https://www.datacamp.com/tutorial/hierarchical-clustering-R
# Análisis de conglomerados jerárquicos con R: https://uc-r.github.io/hc_clustering
# How to Perform Hierarchical Clustering using R: https://www.r-bloggers.com/2017/12/how-to-perform-hierarchical-clustering-using-r/
# Modelado de tópicos con quanteda: https://tutorials.quanteda.io/machine-learning/topicmodel/

# Topic Modeling: http://www.aic.uva.es/cuentapalabras/topic-modeling.html
# Modelado de topicos: https://bookdown.org/gaston_becerra/curso-intro-r/modelado-de-topicos.html
# Jure Leskovec, Anand Rajaraman, and Jeff Ullman, Mining of Massive Datasets (Cambridge University Press, 2011), ch. 3: http://infolab.stanford.edu/~ullman/mmds/ch3n.pdf
# Matthew Casperson, "Minhash for Dummies" (November 14, 2013): http://matthewcasperson.blogspot.com/2013/11/minhash-for-dummies.html
# LDA: https://medium.com/analytics-vidhya/latent-dirichelt-allocation-1ec8729589d4
# Métodos Jerárquicos de Análisis: https://www.ugr.es/~gallardo/pdf/cluster-3.pdf
# Ward’s Hierarchical Agglomerative Clustering Method: http://adn.biol.umontreal.ca/~numericalecology/Reprints/Murtagh_Legendre_J_Class_2014.pdf