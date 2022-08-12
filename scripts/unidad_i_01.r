# Unidad I

# Introducción a las ciencias sociales computacionales y a las humanidades digitales
# ✅ Introducción a la sintaxis básica 
# La sintaxis en R es parecida a la de otros lenguajes de programación. 
# Las siguientes son las definiciones básicas de la sintaxis de R.
# Los espacios en blanco **NO** se tienen en cuenta en el código:

Objeto_01<-"Hola Mundo 1"
Objeto_02 <- "Hola Mundo 2" # esta es la forma recomendada (un espacio en blanco ' ' entre el nombre del objeto 'Objeto_02', la flecha de asignación '<-' y el contenido del objeto "Hola Mundo 2")
Objeto_03  <-  "Hola Mundo 3"
Objeto_04   =   "Hola Mundo 4"
Objeto_05    <-   

print(Objeto_01)
print(Objeto_02)
print(Objeto_03)
print(Objeto_04)
print(Objeto_05)

# R distingue entre **MAYÚSCULAS** y **minúsculas**:

Objeto <- "Hola Mundo"
objetO <- "hola mundo"
print(Objeto)
print(objetO)

Objeto == objetO # ¿es igual?

Objeto != objetO # ¿es distinto?

print(objeto) # ¿hay un objeto llamado objeto?

# Como ya vimos, se pueden escribir comentarios

print("Se pueden escribir comentarios") # como este

# A diferencia de otros lenguajes de programación como PHP, 
# en R no es necesario terminar cada línea de código con ';' (`echo "Hola mundo";`):

print('Hola Mundo'); # incorrecto pero funciona
print('Hola Mundo')  # correcto

# Como ya habrán notado, R es un lenguaje de programación orientado a objetos

paste(
Objeto,
objetO,
Objeto_01,
Objeto_02,
Objeto_03,
Objeto_04,
Objeto_05,
sep = ' <||> '
)

# Pero, ¿qué es un objeto?
# ¿Hay algo en R que no sea un objeto?
# No, todo en R es un objeto: variables, datos, funciones, resultados, etc.
# Los principales objetos en R son los vectores, las matrices, los arrays, 
# los marcos de datos y las listas.

Vector <- c('a','b','c','d')
print(Vector)

Matriz <- matrix(data=1:25, nrow=5, ncol=5)
print(Matriz)

Array <- array(data=1:27, dim=c(3, 3, 3))
print(Array)

DataFrame <- data.frame(variable01 = 1:10, variable02 = letters[1:10], variable03 = rep(c(T, F),5))
print(DataFrame)

Lista <- list(df = DataFrame, a = Array, m = Matriz, v = Vector)
str(Lista)

print(Lista)

# Diferentes tipos de tipos de datos
# En R hay 5 tipos de datos básicos:

# logical
# numeric
# integer
# factor
# character


### Funciones básicas
# ¿En qué directorio estoy trabajando?
getwd()

# ¿En qué directorio quiero trabajar?
setwd('scripts')
getwd()

# Volver al directorio original
setwd('..')
getwd()

# ¿Qué otros directorios tengo disponibles?
list.dirs()

# ¿Qué archivos tengo disponibles en el directorio 'sample_data'?
list.files('scripts')

# ¿Cuál es la función para sumar?
a <- rep(2,10) # diez 2
b <- 1:10      # del 1 al 10
sum(a)
sum(b)
sum(a,b)

# ¿Cuál es la función para calcular la media?
mean(a)
mean(b)

# ¿Cuál es la función para calcular la frecuencia simple de un grupo de valores?
c <- c(rep('negro',5), rep('rojo',15), rep('verde',3), rep('azul',25), rep('marrón',55))
print(c)
tabla <- table(c)
tabla                                       # orden alfabético creciente
tabla[order(names(tabla), decreasing = T)]  # orden alfabético decreciente
tabla[order(tabla)]                         # frecuencia creciente
tabla[order(tabla, decreasing = T)]         # frecuencia decreciente

# ¿Cómo lo visualizo?
barplot(tabla)

# ¿Cómo elaborar un resumen estadístico de un set de datos?
summary(DataFrame)

