###################
##### TAREA 1 #####
###################

## Fernanda Rendón
## 26 - 01 - 2022

library (Biostrings) # Paquete requerido para reconocer el archivo como sec
BiocManager::install ("XVector") # Dise que lo requiere pero ni así
library (XVector)

## Cargar archivo concatenado de secuencias
rna1 <- readRNAStringSet (choose.files ()) # Escoger el archivo desde la carpeta
rna1
# Pequeño detalle que no puedo cargar el archivo, pero mejor quise avanzar
# Creo que es porque no existe ya la función o no es compatible :c

# Alternativa:
rna2 <- readRNAStringSet ("first (1).fasta")


### 1. Encontrar secuencia de AÁ.
rnaAA1 <- translate (rna2) # Pasar la secuencia de RNA a AÁ
rnaAA1

### 2. ROSALIND

## 2.1 Counting DNA Nucleotides
## Con librerías especializadas:
alphabetFrequency (rna2) [ , 1:4] # Cuenta los nc A, T, G, U de cada secuencia.

## Con R base:
secuenciaEj <- readline (prompt = "Ingresa una secuencia de DNA: ")

for (secuenciaEj) {
  secEj <- strsplit (secuenciaEj, "") [[1]]
  Adenina <- countMatches ("A", secEj)
  Timina <- countMatches ("T", secEj)
  Citocina <- countMatches ("C", secEj)
  Guanina <- countMatches ("G", secEj)
  print (paste ("A:", Adenina, ";  T:", Timina, ";  C:", Citocina, ";  G:", Guanina))
}

# Primero se pide la secuencia de DNA, del tamaño que sea.
# Luego se define un ciclo FOR porque solo queremos de esa secuencia.
# También se podría hacer un ciclo WHILE y pedir más secuenicas pero ñe.
# A ese vector de un solo valor, lo separamos para identificar las bases por individual
# Contamos las veces que coincidan en el vector y las guardamos en otro vector
# Luego se imprime el numerito con el texto de ahí, un poco más elegante.


## 2.2 CG Content
## Con librerías especializadas:
CGcont <- (letterFrequency (rna2, letters = "CG", as.prob = TRUE)) * 100 
CGcont # Obtiene solo CG del alfabeto y lo multiplica por 100 como % en cada secuencia.

## Con R base:
secuenciaEj <- readline (prompt = "Ingresa una secuencia de DNA: ")

for (secuenciaEj) {
  secEj <- strsplit (secuenciaEj, "") [[1]]
  Citocina <- countMatches ("C", secEj)
  Guanina <- countMatches ("G", secEj)
  total <- nchar (secuenciaEj)
  Cporcentaje <- Citocina / total * 100
  Gporcentaje <- Guanina / total * 100
  CG <- ((Guanina + Citocina) / total) * 100
  print (paste ("C (%):", Cporcentaje, ";  G (%):", Gporcentaje, ";  CG (%):", CG))
}

# Ingresa una secuencia de DNA.
# Dentro del ciclo FOR, se crea un vector con la secuencia separada por bases
# De esta se cuentan las Cs, Gs y el total del vector original con nchar.
# Se obtiene el porcentaje por separado de C y G, y aparte de CGs.
# Finalmente se imprimen los tres porcentajes.


## 2.3 Complementing a strand of DNA

# Intenté hacer este pero me tomó más tiempo de lo esperado y al final no acabé.
# Igual dejaré mi intento aquí.

## Con librerías especializadas:
complement (rna2) # Genera el complemento de la sec

## Con R base:
secuenciaEj <- readline (prompt = "Ingresa una secuencia de DNA: ")
j <- 1

while (j <= nchar (secuenciaEj)) {
  secEj <- strsplit (secuenciaEj, "") [[1]]
  i <- secEj [j]
  if (i == "A") {
    secEj <- gsub ("A", "T", secEj)
  } else if (i == "T") {
    secEj <- gsub ("T", "A", secEj)
  } else if (i == "C") {
    secEj <- gsub ("C", "G", secEj)
  } else if (i == "G") {
    secEj <- gsub ("G", "C", secEj)
  } 
  secEj <- paste (secEj, collapse = "")
  j <- j + 1
  print (secEj)
}

# Pues sí sale pero sale mal jaja
# Bueno, lo intentamos

# Primero se pide la secuencia y se declaran los valores de secEj y j
# secEj es para guardar la secuencia con espacios entre bases
# j es para declarar que tome el primer valor del vector y para que luego
# le vaya sumando uno, así para que vaya intercambiando base por base
# Eso hasta que el ciclo se detenga, que esto es cuando j sea menor o igual al vector
# Es decir, que hasta que ocupe todas las bases en el vector.
# Y con Ifs se define que sustituya las letras en el vector con la secuencia separada
# Esta nueva secuencia creada se guarda y se le quitan los espacios introducidos
# Finalmente lo imprime.

