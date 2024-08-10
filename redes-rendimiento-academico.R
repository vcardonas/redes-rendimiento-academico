#=========================================================================#
#               Análisis de Redes: Rendimiento Académico                  #
#                   Por: Valentina Cardona Saldaña                        #
#=========================================================================#
# Limpiar consola
rm(list = ls())

#===================#
#### 1. Paquetes ####
#===================#

# Lista de paquetes que se necesitan
paquetes <- c("tidyr", "dplyr", "tidyverse", "magrittr", "igraph")
# Utilizar lapply para cargar cada paquete si aún no está instalado
paquetes_cargar <- lapply(paquetes, 
                          FUN = function(x){
                            if (!require(x, character.only = TRUE)) {
                              install.packages(x, dependencies = TRUE)
                              library(x, character.only = TRUE)
                            }
                          }
)

#================================#
#### 2. Directorio de trabajo ####
#================================#
# Conocer el directorio actual
getwd()

# Directorio de carpeta
myPath <- "/Users/valentinacardona/Documents/Code Nerd/GitHub/redes-rendimiento-academico"
setwd(mypath)

#=================#
#### 3. Datos ####
#=================#
list.files("Datos")

# Importar RData
load("Datos/Datos_COL.RData")

# Encontrar la posición de la columna por el nombre
which(names(STU_QQQ_COL) == "MATH")

unique(STU_QQQ_COL$ICTRES)

STU_QQQ_COL

STU_QQQ_COL %<>% 
  mutate(MATH = rowMeans(across(PV1MATH:PV10MATH), na.rm = TRUE),
         READ = rowMeans(across(PV1READ:PV10READ), na.rm = TRUE),
         SCIE = rowMeans(across(PV1SCIE:PV10SCIE), na.rm = TRUE))

Data <- STU_QQQ_COL[,c(4,7,9,26,967,970,975,976,977,981,982,983,984,985,986,
                       # WLE
                       990:1029, 1037:1083, 
                       # Escalas PISA
                       1279:ncol(STU_QQQ_COL))]
Data %<>%
  mutate(across(everything(), ~ as.numeric(as.character(.)), .names = "{col}")) %>% 
  dplyr::select(where(~ !all(is.na(.))))

# Crear la matriz de correlación y tomar los valores absolutos
cor_matrix <- cor(Data, use = "complete.obs") %>%
  as.data.frame() %>% 
  mutate(across(everything(), abs))
cor_matrix <- as.matrix(cor_matrix)

# Reemplazar los valores en la matriz con 0 en la diagonal
diag(cor_matrix) <- 0

# Crear una matriz de pesos con 0 si la correlación es menor o igual a 0.3
weights_matrix <- ifelse(cor_matrix > 0.3, cor_matrix, 0)

# Crear el grafo a partir de la matriz de adyacencia con pesos
g <- graph_from_adjacency_matrix(weights_matrix, mode = "undirected", weighted = TRUE)

# Plotear el grafo
plot(g, edge.width = E(g)$weight * 5)


