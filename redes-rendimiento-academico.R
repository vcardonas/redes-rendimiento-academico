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
paquetes <- c("readxl", "tidyr", "dplyr", "tidyverse", "magrittr", "igraph", "qgraph")
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

# Calcular media de las áreas
datos <- STU_QQQ_COL %>% 
  left_join(CRT_COG_COL[c("CNTSTUID", paste0("PV", 1:10, "CRTH_NC"))], by = "CNTSTUID") %>% 
  mutate(MATH = rowMeans(across(PV1MATH:PV10MATH), na.rm = TRUE),
         READ = rowMeans(across(PV1READ:PV10READ), na.rm = TRUE),
         SCIE = rowMeans(across(PV1SCIE:PV10SCIE), na.rm = TRUE),
         CRTH = rowMeans(across(PV1CRTH_NC:PV10CRTH_NC), na.rm = TRUE))

# Seleccionar variables de estudio
datos %<>%
  dplyr::select(
    # Identificados
    CNTSTUID,
    # Escalas
    MATH, READ, SCIE, CRTH,
    # Factores demográficos
    ST004D01T, STRATUM, REGION, IMMIG,
    # Factores socioeconómicos
    MISCED, FISCED, ICTRES, HOMEPOS, ESCS,
    # Factores educativos
    RELATST, BELONG, BULLIED, FEELSAFE, SCHRISK,
    SCHSUST, LEARRES, PROBSELF, PARINVOL, PQSCHOOL,
    # Factores psicológicos
    PERSEVAGR, CURIOAGR, COOPAGR, EMOCOAGR, GROSAGR,
    # Factores contextuales
    WORKPAY, WORKHOME, FAMSUP, 
    # Factores de aprendizaje
    STUDYHMW, REPEAT, MISSSC, SKIPPING, FAMSUPSL,
    # Matemáticas
    MATHPREF, MATHEASE, MATHMOT, DISCLIM, TEACHSUP,
    FAMCON, ANXMAT, MATHPERS,
    # Pensamiento creativo
    CREATEFF, CREATSCH, CREATFAM, CREATAS, CREATOOS, CREATOP, 
    OPENART, IMAGINE,
    CREATHME, CREATACT, CREATOPN, CREATOR
    )

# Importar CodeBook
list.files("Datos")
codebook <- read_excel("Datos/CY08MSP_CODEBOOK_27thJune24.xlsx", sheet = "CY08MSP_STU_QQQ")
labels <- codebook %>%
  fill(NAME, VARLABEL, TYPE, FORMAT, VARNUM, MINMAX, .direction = "down") %>% 
  drop_na(VAL)

# Usar codificaciones numéricas
for (col in names(datos)) {
  if (col %in% labels$NAME) {
    label_map <- labels %>%
      filter(NAME == col) %>%
      select(LABEL, VAL) %>%
      deframe()
    datos[[col]] <- recode(datos[[col]], !!!label_map)
  }
}

# Convertir todas las columnas a numéricas
datos %<>% mutate_all(as.numeric)

# Verificar
## Si hay columnas con valores nulos > 50% de los datos
tmp <- colSums(is.na(datos)) / nrow(datos) * 100
names(tmp[tmp > 50])

#=========================#
#### 4. Preparar grafo ####
#=========================#

# Crear la matriz de correlaciones parciales
cor_matrix <- cor_auto(datos[-1], forcePD = TRUE)

# Reemplazar los valores en la matriz con 0 en la diagonal
diag(cor_matrix) <- 0

# Crear el grafo a partir de la matriz de adyacencia con pesos
g <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected", weighted = TRUE)
graph <- qgraph(cor_matrix,
                graph = "glasso",
                sampleSize = nrow(datos),
                layout = "spring")

#================================#
#### 5. Análisis exploratorio ####
#================================#

# Visualización 
plot(g, edge.width = E(g)$weight * 5)

centralityPlot(g)

qgraph(cor_matrix, title = "Correlation Graph", labels = colnames(cor_matrix))
qgraph(cor_matrix, graph = "pcor", layout = "spring", cut = 0)

VSS.scree(cor_matrix)

getWmat(graph)
centralityPlot(graph)

#================================#
#### 6. Modelo ####
#================================#

#=============================#
#### 7. Validación cruzada ####
#=============================#














