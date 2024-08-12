#=========================================================================#
#               Análisis de Redes: Rendimiento Académico                  #
#                   Por: Valentina Cardona Saldaña                        #
#=========================================================================#
# Limpiar consola
rm(list = ls())

#===================#
#### 1. Paquetes ####
#===================#
#devtools::install_github("mohamed-180/gtranslate")

# Lista de paquetes que se necesitan
paquetes <- c("readxl", "tidyr", "dplyr", "tidyverse", "magrittr", "igraph", 
              "qgraph", "mgm", "viridis", "gtranslate", "corrplot", "RColorBrewer", "stats")
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
setwd(myPath)

#=================#
#### 3. Datos ####
#=================#
list.files("Datos")

# Importar RData
load("Datos/Datos_COL.RData")

# Calcular media de las áreas
datos <- STU_QQQ_COL %>% 
  left_join(CRT_COG_COL[c("CNTSTUID", paste0("PV", 1:10, "CRTH_NC"))], by = "CNTSTUID") %>% 
  dplyr::mutate(MATH = rowMeans(across(PV1MATH:PV10MATH), na.rm = TRUE),
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
    # Factores psicológicos
    PERSEVAGR, CURIOAGR, COOPAGR, GROSAGR, ANXMAT,
    # Factores individuales
    ST004D01T, CREATOOS, LEARRES, PROBSELF, EXERPRAC, STUDYHMW,
    # Factores socioeconómicos
    MISCED, FISCED, ICTRES, ESCS, STRATUM, WORKPAY,
    # Contexto escolar
    CREATAS, RELATST, BELONG, BULLIED, FEELSAFE, REPEAT, MISSSC,
    # Contexto familiar
    FAMSUP, FAMSUPSL, WORKHOME
    ) %>% 
  arrange()

# Importar CodeBook
codebook <- read_excel("Datos/CY08MSP_CODEBOOK_27thJune24.xlsx", sheet = "CY08MSP_STU_QQQ")
labels <- codebook %>%
  fill(NAME, VARLABEL, TYPE, FORMAT, VARNUM, MINMAX, .direction = "down") %>% 
  drop_na(VAL)

datos %<>% 
  # Convertir Género a variable dummy
  dplyr::mutate(GENERO = ifelse(ST004D01T == "Male", 0, 
                                ifelse(ST004D01T == "Female", 1, ST004D01T))) %>% 
  dplyr::mutate(STRATUM = tolower(STRATUM)) %>% 
  # Desagregar Región en variables dummy
  dplyr::mutate(SECTOR = ifelse(grepl("private", STRATUM), 0,
                                ifelse(grepl("public", STRATUM), 1, STRATUM))) %>% 
  dplyr::select(-c(ST004D01T, STRATUM))

# Usar codificaciones numéricas
for (col in names(datos)) {
  if (col %in% labels$NAME) {
    label_map <- labels %>%
      filter(NAME == col) %>%
      dplyr::select(LABEL, VAL) %>%
      deframe()
    datos[[col]] <- recode(datos[[col]], !!!label_map)
  }
}

# Convertir todas las columnas a numéricas
datos %<>% mutate_all(~ ifelse(is.na(.), NA, as.numeric(as.character(.))))

head(datos)

rm(label_map, paquetes, paquetes_cargar)

#================#
#### 4. Grafo ####
#================#
datos <- datos %>% dplyr::select(-CNTSTUID)

# Crear grupos
groups_list <- list(
  "Dominios" = c("MATH", "READ", "SCIE", "CRTH"),
  "Factores Psicológicos" = c("PERSEVAGR", "CURIOAGR", "COOPAGR", "GROSAGR", "ANXMAT"),
  "Factores Individuales" = c("GENERO", "CREATOOS", "LEARRES", "PROBSELF", "EXERPRAC", "STUDYHMW"),
  "Factores Socioeconómicos" = c("MISCED", "FISCED", "ICTRES", "ESCS", "SECTOR", "WORKPAY"),
  "Contexto Escolar" = c("CREATAS", "RELATST", "BELONG", "BULLIED", "FEELSAFE", "REPEAT", "MISSSC"),
  "Contexto Familiar" = c("FAMSUP", "FAMSUPSL", "WORKHOME")
)
lists <- unlist(mapply(rep, names(groups_list), sapply(groups_list, length), SIMPLIFY = FALSE))

# Asignar color a grupos
groups_color <- viridis(length(groups_list))

# Ordenar datos
datos <- datos[unlist(groups_list)]

# Extraer nombres
gnames <- codebook$VARLABEL[match(colnames(datos), codebook$NAME)]
gnames[is.na(gnames)] <- c("Matemáticas", "Lectura", "Ciencias", "Pensamiento creativo", "GENERO", "SECTOR")

# Traducir al español
gnames_es <- translate(gnames, from = "en", to = "es")
gnames_es <- gsub(" \\(acuerdo\\)", "", gnames_es)
gnames_es <- gsub(" \\(WLE\\)", "", gnames_es)
gnames_es <- gsub("de la escuela", "del colegio", gnames_es)
gnames_es <- gsub("la escuela", "el colegio", gnames_es)
#View(cbind(colnames(datos), gnames, gnames_es))

# Segmentar
## Función para insertar \n
insert_newline <- function(text, n) {
  words <- unlist(strsplit(text, " "))
  if (length(words) > n) {
    return(paste(c(paste(words[1:n], collapse = " "),
                   paste(words[(n + 1):length(words)], collapse = " ")), collapse = "\n"))
  } else {
    return(text)
  }
}
gnames_es2 <- sapply(gnames_es, insert_newline, n = 12)

# Crear la matriz de correlaciones parciales
cor_matrix <- cor_auto(datos)

# Crear el grafo a partir de la matriz de adyacencia con pesos
set.seed(2024)
g <- qgraph(cor_matrix,
            # Aspectos formales del grafo
            layout = "spring", repulsion = 1, palette = 'pastel',
            groups = lists, labels = colnames(cor_matrix), nodeNames = gnames_es2, 
            legend.mode = "style2", legend.cex = 0.45,
            vsize = ifelse(colnames(datos) %in% groups_list[["Dominios"]], 5.5, 5),
            width = 14, height = 8)

#================================#
#### 5. Análisis exploratorio ####
#================================#
dev.off()

# Corrplot
png(filename = "Imagenes/MatrizCorr.png", width = 800, height = 800)
colors <- brewer.pal(length(unique(lists)), "Dark2")[match(lists, unique(lists))]
corrplot(cor_matrix, 
         method = "circle", tl.cex = 1.8, cl.cex = 1.8,
         diag = FALSE,
         tl.col = colors, 
         addgrid.col = "grey90")
dev.off()

# Visualización 
png(filename = "Imagenes/Grafo_MatrizCorr.png", width = 1400, height = 800)
plot(g)
dev.off()

# Medidas de centralidad
png(filename = "Imagenes/Medidas_MatrizCorr.png", width = 600, height = 400)
centralityPlot(g, include = c("Betweenness", "Closeness", "Strength"))
dev.off()

#==================#
#### 6. G-LASSO ####
#==================#
set.seed(2024)
png(filename = "Imagenes/G_LASSO.png", width = 1400, height = 800)
glasso <- qgraph(cor_matrix,
            # G-Lasso
            graph = "glasso",
            sampleSize = nrow(datos),
            # Aspectos formales del grafo
            layout = "spring", repulsion = 1, palette = 'pastel',
            groups = lists, labels = colnames(cor_matrix), nodeNames = gnames_es2, 
            legend.mode = "style2", legend.cex = 0.45,
            vsize = ifelse(colnames(datos) %in% groups_list[["Dominios"]], 5.5, 5),
            width = 14, height = 8)
plot(glasso)
dev.off()

png(filename = "Imagenes/Medidas_G_LASSO.png", width = 600, height = 400)
centralityPlot(glasso, include = c("Betweenness", "Closeness", "Strength"))
dev.off()

## OTROS
# # MGM
# cat_vars <- c("EXERPRAC","FISCED", "GENERO", "MISCED", "MISSSC", "REPEAT",
#               "SECTOR", "STUDYHMW", "WORKHOME", "WORKPAY")
# mgmtype <- ifelse(colnames(datos) %in% cat_vars, "c", "g")
# mgmlevel <- datos %>%
#   reframe(across(everything(), ~ ifelse(cur_column() %in% cat_vars, length(unique(na.omit(.))), 1))) %>%
#   unlist() %>%
#   as.numeric() %>%
#   c()
# 
# fit_mgm <- mgmfit(as.matrix(datos),
#                   type = mgmtype, level = mgmlevel, missings = 'casewise.zw',
#                   k = 2, lambdaSel = "CV", lambdaFolds = 10, ruleReg = "AND", binarySign = TRUE)







