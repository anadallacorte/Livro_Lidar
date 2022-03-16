##################################################################################
# 01/03/2022
# SCRIPT 1 - PROCESSAMENTO INICIAL
# LIVRO: APLICACOES DO LIDAR PARA O INVENTARIO FLORESTAL - ENFOQUE UNIDADE DE AREA
# AUTORES: DALLA CORTE, ET AL. 2022
# UNIVERSIDADE FEDERAL DO PARANA - UFPR
# CURSO DE ENGENHARIA FLORESTAL
##################################################################################

##################################################################################
###################  PROCESSAMENTO INICIAL - LIVRO LIDAR #########################
##################################################################################

########## PREPARACAO DO AMBIENTE RSTUDIO ########## -------------------------------------------------------------------------------------

# Limpando a memoria do RStudio
rm(list = ls())

#Verificando o diret√≥rio
getwd()

# Direcionando para local especifico no computador
setwd("C:/Livro_Lidar/Item1_preliminar/") 

# Instalando os pacotes necessarios e carregando os mesmos
install.packages("lidR")
install.packages("ggplot2")
install.packages("RCSF")

require("lidR")
require("ggplot2")
require("RCSF")


########## EXPLORACAO INICIAL DA NUVEM DE PONTOS ########## -------------------------------------------------------------------------------------

# Abrindo e inspecionando as nuvens de pontos
dadosLAS<-readLAS("C:/Livro_Lidar/Item1_preliminar/Item_1_Dados_Lidar/dadoslidar.las", select = "xyzia")
plot(dadosLAS, bg = "white", colors = "Intensity", legend = FALSE)

#Conhecendo o cabecalho dos dados
head<-head(dadosLAS)
View(head)

write.csv2(head, "Item_1_Tabelas/head.csv")

#Informacao sobre a nuvem de pontos
las_check(dadosLAS, print = TRUE)

#Histograma de valores de Z
hist(dadosLAS@data$Z, col = height.colors(19),  main = "Histograma (Z)", xlab = "VariaÁ„o das elevaÁıes (m)",  ylab = "FrequÍncia") 

#Parametros espaciais
print(dadosLAS)

########## PROCESSAMENTO E MANIPULACAO DA NUVEM DE PONTOS ##########---------------------------------------------------------------------------------------------------------

#FILTRAGEM E CLASSIFICACAO DOS PONTOS DE SOLOS
dadosLAS1 <- filter_duplicates(dadosLAS)
dados_solo <- classify_ground(dadosLAS1, csf())
plot(dados_solo, bg= "white", color = "Classification")

#Plotando o perfil de elevacao entre dois pontos na nuvem
plot_crossection <- function(las,
                             p1 = c(x1, y1),
                             p2 = c(x2, y2),
                             width = 4, colour_by = NULL)
  
{colour_by <- enquo(colour_by)
data_clip <- clip_transect(las, p1, p2, width)
p <- ggplot(data_clip@data, aes(X,Z)) + geom_point(size = 0.5) + coord_equal() + theme_minimal()
if (!is.null(colour_by))
  p <- p + aes(color = !!colour_by) + labs(color = "")
return(p) }

p1<- c(558700,7306200)
p2<- c(558850,7306500)

las=dados_solo
jpeg("C:/Livro_Lidar/Item1_preliminar/Item_1_Imagens/crossection.jpeg", width = 770, height = 350, units = "px", res = 150, quality = 5000)
plot_crossection(dados_solo, p1=p1, p2=p2, colour_by= factor(Classification))
dev.off()

#DTM (Digital Terrain Model)
DTM_dados <- grid_terrain(dados_solo, res = 1, algorithm = tin())

jpeg("C:/Livro_Lidar/Item1_preliminar/Item_1_Imagens/DTM_dados.jpeg", width = 770, height = 990, units = "px", res = 150, quality = 5000)
plot(DTM_dados, main = 'DTM (m)')
dev.off()

#modelo digital de terreno em 3D
plot_dtm3d(DTM_dados, bg= "white")

#DSM (Digital Surface Model)
DSM_dados <- grid_canopy(dados_solo, 1, p2r())

jpeg("C:/Livro_Lidar/Item1_preliminar/Item_1_Imagens/DSM_dados.jpeg", width = 770, height = 990, units = "px", res = 150, quality = 5000)
plot(DSM_dados, main = 'DSM (m)')
dev.off()

#normalizacao da nuvem de pontos
las_norm <- normalize_height(dados_solo, tin())
plot(las_norm, legend=TRUE, bg = "white")

#Salvando a nuvem normalizada
writeLAS(las_norm,"C:/Livro_Lidar/Item1_preliminar/Item_1_Dados_Lidar/las_normalizada.las")

#CHM (Canopy Height Model)
CHM_dados <- grid_canopy(las_norm, 1, p2r())

jpeg("C:/Livro_Lidar/Item1_preliminar/Item_1_Imagens/CHM_dados.jpeg", width = 770, height = 990, units = "px", res = 150, quality = 5000)
plot(CHM_dados, col=height.colors(30), main = 'CHM (m)')
dev.off()

