##################################################################################
# 01/03/2022
# SCRIPT 2
# LIVRO: APLICACOES DO LIDAR PARA O INVENTARIO FLORESTAL - ENFOQUE UNIDADE DE AREA
# AUTORES: DALLA CORTE, ET AL. 2022
# UNIVERSIDADE FEDERAL DO PARANA - UFPR
# CURSO DE ENGENHARIA FLORESTAL
##################################################################################
#Definindo do diretório
setwd("C:/Livro_Lidar/Item2_Metricas/")

##################################################################################
############### GERACAO DAS METRICAS PARA O INVENTARIO FLORESTAL #################
##################################################################################
#pacotes utilizados neste item
install.packages("lidR")
install.packages("rgl")
require(lidR)
require(rgl)

# Abrindo e inspecionando as nuvens de pontos
dadosLAS_norm<-readLAS("C:/Livro_Lidar/Item2_Metricas/Item_2_Dados_Lidar/las_normalizada.las")

#Recorte da nuvem para area de interesse - unidade amostral
#opcao 1 - recorte com arquivo shapefile
parcela<-shapefile("C:/Livro_Lidar/Item2_Metricas/Item_2_shapefile/ua.shp")
ua1<-clip_roi(dadosLAS_norm, parcela)
plot(ua1, size=15, bg = "white")
rgl::box3d(color="black")
rgl::axes3d(color="black", labels = TRUE)

#Opcao 2 - recorte com ponto central da parcela e raio
ua2 <- clip_circle(dadosLAS_norm, 558800, 7306500, 13.82)
plot(ua2, size=15, bg = "white")
rgl::box3d(color="black")
rgl::axes3d(color="black", labels = TRUE)


#Metrica de Z maximo para a toda a nuvem de pontos
metrics <- cloud_metrics(dadosLAS_norm, max(Z))
metrics

#Metricas default para a unidade amostral
metrics_ua<-cloud_metrics(ua2, .stdmetrics_z)

#Salvando a tabela de dados "cloud metrics"
tabela<- write.csv2(metrics_ua,"Item_2_Tabelas/test.csv")
tabela<-read.csv2("Item_2_Tabelas/test.csv")
View(tabela)