##################################################################################
# 01/03/2022
# SCRIPT 3 - EXPLORANDO AS METRICAS PARA O INVENTARIO FLORESTA
# LIVRO: APLICACOES DO LIDAR PARA O INVENTARIO FLORESTAL - ENFOQUE UNIDADE DE AREA
# AUTORES: DALLA CORTE, ET AL. 2022
# UNIVERSIDADE FEDERAL DO PARANA - UFPR
# CURSO DE ENGENHARIA FLORESTAL
##################################################################################

##################################################################################
############### EXPLORANDO AS METRICAS PARA O INVENTARIO FLORESTAL ###############
##################################################################################

########## PREPARACAO DO AMBIENTE RSTUDIO ########## -------------------------------------------------------------------------

#Indicando o diretorio
setwd("C:/Livro_Lidar/Item3_Correlacao/")

#Instalando e carregando pacotes
install.packages("dplyr")
install.packages("corrplot")
require(dplyr)
require(corrplot)

########## ANALISES DAS METRICAS DAS UNIDADES AMOSTRAIS ########## -------------------------------------------------------------------------
#Abertura da tabela com os dados biometricos e com as metricas do Lidar
dados_bancodados<-read.csv2("C:/Livro_Lidar/Item3_Correlacao/Item_3_Tabelas/DADOS_METRICAS_PARCELAS.csv", header = TRUE)
View(dados_bancodados)

#Selecionando as variaveis que podem participar do calculo da correlacao
dados_cor <- dados_bancodados%>%select(c(16:22, 36:53))
View(dados_cor)

sapply(dados_cor, is.numeric)

#Calculo das correlacoes entre as variaveis
correlacao<-cor(dados_cor)
View(correlacao)

#Salvando a tabela de correlacao
write.csv2(correlacao,"C:/Livro_Lidar/Item3_Correlacao/Item_3_Tabelas/correlacao.csv")

#Construindo a matriz de correlacao entre as variaveis
res1 <- cor.mtest(correlacao, conf.level = .05)
jpeg("C:/Livro_Lidar/Item3_Correlacao/Item_3_Figuras/correlacao.jpeg", width = 5800, height = 5000, units = "px", res = 300, quality = 5000)
corrplot.mixed(correlacao, lower = "number", upper = "ellipse", lower.col = "black", 
               tl.cex=1,number.font = 2, cl.ratio = 0.2, order = "hclust", p.mat = res1$p, sig.level = .95)
dev.off()

#salvando a tabela de dados pareados
write.csv2(dados_cor,"C:/Livro_Lidar/Item3_Correlacao/Item_3_Tabelas/dados_pareados.csv")
