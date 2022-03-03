##################################################################################
#01/03/2022
# SCRIPT 5
# LIVRO: APLICACOES DO LIDAR PARA O INVENTARIO FLORESTAL - ENFOQUE UNIDADE DE AREA
# AUTORES: DALLA CORTE, ET AL. 2022
# UNIVERSIDADE FEDERAL DO PARANA - UFPR
# CURSO DE ENGENHARIA FLORESTAL
##################################################################################

########## PREPARACAO DO AMBIENTE RSTUDIO ########## -------------------------------------------------------------------------------------
#Indicando o diretório
setwd("C:/Livro_Lidar/Item5_IA/")#inserir caminho

# Instalando e carregando os pacotes necessarios
install.packages("dplyr")
install.packages("randomForest")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("caTools")
install.packages("miscTools")

require(dplyr)
require(randomForest)
require(corrplot)
require(ggplot2)
require(caTools)
require(miscTools)


# Abrindo o arquivo "dados_pareados.csv" 
dados_IA<-read.csv2("Item_5_Tabelas/dados_pareados.csv", header = T)

###RANDOM FOREST (RF)------------------------------------------------------------------------------------
#Arquivo temporario para o Random Forest
dados_RF<-dados_IA%>%dplyr::select(c(5, 9:26))
View(dados_RF)

#Atribuindo sementes
set.seed(123)

#Separando a base de dados em treinamento e teste com 80% para treinamento
split<-sample.split(dados_RF$volume_total, SplitRatio = 0.8)
base_treinamento_RF <-subset(dados_RF, split=TRUE)
base_teste_RF<- subset(dados_RF, split = FALSE)

#modelo RF
RF <- randomForest(x=base_treinamento_RF[2:19], y=base_treinamento_RF$volume_total, ntree = 501, mtry=7, importance = TRUE)

#predicao e estatistica
base_teste_RF$previsoesRF <- predict(RF, newdata = base_teste_RF[-1])
rsq_teste<-rSquared(base_teste_RF[['volume_total']], resid=base_teste_RF[['volume_total']]-base_teste_RF$previsoesRF)
rsq_teste

#plot
{
  g1 <- ggplot(base_teste_RF, aes(x =volume_total , y = base_teste_RF$previsoesRF)) + 
    labs(x = "Volume Observado", y = "Volume Estimado") +
    geom_point()+
    geom_abline(intercept = 0,slope = 1, linetype = "dashed")+
    geom_smooth(method=lm, se=FALSE, color = 'red', size = 0.8)+
    scale_x_continuous(limits = c(0,180))+
    scale_y_continuous(limits = c(0,180))+
    theme_bw()+
    theme(text = element_text(colour = 'black', family = 'Times', size = 12),
          axis.text.x = element_text(colour = 'black', family = 'Times', size = 12), 
          axis.text.y = element_text(colour = 'black', angle = 90,
                                     family = 'Times', size = 12),
          axis.title = element_text(colour = 'black', family = 'Times', size = 12),
          axis.title.y = element_text( family = 'Times', size = 12), 
          axis.title.x = element_text( family = 'Times', size = 12), 
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
  
  x11()
  g1 
}

#Salvando o modelo
saveRDS(RF, "Item_5_Modelos/RF.rds")

###SUPPORT VECTOR MACHINES (SVM)------------------------------------------------------------------------------------
# Instalando os pacotes necessarios e carregando os mesmos
install.packages("e1071")
install.packages("caTools")
install.packages("dplyr")
install.packages("miscTools")
install.packages("ggplot2")

require(e1071)
require(caTools)
require(dplyr)
require(miscTools)
require(ggplot2)


# Abrindo o arquivo "dados_pareados.csv" 
dados_IA<-read.csv2("Item_5_Tabelas/dados_pareados.csv", header = T)


#Atribuindo o arquivo para dados_SVM
dados_SVM <-dados_IA%>%select(c(5,9:26))
View(dados_SVM)

#Atribuindo sementes
set.seed(123)

#Modelo SVM
SVM<- svm(formula = dados_SVM$volume_total~., data = dados_SVM,
          type = 'eps-regression', kernel = 'radial')


#predicao e estatitica
dados_SVM$previsoesSVM <-predict(SVM, newdata = dados_SVM[-1])
rsq<-rSquared(dados_SVM[['volume_total']], resid =dados_SVM[['volume_total']]-dados_SVM$previsoesSVM)
rsq

#plot
{
  g1 <- ggplot(dados_SVM, aes(x =volume_total , y = dados_SVM$previsoesSVM)) + 
    labs(x = "Volume Observado", y = "Volume Estimado") +
    geom_point()+
    geom_abline(intercept = 0,slope = 1, linetype = "dashed")+
    geom_smooth(method=lm, se=FALSE, color = 'red', size = 0.8)+
    scale_x_continuous(limits = c(0,180))+
    scale_y_continuous(limits = c(0,180))+
    theme_bw()+
    theme(text = element_text(colour = 'black', family = 'Times', size = 12),
          axis.text.x = element_text(colour = 'black', family = 'Times', size = 12), 
          axis.text.y = element_text(colour = 'black', angle = 90,
                                     family = 'Times', size = 12),
          axis.title = element_text(colour = 'black', family = 'Times', size = 12),
          axis.title.y = element_text( family = 'Times', size = 12), 
          axis.title.x = element_text( family = 'Times', size = 12), 
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
  x11()
  g1 
}

#Salvando o modelo
saveRDS(SVM, "Item_5_Modelos/SVM.rds")

###REDE NEURAIS ARTIFICIAIS (RNA)------------------------------------------------------------------------------------
#setando a pasta

#importando pacotes
install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("miscTools")
install.packages("caTools")


require(caret)
require(dplyr)
require(ggplot2)
require(miscTools)
require(caTools)


# Abrindo o arquivo "dados_pareados.csv" 
dados_IA<-read.csv2("Item_5_Tabelas/dados_pareados.csv", header = T)
View(dados_IA)

#Atribuindo o arquivo para dados_SVM
dados_RNA <- dados_IA%>%dplyr::select(c(5,9:26))
View(dados_RNA)

#verificando dados faltantes
sum(is.na(dados_RNA))

#Definindo o numero de sementes
set.seed(123)

# Separação do banco de dados em treinamento e teste
split_RNA<-sample.split(dados_RNA$volume_total, SplitRatio = 0.8)
base_treinamento_rna <- dados_RNA[split_RNA,] 
base_teste_rna <- dados_RNA[-split_RNA,]

#Construindo modelo de treinamento
nnetGrid<-expand.grid(size = seq(from = 1, to = 10, by = 1),decay = seq(from = 0.1, to = 0.5, by = 0.1))

RNA<-train(volume_total~.,data=base_treinamento_rna,
           method="nnet",
           trControl=trainControl(method="cv",number=10),
           preProcess=c("scale", "center"),
           tuneGrid =nnetGrid,linout=TRUE)


#predicao e estatistica
base_teste_rna$previsoesRNA <-predict(RNA, newdata = base_teste_rna[-1])
rsq<-rSquared(base_teste_rna[['volume_total']], resid =base_teste_rna[['volume_total']]-base_teste_rna$previsoesRNA)
rsq

#plot
{
  g1 <- ggplot(base_teste_rna, aes(x =volume_total , y = base_teste_rna$previsoesRNA)) + 
    labs(x = "Volume Observado", y = "Volume Estimado") +
    geom_point()+
    geom_abline(intercept = 0,slope = 1, linetype = "dashed")+
    geom_smooth(method=lm, se=FALSE, color = 'red', size = 0.8)+
    scale_x_continuous(limits = c(0,180))+
    scale_y_continuous(limits = c(0,180))+
    theme_bw()+
    theme(text = element_text(colour = 'black', family = 'Times', size = 12),
          axis.text.x = element_text(colour = 'black', family = 'Times', size = 12), 
          axis.text.y = element_text(colour = 'black', angle = 90,
                                     family = 'Times', size = 12),
          axis.title = element_text(colour = 'black', family = 'Times', size = 12),
          axis.title.y = element_text( family = 'Times', size = 12), 
          axis.title.x = element_text( family = 'Times', size = 12), 
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
  x11()
  g1 
}

#salvando o modelo
saveRDS(RNA, "Item_5_Modelos/RNA.rds")

### k-Nearest Neighbor(RNA)------------------------------------------------------------------------------------
#importando pacotes
install.packages('caret')
install.packages("dplyr")
install.packages("ggplot2")
install.packages("miscTools")
install.packages("caTools")


require(caret)
require(dplyr)
require(ggplot2)
require(miscTools)
require(caTools)


# Abrindo o arquivo "dados_pareados.csv" 
dados_IA<-read.csv2("Item_5_Tabelas/dados_pareados.csv", header = T)
View(dados_IA)

#Atribuindo o arquivo para dados_SVM
dados_KNN <- dados_IA%>%dplyr::select(c(5,9:26))
View(dados_KNN)

#verificando dados faltantes
sum(is.na(dados_KNN))

#Definindo o numero de sementes
set.seed(123)

# Separação do banco de dados em treinamento e teste
split_KNN<-sample.split(dados_KNN$volume_total, SplitRatio = 0.8)
base_treinamento_knn <- dados_KNN[split_KNN,] 
base_teste_knn <- dados_KNN[-split_KNN,]

#Construindo modelo de treinamento
ctrl <- trainControl(method = "cv",
                     number = 10)

KNN <- train(volume_total ~ .,
             method     = "knn",
             preProcess = c("center","scale"),
             tuneLength = 20,
             trControl  = ctrl,
             metric     = "RMSE",
             data       = base_treinamento_knn)

#predicao e estatistica
base_teste_knn$previsoesKNN<-predict(KNN, newdata=base_teste_knn[-1])
rsq<-rSquared(base_teste_knn[['volume_total']],resid=base_teste_knn[['volume_total']]-base_teste_knn$previsoesKNN)
rsq

#plot
{
  g1 <- ggplot(base_teste_knn, aes(x =volume_total, y = base_teste_knn$previsoesKNN)) + 
    labs(x = "Volume Observado", y = "Volume Estimado") +
    geom_point()+
    geom_abline(intercept = 0,slope = 1, linetype = "dashed")+
    geom_smooth(method=lm, se=FALSE, color = 'red', size = 0.8)+
    scale_x_continuous(limits = c(0,180))+
    scale_y_continuous(limits = c(0,180))+
    theme_bw()+
    theme(text = element_text(colour = 'black', family = 'Times', size = 12),
          axis.text.x = element_text(colour = 'black', family = 'Times', size = 12), 
          axis.text.y = element_text(colour = 'black', angle = 90,
                                     family = 'Times', size = 12),
          axis.title = element_text(colour = 'black', family = 'Times', size = 12),
          axis.title.y = element_text( family = 'Times', size = 12), 
          axis.title.x = element_text( family = 'Times', size = 12), 
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
  x11()
  g1 
}

#salvando o modelo
saveRDS(KNN, "Item_5_Modelos/KNN.rds")


############################################################################################
#######################################ESPACIALIZAÇÃO#######################################
############################################################################################

#Instalando e requerindo os pacotes necessarios
install.packages("lidR")
install.packages("raster")
install.packages("rgdal")
require(lidR)
require(raster)
require(rgdal)

#importando a base de dados las e plotando
dadosLAS_norm<-readLAS("Item_5_Dados_LiDAR/las_normalizada.las", select = "xyz",filter = " -drop_z_below 0")
plot(dadosLAS_norm, color = "Z", size = 3, bg = "white", legend = T) 

#importando o shapefile para recorte da area de interesse
corte<-shapefile("Item_5_Shapefile/corte.shp")
plot(corte)

#recorte da area de interesse
LAS<-clip_roi(dadosLAS_norm, corte)
plot(LAS, bg = "white", legend = T)

#gerando as variaveis provienientes do lidar na area amostral
metrics = grid_metrics(LAS, .stdmetrics_z,24.49)
plot(metrics)

#gerando o stack com os arquivos raster das variaveis LiDAR
stack<-stack(metrics)

#Modelos gerados
RF<-readRDS("Item_5_Modelos/RF.rds")
SVM<-readRDS("Item_5_Modelos/SVM.rds")
RNA<-readRDS("Item_5_Modelos/RNA.rds")
KNN<-readRDS("Item_5_Modelos/KNN.rds")

#Aplicando o modelo por meio das variaveis geradas pelo metrics
e<-predict(stack,RF)
f<-predict(stack,SVM)
g<-predict(stack,RNA)
h<-predict(stack,KNN)


#Plot da espacializacao dos modelos
plot(e, main ="volume RF m³")
plot(f, main ="volume SVM m³")
plot(g, main ="volume RNA m³")
plot(h, main ="volume KNN m³")