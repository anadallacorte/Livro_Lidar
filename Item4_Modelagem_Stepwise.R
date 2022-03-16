##################################################################################
# 01/03/2022
# SCRIPT 4
# LIVRO: APLICACOES DO LIDAR PARA O INVENTARIO FLORESTAL - ENFOQUE UNIDADE DE AREA
# AUTORES: DALLA CORTE, ET AL. 2022
# UNIVERSIDADE FEDERAL DO PARANA - UFPR
# CURSO DE ENGENHARIA FLORESTAL
##################################################################################

##################################################################################
###################  MODELAGEM  #########################
##################################################################################

########## PREPARACAO DO AMBIENTE RSTUDIO ########## -------------------------------------------------------------------------------------
#Indicando o diretorio
setwd("C:/Livro_Lidar/Item4_Modelagem_Stepwise/")

# Instalando os pacotes necessarios e carregando os mesmos
install.packages("dplyr")
install.packages("stats")
install.packages("olsrr")
install.packages("car")
install.packages("lmtest")
install.packages("faraway")
install.packages("ggplot")
install.packages("Metrics")
require(dplyr)
require(stats)
require(olsrr)
require(car)
require(lmtest)
require(faraway)
require(ggplot2)
require(Metrics)

# Abrindo o arquivo "dados_pareados"
dados_modelagem<-read.csv2("Item_4_Tabelas/dados_pareados.csv",header = TRUE)
View(dados_modelagem)

##############################STEPWISE####################################
####Stepwise para a variavel volume####

#separando as colunas x e y para executar a funcao step
step_vol<-dados_modelagem%>%dplyr::select(c(4,8:25))
View(step_vol)

#aplicando a regressao e o step
independentes_vol<-lm(volume_total~., data=step_vol)

#modelos stepwise forward, backward e both
modelo_step_volume_p<-ols_step_forward_p(independentes_vol, penter = 0.05, prem = 0.1, details = TRUE)
modelo_step_volume_p<-ols_step_backward_p(independentes_vol, penter = 0.05, prem = 0.1, details = TRUE)
modelo_step_volume_p<-ols_step_both_p(independentes_vol, penter = 0.05, prem = 0.1, details = TRUE)

#aplicacao do modelo
dados_modelagem$volume_total_est<-(-60.44+(27.33*dados_modelagem$zmean))

#criacao dos objetos para obtencao do BIAS
volume_total<-dados_modelagem$volume_total
volume_estimado<-dados_modelagem$volume_total_est
bias_volume<-bias(volume_estimado, volume_total)
bias_volume

#plot com as estatisticas desenvolvidas
plot(dados_modelagem$volume_total, dados_modelagem$volume_total_est,xlab="Volume Total",ylab="Volume Estimado" )
abline(1,1)
text(380,250,expression(Y== -60.44 + (27.33*zmean)),cex=1)
text(380,230,expression(R^2==0.87),cex=1)
text(380,210,expression(RMSE==26.56),cex=1)
text(380,190,expression(BIAS==0.0196),cex=1)


########## MODELAGEM DA AREA BASAL ##########---------------------------------------------------------------------------------------------------------
#separando as colunas x e y para executar a funcao step
step_ab <-dados_modelagem%>%dplyr::select(c(3,8:25))
View(step_ab)

#aplicando a regressao e o step
independentes_ab<-lm(area_basal.BA.~., data=step_ab)

#modelos stepwise forward, backward e both
modelo_step_ab_p_forward<-ols_step_forward_p(independentes_ab, penter = 0.05, prem = 0.1, details = TRUE)
modelo_step_ab_p_backward<-ols_step_backward_p(independentes_ab, penter = 0.05, prem = 0.1, details = TRUE)
modelo_step_ab_p_both<-ols_step_both_p(independentes_ab, penter = 0.05, prem = 0.1, details = TRUE)

#aplicacao do modelo
dados_modelagem$ab_est<-(6.417+(3.529*dados_modelagem$zq25))

#criacao dos objetos para obtencao do BIAS
ab<-dados_modelagem$area_basal.BA.
ab_estimado<-dados_modelagem$ab_est
bias_ab<-bias(ab_estimado, ab)
bias_ab

#plot com as estatisticas desenvolvidas
plot(dados_modelagem$area_basal.BA., dados_modelagem$ab_est,xlab="Área Basal",ylab="Área Basal Estimada")
abline(1,1)
text(50,25,expression(Y== 6.417+(3.529*zq25)),cex=1)
text(50,23,expression(R^2==0.87),cex=1)
text(50,21,expression(RMSE==3.319),cex=1)
text(50,19,expression(BIAS==0.0006),cex=1)

########## MODELAGEM DA BIOMASSA ##########---------------------------------------------------------------------------------------------------------
#separando as colunas x e y para executar a funcao step
step_bio <- dados_modelagem%>%dplyr::select(c(7,8:25))
View(step_bio)

#aplicando a regressao e o step
independentes_bio<-lm(biomassa.kg.~., data=step_bio)

#modelos stepwise forward, backward e both
modelo_step_bio_p_forward<-ols_step_forward_p(independentes_bio, penter = 0.05, prem = 0.1, details = TRUE)
modelo_step_bio_p_backward<-ols_step_backward_p(independentes_bio, penter = 0.05, prem = 0.1, details = TRUE)
modelo_step_bio_p_both<-ols_step_both_p(independentes_bio, penter = 0.05, prem = 0.1, details = TRUE)

#aplicacao do modelo
dados_modelagem$bio_est<-(-23.812+(10.767*dados_modelagem$zmean))

#criacao dos objetos para obtencao do BIAS
bio<-dados_modelagem$biomassa.kg.
bio_estimado<-dados_modelagem$bio_est
bias_bio<-bias(bio_estimado, bio)
bias_bio

#plot com as estatisticas desenvolvidas
plot(dados_modelagem$biomassa.kg., dados_modelagem$bio_est,xlab="Biomassa",ylab="Biomassa Estimada")
abline(1,1)
text(140,80,expression(Y== -23.812+(10.767*zmean)),cex=1)
text(140,70,expression(R^2==0.87),cex=1)
text(140,60,expression(RMSE==10.466),cex=1)
text(140,50,expression(BIAS==0.0019),cex=1)


########################################################
##################### Pressupostos Estatisticos #####################
########################################################
#Definicao final dos modelos
volume<- modelo_step_volume_p$model
ab<- modelo_step_ab_p_both$model
bio<- modelo_step_bio_p_both$model


#Normalidade dos residuos pelo teste de Shapiro-Wilk
#H0: distribuicao normal  (p>0.05)
#H1: nao possui distribuicao normal (p<=0.05)
shapiro_volume_total_step<- shapiro.test(volume$residuals)
shapiro_ab_step<- shapiro.test(ab$residuals)
shapiro_bio_step<- shapiro.test(bio$residuals)
print(shapiro_volume_total_step)
print(shapiro_ab_step)
print(shapiro_bio_step)

#Outliers nos residuos Z-SCORE
#adicionar o teste residuos
summary(rstandard(volume))
summary(rstandard(ab))
summary(rstandard(bio))


#Independencia dos residuos (Durbin-Watson)
#H0: nao existe correlacao entre os residuos - sao independentes (p>0.05)
#H1: existe correlacao entre os residuos - nao sao independentes (p<=0.05)
durbinWatsonTest(volume)
durbinWatsonTest(ab)
durbinWatsonTest(bio)


#Homocedasticidade (Breusch-Pagan)
#H0: ha homocedasticidade (p>0.05)
#H1: nao ha homocedasticidade (p<=0.05)
bptest(volume)
bptest(ab)
bptest(bio)

#Multicolinearidade
#pelo Fator de Inflacao de Variancia (VIF)

faraway::vif(volume)
faraway::vif(ab)
faraway::vif(bio)

#Plotagem do valor estimado em relacao ao real de volume
#Plotagem do valor estimado em relacao ao real de volume
{
  g1 <- ggplot(dados_modelagem, aes(x = volume_total_est, y = volume_total)) + 
    labs(x = "Volume Estimado", y = "Volume Observado") +
    geom_point()+
    geom_abline(intercept = 0,slope = 1, linetype = "dashed")+
    geom_smooth(method=lm, se=FALSE, color = 'red', size = 0.8)+
    scale_x_continuous(limits = c(0,350))+
    scale_y_continuous(limits = c(0,350))+
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

{
  g1 <- ggplot(dados_modelagem, aes(x = ab_est, y = area_basal.BA.)) + 
    labs(x = "Ãrea Basal Estimada", y = "Ãrea Basal Observada") +
    geom_point()+
    geom_abline(intercept = 0,slope = 1, linetype = "dashed")+
    geom_smooth(method=lm, se=FALSE, color = 'red', size = 0.8)+
    scale_x_continuous(limits = c(0,60))+
    scale_y_continuous(limits = c(0,60))+
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


{
  g1 <- ggplot(dados_modelagem, aes(x = bio_est, y = biomassa.kg.)) + 
    labs(x = "Biomassa Estimada", y = "Biomassa Observada") +
    geom_point()+
    geom_abline(intercept = 0,slope = 1, linetype = "dashed")+
    geom_smooth(method=lm, se=FALSE, color = 'red', size = 0.8)+
    scale_x_continuous(limits = c(0,100))+
    scale_y_continuous(limits = c(0,100))+
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


#######################################################################
########################Espacializacao#################################
#######################################################################
install.packages("lidR")
install.packages("raster")
require(lidR)
require(raster)

#importando a base de dados las e plotando
dadosLAS_norm<-readLAS("Item_4_Dados_LiDAR/las_normalizada.las", select = "xyz",filter = " -drop_z_below 0")
plot(dadosLAS_norm, color = "Z", size = 3, bg = "white", legend = T)

#importando o shapefile para recorte da area de interesse
corte<-shapefile("Item_4_Shapefile/corte.shp")
plot(corte)

#recorte da area de interesse
LAS<- clip_roi(dadosLAS_norm, corte)
plot(LAS, bg = "white")

#gerando as variaveis provienientes do lidar na area amostral
metrics <- grid_metrics(LAS, .stdmetrics_z,24.49)
plot(metrics)

#gerando o stack com os arquivos raster das variaveis LiDAR
stack<-stack(metrics)

#buscando os modelos de regressÃ£o
volume<- modelo_step_volume_p$model
ab<- modelo_step_ab_p_both$model
bio<- modelo_step_bio_p_both$model

#predicao entre o modelo e os arquivos raster 
a<-predict(stack,volume)
b<-predict(stack,ab)
c<-predict(stack,bio)

#espacializando a informacao calculada para a area de interesse
plot(a, main = "Espacialização - Volume")   
plot(b, main = "Espacialização - Área Basal")
plot(c, main = "Espacialização - Biomassa") 

