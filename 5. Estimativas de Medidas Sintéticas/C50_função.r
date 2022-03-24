############################################
## Estimativa da medida de dispers�o C50 ###
############################################

## Biblioteca necess�ria
library(dplyr)

## Dados
dados<-read.csv('dxs_grupos.csv', sep=',', fileEncoding='UTF-8-BOM', header=T)

head(dados);tail(dados)

#################################################
## Leia a fun��o completa para estimar o C50 ####
#################################################
C50_function<-function(x){
tab<-data.frame(x)
cod<-factor(x$grupo)
tab<-split(tab,cod)
for(i in seq(along = tab)){tab[[i]]$n<-seq(1,nrow(tab[[i]]))}                                                                        ## n 
for(i in seq(along = tab)){tab[[i]]$age_modal<-subset(tab[[i]]$idade,tab[[i]]$dx==max(tab[[i]]$dx))}                                   ## modal age
for(i in seq(along = tab)){tab[[i]]$dx_modal<-sort(tab[[i]]$dx,decreasing=T)}                                                        ## obitos ordenados pela idade modal
for(i in seq(along = tab)){tab[[i]]$dx_modal_cum<-cumsum(tab[[i]]$dx_modal)}                                                         ## acumulado a partir da idade modal
for(i in seq(along = tab)){tab[[i]]$dx_modal_perc<-round(tab[[i]]$dx_modal_cum/sum(tab[[i]]$dx)*100,digits=2)}                       ## % dx acumulado
for(i in seq(along = tab)){tab[[i]]$dx_c50<-tab[[i]][findInterval(sum(tab[[i]]$dx*.50),tab[[i]]$dx_modal_cum)+1,'dx_modal']}         ## find dx
for(i in seq(along = tab)){tab[[i]]$dx_c50_cum<-tab[[i]][findInterval(sum(tab[[i]]$dx*.50),tab[[i]]$dx_modal_cum)+1,'dx_modal_cum']} ## find dx cumulative
for(i in seq(along = tab)){tab[[i]]$n_c50<-tab[[i]][findInterval(sum(tab[[i]]$dx*.50),tab[[i]]$dx_modal_cum)+1,'n']}                 ## find n
for(i in seq(along = tab)){
tab[[i]]$C50<-tab[[i]]$n_c50 -((tab[[i]]$dx_c50_cum - sum(tab[[i]]$dx*.50))/tab[[i]]$dx_c50) 
}
tab<-unsplit(tab,cod)
Modal_age<-tapply(tab$age_modal,tab$grupo,mean)
C50<-tapply(tab$C50,tab$grupo,mean)
resultado<-cbind(Modal_age,C50)
results<-list(resultado=resultado)
return(results)
}

## Aqui, separa-se o sexo (sexo ==) e a informa��o de �bito (info ==)   
## �bitos: 'obs'= observado, 'top'= topals, 'van'= Van der Maen e 'gom'= Gompertz 
## sexo: f = feminino e m = masculino
dat= dados %>%
  filter(sexo =='f', info =='top')

## Aqui estima-se o C50 para a sele��o indicada
C50_function(dat)

## Repita a opera��o para outras informa��es de �bito e sexo.

