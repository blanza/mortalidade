############################################
## Estimativa da medida de dispersão IQR ###
############################################

## Dados
dados<-read.csv('lxs_grupos.csv', sep=',', header=T, fileEncoding='UTF-8-BOM')

## Separando as bases para diferentes estimativas e sexos  

table(dados$grupo);table(dados$idade)

## Separando por sexo 
dados.m=subset(dados, sexo=='m')
dados.f=subset(dados, sexo=='f')

## Separando por método e dados observados 
## Homens
dat.m.obs<-dados.m[,c("grupo", "idade", "lx.obs")]; colnames(dat.m.obs)=c('grupo','idade','lx')
dat.m.van<-dados.m[,c("grupo", "idade", "lx.van")];colnames(dat.m.van)=c('grupo','idade','lx') 
dat.m.gom<-dados.m[,c("grupo", "idade", "lx.gom")];colnames(dat.m.gom)=c('grupo','idade','lx')
dat.m.top<-dados.m[,c("grupo", "idade", "lx.top")];colnames(dat.m.top)=c('grupo','idade','lx')

## Mulheres
dat.f.obs<-dados.f[,c("grupo", "idade", "lx.obs")]; colnames(dat.f.obs)=c('grupo','idade','lx')
dat.f.van<-dados.f[,c("grupo", "idade", "lx.van")];colnames(dat.f.van)=c('grupo','idade','lx')
dat.f.gom<-dados.f[,c("grupo", "idade", "lx.gom")];colnames(dat.f.gom)=c('grupo','idade','lx')
dat.f.top<-dados.f[,c("grupo", "idade", "lx.top")];colnames(dat.f.top)=c('grupo','idade','lx')

## Para homens: dat.m.obs = dados observados, dat.m.van = Van der Maen, 
## dat.m.gom = Gompertz e dat.m.top = topals

## Para mulheres: dat.f.obs = dados observados, dat.f.van = Van der Maen, 
## dat.f.gom = Gompertz e dat.f.top = topals

#################################################
## Leia a função completa para estimar o IQR ####
#################################################
IQR_function<-function(x){
tab<-data.frame(x)
cod<-factor(x$grupo)
tab<-split(tab,cod)
for(i in seq(along = tab)){tab[[i]]$First_Q<-0.25}
for(i in seq(along = tab)){tab[[i]]$Third_Q<-0.75}
for(i in seq(along = tab)){
tab[[i]]$FQ_idade_max<-106-findInterval(tab[[i]]$First_Q, sort(tab[[i]]$lx))
} # 106 pq são 105 idades consideradas
for(i in seq(along = tab)){
tab[[i]]$FQ_idade_min<-(106-findInterval(tab[[i]]$First_Q, sort(tab[[i]]$lx)))-1
}
for(i in seq(along = tab)){
tab[[i]]$FQ_lx_max<-sort(tab[[i]]$lx)[sort(tab[[i]]$idade, decreasing=T)==
                                        tab[[i]]$FQ_idade_max[1]]
}
for(i in seq(along = tab)){
tab[[i]]$FQ_lx_min<-sort(tab[[i]]$lx)[sort(tab[[i]]$idade, decreasing=T)== 
                                        tab[[i]]$FQ_idade_min[1]]
}
for(i in seq(along = tab)){
tab[[i]]$l25<-(((tab[[i]]$FQ_idade_max-tab[[i]]$FQ_idade_min)*
                  (tab[[i]]$First_Q-tab[[i]]$FQ_lx_min))+
((tab[[i]]$FQ_lx_max-tab[[i]]$FQ_lx_min)*tab[[i]]$FQ_idade_min))/
  (tab[[i]]$FQ_lx_max-tab[[i]]$FQ_lx_min)
}
for(i in seq(along = tab)){
tab[[i]]$TQ_idade_max<-106-findInterval(tab[[i]]$Third_Q, sort(tab[[i]]$lx))
}
for(i in seq(along = tab)){
tab[[i]]$TQ_idade_min<-(106-findInterval(tab[[i]]$Third_Q, sort(tab[[i]]$lx)))-1
}
for(i in seq(along = tab)){
tab[[i]]$TQ_lx_max<-sort(tab[[i]]$lx)[sort(tab[[i]]$idade, decreasing=T)== 
                                        tab[[i]]$TQ_idade_max[1]]
}
for(i in seq(along = tab)){
tab[[i]]$TQ_lx_min<-sort(tab[[i]]$lx)[sort(tab[[i]]$idade, decreasing=T)== 
                                        tab[[i]]$TQ_idade_min[1]]
}
for(i in seq(along = tab)){
tab[[i]]$l75<-(((tab[[i]]$TQ_idade_max-tab[[i]]$TQ_idade_min)*
                  (tab[[i]]$Third_Q-tab[[i]]$TQ_lx_min))+
((tab[[i]]$TQ_lx_max-tab[[i]]$TQ_lx_min)*tab[[i]]$TQ_idade_min))/
  (tab[[i]]$TQ_lx_max-tab[[i]]$TQ_lx_min)
}
for(i in seq(along = tab)){
tab[[i]]$IQR<-tab[[i]]$l25-tab[[i]]$l75
}
tab<-unsplit(tab,cod)
l25<-tapply(tab$l25,tab$grupo,mean)
l75<-tapply(tab$l75,tab$grupo,mean)
IQR<-tapply(tab$IQR,tab$grupo,mean)
resultado<-cbind(l25,l75,IQR)
results<-list(resultado=resultado)
return(results)
}

## Aqui estima-se o IQR para a seleção indicada
IQR_function(dat.m.top)

## Altere o nome do banco e estime para cada método e sexo
