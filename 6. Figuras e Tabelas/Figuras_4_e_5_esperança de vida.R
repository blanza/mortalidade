###################################################
## Gr�ficos das esperan�as de vida condicionais ###
###################################################

rm(list=ls())

## Bibliotecas necess�rias
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(data.table)

## Dados com as estimativas de esperan�a de vida
dat.ex=read.csv('ex_condicional.csv', sep=',', header = T, fileEncoding = 'UTF-8-BOM')

## Cont�m informa��es de esperan�a de vida para 
## 'api.rur', 'api.urb', 'atc', 'bpc', 'ibge' e 'total'

################################################
### Gr�ficos para os grupos de benefici�rios ###
################################################

## Separando as bases por sexos e retirando os totais
datf.ex=subset(dat.ex, sexo=='f' & grupo != 'total' & grupo !='ibge');
datm.ex=subset(dat.ex, sexo=='m' & grupo != 'total' & grupo !='ibge')

## Ordenando a informa��o dos grupos de benefici�rios
datf.ex$grupo=factor( datf.ex$grupo,levels = c('atc', 'api.urb','api.rur','bpc'))
datm.ex$grupo=factor( datm.ex$grupo,levels = c('atc', 'api.urb','api.rur','bpc'))

## Informa��o de idade auxiliar
idade= seq(65,105,by=5) 
idade

##########################################################################
### Comparando cada grupo com o observado em termos de diferen�a em ex ###
##########################################################################

## Separando bases por grupos, sexo e criando uma coluna de observados
codi=factor(datf.ex$grupo);
tabm=split(datm.ex,codi);
tabf=split(datf.ex,codi)

## Mulheres
for(i in seq(along = tabf)){
  tabf[[i]]$obs<-with(subset(tabf[[i]],info=='obs'),
                      rep(ex,4))} 
tabf<-unsplit(tabf,codi)

## Homens
for(i in seq(along = tabm)){
  tabm[[i]]$obs<-with(subset(tabm[[i]],info=='obs'),
                      rep(ex,4))} 
tabm<-unsplit(tabm,codi)

## Retirando diferen�as entre estimado pelos m�todos e observado 
tabf$dif=tabf$ex-tabf$obs;tabm$dif=tabm$ex-tabm$obs

## Retirando parte que n�o entra na an�lise
tabf=subset(tabf, info!='obs');tabm=subset(tabm, info!='obs')

## Informa��o gr�fica para mulheres
mutop.dif=ggplot(subset(tabf, info=='top'))+ 
  geom_point(mapping = aes(x = dif, y = idade, color = grupo))+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(breaks = seq(65,105, by=5))+
  labs(title="Diferen�a, esperan�a de vida condicional feminina\n(estimado - observado). Modelo Topals", 
       x='Diferen�a em Ex condicional', 
       y='Idade alcan�ada pelo contribuinte', 
       color = "Tipo de contribuinte")+
  scale_color_manual(values=c('black','red','blue','darkgreen'),
                     labels=c('ATC', 'Api urbano','Api rural','BPC'))+
  theme_bw()

muvan.dif=ggplot(subset(tabf, info=='van'))+ 
  geom_point(mapping = aes(x = dif, y = idade, color = grupo))+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(breaks = seq(65,105, by=5))+
  labs(title="Diferen�a, esperan�a de vida condicional feminina\n(estimado - observado). Modelo Van der Maen", 
       x='Diferen�a em Ex condicional', 
       y='Idade alcan�ada pelo contribuinte', 
       color = "Tipo de contribuinte")+
  scale_color_manual(values=c('black','red','blue','darkgreen'),
                     labels=c('ATC', 'Api urbano','Api rural','BPC'))+
  theme_bw()

mugom.dif=ggplot(subset(tabf, info=='gom'))+ 
  geom_point(mapping = aes(x = dif, y = idade, color = grupo))+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(breaks = seq(65,105, by=5))+
  labs(title="Diferen�a, esperan�a de vida condicional feminina\n(estimado - observado). Modelo Gompterz", 
       x='Diferen�a em Ex condicional', 
       y='Idade alcan�ada pelo contribuinte', 
       color = "Tipo de contribuinte")+
  scale_color_manual(values=c('black','red','blue','darkgreen'),
                     labels=c('ATC', 'Api urbano','Api rural','BPC'))+
  theme_bw()

## Informa��o gr�fica para homens
hotop.dif=ggplot(subset(tabm, info=='top'))+ 
  geom_point(mapping = aes(x = dif, y = idade, color = grupo))+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(breaks = seq(65,105, by=5))+
  labs(title="Diferen�a, esperan�a de vida condicional masculina\n(estimado - observado). Modelo Topals", 
       x='Diferen�a em Ex condicional', 
       y='Idade alcan�ada pelo contribuinte', 
       color = "Tipo de contribuinte")+
  scale_color_manual(values=c('black','red','blue','darkgreen'),
                     labels=c('ATC', 'Api urbano','Api rural','BPC'))+
  theme_bw()

hovan.dif=ggplot(subset(tabm, info=='van'))+ 
  geom_point(mapping = aes(x = dif, y = idade, color = grupo))+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(breaks = seq(65,105, by=5))+
  labs(title="Diferen�a, esperan�a de vida condicional masculina\n(estimado - observado). Modelo Van der Maen", 
       x='Diferen�a em Ex condicional', 
       y='Idade alcan�ada pelo contribuinte', 
       color = "Tipo de contribuinte")+
  scale_color_manual(values=c('black','red','blue','darkgreen'),
                     labels=c('ATC', 'Api urbano','Api rural','BPC'))+
  theme_bw()

hogom.dif=ggplot(subset(tabm, info=='gom'))+ 
  geom_point(mapping = aes(x = dif, y = idade, color = grupo))+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(breaks = seq(65,105, by=5))+
  labs(title="Diferen�a, esperan�a de vida condicional masculina\n(estimado - observado). Modelo Gompterz", 
       x='Diferen�a em Ex condicional', 
       y='Idade alcan�ada pelo contribuinte', 
       color = "Tipo de contribuinte")+
  scale_color_manual(values=c('black','red','blue','darkgreen'),
                     labels=c('ATC', 'Api urbano','Api rural','BPC'))+
  theme_bw()

### FIGURA 4 DO ARTIGO: Gr�fico de diferen�as em ex, observado vs. estimado ####
windows()
grid.arrange( mutop.dif, muvan.dif, mugom.dif, hotop.dif, hovan.dif, hogom.dif, ncol= 3, nrow = 2)

#####################################################################

### Limpando os objetos para trabalhar outros gr�ficos ####
rm(list = ls())

## Dados - esperan�a de vida
dat.ex=read.csv('ex_condicional.csv', sep=',', header = T, fileEncoding = 'UTF-8-BOM')

## Comparando as estimativas dos totais com o IBGE, idades 65 e 75

## Filtrando as mulheres
datf.ex.n= dat.ex %>%
  filter(sexo=='f', grupo == 'total' | grupo == 'ibge', idade < 85) 
  
## Filtrando os homens
datm.ex.n= dat.ex %>%
  filter(sexo=='m', grupo == 'total' | grupo == 'ibge', idade < 85) 

## Criando um objeto auxiliar de idade
idade.n=seq(60,80, by=5)

## Ordenando as informa��es dos grupos para cada base
datf.ex.n$info=factor(datf.ex.n$info, levels = c("obs", "ibge", "top","van","gom"))
datm.ex.n$info=factor(datm.ex.n$info, levels = c("obs", "ibge", "top","van","gom"))

### Gerando informa��es gr�ficas para ambos os sexos ####

## Mulheres
muir=ggplot(datf.ex.n, mapping = aes(x = idade, y = ex, fill = info))+  
  geom_bar(stat = 'identity', width = 4, position = position_dodge(6))+
  scale_x_continuous(breaks=seq(65,75, by=10))+ 
  scale_y_continuous(limits = c(0,25))+
  theme_bw()+
  ggtitle("Esperan�a de vida condicional feminina para diferentes idades,\ne m�todos.Todos os contribuintes e IBGE")+
  theme(plot.title = element_text(size = 11))+
  xlab('Idade alcan�ada pelo contribuinte')+
  ylab('Tempo de vida condicional\nquando alcan�ado determinada idade')+ 
  labs(fill = "Tipo de dado")+
  theme(legend.title = element_text(size=8), 
        legend.text = element_text(size=8))+
  guides(fill = guide_legend(override.aes = list(size = 2)))+
  scale_fill_manual(values=c('black','red','blue','darkgreen', 'grey'),
                    labels=c('Observado','IBGE','Topals','Van der Maen','Gompertz')) 

## Homens
homi=ggplot(datm.ex.n, mapping = aes(x = idade, y = ex, fill = info))+  
  geom_bar(stat = 'identity', width = 4, position = position_dodge(6))+
  scale_x_continuous(breaks=seq(65,75, by=10))+ 
  scale_y_continuous(limits = c(0,25))+
  theme_bw()+
  ggtitle("Esperan�a de vida condicional masculina para diferentes idades,\ne m�todos.Todos os contribuintes e IBGE")+
  theme(plot.title = element_text(size = 11))+
  xlab('Idade alcan�ada pelo contribuinte')+
  ylab('Tempo de vida condicional\nquando alcan�ado determinada idade')+ 
  labs(fill = "Tipo de dado")+
  theme(legend.title = element_text(size=8), 
        legend.text = element_text(size=8))+
  guides(fill = guide_legend(override.aes = list(size = 2)))+
  scale_fill_manual(values=c('black','red','blue','darkgreen', 'grey'),
                    labels=c('Observado','IBGE','Topals','Van der Maen','Gompertz')) 

### FIGURA 5 DO ARTIGO: Gr�fico de barras ####
windows(height = 7, width = 15)
grid.arrange( muir, homi, ncol= 2, nrow = 1)
