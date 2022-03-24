#####################################################
## Gráficos das medidas de dispersão condicionais ###
#####################################################

rm(list=ls())

## Bibliotecas necessárias
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(gridExtra)

## Dados das medidas de dispersão

## IQR
dat.iqr=read.csv('resultados_IQR.csv', sep=',', header = T, fileEncoding = 'UTF-8-BOM')

## C50
dat.c50=read.csv('resultados_C50.csv', sep=',', header = T, fileEncoding = 'UTF-8-BOM')

## Filtrando por tipo de medida e sexo
datf.iqr=subset(dat.iqr, sexo=='f');datm.iqr=subset(dat.iqr, sexo=='m')
datf.c50=subset(dat.c50, sexo=='f');datm.c50=subset(dat.c50, sexo=='m')

## Tamanho do intervalo do IQR
datf.iqr$sd=datf.iqr$IQR/2;datm.iqr$sd=datm.iqr$IQR/2

## Colocando um dodge para os pontos não se sobreporem 
pd <- position_dodge(0.5)

## Informações gráficas para o IQR

## Mulheres
muiqr= ggplot(data = datf.iqr, mapping = aes(x = IQR, y=grupo, color = info))+
  geom_errorbar(aes(xmin=IQR-sd, xmax=IQR+sd), width=.5, position=pd) +
  scale_x_continuous(limits = c(0, 25))+
  geom_point(position = pd)+
  labs(title="Comparativo IQR\n Mulheres", 
       x='Medida de dispersão\nDistância Interquatilica', 
       y='Grupo de contribuinte', 
       color="Tipo de informação\n")+
  scale_color_manual(values=c('black','blue','red','darkgreen'),
                     labels=c('Gompertz', 'Observado','Topals','Van der Maen'))+
  theme_bw()

## Homens
homqr= ggplot(data = datm.iqr, mapping = aes(x = IQR, y=grupo, color = info))+
  geom_errorbar(aes(xmin=IQR-sd, xmax=IQR+sd), width=.5, position=pd) +
  scale_x_continuous(limits = c(0, 25))+
  geom_point(position = pd)+
  labs(title="Comparativo IQR\n Homens", 
       x='Medida de dispersão\nDistância Interquatilica', 
       y='Grupo de contribuinte', 
       color="Tipo de informação\n")+
  scale_color_manual(values=c('black','blue','red','darkgreen'),
                     labels=c('Gompertz', 'Observado','Topals','Van der Maen'))+
  theme_bw()

## Informações gráficas C50

## Mulheres
muic50 = ggplot(data = datf.c50, mapping = aes(x = C50, y= grupo, color = info))+ 
  geom_point(position=position_dodge(0.5))+
  scale_x_continuous(limits = c(0, 25))+
  labs(title="Comparativo C50\n Mulheres", x='Medida de dispersão C50', 
       y='Grupo de contribuinte', color="Tipo de informação\n")+
  scale_color_manual(values=c('black','blue','red','darkgreen'),
                     labels=c('Gompertz', 'Observado','Topals','Van der Maen'))+
  theme_bw()

## Homens
homc50 = ggplot(data = datm.c50, mapping = aes(x = C50, y= grupo, color = info))+ 
  geom_point(position=position_dodge(0.5))+
  scale_x_continuous(limits = c(0, 25))+
  labs(title="Comparativo C50\n Homens", x='Medida de dispersão C50', 
       y='Grupo de contribuinte', color="Tipo de informação\n")+
  scale_color_manual(values=c('black','blue','red','darkgreen'),
                     labels=c('Gompertz', 'Observado','Topals','Van der Maen'))+
  theme_bw()

### FIGURA 6 DO ARTIGO: Gerando o gráfico das duas medidas de dispersão 
windows()
grid.arrange(muiqr, homqr, muic50, homc50, ncol=2, nrow = 2)


