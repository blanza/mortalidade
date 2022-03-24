#############################################################################
#### Seleção dos grupos de idade através do Root Mean Square Error (RMSE) ###
##################, ou Error Quadrático Médio ###############################

### Bibliotecas necessárias 

## Instalação 
install.packages('MortalityLaws')
install.packages('Metrics')

## leitura das bibliotecas
library(MortalityLaws)
library(Metrics)

## Remoção de notação científica
options(scipen = 999)

###########################################
### Leitura dos dados para o ano de 2015 ##
###########################################
dat=read.csv('dados.rgps15.csv', sep=',', header = T, 
             fileEncoding='UTF-8-BOM')

## Verificando informações no banco
names(dat)

## Estimando taxas de mortalidade  
dat$mx=dat$obito/dat$pop

###########################################################################
### Importante. Nesta análise, estimamos o RMSE separado por grupo de #####
### beneficiário e sexo. Assim, caso queira estimar a estatística para ####
### um grupo e sexo específicos, é necessário ler #########################
### apenas a linha de comando e manipular o "#" ########################### 


# Exemplo, estamos gerando bases para homens e considerando, total e api rural
# beneficiários. Repetimos o processo para os demais grupos. 

dat.mt=subset(dat,sexo=='m' & grupo=='total')
dat.m=subset(dat,sexo=='m' & grupo=='api.rur')
#dat.m1=subset(dat,sexo=='m' & grupo=='api.urb')
#dat.m2=subset(dat,sexo=='m' & grupo=='atc')
#dat.m3=subset(dat,sexo=='m' & grupo=='bpc')

# Repita o procedimento para as mulheres
dat.ft=subset(dat,sexo=='f' & grupo=='total')
#dat.f=subset(dat,sexo=='f' & grupo=='api.rur')
#dat.f1=subset(dat,sexo=='f' & grupo=='api.urb')
#dat.f2=subset(dat,sexo=='f' & grupo=='atc')
#dat.f3=subset(dat,sexo=='f' & grupo=='bpc')

### Esta função estima o RMSE para um lei de mortalidade selecionada
### Caso queira aplicá-la com outra lei, 
### basta alterar o argumento law=, entre 'vandermaen' ou 'gompertz'

## Neste exemplo, a função estima o RMSE entre as idades de 65 e 110 anos
## considerando a lei de mortalidade vandermaen.
bestage=function(y, minid, maxid){
                  mod=predict(MortalityLaw(x=minid:maxid, 
                  mx=with(subset(y, idade>=minid & idade<=maxid),
                  mx), law='vandermaen'),65:110)
                       {
                         modi=as.data.frame(cbind(mod,'idade'=65:110))
                         }
                  {
                     u=rmse(with(subset(y, idade>=minid & idade<=maxid),mx),
                            with(subset(modi, idade>=minid & idade<=maxid),mod))
                  }
     return(u)
                  }


# Altere o grupo, a lei e busque o menor RMSE com base em idades
# máximas e mínimas. Altere as idades minid e maxid manualmente. 

## Exemplo considerando o grupo api rural e homens
bestage(dat.m, minid = 65, maxid = 80)


## Resultados por sexo e lei (Gompertz e Van der Maen)

## usando a lei Van der Maen - homens
# idades de 66-81 total      RMSE = 0.0006709926 Lei Vandermaen
# idades de 65-80 api rural  RMSE = 0.0006689453 Lei Vandermaen
# idades de 65-80 api urbano RMSE = 0.001625716 Lei Vandermaen
# idades de 67-82 para ATC   RMSE = 0.0005282149 Lei Vandermaen
# idades de 68-83 para BPC   RMSE = 0.00145121 Lei Vandermaen 

## usando a lei Van der Maen - Mulheres
# idades de 66-81 total      RMSE = 0.0003731422 Lei Vandermaen
# idades de 65-81 api rural  RMSE = 0.00145121 Lei Vandermaen  
# idades de 65-81 api urbano RMSE = 0.001625716 Lei Vandermaen
# idades de 65-80 para ATC   RMSE = 0.0005282149 Lei Vandermaen
# idades de 66-81 para BPC   RMSE = 0.0006689453 Lei Vandermaen

## usando a lei Gompertz - homens
# idades de 65-80 total      RMSE = 0.000673296 Lei Gompertz
# idades de 65-80 api rural  RMSE = 0.000532695 Lei Gompertz
# idades de 66-81 api urbano RMSE = 0.001706903 Lei Gompertz
# idades de 66-81 para ATC   RMSE = 0.000469946 Lei Gompertz
# idades de 69-84 para BPC   RMSE = 0.001588005 Lei Gompertz

## usando a lei Gompertz - Mulheres
# idades de 65-80 total      RMSE = 0.000309822 Lei Gompertz  
# idades de 66-81 api rural  RMSE = 0.000719229 Lei Gompertz  
# idades de 65-81 api urbano RMSE = 0.000493986 Lei Gompertz
# idades de 65-80 para ATC   RMSE = 0.000846330 Lei Gompertz
# idades de 66-84 para BPC   RMSE = 0.001344147 Lei Gompertz