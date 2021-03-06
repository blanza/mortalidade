############################################
#### An�lise da qualidade da informa��o ####
############################################

rm(list=ls())

##############################################################
#### Necess�rio primeiro instala��o de pacotes auxiliares ####
##############################################################

## instala��o do devtools - auxiliar de leitura
install.packages('devtools')

## leitura do pacote em ambiente R
library(devtools)

## Instalando o pacote Demotools -- remotamente
Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_github("timriffe/DemoTools")

# Escolha a op��o 3, para n�o fazer update. 

## leitura do pacote em ambiente R
library(DemoTools)

## Removendo nota��es cient�ficas
options(scipen = 999)

#########################################
### Prepara��o dos dados utilizados #####
#########################################

## Leitura da base para 2015 
dat=read.csv('dados.rgps15.csv', sep=',', header = T, 
             fileEncoding='UTF-8-BOM')

## Filtrando as informa��es e por sexo e idade

# Selecionando homens com idades entre 65 e 100 anos
dat.m=subset(dat,sexo=='m' & idade<101); 

# Selecionando mulheres com idades entre 65 e 100 anos
dat.f=subset(dat,sexo=='f' & idade<101)

## Gerando uma lista para ambas as bases 
codm=factor(dat.m$grupo)
codf=factor(dat.f$grupo)

tabm=split(dat.m,codm)
tabf=split(dat.f,codf)

#############################################################################
# Primeira an�lise -- testando prefer�ncia digital pelo m�todo Coale and Li #
#############################################################################

## Testes de pref�ncias por d�gitos 0 e 5. 
# OBS: Pode testar outros d�gitos. Baste alterar o argumento 'digit' e colocar outro valor de 0 a 9, por ex.

## testes para dados masculinos de popula��o e �bitos

# Popula��o
h.res.pop <- lapply(tabm, function(x) 
{check_heaping_coale_li(x$pop, Age=65:100, ageMin = 65, 
                        ageMax=100, terms = 5, digit=c(0,5))})
# �bitos
h.res.ob <- lapply(tabm, function(x) 
{check_heaping_coale_li(x$obito, Age=65:100, ageMin = 65, 
                        ageMax=100, terms = 5, digit=c(0,5))})

## testes para dados femininos de popula��o e �bitos

# Popula��o
m.res.pop <- lapply(tabf, function(x) 
{check_heaping_coale_li(x$pop, Age=65:100, ageMin = 65, 
                        ageMax=100, terms = 5, digit=c(0,5))})

# �bitos
m.res.ob <- lapply(tabf, function(x) 
{check_heaping_coale_li(x$obito, Age=65:100, ageMin = 65, 
                        ageMax=100, terms = 5, digit=c(0,5))})

## Resultados ###

# Rearranjando e Juntando resultados de ambos os sexos e por grupo
coale.li=rbind(unique(unsplit(h.res.pop, codm)),unique(unsplit(h.res.ob, codm)),
               unique(unsplit(m.res.pop, codf)),unique(unsplit(m.res.ob, codf))
)
colnames(coale.li)=c("total", "atc", "api.urb", "api.rur", "bpc")
rownames(coale.li)=c("pop masculina","obito masculino","pop feminina","obito feminino")

## Resultado final
coale.li

# Interpreta��o. Valores mais pr�ximos de 0 indicam inexist�ncia de prefer�ncia por d�gito. Valores pr�ximos a 5, muita prefer�ncia digital. 

# Resultado: Tanto informa��es de �bito quanto popula��o n�o apresentam prefer�ncia digital em 0 e 5 entre as idades de 65 a 100, para ambos os sexos.

###############################################################
### Segunda an�lise -- �ndice de prefer�ncia digital Jdanov ###   
###############################################################

## Gerando o �ndice para Su�cia, padr�o ouro como base comparativa para an�lise de prefer�ncia digital de mortes em idades avan�adas

## Base de �bitos da Su�cia
d.swe=read.table('Deaths_1x1_Sweden.txt',sep='', header=T, skip=2)

# uma informa��o de idade
idade=0:110

# Selecionando anos recentes e idades 65 a 110
d.swe.r=subset(d.swe, Year > 2009 & idade>64)

# Particionando a informa��o por ano
codswe=factor(d.swe.r$Year);
tabswe=split(d.swe.r,codswe);

# Teste para homens entre os anos de 2010 a 2019
res.swe.h <- lapply(tabswe, function(x) 
{check_heaping_jdanov(x$Male, Age=65:110, Agei = c(95,100,105))})

# Teste para mulheres entre os anos de 2010 a 2019
res.swe.m <- lapply(tabswe, function(x) 
{check_heaping_jdanov(x$Female, Age=65:110, Agei = c(95,100,105))})

## Rearranjando os resultados
suecia=rbind(unique(unsplit(res.swe.h, codswe)),unique(unsplit(res.swe.m, codswe)))
rownames(suecia)=c('homem','mulher')
colnames(suecia)=2010:2019

# Valores do padr�o "ouro" Su�cia
suecia

# Ambos ficam entre o intervalo de 88-104

# valor m�dio
rowMeans(suecia)
# Valores m�dios, 94.7 para homens e 97.5 para mulheres


### Nossos dados e novas sele��es et�rias ###
dadom=subset(dat,sexo=='m' & idade<111); dadof=subset(dat,sexo=='f' & idade<111)

# Gerando as listas para as estimativas
breakm=factor(dadom$grupo);listm=split(dadom,breakm)
breakf=factor(dadof$grupo);listf=split(dadof,breakf)

# Teste homens
res.m <- lapply(listm, function(x) 
{check_heaping_jdanov(x$obito, Age=65:110, Agei = c(95,100,105))})

# Teste mulheres
res.f <- lapply(listf, function(x) 
{check_heaping_jdanov(x$obito, Age=65:110, Agei = c(95,100,105))})

## Rearranjando e juntando as informa��es
indice.jdanov=rbind(unique(unsplit(res.m, breakm)),unique(unsplit(res.f, breakf)))
rownames(indice.jdanov)=c('homem','mulher')
colnames(indice.jdanov)=c("total", "atc", "api.urb", "api.rur", "bpc")

## Resultados do �ndice Jdanov para os grupos e sexos
indice.jdanov