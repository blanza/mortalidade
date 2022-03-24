############################################
#### Análise da qualidade da informação ####
############################################

rm(list=ls())

##############################################################
#### Necessário primeiro instalação de pacotes auxiliares ####
##############################################################

## instalação do devtools - auxiliar de leitura
install.packages('devtools')

## leitura do pacote em ambiente R
library(devtools)

## Instalando o pacote Demotools -- remotamente
Sys.setenv(R_REMOTES_STANDALONE="true")
remotes::install_github("timriffe/DemoTools")

# Escolha a opção 3, para não fazer update. 

## leitura do pacote em ambiente R
library(DemoTools)

## Removendo notações científicas
options(scipen = 999)

#########################################
### Preparação dos dados utilizados #####
#########################################

## Leitura da base para 2015 
dat=read.csv('dados.rgps15.csv', sep=',', header = T, 
             fileEncoding='UTF-8-BOM')

## Filtrando as informações e por sexo e idade

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
# Primeira análise -- testando preferência digital pelo método Coale and Li #
#############################################################################

## Testes de prefências por dígitos 0 e 5. 
# OBS: Pode testar outros dígitos. Baste alterar o argumento 'digit' e colocar outro valor de 0 a 9, por ex.

## testes para dados masculinos de população e óbitos

# População
h.res.pop <- lapply(tabm, function(x) 
{check_heaping_coale_li(x$pop, Age=65:100, ageMin = 65, 
                        ageMax=100, terms = 5, digit=c(0,5))})
# óbitos
h.res.ob <- lapply(tabm, function(x) 
{check_heaping_coale_li(x$obito, Age=65:100, ageMin = 65, 
                        ageMax=100, terms = 5, digit=c(0,5))})

## testes para dados femininos de população e óbitos

# População
m.res.pop <- lapply(tabf, function(x) 
{check_heaping_coale_li(x$pop, Age=65:100, ageMin = 65, 
                        ageMax=100, terms = 5, digit=c(0,5))})

# óbitos
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

# Interpretação. Valores mais próximos de 0 indicam inexistência de preferência por dígito. Valores próximos a 5, muita preferência digital. 

# Resultado: Tanto informações de óbito quanto população não apresentam preferência digital em 0 e 5 entre as idades de 65 a 100, para ambos os sexos.

###############################################################
### Segunda análise -- Índice de preferência digital Jdanov ###   
###############################################################

## Gerando o índice para Suécia, padrão ouro como base comparativa para análise de preferência digital de mortes em idades avançadas

## Base de óbitos da Suécia
d.swe=read.table('Deaths_1x1_Sweden.txt',sep='', header=T, skip=2)

# uma informação de idade
idade=0:110

# Selecionando anos recentes e idades 65 a 110
d.swe.r=subset(d.swe, Year > 2009 & idade>64)

# Particionando a informação por ano
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

# Valores do padrão "ouro" Suécia
suecia

# Ambos ficam entre o intervalo de 88-104

# valor médio
rowMeans(suecia)
# Valores médios, 94.7 para homens e 97.5 para mulheres


### Nossos dados e novas seleções etárias ###
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

## Rearranjando e juntando as informações
indice.jdanov=rbind(unique(unsplit(res.m, breakm)),unique(unsplit(res.f, breakf)))
rownames(indice.jdanov)=c('homem','mulher')
colnames(indice.jdanov)=c("total", "atc", "api.urb", "api.rur", "bpc")

## Resultados do índice Jdanov para os grupos e sexos
indice.jdanov