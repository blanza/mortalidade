#####################################################################
## Estimativas de tábuas de vida por grupo de beneficiário e sexo ###
#####################################################################

rm(list=ls())

## Instalação de bibliotecas auxiliares
install.packages('data.table')
install.packages('dplyr')

## Lendo as bibliotecas
library(data.table)
library(dplyr)

#############################################################
### Leitura das bases com os diferentes métodos empregados ##
#############################################################

dat = read.csv('final_data.csv', header=TRUE, sep=',')


## Trabalhando apenas com as idades de abaixo de 106 anos
dat=subset(dat, idade <= 105)

names(dat)

## Esta base contém informações do log das taxas que são extraídas, observadas = "logmx", 
## topals = "topals", e as estimativas destas com os limites inferior e superior,
## "topals.inf" e "topals.sup" e pelas 
## leis Van der Maen "vand.hat" e Gompertz "gomp.hat"

## Esta função estima a tábua de vida completa. Leia ela inteira

life.table <- function( x, nMx){
  # simple lifetable using Keyfitz and Flieger separation factors and 
  # exponential tail of death distribution (to close out life table)
  b0 <- 0.07;   b1<- 1.7;      
  nmax <- length(x)
  #nMx = nDx/nKx   
  n <- c(diff(x), 999)          		  # width of the intervals
  nax <- n/2;		            	        # default to .5 of interval
  nax[1] <- b0 + b1 * nMx[1]    		  # from Keyfitz & Flieger(1968)
  nax[nmax] <- 1/nMx[nmax] 	  	      # e_x at open age interval
  nqx <- (n * nMx) / (1 + (n - nax) * nMx)
  nqx<-ifelse(nqx > 1, 1, nqx);		    # necessary for high nMx
  nqx[nmax] <- 1.0
  lx <- c(1, cumprod(1 - nqx));   	  # survivorship lx
  lx <- lx[1:length(nMx)]
  ndx <- lx * nqx;
  nLx <- n * lx - nax * ndx;      	 # equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax] * nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax], NA);
  lt <- data.table(age = x, nqx = nqx, lx = lx, ndx = ndx, nLx = nLx, Tx = Tx, ex = ex, nMx = nMx)
  return(lt)
}

## Criando idades entre 65 a 105 anos 
idade=65:105


## Aqui o usuário deve apenas selecionar o grupo de beneficiário
## api.urb, api.rur, atc, bpc e o sexo, masculino 'm' ou feminino 'f'

## Exemplo de estimação para homens de todos os grupos de beneficíarios
tmp = dat %>%
  filter(grupo =='total', sexo =='m') %>%
  select(idade, sexo, logmx) %>%
  mutate(mx = exp( logmx))
head(tmp)

## Manualmente, selecionamos outro grupo e sexo em grupo == , sexo ==  

## Aqui estimamos a tábua de vida completa para a seleção indicada
mx = tmp$mx
lt = life.table(idade, mx)
lt = lt %>% select(nqx, lx, ndx, nLx, Tx, ex)

## Resultado final
result = cbind(tmp, lt)
result

## Basta exportar os resultados de todas as tábuas para em Excel
## Todas as tábuas estão no arquivo "tabuas de vida por grupo contribuinte"
## Com as informações das tábuas, extraímos os dados, "dx", "lx" e "ex".  


