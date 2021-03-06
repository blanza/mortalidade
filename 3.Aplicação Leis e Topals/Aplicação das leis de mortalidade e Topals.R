#####################################################################
#### Aplicando as leis de mortalidade depois da sele��o de idades ###
#####################################################################

rm(list=ls())

### Bibliotecas necess�rias 

## Instala��o 
install.packages('MortalityLaws')

## leitura das bibliotecas
library(MortalityLaws)


## Remo��o de nota��o cient�fica
options(scipen = 999)

###########################################
### Leitura dos dados para o ano de 2015 ##
###########################################
dat=read.csv('dados.rgps15.csv', sep=',', header = T, 
             fileEncoding='UTF-8-BOM')

## Verificando informa��es no banco
names(dat)

## Estimando taxas de mortalidade  
dat$mx=dat$obito/dat$pop

## separando as informa��es por sexos
dat.m=subset(dat,sexo=='m');dat.f=subset(dat,sexo=='f')

## Estimando as taxas de mortalidade para os diferentes grupos 
codi=factor(dat.m$grupo);
tabm=split(dat.m,codi);tabf=split(dat.f,codi)

####################################
### Aplicando a lei de Gompertz ####
####################################

## Para homens - modelo Gompertz com diferentes idades. A sele��o de idades foi feita pelo RMSE. Estimamos taxas at� a idade dos 115 anos.

# ignore as mensagens de erro na fun��o
lapply(tabm, 
       tabm[]$total$gomp<-predict(MortalityLaw(x=65:80, mx=tabm[]$total$mx[1:16],                                          law='gompertz'),65:115))
lapply(tabm, 
       tabm[]$api.rur$gomp<-predict(MortalityLaw(x=65:80, mx=tabm[]$api.rur$mx[1:16],                                          law='gompertz'),65:115))
lapply(tabm,       
       tabm[]$api.urb$gomp<-predict(MortalityLaw(x=66:81, mx=tabm[]$api.urb$mx[2:17],                                           law='gompertz'),65:115))
lapply(tabm,
       tabm[]$atc$gomp<-predict(MortalityLaw(x=66:81, mx=tabm[]$atc$mx[2:17],                                           law='gompertz'),65:115))
lapply(tabm,
       tabm[]$bpc$gomp<-predict(MortalityLaw(x=69:84, mx=tabm[]$bpc$mx[5:20],                                           law='gompertz'),65:115))

## Agregando o resultado numa �nica base
tabm1=unsplit(tabm,codi)

names(tabm1)

## Vendo a estimativas para as cinco primeiras e �ltimas linhas 
head(tabm1);tail(tabm1)

## Para mulheres repetimos o procedimento

# ignore as mensagens de erro na fun��o
lapply(tabf, 
       tabf[]$total$gomp<-predict(MortalityLaw(x=65:80, mx=tabf[]$total$mx[1:16],                                          law='gompertz'),65:115))
lapply(tabf, 
       tabf[]$api.rur$gomp<-predict(MortalityLaw(x=66:81, mx=tabf[]$api.rur$mx[2:17],                                          law='gompertz'),65:115))
lapply(tabf,       
       tabf[]$api.urb$gomp<-predict(MortalityLaw(x=65:81, mx=tabf[]$api.urb$mx[1:17],                                           law='gompertz'),65:115))
lapply(tabf,
       tabf[]$atc$gomp<-predict(MortalityLaw(x=65:80, mx=tabf[]$atc$mx[1:16],                                           law='gompertz'),65:115))
lapply(tabf,
       tabf[]$bpc$gomp<-predict(MortalityLaw(x=66:84, mx=tabf[]$bpc$mx[2:20],                                           law='gompertz'),65:115))

tabf1=unsplit(tabf,codi)

## Vendo os resultados em �nica base
names(tabf1)
head(tabf1);tail(tabf1)

## Aqui temos as taxas observadas e extrapoladas pela lei Gompertz
View(tabf1);View(tabm1)

#######################################################
## Repetimos o procedimento com a lei Van der Maen ####
#######################################################

## Para homens - modelo Van der Maen com diferentes idades. Sele��o das idades feita pelo RMSE

# ignore as mensagens de erro na fun��o
lapply(tabm, 
       tabm[]$total$maen<-predict(MortalityLaw(x=66:81, mx=tabm[]$total$mx[2:17],                                          law='vandermaen'),65:115))
lapply(tabm, 
       tabm[]$api.rur$maen<-predict(MortalityLaw(x=65:80, mx=tabm[]$api.rur$mx[1:16],                                          law='vandermaen'),65:115))
lapply(tabm,       
       tabm[]$api.urb$maen<-predict(MortalityLaw(x=65:80, mx=tabm[]$api.urb$mx[1:16],                                           law='vandermaen'),65:115))
lapply(tabm,
       tabm[]$atc$maen<-predict(MortalityLaw(x=67:82, mx=tabm[]$atc$mx[3:18],                                           law='vandermaen'),65:115))
lapply(tabm,
       tabm[]$bpc$maen<-predict(MortalityLaw(x=68:83, mx=tabm[]$bpc$mx[4:19],                                           law='vandermaen'),65:115))

tabm1=unsplit(tabm,codi)

names(tabm1)
head(tabm1);tail(tabm1)

## Para mulheres - modelo Van der Maen com diferentes idades. Sele��o foi feita pelo RMSE 

# ignore as mensagens de erro na fun��o
lapply(tabf, 
       tabf[]$total$maen<-predict(MortalityLaw(x=66:81, mx=tabf[]$total$mx[2:17],                                          law='vandermaen'),65:115))
lapply(tabf, 
       tabf[]$api.rur$maen<-predict(MortalityLaw(x=65:81, mx=tabf[]$api.rur$mx[1:17],                                          law='vandermaen'),65:115))
lapply(tabf,       
       tabf[]$api.urb$maen<-predict(MortalityLaw(x=65:81, mx=tabf[]$api.urb$mx[1:17],                                           law='vandermaen'),65:115))
lapply(tabf,
       tabf[]$atc$maen<-predict(MortalityLaw(x=65:80, mx=tabf[]$atc$mx[1:16],                                           law='vandermaen'),65:115))
lapply(tabf,
       tabf[]$bpc$maen<-predict(MortalityLaw(x=66:81, mx=tabf[]$bpc$mx[2:17],                                           law='vandermaen'),65:115))

tabf1=unsplit(tabf,codi)

names(tabf1)
head(tabf1);tail(tabf1)

## Aqui temos as taxas observadas e extrapoladas pelas lei Gompertz e Van der Maen
View(tabf1);View(tabm1)

## Usu�rio pode salvar as bases em formato csv com a fun��o, "write.csv"




############################################################################
#### Aplicando do Modelo Topals com base no padr�o de mortalidade do HMD ###
############################################################################

### Pacotes necess�rios ###

library(dplyr)
library(splines)

### Dados de entrada ###

# Evento e exposi��o por sexo, idade e ano calend�rio do RGPS:
rgps  = read.csv('dados.rgps.csv', header=TRUE, 
                 stringsAsFactors = FALSE, sep=';')
# T�buas de vida dispon�veis no Humam Mortality Database (https://www.mortality.org/):
HMDdata     <- read.table("pooled_Br_MHD-mx-pop-schedules.txt", header=T, 
                          sep="", dec=".", stringsAsFactors = FALSE)
HMDdata$mx  <- as.numeric(HMDdata$mx)
HMDdata$pop <- as.numeric(HMDdata$pop)

### Padr�o ouro - taxas de mortalidade de v�rias pa�ses do HMD ###

# Padr�o feminino:
HMDdata <- HMDdata %>% filter(!is.na(mx))
group = list(
  allHMD   = unique(HMDdata$ctry),
  Chile = 'Chile',
  Sweden = 'Sweden',
  France = 'France',
  EEurope = c('Russia','Slovakia','Poland','Lithuania','Belarus','Bulgaria',
              'Estonia', 'Slovenia','Latvia', 'Hungary'),
  Anglo = c('Ireland','UK','USA','Australia','New_Zealand'),
  Asia  = c('Japan','Taiwan')
)
for (nm in names(group)) {
  tmp = HMDdata %>%
    filter( ctry %in% group[[nm]], sex=='f', year > 1980) %>%
    group_by(age) %>%
    summarize( logmx = log(weighted.mean(mx, pop)))
  assign(nm, tmp$logmx)
}
# Su�cia pr� 1800:
Sweden1700s = (HMDdata %>%
                 filter( ctry == 'Sweden', sex=='f', year %in% 1700:1799) %>%
                 group_by(age) %>%
                 summarize( logmx = log(weighted.mean(mx, pop))))$logmx 
group[['Sweden1700s']] = 'Sweden 18th century'
standards = data.frame(age = 0:110)
for (k in names(group)) standards[[k]] = get(k)
padrao.fem = round(standards,4)
padrao.fem = padrao.fem %>%
  filter(age >=65) %>%
  select(allHMD)
padrao.fem = as.matrix(padrao.fem)
write.csv(padrao.fem, 'padrao.fem.HMD.csv', row.names = FALSE)

# Padr�o masculino:
for (nm in names(group)) {
  tmp = HMDdata %>%
    filter( ctry %in% group[[nm]], sex=='m', year > 1980) %>%
    group_by(age) %>%
    summarize( logmx = log(weighted.mean(mx, pop)))
  assign(nm, tmp$logmx)
}
# Su�cia pr� 1800:
Sweden1700s = (HMDdata %>%
                 filter( ctry == 'Sweden', sex=='m', year %in% 1700:1799) %>%
                 group_by(age) %>%
                 summarize( logmx = log(weighted.mean(mx, pop))))$logmx 
group[['Sweden1700s']] = 'Sweden 18th century'
standards = data.frame(age = 0:110)
for (k in names(group)) standards[[k]] = get(k)
padrao.masc = round(standards,4)
padrao.masc = padrao.masc %>%
  filter(age >=65) %>%
  select(allHMD)
padrao.masc = as.matrix(padrao.masc)
write.csv(padrao.masc, 'padrao.masc.HMD.csv', row.names = FALSE)

### Fun��o Topals para os grupos de benefici�rios: Total, ATC e API-UBR ###

# In�cio da Fun��o Totpals (explica��o detalhadas sobre o modelo e a 
# fun��o dispon�vel em <https://topals-mortality.schmert.net/>)
TOPALS_fit = function( N, D, std,
                       max_age        = 110,
                       knot_positions = c(65,70,75,80,85,95), 
                       smoothing_k    = 1,
                       max_iter       = 20,
                       alpha_tol      = .00005,
                       details        = FALSE) {
  require(splines)
  # vetor de idade por anos simples de 65 a max_age
  age = 65:max_age
  # B � uma matriz Ax7. Cada coluna � um fun��o de base linear (B-spline)
  B      = splines::bs( age, knots=knot_positions, degree=1 )
  nalpha = ncol(B) 
  # Fun��o de verossimilhan�a penalizada
  # O termo de penalidade aqui - diff(alpha)^2 - n�o tem efeito significativo
  # quando a exposi��o � relativamente elevada
  Q = function(alpha) {
    lambda.hat = as.numeric( std + B %*% alpha)
    penalty    = smoothing_k * sum( diff(alpha)^2 )
    return( sum(D * lambda.hat - N * exp(lambda.hat)) - penalty)
  }
  # Fun��o de �bitos esperados:
  Dhat = function(alpha) {
    lambda.hat = std + B %*% alpha
    return(  as.numeric( N * exp(lambda.hat) ))
  }      
  # Matriz S para penalidade
  S = matrix(0,nalpha-1,nalpha) 
  diag(S[, 1:(nalpha-1)]) = -1
  diag(S[, 2:(nalpha)  ]) = +1
  SS = crossprod(S)
  # Fun��o de intera��o: pr�ximo vetor alpha com uma fun�o do alpha atual
  next_alpha = function(alpha) {
    dhat = Dhat(alpha)
    M = solve ( t(B) %*% diag(dhat) %*% B + 2*smoothing_k *SS)
    v = t(B) %*% (D - dhat) - 2* (smoothing_k * (SS %*% alpha))
    return( alpha + M %*% v)
  }
  # Intera��o principal:     
  a = rep(0, nalpha)
  niter = 0
  repeat {
    niter      = niter + 1
    last_param = a
    a          = next_alpha( a )# atualiza
    change     = a - last_param
    converge = all( abs(change) < alpha_tol)
    overrun  = (niter == max_iter)
    if (converge | overrun) { break }
  } # repete
  if (details | !converge | overrun) {
    if (!converge) print('did not converge')
    if (overrun) print('exceeded maximum number of iterations')
    dhat = Dhat(a)
    covar = solve( t(B) %*% diag(dhat) %*% B + 2*smoothing_k *SS)
    return( list( alpha    = a, 
                  covar    = covar,
                  Qvalue   = Q(a),
                  converge = converge, 
                  maxiter  = overrun))
  } else return( a) 
} 
# Fim da fun��o TOPALS

# Rodando a fun��o Topals para os grupos de benefici�rios: Total, ATC e API-UBR em 2015:
rgps.atc.urb = filter(rgps, grupo=='atc' | grupo=='api.urb' | grupo=='total')
idade = 65:110
grupo = unique(rgps.atc.urb$grupo)
B   = bs(65:110, knots=c(65,70,75,80,85,95), degree=1 )
# Estimativas para homens:
rgps.fit.m = as.data.frame(matrix(NA, 0, 10))
for(this.group in grupo){
  Poisson.par = rgps.atc.urb %>%
    filter(grupo==this.group, sexo=='m', idade<111, ano==2015) %>% 
    mutate(logmx = log(obito/pop))
  N = Poisson.par$pop
  mu = Poisson.par$logmx
  D = Poisson.par$obito
  fit = TOPALS_fit(N, D, std=padrao.masc,details=TRUE)
  L = padrao.masc + B %*% fit$a   # matrix de log(mx) ajustada
  se_logmx = sqrt( diag (B %*% fit$covar %*% t(B)) ) # erro padr�o para log(mx)
  Q5.logmx  = L -1.96 * se_logmx
  Q95.logmx = L +1.96 * se_logmx
  tmp = cbind(Poisson.par, L, Q5.logmx, Q95.logmx)
  rgps.fit.m = rbind(rgps.fit.m, tmp)
}
colnames(rgps.fit.m) = c('ano','idade', 'sexo', 'grupo','pop', 'obito', 'logmx',
                         'logmx.hat', 'logmx.hat.inf', 'logmx.hat.sup')
# Estimativas para mulheres:
rgps.fit.f = as.data.frame(matrix(NA, 0, 10))
for(this.group in grupo){
  Poisson.par = rgps.atc.urb %>%
    filter(grupo==this.group, sexo=='f', idade<111, ano==2015) %>% 
    mutate(logmx = log(obito/pop))
  N = Poisson.par$pop
  mu = Poisson.par$logmx
  D = Poisson.par$obito
  fit = TOPALS_fit(N, D, std=padrao.fem,details=TRUE)
  L = padrao.fem + B %*% fit$a   # matrix de log(mx) ajustada
  se_logmx = sqrt( diag (B %*% fit$covar %*% t(B)) ) # erro padr�o para log(mx)
  Q5.logmx  = L -1.96 * se_logmx
  Q95.logmx = L +1.96 * se_logmx
  tmp = cbind(Poisson.par, L, Q5.logmx, Q95.logmx)
  rgps.fit.f = rbind(rgps.fit.f, tmp)
}
colnames(rgps.fit.f) = c('ano','idade', 'sexo', 'grupo','pop', 'obito', 'logmx',
                         'logmx.hat', 'logmx.hat.inf', 'logmx.hat.sup')
# Resultado em base �nica:
rgps.fit1 = rbind(rgps.fit.f, rgps.fit.m)
write.csv(rgps.fit1, 'rgps.topals.fit.2015[1].csv', row.names = FALSE)

### Fun��o Topals para os grupos de benefici�rios: BPC e API-RUR ###

# In�cio da Fun��o Totpals
TOPALS_fit = function( N, D, std,
                       max_age        = 110,
                       knot_positions = c(65,70,75,80), 
                       smoothing_k    = 1,
                       max_iter       = 20,
                       alpha_tol      = .00005,
                       details        = FALSE) {
  require(splines)
  # vetor de idade por anos simples de 65 a max_age
  age = 65:max_age
  # B � uma matriz Ax7. Cada coluna � um fun��o de base linear (B-spline)
  B      = splines::bs( age, knots=knot_positions, degree=1 )
  nalpha = ncol(B) 
  # Fun��o de verossimilhan�a penalizada
  # O termo de penalidade aqui - diff(alpha)^2 - n�o tem efeito significativo
  # quando a exposi��o � relativamente elevada
  Q = function(alpha) {
    lambda.hat = as.numeric( std + B %*% alpha)
    penalty    = smoothing_k * sum( diff(alpha)^2 )
    return( sum(D * lambda.hat - N * exp(lambda.hat)) - penalty)
  }
  # Fun��o de �bitos esperados:
  Dhat = function(alpha) {
    lambda.hat = std + B %*% alpha
    return(  as.numeric( N * exp(lambda.hat) ))
  }      
  # Matriz S para penalidade
  S = matrix(0,nalpha-1,nalpha) 
  diag(S[, 1:(nalpha-1)]) = -1
  diag(S[, 2:(nalpha)  ]) = +1
  SS = crossprod(S)
  # Fun��o de intera��o: pr�ximo vetor alpha com uma fun�o do alpha atual
  next_alpha = function(alpha) {
    dhat = Dhat(alpha)
    M = solve ( t(B) %*% diag(dhat) %*% B + 2*smoothing_k *SS)
    v = t(B) %*% (D - dhat) - 2* (smoothing_k * (SS %*% alpha))
    return( alpha + M %*% v)
  }
  # Intera��o principal:     
  a = rep(0, nalpha)
  niter = 0
  repeat {
    niter      = niter + 1
    last_param = a
    a          = next_alpha( a )# atualiza
    change     = a - last_param
    converge = all( abs(change) < alpha_tol)
    overrun  = (niter == max_iter)
    if (converge | overrun) { break }
  } # repete
  if (details | !converge | overrun) {
    if (!converge) print('did not converge')
    if (overrun) print('exceeded maximum number of iterations')
    dhat = Dhat(a)
    covar = solve( t(B) %*% diag(dhat) %*% B + 2*smoothing_k *SS)
    return( list( alpha    = a, 
                  covar    = covar,
                  Qvalue   = Q(a),
                  converge = converge, 
                  maxiter  = overrun))
  } else return( a) 
} 
# Fim da fun��o TOPALS

# Rodando a fun��o Topals para os grupos de benefici�rios: Total, ATC e API-UBR em 2015:
rgps.bpc.rur = filter(rgps, grupo=='bpc' | grupo=='api.rur')
idade = 65:110
grupo = unique(rgps.bpc.rur$grupo)
B   = bs(65:110, knots=c(65,70,75,80), degree=1 )
# Estimativas para homens:
rgps.fit.m = as.data.frame(matrix(NA, 0, 10))
for(this.group in grupo){
  Poisson.par = rgps.bpc.rur %>%
    filter(grupo==this.group, sexo=='m', idade<111, ano==2015) %>% 
    mutate(logmx = log(obito/pop))
  N = Poisson.par$pop
  mu = Poisson.par$logmx
  D = Poisson.par$obito
  fit = TOPALS_fit(N, D, std=padrao.masc,details=TRUE)
  L = padrao.masc + B %*% fit$a   # matrix de log(mx) ajustada
  se_logmx = sqrt( diag (B %*% fit$covar %*% t(B)) ) # erro padr�o para log(mx)
  Q5.logmx  = L -1.96 * se_logmx
  Q95.logmx = L +1.96 * se_logmx
  tmp = cbind(Poisson.par, L, Q5.logmx, Q95.logmx)
  rgps.fit.m = rbind(rgps.fit.m, tmp)
}
colnames(rgps.fit.m) = c('ano','idade', 'sexo', 'grupo','pop', 'obito', 'logmx',
                         'logmx.hat', 'logmx.hat.inf', 'logmx.hat.sup')
# Estimativas para mulheres:
rgps.fit.f = as.data.frame(matrix(NA, 0, 10))
for(this.group in grupo){
  Poisson.par = rgps.bpc.rur %>%
    filter(grupo==this.group, sexo=='f', idade<111, ano==2015) %>% 
    mutate(logmx = log(obito/pop))
  N = Poisson.par$pop
  mu = Poisson.par$logmx
  D = Poisson.par$obito
  fit = TOPALS_fit(N, D, std=padrao.fem,details=TRUE)
  L = padrao.fem + B %*% fit$a   # matrix de log(mx) ajustada
  se_logmx = sqrt( diag (B %*% fit$covar %*% t(B)) ) # erro padr�o para log(mx)
  Q5.logmx  = L -1.96 * se_logmx
  Q95.logmx = L +1.96 * se_logmx
  tmp = cbind(Poisson.par, L, Q5.logmx, Q95.logmx)
  rgps.fit.f = rbind(rgps.fit.f, tmp)
}
colnames(rgps.fit.f) = c('ano','idade', 'sexo', 'grupo','pop', 'obito', 'logmx',
                         'logmx.hat', 'logmx.hat.inf', 'logmx.hat.sup')
# Resultado em base �nica
rgps.fit2 = rbind(rgps.fit.f, rgps.fit.m)
write.csv(rgps.fit2, 'rgps.topals.fit.2015[2].csv', row.names = FALSE)






