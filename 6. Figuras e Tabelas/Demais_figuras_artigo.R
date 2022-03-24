#--------------------------------
# Gráficos Artigo
#--------------------------------

rm(list=ls())

library(dplyr)

graphics.off()
if (.Platform$OS.type == 'windows') windows(record=TRUE)

### Dados de entrada #####

# Modelo Topals:
rgps.tp1  = read.csv('rgps.topals.fit.2015[1].csv', header=TRUE, 
                     stringsAsFactors = FALSE, sep=',')
rgps.tp2  = read.csv('rgps.topals.fit.2015[2].csv', header=TRUE, 
                     stringsAsFactors = FALSE, sep=',')
# Modelo Van Der Maen:
vand.m  = read.csv('estvandermaen_homens15.csv', header=TRUE, 
                   stringsAsFactors = FALSE, sep=',')
vand.f  = read.csv('estvandermaen_mulheres15.csv', header=TRUE, 
                   stringsAsFactors = FALSE, sep=',')
# Modelo Gompertz:
gomp  = read.csv('gompertz.fit.csv', header=TRUE, stringsAsFactors = FALSE, sep=',')

data.bygrupo  = read.csv('final_data.csv', header=TRUE, 
                         stringsAsFactors = FALSE, sep=',')
# Tábuas de vida baseadas nas taxas observadas e nas estimativas por todos os modelos:
data  = read.csv('tabua_de_vida.csv', header=TRUE, 
                 stringsAsFactors = FALSE, sep=';')
# Padrão de Mortalidade do Human Mortality Database:
hmd.masc  = read.csv('padrao.masc.HMD.csv', header=TRUE, 
                     stringsAsFactors = FALSE, sep=';')
hmd.fem  = read.csv('padrao.fem.HMD.csv', header=TRUE, 
                    stringsAsFactors = FALSE, sep=';')
#Tábua de vida completa do IBGE extrapolada para a idade de 110 anos:
ibge.extrap  = read.csv('tabua.vida.ibge-extrapolada.csv', header=TRUE, 
                        stringsAsFactors = FALSE, sep=';')

data = mutate(data, logqx=log(nqx))

ibge.extrap = filter(ibge.extrap, idade>64 & idade<106)
ibge.extrap$logqx = log(ibge.extrap$qx) 


# Organizando os dados:
rgps.fit = rbind(rgps.tp1, rgps.tp2)
vand.fit = rbind(vand.m, vand.f)
vand.fit = vand.fit %>%
        filter(idade<111) %>%
        mutate(logmx=log(mx), logmx.hat=log(maen))
gomp = gomp %>%
        filter(idade<111) %>%
        mutate(logmx=log(mx), logmx.hat=log(gomp))
idade = 65:110

## Convertendo mx em qx:
hmd.masc = hmd.masc %>%
        mutate(mx=exp(allHMD)) %>%
        mutate(qx=mx*1/(1+(1-0.5)*mx)) %>%
        mutate(logqx=log(qx))
hmd.masc$idade = 65:110
hmd.masc=filter(hmd.masc, idade<106)

hmd.fem = hmd.fem %>%
        mutate(mx=exp(allHMD)) %>%
        mutate(qx=mx*1/(1+(1-0.5)*mx)) %>%
        mutate(logqx=log(qx))
hmd.fem$idade = 65:110
hmd.fem=filter(hmd.fem, idade<106)

grupo.fit = data.bygrupo %>%
        mutate(logqx = log(exp(logmx)*1/(1+(1-0.5)*exp(logmx)))) %>%
        mutate(topals.fit = log(exp(topals)*1/(1+(1-0.5)*exp(topals)))) %>%
        mutate(topals.fit.inf = log(exp(topals.inf)*1/(1+(1-0.5)*exp(topals.inf)))) %>%
        mutate(topals.fit.sup = log(exp(topals.sup)*1/(1+(1-0.5)*exp(topals.sup)))) %>%
        mutate(vand.fit = log(exp(vand.hat)*1/(1+(1-0.5)*exp(vand.hat)))) %>%
        select(idade, sexo, grupo, logqx, topals.fit, topals.fit.inf, topals.fit.sup, vand.fit)


#### FIGURA 1 DO ARTIGO ####

jpeg("Figura1.jpeg",  width=15, height=10, unit='in', res=300)
# Parâmetros gráficos:
par(mfrow=c(2,4))
par(cex = 0.9) 
par(mgp = c(2, 1, 0))
par(mar = c(3, 3, 2, 0), oma = c(0, 0, 0, 0))
idade = 65:110

tmp = filter(rgps.fit, grupo=='atc', sexo=='m')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='ATC (Homens)', xlab = "", ylab = "Log taxa de mortalidade") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='atc', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='atc', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")

tmp = filter(rgps.fit, grupo=='api.urb', sexo=='m')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-URB (Homens)', xlab = "", ylab = "") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='api.urb', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='api.urb', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")

tmp = filter(rgps.fit, grupo=='bpc', sexo=='m')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='BPC (Homens)', xlab = "", ylab = "") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='bpc', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='bpc', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")

tmp = filter(rgps.fit, grupo=='api.rur', sexo=='m')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-RUR (Homens)', xlab = "", ylab = "") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='api.rur', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='api.rur', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")


tmp = filter(rgps.fit, grupo=='atc', sexo=='f')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='ATC (Mulheres)', xlab = "Idade", ylab = "Log taxa de mortalidade") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='atc', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='atc', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")

tmp = filter(rgps.fit, grupo=='api.urb', sexo=='f')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-URB (Mulheres)', xlab = "Idade", ylab = "") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='api.urb', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='api.urb', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")

tmp = filter(rgps.fit, grupo=='bpc', sexo=='f')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='BPC (Mulheres)', xlab = "Idade", ylab = "") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='bpc', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='bpc', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")

tmp = filter(rgps.fit, grupo=='api.rur', sexo=='f')
plot(idade, tmp$logmx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-RUR (Mulheres)', xlab = "Idade", ylab = "") 
lines(idade, tmp$logmx.hat, col='darkred', lwd=3)
segments(idade, tmp$logmx.hat.inf, idade, tmp$logmx.hat.sup, col='darkred', lwd=1)
tmp = filter(vand.fit, grupo=='api.rur', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkblue', lwd=3)
tmp = filter(gomp, grupo=='api.rur', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkgreen', lwd=3)
legend(65, 0, legend = c("Log(mx) obs", "Topals (IC 95%)", "Van der Maen",
                         'Gompertz'), 
       col = c("black", "darkred", "darkblue",'darkgreen'), pch = c(1), 
       lwd = c('', 3, 3,3), bty="n")

dev.off()


### FIGURA 2 DO ARTIGO #####
jpeg("Figura2.jpeg", width=9, height=12, unit='in', res=300)

par(mfrow=c(3,2))
par(cex = 0.9) #altera tamanho da fonte do nome dos eixos
par(mgp = c(2, 1, 0)) #altera distância dos nomes e valores dos eixos em relação ao gráfico
par(mar = c(3, 3, 2, 0), oma = c(0, 0, 0, 0)) # altera a distância entre os limites do gráficos e as bordas da janela gráfica

tmp = filter(rgps.fit, grupo=='atc', sexo=='m')
plot(idade, tmp$logmx.hat, type = 'l', ylim=c(-5,0), col='brown4', lwd=3,
     main='Topals - Homens', xlab = "Idade", ylab = "Log taxa de mortalidade") 
tmp = filter(rgps.fit, grupo=='api.urb', sexo=='m')
lines(idade, tmp$logmx.hat, col='aquamarine4', lwd=3)
tmp = filter(rgps.fit, grupo=='api.rur', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkviolet', lwd=3)
tmp = filter(rgps.fit, grupo=='bpc', sexo=='m')
lines(idade, tmp$logmx.hat, col='chocolate', lwd=3)
legend(65, 0, legend = c("ATC", "API-URB", "API-RUR", "BPC" ), 
       col = c("brown4", "aquamarine4", "darkviolet", "chocolate"),  
       lwd = c(3, 3, 3, 3), bty="n")

tmp = filter(rgps.fit, grupo=='atc', sexo=='f')
plot(idade, tmp$logmx.hat, type = 'l', ylim=c(-5,0), col='brown4', lwd=3,
     main='Topals - Mulheres', xlab = "Idade", ylab = "Log taxa de mortalidade") 
tmp = filter(rgps.fit, grupo=='api.urb', sexo=='f')
lines(idade, tmp$logmx.hat, col='aquamarine4', lwd=3)
tmp = filter(rgps.fit, grupo=='api.rur', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkviolet', lwd=3)
tmp = filter(rgps.fit, grupo=='bpc', sexo=='f')
lines(idade, tmp$logmx.hat, col='chocolate', lwd=3)
legend(65, 0, legend = c("ATC", "API-URB", "API-RUR", "BPC" ), 
       col = c("brown4", "aquamarine4", "darkviolet", "chocolate"),  
       lwd = c(3, 3, 3, 3), bty="n")

tmp = filter(vand.fit, grupo=='atc', sexo=='m')
plot(idade, tmp$logmx.hat, type = 'l', ylim=c(-5,0), col='brown4', lwd=3,
     main='Vandermaen - Homens', xlab = "Idade", ylab = "Log taxa de mortalidade") 
tmp = filter(vand.fit, grupo=='api.urb', sexo=='m')
lines(idade, tmp$logmx.hat, col='aquamarine4', lwd=3)
tmp = filter(vand.fit, grupo=='api.rur', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkviolet', lwd=3)
tmp = filter(vand.fit, grupo=='bpc', sexo=='m')
lines(idade, tmp$logmx.hat, col='chocolate', lwd=3)
legend(65, 0, legend = c("ATC", "API-URB", "API-RUR", "BPC" ), 
       col = c("brown4", "aquamarine4", "darkviolet", "chocolate"),  
       lwd = c(3, 3, 3, 3), bty="n")

tmp = filter(vand.fit, grupo=='atc', sexo=='f')
plot(idade, tmp$logmx.hat, type = 'l', ylim=c(-5,0), col='brown4', lwd=3,
     main='Vandermaen - Mulheres', xlab = "Idade", ylab = "Log taxa de mortalidade") 
tmp = filter(vand.fit, grupo=='api.urb', sexo=='f')
lines(idade, tmp$logmx.hat, col='aquamarine4', lwd=3)
tmp = filter(vand.fit, grupo=='api.rur', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkviolet', lwd=3)
tmp = filter(vand.fit, grupo=='bpc', sexo=='f')
lines(idade, tmp$logmx.hat, col='chocolate', lwd=3)
legend(65, 0, legend = c("ATC", "API-URB", "API-RUR", "BPC" ), 
       col = c("brown4", "aquamarine4", "darkviolet", "chocolate"),  
       lwd = c(3, 3, 3, 3), bty="n")

tmp = filter(gomp, grupo=='atc', sexo=='m')
plot(idade, tmp$logmx.hat, type = 'l', ylim=c(-5,0), col='brown4', lwd=3,
     main='Gompertz - Homens', xlab = "Idade", ylab = "Log taxa de mortalidade") 
tmp = filter(gomp, grupo=='api.urb', sexo=='m')
lines(idade, tmp$logmx.hat, col='aquamarine4', lwd=3)
tmp = filter(gomp, grupo=='api.rur', sexo=='m')
lines(idade, tmp$logmx.hat, col='darkviolet', lwd=3)
tmp = filter(gomp, grupo=='bpc', sexo=='m')
lines(idade, tmp$logmx.hat, col='chocolate', lwd=3)
legend(65, 0, legend = c("ATC", "API-URB", "API-RUR", "BPC" ), 
       col = c("brown4", "aquamarine4", "darkviolet", "chocolate"),  
       lwd = c(3, 3, 3, 3), bty="n")

tmp = filter(gomp, grupo=='atc', sexo=='f')
plot(idade, tmp$logmx.hat, type = 'l', ylim=c(-5,0), col='brown4', lwd=3,
     main='Gompertz - Mulheres', xlab = "Idade", ylab = "Log taxa de mortalidade") 
tmp = filter(gomp, grupo=='api.urb', sexo=='f')
lines(idade, tmp$logmx.hat, col='aquamarine4', lwd=3)
tmp = filter(gomp, grupo=='api.rur', sexo=='f')
lines(idade, tmp$logmx.hat, col='darkviolet', lwd=3)
tmp = filter(gomp, grupo=='bpc', sexo=='f')
lines(idade, tmp$logmx.hat, col='chocolate', lwd=3)
legend(65, 0, legend = c("ATC", "API-URB", "API-RUR", "BPC" ), 
       col = c("brown4", "aquamarine4", "darkviolet", "chocolate"),  
       lwd = c(3, 3, 3, 3), bty="n")

dev.off()


### FIGURA 3 DO ARTIGO #####
jpeg("Figura3.jpeg",   width=15, height=10, unit='in', res=300)
# Parâmetros gráficos:
par(mfrow=c(2,4))
par(cex = 0.9) 
par(mgp = c(2, 1, 0))
par(mar = c(3, 3, 2, 0), oma = c(0, 0, 0, 0))
idade = 65:105

tmp = filter(grupo.fit, grupo=='atc',sexo=='m', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='ATC (Homens)', xlab = "", ylab = "Log probabilidade de morte")
lines(idade, tmp$topals.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='m')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='m')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.masc$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Topals', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")

tmp = filter(grupo.fit, grupo=='api.urb',sexo=='m', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-Urb (Homens)', xlab = "", ylab = "")
lines(idade, tmp$topals.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='m')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='m')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.masc$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Topals', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")

tmp = filter(grupo.fit, grupo=='bpc',sexo=='m', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='BPC (Homens)', xlab = "", ylab = "")
lines(idade, tmp$vand.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='m')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='m')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.masc$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Van der Maen', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")

tmp = filter(grupo.fit, grupo=='api.rur',sexo=='m', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-Rur (Homens)', xlab = "", ylab = "")
lines(idade, tmp$vand.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='m')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='m')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.masc$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Van der Maen', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")


tmp = filter(grupo.fit, grupo=='atc',sexo=='f', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='ATC (Mulheres)', xlab = "Idade", ylab = "Log probabilidade de morte")
lines(idade, tmp$topals.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='f')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='f')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.fem$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Topals', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")

tmp = filter(grupo.fit, grupo=='api.urb',sexo=='f', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-Urb (Mulheres)', xlab = "Idade", ylab = "")
lines(idade, tmp$topals.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='f')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='f')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.fem$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Topals', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")

tmp = filter(grupo.fit, grupo=='bpc',sexo=='f', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='BPC (Mulheres) ', xlab = "Idade", ylab = "")
lines(idade, tmp$vand.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='f')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='f')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.fem$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Van der Maen', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")

tmp = filter(grupo.fit, grupo=='api.rur',sexo=='f', idade<106)
plot(idade, tmp$logqx, pch=1, cex=1.2, ylim=c(-5,0), col='black',
     main='API-Rur (Mulheres)', xlab = "Idade", ylab = "")
lines(idade, tmp$vand.fit, col='blue',lwd=2)
tmp = filter(data, modelo=='ibge',sexo=='f')
lines(idade, tmp$logqx, col='red', lwd=4)
tmp = filter(ibge.extrap, sexo=='f')
lines(idade, tmp$logqx, col='black', lwd=2)
lines(idade, hmd.fem$logqx, col='gray', lwd=2)
legend(65, 0, legend = c("Observada", 'Ajuste Van der Maen', "IBGE (oficial)", 'IBGE(extrapolada)',
                         'Padrão HMD'), 
       col = c("black",'blue', 'red','black', 'gray'), 
       cex = 1.1, pch = c(1,151,151,151,151), lwd = c('',4,3, 3, 3), bty="n")

dev.off()


