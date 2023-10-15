source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

setwd('D:/Downloads/ESTAT')
library(readxl)
library(tidyverse)
library(car)
library(agricolae)
library(gridExtra)
library(grid)
library(ExpDes.pt)
library(easyanova)

' o experimento foi conduzido em delineamento em blocos casualizados com 15 repetições '

' espécie da planta como um segundo fator, está sob controle do pesquisador e 
para o qual diferentes níveis ou condições são deliberadamente estabelecidos a 
fim de avaliar seu impacto no resultado do experimento '

' 1 Quantitativa (característica de interesse) 2 Categóricas (tratamentos e especies) 
                                   = 
         ANOVA 2-fatores (ou, no caso não-paramétrico, Friedman) '

' modelo:
        yij = m + tali + bj + (talb)ij + eij 
yij é o valor observado no i-ésimo nivel do Fator A e j-ésima nível do Fator B;
m é uma constante que representa a média geral;
tali é o efeito do i-ésimo nível do fator A;
bj é o efeito do j-ésimo nível do fator B;
(talb)ij é o efeito da interação entre tali e bj;
eij é o componente de erro aleatório '

############################## NUTRIENTES CERRADO: ##############################

nutrientes<- read_excel('Cerrado.xlsx', sheet = 1)
especies<- nutrientes$Species
tratamentos<- nutrientes$Treatments
bloco<- nutrientes$sites
K<- nutrientes$`K(g.kg-1)`
Ca<-nutrientes$`Ca(g.kg-1)`
Mg<- nutrientes$`Mg(g.kg-1)`
N<- nutrientes$`N(G.KG-1)`
P<- nutrientes$`P(g.kg-1)`

unique(especies)

############### K ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=K)
attach(dados)
X="";Y=""

' foi necessária a transformação dos dados (logarítmica) para que os pressupostos fossem atendidos,
essa transformação é indicada para variáveis contínuas positivas e diferentes de zero. '

'É sabido que muitas variáveis biológicas tem distribuição log-normal. Desta forma, 
após a transformação logarítmica, os valores passam a ter distribuição normal.'

' logarítmica (para corrigir distribuições assimétricas e para remover a dependência
entre média e variância, além de homogeneizar variâncias entre grupos), a escala de valores 
não é a mesma, os dados transformados perdem seu significado biológico '

dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F1, mean))
points(mediab, pch='+', cex=1.5, col='red')

# fator 2 (especie)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F2, mean))
points(mediab, pch='+', cex=1.5, col='red')

# ambos os fatores
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1*F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))

###### INTERAÇÃO ######

with(dados, interaction.plot(F2, F1, resp, las=1, col=1:17, bty='l', 
                             xlab='', ylab='K', trace.label="FATOR1"))

# FATOR1 e FATOR2
with(dados, interaction.plot(F1, F2, resp, las=1, col=1:17, bty='l', 
                             xlab='', ylab='K', trace.label="FATOR2"))

###### MODELO ######

mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência

plot(mod$residuals)

###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados

############### Ca ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=Ca)
attach(dados)
X="";Y=""

dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F1, mean))
points(mediab, pch='+', cex=1.5, col='red')

# fator 2 (especie)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F2, mean))
points(mediab, pch='+', cex=1.5, col='red')

# ambos os fatores
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1*F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência

plot(mod$residuals)

###### TESTE DE TUKEY  ######

(comparacao <- ea2(dados, design=2))

############### Mg ###############

' A HOMOGENEIDADE DO FATOR 2 NÃO FOI ATENDIDA, oq pode ser?? 
A transformação nao ajuda '

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=Mg)
attach(dados)
X="";Y=""

# dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F1, mean))
points(mediab, pch='+', cex=1.5, col='red')

# fator 2 (especie)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F2, mean))
points(mediab, pch='+', cex=1.5, col='red')

# ambos os fatores
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1*F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência

plot(mod$residuals)

############### N ###############

' PRESSSUPOSTO DE NORMALIDADE NÃO ATENDIDO '

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=N)
attach(dados)
X="";Y=""

# dados$resp <- log(dados$resp + 2)

' N possui valores negativos, uma saída é adicionar uma constante a cada número 
para torná-lo positivo e não zero. Adicionei 2.

No entanto, a normalidade não foi atendida mesmo assim '

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F1, mean))
points(mediab, pch='+', cex=1.5, col='red')

# fator 2 (especie)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F2, mean))
points(mediab, pch='+', cex=1.5, col='red')

# ambos os fatores
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1*F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência

plot(mod$residuals)

############### P ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=P)
attach(dados)
X="";Y=""

# dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F1, mean))
points(mediab, pch='+', cex=1.5, col='red')

# fator 2 (especie)
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))
mediab=with(dados,tapply(resp, F2, mean))
points(mediab, pch='+', cex=1.5, col='red')

# ambos os fatores
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1*F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência

plot(mod$residuals)














############################## FOTOSSÍNTESE CERRADO: ##############################

fotossintese<- read_excel('Cerrado.xlsx', sheet = 2)
parcelas <- fotossintese$Site
unique(parcelas) # 3, 11, 19 queimados e 1,9, 17 não queimados
especies<- fotossintese$species
especies <- especies[seq(1, length(especies), by = 3)]
tratamentos<- fotossintese$Treatments
E<- fotossintese$E
gs<- fotossintese$gs
A<- fotossintese$A
EIUA<- fotossintese$EIUA

' foram realizadas 3 medidas em 3 folhas do mesmo indivíduo (considerar a média?) '

table(fotossintese$Site, fotossintese$species)
