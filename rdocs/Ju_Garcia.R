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

fotossintese<-read_excel("banco/Fotossintese - cerrado e campo 1.xlsx", sheet = 3)
especies<- fotossintese$Species
tratamentos<- fotossintese$Treatments
E <- fotossintese$E
gs <- fotossintese$gs
A <- fotossintese$A
EIUA <- fotossintese$EIUA

unique(especies)

############### K ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
#bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=E)
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

# fator 1 (tratamento)

ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Tratamento", y = "E") +
  theme_estat()
#ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")


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

mod = with(dados, aov(resp~F1*F2))
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

