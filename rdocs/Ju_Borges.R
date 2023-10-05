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

# NUTRIENTES CERRADO: 

nutrientes<- read_excel('Cerrado.xlsx', sheet = 1)
especies<- nutrientes$Species
unique(especies)
K<- nutrientes$`K(g.kg-1)`
Ca<-nutrientes$`Ca(g.kg-1)`
Mg<- nutrientes$`Mg(g.kg-1)`
N<- nutrientes$`N(G.KG-1)`
P<- nutrientes$`P(g.kg-1)`

# FOTOSSÍNTESE CERRADO:

fotossintese<- read_excel('Cerrado.xlsx', sheet = 2)
parcelas <- fotossintese$Site
unique(parcelas) # 3, 11, 19 queimados e 1,9, 17 não queimados
especies<- fotossintese$species
unique(especies)
tratamentos<- fotossintese$Treatments
E<- fotossintese$E
gs<- fotossintese$gs
A<- fotossintese$A
EIUA<- fotossintese$EIUA

table(fotossintese$Site, fotossintese$species)
###############################################################################

# ANOVA ou MANOVA ?

modeloE <- aov(E ~ Treatments + Site + species , data = fotossintese) # considera ou não interações?
summary(modeloE)

shapiro.test(modeloE$residuals)

plot(modeloE$residuals)

leveneTest(modeloE$residuals ~ fotossintese$Treatments)

modelo_manova <- manova(cbind(E, A, EIUA, gs) ~ Treatments * species * Site, data = fotossintese)
summary(modelo_manova)


###############################################################################
modelogs <- aov(gs ~ Treatments + Site + species , data = fotossintese)
summary(modelogs)

shapiro.test(modelogs$residuals)

plot(modelogs$residuals)

leveneTest(modelogs$residuals ~ fotossintese$Treatments)
###############################################################################
modeloA <- aov(A ~ Treatments + Site + species , data = fotossintese)
summary(modeloA)

shapiro.test(modeloA$residuals)

plot(modeloA$residuals)

leveneTest(modeloA$residuals ~ fotossintese$Treatments)
###############################################################################
modeloEIUA <- aov(EIUA ~ Treatments + Site + species , data = fotossintese)
summary(modeloEIUA)

shapiro.test(modeloEIUA$residuals)

plot(modeloEIUA$residuals)

leveneTest(modeloEIUA$residuals ~ fotossintese$Treatments)
