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
Species<- fotossintese$Species
Treatments<- fotossintese$Treatments
E <- fotossintese$E
gs <- fotossintese$gs
A <- fotossintese$A
EIUA <- fotossintese$EIUA

unique(Species)

############### E ###############

F1=as.factor(Treatments)
F2=as.factor(Species)
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
  labs(x = "Treatments", y = "Leaf Transpiration (E)") +
  theme_estat()
ggsave("box_bi1.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi1.tiff", width = 158, height = 93, units = "mm")

# fator 2 (especie)
ggplot(dados) +
  aes(
    x = F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatments", y = "Leaf Transpiration (E)") +
  theme_estat()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))

ggsave("box_bi2.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi2.tiff", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) +
  theme_estat()
plot1

ggsave("plot1.pdf", width = 158, height = 93, units = "mm")
ggsave("plot1.tiff", width = 158, height = 93, units = "mm")

## Transformação em log
dados$resp <- log(dados$resp)

###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) +
  theme_estat()
plot1

ggsave("plot1.pdf", width = 158, height = 93, units = "mm")
ggsave("plot1.tiff", width = 158, height = 93, units = "mm")

# Teste não paramétrico
require(ExpDes.pt)

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)


############### gs
F1=as.factor(Treatments)
F2=as.factor(Species)
#bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=gs)
attach(dados)
X="";Y=""

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
  labs(x = "Treatments", y = "Stomatal Conductance (gs)") +
  theme_estat()
ggsave("box_bi3.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi3.tiff", width = 158, height = 93, units = "mm")

# fator 2 (especie)
ggplot(dados) +
  aes(
    x = F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatments", y = "Stomatal Conductance (gs)") +
  theme_estat()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))

ggsave("box_bi4.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi4.tiff", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) +
  theme_estat()
plot1

ggsave("plot2.pdf", width = 158, height = 93, units = "mm")
ggsave("plot2.tiff", width = 158, height = 93, units = "mm")

## Transformação em log
dados$resp <- log(dados$resp)
###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# Teste não paramétrico
require(ExpDes.pt)

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)


############ A

F1=as.factor(Treatments)
F2=as.factor(Species)
#bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=A)
attach(dados)
X="";Y=""

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
  labs(x = "Treatments", y = "Net Photosynthesis(A)") +
  theme_estat()
ggsave("box_bi5.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi5.tiff", width = 158, height = 93, units = "mm")

# fator 2 (especie)
ggplot(dados) +
  aes(
    x = F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatments", y = "Net Photosynthesis(A)") +
  theme_estat()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))

ggsave("box_bi6.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi6.tiff", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) +
  theme_estat()
plot1

ggsave("plot3.pdf", width = 158, height = 93, units = "mm")
ggsave("plot3.tiff", width = 158, height = 93, units = "mm")


## Transformação em log
dados$resp <- log(dados$resp)
###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# Teste não paramétrico
require(ExpDes.pt)

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)


############### EIUA
F1=as.factor(Treatments)
F2=as.factor(Species)
#bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=EIUA)
attach(dados)
X="";Y=""

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
  labs(x = "Treatments", y = "Instant water use efficiency(EIUA)") +
  theme_estat()
ggsave("box_bi7.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi7.tiff", width = 158, height = 93, units = "mm")

# fator 2 (especie)
ggplot(dados) +
  aes(
    x = F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatments", y = "Instant water use efficiency(EIUA)") +
  theme_estat()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))

ggsave("box_bi8.pdf", width = 158, height = 93, units = "mm")
ggsave("box_bi8.tiff", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) +
  theme_estat()
plot1

ggsave("plot4.pdf", width = 158, height = 93, units = "mm")
ggsave("plot4.tiff", width = 158, height = 93, units = "mm")

## Transformação em log
dados$resp <- log(dados$resp)
###### MODELO ######

mod = with(dados, aov(resp~F1*F2))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

# Teste não paramétrico
require(ExpDes.pt)

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)


###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados

