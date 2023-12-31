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
library(xtable)
library(stats)
library(olsrr)
require(lmtest)
require(lawstat)
library(EnvStats)
require(leaps)
library(caret)

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

############### K ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=K)
dados$bloco[20] <- 3
attach(dados)
X="";Y=""

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
  labs(x = "Treatment", y = "K (g.kg-1)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("KboxTrat1.pdf", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "K (g.kg-1)")+
  theme_estat()
#ggsave("KboxEsp1.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)

ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "K (g.kg-1)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("KboxInt.pdf", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "K (g.kg-1)"
  ) +
  theme_estat()
#ggsave("qqplotK1.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("KplotFV1.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("Kplot.pdf", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)

#dados <- dados[-c(4,5,34), ]

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
dados$bloco[20] <- 3

dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatment", y = "Ca (g.kg-1)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("CaboxTrat1.pdf", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "Ca (g.kg-1)")+
  theme_estat()
#ggsave("CaboxEsp1.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)

ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "Ca (g.kg-1)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("CaboxInt.pdf", width = 158, height = 93, units = "mm")

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######


# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "Ca (g.kg-1)"
  ) +
  theme_estat()
#ggsave("qqplotCa1.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("CaplotFV1.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("Caplot.pdf", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


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
dados$bloco[20] <- 3

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
  labs(x = "Treatment", y = "Mg (g.kg-1)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("MgboxTrat.pdf", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "Mg (g.kg-1)")+
  theme_estat()
#ggsave("MgboxEsp1.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)

ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "Mg (g.kg-1)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("MgboxInt.pdf", width = 158, height = 93, units = "mm")

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "Mg (g.kg-1)"
  ) +
  theme_estat()
#ggsave("qqplotMg1.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("MgplotFV.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("Mgplot.pdf", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados











############### N ###############

' PRESSSUPOSTO DE NORMALIDADE NÃO ATENDIDO '

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=N)
attach(dados)
X="";Y=""
dados$bloco[20] <- 3

# dados$resp <- log(dados$resp + 2)

' N possui valores negativos, uma saída é adicionar uma constante a cada número 
para torná-lo positivo e não zero. Adicionei 2.

No entanto, a normalidade não foi atendida mesmo assim '

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatment", y = "N (g.kg-1)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("NboxTrat.pdf", width = 158, height = 93, units = "mm")

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
  labs(x = "Specie", y = "N (g.kg-1)")+
  theme_estat()
#ggsave("NboxEsp.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)

ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "N (g.kg-1)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("NboxInt.pdf", width = 158, height = 93, units = "mm")

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "N (g.kg-1)"
  ) +
  theme_estat()
#ggsave("qqplotN.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("NplotFV.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("Nplot.pdf", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados










############### P ###############

' PRESSSUPOSTO DE NORMALIDADE NÃO ATENDIDO '

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=P)
attach(dados)
X="";Y=""
dados$bloco[20] <- 3

dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
 ggplot(dados) +
   aes(
     x = F1,
     y = resp
   ) +
   geom_boxplot(fill = c("#A11D21"), width = 0.5) +
   stat_summary(
     fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
   ) +
   labs(x = "Treatment", y = "P (g.kg-1)") +
   scale_x_discrete(labels=c('Burned', 'Unburned'))+
   theme_estat()
#ggsave("PboxTrat.pdf", width = 158, height = 93, units = "mm")
 
 
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
   labs(x = "Specie", y = "P (g.kg-1)")+
   theme_estat()
#ggsave("PboxEsp.pdf", width = 158, height = 93, units = "mm")
 
 # ambos os fatores (organizar em painel pra ficar mais claro)
 
 ggplot(dados) +
   aes(
     x = F1:F2,
     y = resp
   ) +
   geom_boxplot(fill = c("#A11D21"), width = 0.5) +
   stat_summary(
     fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
   ) +
   labs(x = "Interação", y = "P (g.kg-1)") +
   theme_estat()+
   scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
   facet_wrap(~F1, scales = "free_y", nrow = 1) +
   coord_flip()
 #ggsave("PboxInt.pdf", width = 158, height = 93, units = "mm")
 
###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "P (g.kg-1)"
  ) +
  theme_estat()
#ggsave("qqplotP1.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("PplotFV1.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("Pplot1.pdf", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE FIEDMAN  ######

' ADICIONAR 0´S ONDE NÃO TEM O BLOCO '

dados$F1<-factor(dados$F1)
dados$bloco<-factor(dados$bloco)
table(dados$F1,dados$bloco)

Pbanco<- read_excel('P.xlsx', col_names = FALSE)
Pbanco<- rename(Pbanco, x='...3', desb='...1', bloco='...2')

teste = aggregate(dados$resp, 
                     by = list(desb = dados$F1,
                               bloco = dados$bloco), 
                     FUN = mean)
teste = aggregate(dados$resp, 
                  by = list(desb = dados$F2,
                            bloco = dados$bloco), 
                  FUN = mean)

friedman.test(x ~ desb | bloco ,data=Pbanco)


Pbanco1<- read_excel('P1.xlsx', col_names = FALSE)
Pbanco1<- rename(Pbanco1, x='...3', desb='...1', bloco='...2')
friedman.test(x ~ desb | bloco ,data=Pbanco1)



############################## FOTOSSÍNTESE CERRADO: ##############################

fotossintese<- read_excel('Cerrado.xlsx', sheet = 2)
parcelas <- fotossintese$Site
# parcelas <- parcelas[seq(1, length(parcelas), by = 3)]
especies<- fotossintese$species
# especies <- especies[seq(1, length(especies), by = 3)]
tratamentos<- fotossintese$Treatments
# tratamentos <- tratamentos[seq(1, length(tratamentos), by = 3)]

' foram realizadas 3 medidas em 3 folhas do mesmo indivíduo (considerar a média?) '

E<- fotossintese$E
# num_conjuntos <- length(E) / 3
# Emedio <- numeric(num_conjuntos)
# for (i in 1:num_conjuntos) {
#   inicio <- (i - 1) * 3 + 1
#   fim <- i * 3
#   Emedio[i] <- mean(E[inicio:fim])
# }

gs<- fotossintese$gs
# num_conjuntos <- length(gs) / 3
# gsmedio <- numeric(num_conjuntos)
# for (i in 1:num_conjuntos) {
#   inicio <- (i - 1) * 3 + 1
#   fim <- i * 3
#   gsmedio[i] <- mean(gs[inicio:fim])
# }

A<- fotossintese$A
# num_conjuntos <- length(A) / 3
# Amedio <- numeric(num_conjuntos)
# for (i in 1:num_conjuntos) {
#   inicio <- (i - 1) * 3 + 1
#   fim <- i * 3
#   Amedio[i] <- mean(A[inicio:fim])
# }

EIUA<- fotossintese$EIUA
# num_conjuntos <- length(gs) / 3
# EIUAmedio <- numeric(num_conjuntos)
# for (i in 1:num_conjuntos) {
#   inicio <- (i - 1) * 3 + 1
#   fim <- i * 3
#   EIUAmedio[i] <- mean(EIUA[inicio:fim])
# }

############### E ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=E)
attach(dados)
X="";Y=""

hist(E)
boxplot(E)

dados$resp <- log(dados$resp)

#dados<-dados[-c(20),]

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatment", y = "Leaf Transpiration (E)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("EboxTrat.pdf", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "Leaf Transpiration (E)")+
  theme_estat()
# ggsave("EboxEsp.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)

ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "Leaf Transpiration (E)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("EboxInt.pdf", width = 158, height = 93, units = "mm")


###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
boxplot(resíduos)
hist(resíduos)
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "Leaf Transpiration (E)"
  ) +
  theme_estat()
#ggsave("qqplotE1.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("EplotFV.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("Eplot.pdf", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
k<-cooks.distance(mod)
max(k)
indice<-c(1:282)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados

############### gs ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=gs)
attach(dados)
X="";Y=""
min(gs)

dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatment", y = "Stomatal Conductance (gs)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("gsboxTrat.pdf", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "Stomatal Conductance (gs)")+
  theme_estat()
#ggsave("gsboxEsp.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)
ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "Stomatal Conductance (gs)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()

#ggsave("gsboxInt1.pdf", width = 158, height = 93, units = "mm")


###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "Stomatal Conductance (gs)"
  ) +
  theme_estat()
#ggsave("qqplotgs1.pdf", width = 158, height = 93, units = "mm")
hist(resíduos)

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("gsplotFV1.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("gsplot1.pdf", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
k<-cooks.distance(mod)
max(k)
indice<-c(1:282)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados

















############### A ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=A)
attach(dados)
X="";Y=""

dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatment", y = "Net Photosynthesis (A)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("AboxTrat.pdf", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "Net Photosynthesis (A)")+
  theme_estat()
#ggsave("AboxEsp.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)

ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "Net Photosynthesis (A)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("AboxInt.pdf", width = 158, height = 93, units = "mm")


###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))

resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "Net Photosynthesis (A)"
  ) +
  theme_estat()
#ggsave("qqplotA.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("AplotFV.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("Aplot.pdf", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados



############### EIUA ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=EIUA)
attach(dados)
X="";Y=""

dados$resp <- log(dados$resp)

###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tartamento)
ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatment", y = "Instantaneous Water Use Efficiency (EIUA)") +
  scale_x_discrete(labels=c('Burned', 'Unburned'))+
  theme_estat()
#ggsave("EIUAboxTrat.pdf", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "Instantaneous Water Use Efficiency (EIUA)")+
  theme_estat()
#ggsave("EIUAboxEsp.pdf", width = 158, height = 93, units = "mm")

# ambos os fatores (organizar em painel pra ficar mais claro)

ggplot(dados) +
  aes(
    x = F1:F2,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Interação", y = "Instantaneous Water Use Efficiency (EIUA)") +
  theme_estat()+
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("EIUAboxInt.pdf", width = 158, height = 93, units = "mm")


###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))


resíduos <- mod$residuals
fit<- mod$fitted.values
Res <- rstudent(mod)
data<-data.frame(resíduos,fit,Res)
ggplot(data) + 
  aes(sample = resíduos) +
  stat_qq(colour = "#A11D21") +
  stat_qq_line(size = 0.8) + 
  labs(
    x = "Normal Quantiles",
    y = "Instantaneous Water Use Efficiency (EIUA)"
  ) +
  theme_estat()
#ggsave("qqplotEIUA1.pdf", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") +
  theme_estat()
#ggsave("EIUAplotFV1.pdf", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") +
  theme_estat()
#ggsave("EIUAplot1.pdf", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) +
  theme_estat()

' Regra básica: Se Di ≥ 0, 80, a i-´esima observacao sera considerada
influente.'
cooks.distance(mod)
indice<-c(1:282)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

' ajustar o nível de significância (alfa) para corrigir o problema das múltiplas comparações '

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados
