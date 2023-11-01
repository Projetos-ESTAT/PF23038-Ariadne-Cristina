pacman::p_load(tidyverse, readxl, car, agricolae, gridExtra, grid, ExpDes.pt,
               easyanova, xtable, stats, olsrr, lmtest, lawstat, EnvStats, 
               leaps, caret)


####################### NUTRIENTES: #######################

nutrientes <- read_excel("banco/Nutrientes - atualizada.xlsx", sheet = 5)
Species<- nutrientes$Espécie
Treatments<- nutrientes$Treatments
N <- nutrientes$`N (G.KG-1)`
P <- nutrientes$`P (g.kg-1)`
K <- nutrientes$`K (g.kg-1)`
Ca <- nutrientes$`Ca (g.kg-1)`
Mg <- nutrientes$`Mg (g.kg-1)`

unique(Species)

############### N ###############

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=N)
attach(dados)
X="";Y=""


###### ANÁLISE EXPLORATÓRIA ######

# fator 1 (tratamento)
summary(dados$resp)
ggplot(dados) +
  aes(
    x = F1,
    y = resp
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Treatments", y = "N (G.KG-1)") 
#ggsave("N_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  labs(x = "Treatments", y = "N (G.KG-1)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))
#ggsave("N_box_Esp.TIFF", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp ~F1))
anova(mod)

mod1 = with(dados, aov(resp ~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  )
plot1
#ggsave("N_plot.TIFF", width = 158, height = 93, units = "mm")

###### Transformação em log ######
dados$resp <- log(dados$resp)


###### MODELO ######

mod = with(dados, aov(resp~F1))
anova(mod)

mod1 = with(dados, aov(resp~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))


###### Teste não paramétrico ######
require(ExpDes.pt)

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=N)
attach(dados)
X="";Y=""

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)


############### P ###############

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=P)
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
  labs(x = "Treatments", y = "P (g.kg-1)")
#ggsave("P_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  labs(x = "Treatments", y = "P (g.kg-1)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))
#ggsave("P_box_Esp.TIFF", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp~F1))
anova(mod)


mod1 = with(dados, aov(resp~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
(norm=shapiro.test(mod1$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) 
plot1
#ggsave("P_plot.TIFF", width = 158, height = 93, units = "mm")

###### Transformação em log ######
dados$resp <- log(dados$resp)

###### MODELO ######

mod = with(dados, aov(resp~F1))
anova(mod)

mod1 = with(dados, aov(resp~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

###### Teste não paramétrico ######
require(ExpDes.pt)

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=P)
attach(dados)
X="";Y=""

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)


############### K ###############

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=K)
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
  labs(x = "Treatments", y = "K (g.kg-1)") 
#ggsave("K_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  labs(x = "Treatments", y = "K (g.kg-1)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))
#ggsave("K_box_Esp.TIFF", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp ~F1))
anova(mod)

mod1 = with(dados, aov(resp ~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) 
plot1
#ggsave("K_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação em log ######
dados$resp <- log(dados$resp)

###### MODELO ######

mod = with(dados, aov(resp~F1))
anova(mod)

mod1 = with(dados, aov(resp~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

###### Teste não paramétrico ######
require(ExpDes.pt)

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=K)
attach(dados)
X="";Y=""

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)



############### Ca ###############

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=Ca)
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
  labs(x = "Treatments", y = "Ca (g.kg-1)") 
#ggsave("Ca_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  labs(x = "Treatments", y = "Ca (g.kg-1)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))
#ggsave("Ca_box_Esp.TIFF", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp ~F1))
anova(mod)

mod1 = with(dados, aov(resp ~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) 
plot1
#ggsave("Ca_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação em log ######
dados$resp <- log(dados$resp)

###### MODELO ######

mod = with(dados, aov(resp~F1))
anova(mod)

mod1 = with(dados, aov(resp~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))



############### Mg ###############

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=Mg)
attach(dados)
X="";Y=""

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
  labs(x = "Treatments", y = "Mg (g.kg-1)") 
#ggsave("Mg_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  labs(x = "Treatments", y = "Mg (g.kg-1)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))
#ggsave("Mg_box_Esp.TIFF", width = 158, height = 93, units = "mm")


###### MODELO ######

mod = with(dados, aov(resp ~F1))
anova(mod)

mod1 = with(dados, aov(resp ~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

# independência
mod1 <- data.frame(mod$residuals)
mod1$id <- 1:nrow(mod1)

plot1 <- ggplot(mod1, aes(x = id, y = mod.residuals)) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observation",
    y = "Residuals"
  ) 
plot1
#ggsave("Mg_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação em log ######
dados$resp <- log(dados$resp)

###### MODELO ######

mod = with(dados, aov(resp~F1))
anova(mod)

mod1 = with(dados, aov(resp~F2))
anova(mod1)

###### PRESSUPOSTOS ######

# normalidade
(norm=shapiro.test(mod$res))
qqnorm(mod$res)

# homogeneidade
with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))

###### Teste de Tukey ######
TukeyHSD(mod1)






############################## FOTOSSINTESE: ##############################

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
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=E)
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
  labs(x = "Treatments", y = "Leaf Transpiration (E)") 
#ggsave("E_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))
#ggsave("E_box_Esp.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
plot1
#ggsave("E_plot.TIFF", width = 158, height = 93, units = "mm")

###### Transformação em log ######
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

###### Teste não paramétrico ######
require(ExpDes.pt)

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=E)
attach(dados)
X="";Y=""

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)

############### gs ############### 

F1=as.factor(Treatments)
F2=as.factor(Species)
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
  labs(x = "Treatments", y = "Stomatal Conductance (gs)") 
#ggsave("gs_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))
#ggsave("gs_box_Esp.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
plot1

#ggsave("gs2_plot.TIFF", width = 158, height = 93, units = "mm")

###### Transformação em log ######
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

###### Teste não paramétrico ######
require(ExpDes.pt)

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=gs)
attach(dados)
X="";Y=""

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)

###### Teste de Tukey ######
tukey <- TukeyHSD(mod)
TukeyHSD(mod, "F1")
TukeyHSD(mod, "F2")
TukeyHSD(mod, "F1:F2")

tukey <- tukey$`F1:F2`
tukey <- as.data.frame(tukey)
tukey %>% 
  filter(`p adj` < 0.05)




############ A ############

F1=as.factor(Treatments)
F2=as.factor(Species)
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
  labs(x = "Treatments", y = "Net Photosynthesis(A)")
#ggsave("A_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))

#ggsave("A_box_Esp.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
plot1

#ggsave("A_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação em log ######
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

###### Teste não paramétrico ######
require(ExpDes.pt)

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=A)
attach(dados)
X="";Y=""

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)



############### EIUA ###############

F1=as.factor(Treatments)
F2=as.factor(Species)
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
  labs(x = "Treatments", y = "Instant water use efficiency(EIUA)") 
#ggsave("EIUA_box_Trat.TIFF", width = 158, height = 93, units = "mm")

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
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.35, size = 9),  # Tamanho e rotação das labels do eixo x
        axis.text.y = element_text(size = 9))

#ggsave("EIUA_box_Esp.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
plot1

#ggsave("EIUA_plot.TIFF", width = 158, height = 93, units = "mm")

###### Transformação em log ######
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

###### Teste não paramétrico ######
require(ExpDes.pt)

F1=as.factor(Treatments)
F2=as.factor(Species)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=EIUA)
attach(dados)
X="";Y=""

kruskal.test(dados$F1, dados$resp)
kruskal.test(dados$F2, dados$resp)

