pacman::p_load(tidyverse, readxl, car, agricolae, gridExtra, grid, ExpDes.pt,
               easyanova, xtable, stats, olsrr, lmtest, lawstat, EnvStats, 
               leaps, caret)

############################## NUTRIENTES CERRADO: ##############################

nutrientes <- read_excel("banco/Cerrado (1).xlsx")
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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "K (g.kg-1)")
#ggsave("K_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("K_box_Int.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
#ggsave("qqplot_K.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("K_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("K2_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação logarítmica ######
dados$resp <- log(dados$resp)

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
  ) 
#ggsave("qqplot_K_pos.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("K_plot_pos.TIFF", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###
plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)

#dados <- dados[-c(4,5,34), ]

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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
#ggsave("K_box_Trat_pos.TIFF", width = 158, height = 93, units = "mm")


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
  labs(x = "Specie", y = "K (g.kg-1)")
#ggsave("K_box_Esp_pos.TIFF", width = 158, height = 93, units = "mm")

###### TESTE DE TUKEY  ######

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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "Ca (g.kg-1)")
#ggsave("Ca_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("Ca_box_Int.TIFF", width = 158, height = 93, units = "mm")

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
  ) 
#ggsave("qqplot_Ca.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("Ca_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals")
#ggsave("Ca2_plot.TIFF", width = 158, height = 93, units = "mm")

###### Transformação logarítmica ######
dados$resp <- log(dados$resp)

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)

###### ANÁLISE EXPLORATÓRIA ######

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
  labs(x = "Specie", y = "Ca (g.kg-1)")
#ggsave("Ca_box_Esp_pos.TIFF", width = 158, height = 93, units = "mm")


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
  )
#ggsave("qqplot_Ca_pos.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals")
#ggsave("Ca_plot_pos.TIFF", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

(comparacao <- ea2(dados, design=2))


############### Mg ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=Mg)
attach(dados)
X="";Y=""
dados$bloco[20] <- 3

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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "Mg (g.kg-1)")
#ggsave("Mg_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("Mg_box_Int.TIFF", width = 158, height = 93, units = "mm")

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
  ) 
#ggsave("qqplot_Mg.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("Mg_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals")
#ggsave("Mg2_plot.TIFF", width = 158, height = 93, units = "mm")

###### Transformação logarítmica ######
dados$resp <- log(dados$resp)

###### MODELO ######
mod = with(dados, aov(resp~F1*F2+bloco))
anova(mod)


###### ANÁLISE EXPLORATÓRIA ######

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
  labs(x = "Specie", y = "Mg (g.kg-1)")
#ggsave("Mg_box_Esp_pos.TIFF", width = 158, height = 93, units = "mm")

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
  ) 
#ggsave("qqplot_Mg_pos.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals")
#ggsave("Mg_plot_pos.TIFF", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados


############### N ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=N)
attach(dados)
X="";Y=""
dados$bloco[20] <- 3

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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "N (g.kg-1)")
#ggsave("N_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("N_box_Int.TIFF", width = 158, height = 93, units = "mm")

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
  ) 
#ggsave("qqplot_N.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("N_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("N2_plot.TIFF", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados



############### P ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(bloco)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=P)
attach(dados)
X="";Y=""
dados$bloco[20] <- 3


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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "P (g.kg-1)")
#ggsave("P_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("P_box_Int.TIFF", width = 158, height = 93, units = "mm")

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
  ) 
#ggsave("qqplot_P.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals")
#ggsave("P_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("P2_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação logarítmica ######
dados$resp <- log(dados$resp)

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
  ) 
#ggsave("qqplot_P_pos.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals")
#ggsave("P_plot_pos.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals")
#ggsave("P2_plot_pos.TIFF", width = 158, height = 93, units = "mm")

### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)







############################## FOTOSSÍNTESE CERRADO: ##############################

fotossintese <- read_excel("banco/Cerrado (1).xlsx", sheet = "Fotossíntese")
parcelas <- fotossintese$Site
especies<- fotossintese$species
tratamentos<- fotossintese$Treatments

E<- fotossintese$E
gs<- fotossintese$gs
A<- fotossintese$A
EIUA<- fotossintese$EIUA


############### E ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=E)
attach(dados)
X="";Y=""


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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "Leaf Transpiration (E)")
#ggsave("E_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("E_box_Int.TIFF", width = 158, height = 93, units = "mm")


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
    y = "Leaf Transpiration (E)"
  ) 
#ggsave("qqplot_E.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("E_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("E2_plot.TIFF", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

k<-cooks.distance(mod)
max(k)
indice<-c(1:282)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados



############### gs ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=gs)
attach(dados)
X="";Y=""


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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "Stomatal Conductance (gs)")
#ggsave("gs_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("gs_box_Int.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
#ggsave("qqplot_gs.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals")
#ggsave("gs_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("gs2_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação logarítmica ######
dados$resp <- log(dados$resp)

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
  ) 
#ggsave("qqplot_gs_pos.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals")
#ggsave("gs_plot_pos.TIFF", width = 158, height = 93, units = "mm")


###### ANÁLISE EXPLORATÓRIA ######

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("gs_box_Int_pos.TIFF", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

k<-cooks.distance(mod)
max(k)
indice<-c(1:282)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados



############### A ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=A)
attach(dados)
X="";Y=""


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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "Net Photosynthesis (A)")
#ggsave("A_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("A_box_Int.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
#ggsave("qqplot_A.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("A_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("A2_plot.TIFF", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

cooks.distance(mod)
indice<-c(1:181)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)


###### TESTE DE TUKEY  ######

(comparacao <- ea2(dados, design=2)) # fatorial duplo em blocos casualizados



############### EIUA ###############

F1=as.factor(tratamentos)
F2=as.factor(especies)
bloco=as.factor(parcelas)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,bloco,resp=EIUA)
attach(dados)
X="";Y=""

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
  scale_x_discrete(labels=c('Burned', 'Unburned'))
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
  labs(x = "Specie", y = "Instantaneous Water Use Efficiency (EIUA)")
#ggsave("EIUA_box_Esp.TIFF", width = 158, height = 93, units = "mm")

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
  scale_x_discrete(labels = c("TM","SV","SR","PC","PB","OS","ML",'MB','MA','LH','LE','DH','DF','CA','AV','AND','AH')) +
  facet_wrap(~F1, scales = "free_y", nrow = 1) +
  coord_flip()
#ggsave("EIUA_box_Int.TIFF", width = 158, height = 93, units = "mm")


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
  ) 
#ggsave("qqplot_EIUA.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("EIUA_plot.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("EIUA2_plot.TIFF", width = 158, height = 93, units = "mm")


###### Transformação logarítmica ######
dados$resp <- log(dados$resp)

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
  ) 
#ggsave("qqplot_EIUA_pos.TIFF", width = 158, height = 93, units = "mm")

# homogeneidade

with(dados, leveneTest(mod$residuals~F1))
with(dados, leveneTest(mod$residuals~F2))
grupos <- interaction(dados$F1, dados$F2)
with(dados, leveneTest(mod$residuals~grupos))

ggplot(data) +
  aes(x = fit, y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Fitted Values", y = "Residuals") 
#ggsave("EIUA_plot_pos.TIFF", width = 158, height = 93, units = "mm")


# independência

ggplot(data) +
  aes(x = 1:length(resíduos), y=resíduos) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(x = "Observation", y = "Residuals") 
#ggsave("EIUA2_plot_pos.TIFF", width = 158, height = 93, units = "mm")


### ANALISAR OUTLIERS ###

plot(mod)

ggplot(data) +
  aes(x = 1:length(Res), y = Res) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Observação",
    y = "Resíduos Studentizados"
  ) 

cooks.distance(mod)
indice<-c(1:282)
plot(indice,cooks.distance(mod), type = "l")
plot(mod,which=4)



