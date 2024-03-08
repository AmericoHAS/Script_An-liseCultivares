##############################################################################
#################  ANÁLISE DE DADOS PARA O TCC 2023 ##########################
##############################################################################

# Info: Análise de dados utilizada no TCC2023 da Unespar


##############################################################################

# 1 - Pacotes


if(!require(ggplot2)) install.packages("ggplot2")  
library(ggplot2)

if(!require(stats)) install.packages("stats")  
library(stats)

if(!require(hnp)) install.packages("hnp") 
library(hnp)

if(!require(lmtest)) install.packages("lmtest") 
library(lmtest) #teste de homocedasticidade

if(!require(nlme)) install.packages("nlme") 
library(lme4)
library(nlme)

if(!require(ScottKnott)) install.packages("ScottKnott")
library(ScottKnott)

if(!require(multcomp)) install.packages("multcomp")
library(multcomp) #teste de tukey no modelo lme



##############################################################################

# 2 . Banco da dados

# info: dados provenientes do livro " Experimentação Agronomica" sobre cuntivares de cebola


peso <- c(1222,1195,1118,1002,1452,1062,1195,1414,1768,1482,1753,1805,1870,1598,
          1437,1856,1544,1300,1224,1500,2100,2117,2150,1618,2063,2063,1707,1592,
          850,928,928,500)
cultivares <- as.factor(rep(c("Baia do Cedo","Texas Grano","Pera IPA 1",
                              "Pera IPA 2","Pera IPA 3","Pera IPA 4","Pira Ouro",
                              "Red Creole"),each=4))
blocos <- as.factor(rep(1:4,8))


dados <- data.frame(blocos, cultivares, peso)
print(dados)
View(dados)


#############################################################################

# 3 - análises iniciais dos dados 



#boxplot

ggplot(dados, aes(x=cultivares, y=peso)) +
  geom_boxplot()+stat_summary(fun=mean, geom = 'point', col='blue')


# estatísticas

estat <- aggregate(dados$peso, by = list(dados$cultivares), 
                   FUN = function(x) c(Mínimo = min(x), 
                                       Máximo = max(x), 
                                       Média = mean(x), 
                                       Desvio_Padrão = sd(x), 
                                       Coeficiente_de_Variação = sd(x) / mean(x)))
print(estat)
View(estat)


# teste de homogeneidade das variaveis

bartlett.test(peso ~ cultivares)

##############################################################################
# 4 - construção dos modelos 


m1 <- aov(peso ~ cultivares + blocos , data = dados)
summary(m1)
anova(m1)

m2 <- lme(peso ~ cultivares, data = dados, random = ~1|blocos, method = "REML")
summary(m2)
anova(m2)

m2.2 <- lmer(peso ~ cultivares + (1|blocos), data=dados)
summary(m2.2)



################################################################################
# TESTES DE TUKEY


#m1

sk1 <-  SK(m1, which='cultivares')
summary(sk1)


#m3
sk2 <-  SK(m2.2,
           which='cultivares')
summary(sk2)





#############################################################
# gráfico de envelope simulado

par(mfrow = c(1, 2))

hnp(rstandard(m1), halfnormal = F, print.on = T)
hnp(resid(m2, type = "p"), halfnormal = F,  print.on = T)

par(mfrow = c(1, 1))









####################################################################
# ajuste dos dados modelo m3

dados<- transform(dados, bl=ifelse(blocos=="1" | blocos=="3", "t1", "t2"))

m3<- lme(peso ~ cultivares, random = list(blocos=pdIdent(~1)),
         weight= varIdent(form=~1|bl), data = dados)

summary(m3)
anova(m3)


#análise gráfica

par(mfrow = c(1, 2))

hnp(resid(m2, type = "p"), halfnormal = F, print.on = T)
hnp(resid(m3, type = "p"), halfnormal = F, print.on = T)

par(mfrow = c(1, 1))
