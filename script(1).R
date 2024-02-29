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


# gráficos box plot



ggplot(dados, aes(x=cultivares, y=peso)) +
  geom_boxplot()

ggplot(dados, aes(x=cultivares, y=peso))+
  geom_boxplot(fill= 'red' , alpha=0.1 , width=.5, outlier.color = 'red', 
               outlier.shape =16,outlier.size = 2, outlier.fill = "red",
               outlier.alpha = 1)+
  stat_summary(fun=mean, geom = 'point', col='blue')+
  labs(x='Cultivares', y='Pesos')+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=7))


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

#shapiro_results <- by(dados$peso, dados$cultivares, shapiro.test)
#shapiro_results

bartlett.test(peso ~ cultivares) # tem homogeneidade
bartlett.test(peso ~ blocos) # tem homegeneidade 


##############################################################################
# 4 - construção do modelo ANOVA


m1 <- aov(peso ~ cultivares + blocos , data = dados)
summary(m1)
anova(m1)

# teste de normalidade dos resíduos

shapiro.test(m1$residuals)

# teste de homocedasticidade 

bptest(m1) # considera-se a H1 para o modelo

# cronstruções gráficas

par (mfrow = c(2,2))
plot(m1)
text(1.5, max(m1$fitted.values), "A", cex = 1.5)
text(1.5, max(m1$fitted.values), "B", cex = 1.5, adj = 0)
text(1.5, max(m1$fitted.values), "C", cex = 1.5, adj = 0, pos = 2)
text(1.5, max(m1$fitted.values), "D", cex = 1.5, adj = 0, pos = 2, offset = -1)
par (mfrow = c(1,1))


# gráfico com intervalo interquartil
hnp(m1, print.on = T)






#análise dos resíduos

residuosm1 <- rstandard(m1)
print(residuosm1)
hnp(residuosm1, halfnormal = F)



################################################################################
# 5 - modelo lme


m2 <- lm(peso ~ cultivares + blocos, data=dados)
summary(m2)

m3 <- lme(peso ~ cultivares, data = dados, random = ~1|blocos, method = "REML")
summary(m3)

anova(m3)


m4 <- lmer(peso ~ cultivares + (1|blocos), data=dados)
summary(lmer2)


# teste de normalidade dos resíduos

shapiro.test(m1$residuals) # tem normalidade
shapiro.test(m3$residuals)

residuos <- resid(m3)
teste_shapiro <- shapiro.test(residuos)
print(teste_shapiro)


install.packages("goftest")
library(goftest)
teste_anderson_darling <- ad.test(residuos)
print(teste_anderson_darling $ p.value)

# gráfico com intervalo interquartil
hnp(m1, print.on = T) #possivelmente desconsiderar




#análise dos resíduos

hnp(resid(m1, type = "p"), halfnormal = F)
hnp(resid(m3, type = "p"), halfnormal = F)





################################################################################
# 6 - TESTES DE TUKEY


#m1

sk1 <-  SK(m1, which='cultivares')
summary(sk1)


#m3
sk2 <-  SK(m4,
           which='cultivares')
summary(sk2)

# Obs: os dois modelos apresentam as mesmas informações no teste

################################################################################
# 7 - TESTE AIC E BIC


AIC(m1)
AIC(m2)
AIC(m3) # menor valor
AIC(m4)

BIC(m1)
BIC(m2)
BIC(m3) # menor valor
BIC(m4)


