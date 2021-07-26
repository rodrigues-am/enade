#############################################
### Teste de variancia
###
#############################################

# Carrega biblioteca
library(dplyr)
library(ggplot2)

gen.p <- ggplot(compespec, aes(qe_i69, nt_fis))+
  geom_boxplot()
gen.p

# Testa Nota de Física em relação ao sexo
summary(compespec$nt_fis ~ compespec$tp_sexo)
test <- aov(nt_fis ~ tp_sexo, data=compespec)
summary(test)
model.tables(test)

# Interesse em exercer a docência
# Testa Nota de Física
summary(compespec$nt_fis ~ compespec$qe_i69)
test <- aov(nt_fis ~ qe_i69, data=compespec)
summary(test)
model.tables(test, "effects")

# Testa Nota de Pedagógica
summary(compespec$nt_ped ~ compespec$qe_i69)
test <- aov(nt_ped ~ qe_i69, data=compespec)
summary(test)
model.tables(test, "effects")

# Testa Nota Geral
summary(compespec$nt_ger ~ compespec$qe_i69)
test <- aov(nt_ped ~ qe_i69, data=compespec)
summary(test)
model.tables(test, "effects")

# Testa Nota de Formação Geral
summary(compespec$nt_fg ~ compespec$qe_i69)
test <- aov(nt_ped ~ qe_i69, data=compespec)
summary(test)
model.tables(test, "effects")

test <- aov(nt_fis ~ tp_ies*qe_i69, data=compespec)
summary(test)
model.tables(test, "means")

test <- aov(nt_fis ~ qe_i73, data=compespec)
summary(test)
model.tables(test, "means")

test <- aov(nt_fis ~ REG, data=compespec)
summary(test)
model.tables(test, "means")
boxplot(nt_fis ~ REG, data=compespec)
tk <- TukeyHSD(test)
plot(tk)
tk



# * ainda tem que retirar 7, 8.

test0 <- filter(compespec, qe_i50<6 )

test <- aov(qe_i50 ~ qe_i69, data=test0)
summary(test)
model.tables(test, "means")
gen.p <- ggplot(test0, aes(qe_i69, qe_i50))+
  geom_boxplot()
gen.p



# Teste idade
test <- aov(nu_idade ~ qe_i69, data=compespec)
summary(test)
model.tables(test, "means")
tk <- TukeyHSD(test)
plot(tk)
tk
gen.p <- ggplot(test0, aes(qe_i69, nu_idade))+
  geom_boxplot()+
  theme_tufte()
gen.p


### Teste ANOVA e Turkey
test <- aov(nt_ger ~ qe_i69, data=compespec)
summary(test)
model.tables(test, "means")
tk <- TukeyHSD(test)
plot(tk)
tk
####
gen.p <- ggplot(test0, aes(qe_i69, nt_ger))+
  geom_boxplot()+
  theme_tufte()
gen.p
