#############################################################
# Modelos iniciais de regressão linear multivariada
# Teste de 5 modelos de previsão da nota final do Enade 2014
#############################################################

#### Carrega bibliotecas ####
library(dplyr)

#### Organização dos dados ####


en14.lm <- enade14 %>%
  filter(co_grupo %in% c(1402)) %>%
  filter(nt_ger!="", nt_ger!="NA", tp_pr_ger==555,
         qe_i8!="",qe_i8!="NA",
         tp_sexo!="",tp_sexo!="N",tp_sexo!="NA",
         qe_i2!="",qe_i2!="NA")

#### Modelos ####

# Cria modelos com renda e sexo
mod1 <- lm(nt_ger ~ qe_i8, data=en14.lm)
summary(mod1)

mod2 <- lm(nt_ger ~ qe_i8 + tp_sexo, data=en14.lm)
summary(mod2)

# Compara modelos
comp <- anova(mod1, mod2) 
comp

# Cria modelo com renda, sexo e raça
mod3 <- lm(nt_ger ~ qe_i8 + tp_sexo + qe_i2, data=en14.lm)
summary(mod3)

# Compara modelos
comp1 <- anova(mod2, mod3)
comp1

# Cria modelo com sexo e interação entre renda e raça
mod4 <- lm(nt_ger ~ qe_i2:qe_i8 + tp_sexo, data=en14.lm)
summary(mod4)

# Compara modelos
comp2 <- anova(mod3, mod4)
comp2

# Cria modelo com sexo, renda, raça e interação entre renda e raça
mod5 <- lm(nt_ger ~ qe_i2*qe_i8 + tp_sexo, data=en14.lm)
summary(mod5)

# Compara modelos
comp3 <- anova(mod4, mod5)
comp3

# O modelo 4 é melhor.