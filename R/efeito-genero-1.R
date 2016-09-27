############################################

### Comparação entre licenciaturas de Ciências e Matemática
### Trabalho a ser enviado ao ENPEC e a ENSEÑANZA

############################################


# Carrega bibliotecas
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(xtable)
library(knitr)
library(markdown)
library(tableone)

# 1. Seleção dos dados

# Limpa dados
ci <- enade14 %>%
  filter(co_grupo %in% c(702,1402,1502,1602), 
         tp_pr_ob_fg==555, nt_obj_fg!="NA", tp_sexo!="N") %>%
  select(co_grupo, nt_obj_fg, tp_sexo) %>%
  mutate(curso=ifelse(co_grupo==702, "Matemática",
                      ifelse(co_grupo==1402, "Física",
                             ifelse(co_grupo==1502, "Química",
                                    ifelse(co_grupo==1602,"Ciências Biológicas",0))))) %>%
  droplevels()

# Cria um sumário
ci.s <- ci %>%
  group_by(tp_sexo, curso) %>%
  summarise(med=mean(nt_obj_fg),
            desv=sd(nt_obj_fg),
            errop=desv/sqrt(n()),
            total=n())

# 2. Estatistica descritiva

tb1 <- CreateTableOne(var=c("curso", "tp_sexo", "nt_obj_fg"), data=ci)
tb1
summary(tb1)

# 3. Comparação e Teste ANOVA


# Gráfico 1 barras com erro padrão
pfg1 <- ggplot(data=ci.s, aes(y=med, x=curso, fill=tp_sexo)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", size=.3) +
  geom_errorbar(aes(ymin=med-errop, ymax=med+errop),
                size=.3, width=.2, position=position_dodge(.9)) +
  labs(x="Cursos de Licenciatura", y="Notas na formação geral" , title="Notas na formação geral nas licenciaturas") +
  scale_fill_economist(name="Sexo", 
                       labels=c("Mulheres", "Homens"))+
  theme_minimal() +
  theme(axis.text.x =element_text(angle=90, vjust =0.5))
pfg1

pfg2 <- ggplot(data=ci.s, aes(y=med, x=curso, group=tp_sexo, col=tp_sexo)) +
  geom_point(aes(shape=tp_sexo))+
  scale_shape_manual(values=c(1,16), name="Sexo", 
                     labels=c("Mulheres", "Homens"))+
  geom_line(aes(linetype=tp_sexo)) +
  scale_linetype_discrete(name="Sexo", 
                          labels=c("Mulheres", "Homens"))+ 
  geom_errorbar(aes(ymin=med-errop, ymax=med+errop),
                size=.3, width=.2) +
  labs(x="Cursos de Licenciatura", y="Notas na formação geral" , title="Notas na formação geral nas licenciaturas") +
  scale_color_economist(name="Sexo", 
                       labels=c("Mulheres", "Homens"))+  theme_minimal() +
  theme(axis.text.x =element_text(angle=90, vjust =0.5))
pfg2





# testes a nova com interação
m1 <- aov(nt_obj_fg~curso, data=ci)
summary(m1)

m2 <- aov(nt_obj_fg~curso*tp_sexo, data=ci)
summary(m2)

# Modelo de regressão
mr1 <- lm(nt_obj_fg~curso, data=ci)
summary(mr1)

mr2 <- lm(nt_obj_fg~curso+tp_sexo, data=ci)
summary(mr2)

comp.model <- anova(mr1,mr2, test="Chisq")
comp.model


# 4. Súmarios e conclusões.