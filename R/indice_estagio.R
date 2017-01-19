#################################
# Indice
#
#################################


# Seleção de dados
library(dplyr)
library(plotrix)
library(ggplot2)


en14 <- enade14 %>%
  filter(co_grupo %in% c(1402,1502, 1602, 702), 
         qe_i50<=6, qe_i50>=1,
         qe_i76!="NA", qe_i76!="",
         qe_i80!="NA", qe_i80!="",
         qe_i81!="NA", qe_i81!="",
         qe_i69!="NA", qe_i69!="") %>%
  droplevels() %>%
  group_by(co_grupo) %>%
  mutate(serprof=ifelse(qe_i69 %in% c("a","b"), 1, 0)) %>%
  mutate(qe_i76=as.numeric(qe_i76),
         qe_i80=as.numeric(qe_i80),
         qe_i81=as.numeric(qe_i81))%>%
  mutate(q50=as.numeric(scale(qe_i50)),
         q76=as.numeric(scale(qe_i76)),
         q80=as.numeric(scale(qe_i80)),
         q81=as.numeric(scale(qe_i81))) %>%
  mutate(est.index=(q50+q76+q80+q81))


summary(en14$est.index)

en14 %>%
  group_by(co_grupo) %>%
  summarise(med=mean(est.index),
            desvpad=sd(est.index),
            errop=std.error(est.index)) %>%
  arrange(desc(med)) %>%
  View()

summary(aov(est.index ~ factor(co_regiao_curso), data=en14))


mlogit <- glm(serprof ~ est.index+factor(co_grupo), family = "binomial", data=en14)
summary(mlogit)
exp(cbind(OR = coef(mlogit), confint(mlogit)))






sum <- en14 %>%
  group_by(co_ies, co_grupo) %>%
  summarise(med.est=mean(est.index),
            med.serp=length(est.index[serprof==1])/sum(n()),
            qt50=mean(qe_i50))


summary(lm(med.serp ~ med.est + I(med.est^2), data=sum))

p.sum <- ggplot(sum, aes(med.serp, med.est, col=factor(co_grupo)))+
  geom_point(shape=21)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  facet_grid(co_grupo ~ .)

p.sum
