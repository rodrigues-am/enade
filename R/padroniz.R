# Carregar bibliotecas
library(sacle)
library(dplyr)
library(ggplot2)

# Padroniza as notas 
pad <- enade14 %>%
  filter(co_grupo!="NA", tp_sexo!="N") %>%
  group_by(co_grupo) %>%
  mutate(nota.pad= as.numeric(scale(nt_ger))) %>%
  group_by(tp_sexo, co_grupo) %>%
  summarise(med=mean(nota.pad, na.rm=TRUE),
            dp=sd(nota.pad, na.rm=TRUE))

# Cria gráfico de barras da diferença
gpad <- ggplot(pad, aes(x=factor(co_grupo), y=med, fill=factor(tp_sexo)))+
  geom_bar(stat="identity")+
  theme_minimal()

gpad
