########################################################
### Produção de Gráficos relacionados ao estágio
### Utiliza Enade 2014 para questão 69 - você deseja exercer o magistério?
########################################################

# Carrega bibliotecas
library(dplyr) # Gerenciamento de data frame
library(scales) # Permite modificar formato dos núemros para relatório
library(ggplot2) # Produção de gráficos
library(ggthemes) # Temas de gráficos
library(viridis) # Tema de cores para gráficos
library(gridExtra)


##  73  #############################################

# Prepara dados para gráfico
c73 <- cross$qe_i73$prop.row %>%
  as.data.frame()
c73$x <- factor(c73$x,levels=c("d", "c", "b", "a"))

# Gráfico

p73 <- ggplot(c73, aes(x=x,y=Freq, fill=y))+
  geom_bar(stat="identity", alpha=0.6)+
  coord_flip()+
  scale_fill_brewer(palette='Set1')              +
  labs(x=NULL, y=NULL, title="Tipo de escola")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(axis.ticks=element_blank())+
  scale_y_continuous(labels =scales::percent)+
  scale_x_discrete(labels=c("Não realizei", "Não especificada", "Privada", "Pública"))+
  theme(legend.position="none")
p73

##  74  #############################################

# Prepara dados para gráfico
c74 <- cross$qe_i74$prop.row %>%
  as.data.frame() 
c74$x <- factor(c74$x,levels=c("e", "d", "c", "b", "a"))

# Gráfico

p74 <- ggplot(c74, aes(x=x,y=Freq, fill=y))+
  geom_bar(stat="identity", alpha=0.6)+
  coord_flip()+
  scale_fill_brewer(palette='Set1')+
  labs(x=NULL, y=NULL, title="Período do estágio")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(axis.ticks=element_blank())+
  scale_y_continuous(labels =scales::percent)+
  scale_x_discrete(labels=c("Não realizei","Integral", "Noturno", "Vespertino", "Matutino"))+
  theme(legend.position="none")
p74

##  75  #############################################

# Prepara dados para gráfico
c75 <- cross$qe_i75$prop.row %>%
  as.data.frame() 
c75$x <- factor(c75$x,levels=c("f", "e", "d", "c", "b", "a"))

# Gráfico

p75 <- ggplot(c75, aes(x=x,y=Freq, fill=y))+
  geom_bar(stat="identity", alpha=0.6)+
  coord_flip()+
  scale_fill_brewer(palette='Set1')+
  labs(x=NULL, y=NULL, title="Modalidade e etapa de ensino")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(axis.ticks=element_blank())+
  scale_y_continuous(labels =scales::percent)+
  scale_x_discrete(labels=c("Não realizei", "Outra modalidade \nde ensino", "Ensino Médio", "Educação Infantil e \nEnsino Fundamental"))+
  theme(legend.position="none")
p75

"Educação Infantil e \nEnsino Fundamental"

##  76  #############################################

# Prepara dados para gráfico
c76 <- cross$qe_i76$prop.row %>%
  as.data.frame() 
c76$x <- factor(c76$x,levels=c("f", "e", "d", "c", "b", "a"))

# Gráfico

p76 <- ggplot(c76, aes(x=x,y=Freq, fill=y))+
  geom_bar(stat="identity", alpha=0.6)+
  coord_flip()+
  scale_fill_brewer(palette='Set1')+
  labs(x=NULL, y=NULL, title="Tempo de estágio")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(axis.ticks=element_blank())+
  scale_y_continuous(labels =scales::percent)+
  scale_x_discrete(labels=c("Não realizei", "Mais de 400", "De 301 a 400", "De 201 a 300", "De 101 a 200", "Até 100"))+
  theme(legend.position="none")
p76


##  80  #############################################

# Prepara dados para gráfico
c80 <- cross$qe_i80$prop.row %>%
  as.data.frame() 
c80$x <- factor(c80$x,levels=c("d", "c", "b", "a"))

# Gráfico

p80 <- ggplot(c80, aes(x=x,y=Freq, fill=y))+
  geom_bar(stat="identity", alpha=0.6)+
  coord_flip()+
  scale_fill_brewer(palette='Set1')+
  labs(x=NULL, y=NULL, title="Houve supervisão")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(axis.ticks=element_blank())+
  scale_y_continuous(labels =scales::percent)+
  scale_x_discrete(labels=c("Não", "Apenas em algumas \ndisciplinas/situações", "Sim, em grande \nparte do tempo", "Sim, durante \ntodo o tempo"))+
  theme(legend.position="none")
p80


##  81  #############################################

# Prepara dados para gráfico
c81 <- cross$qe_i81$prop.row %>%
  as.data.frame() 
c81$x <- factor(c81$x,levels=c("d", "c", "b", "a"))

# Gráfico

p81 <- ggplot(c81, aes(x=x,y=Freq, fill=y))+
  geom_bar(stat="identity", alpha=0.6)+
  coord_flip()+
  scale_fill_brewer(palette='Set1', name="Respostas:", 
                    labels=c("Sim, como atuação \nprofissional principal.", "Sim, mas esta não será a minha \natuação profissional principal. ", "Não", "Ainda não decidi"))+
  labs(x=NULL, y=NULL, title="Houve acompanhamento")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(axis.ticks=element_blank())+
  scale_y_continuous(labels =scales::percent)+
  scale_x_discrete(labels=c("Não", "Apenas em algumas \ndisciplinas/situações", "Sim, em grande \nparte do tempo", "Sim, durante \ntodo o tempo"))+
  theme(legend.position="bottom")
p81



# organiza gráficos de estaǵio
 #legenda extra
  #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
leg <-g_legend(p81)

gest <- grid.arrange(arrangeGrob(p73,p74,p75,p76,p80,p81+theme(legend.position="none"),ncol=2),
                   leg, nrow=2,heights=c(10, 1))