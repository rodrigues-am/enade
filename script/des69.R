########################################
### Criação do gráfico - questão 69
### ENADE 2014 - desempenho
########################################

# Carrega bibliotecas
library(dplyr) # Gerenciamento de data frame
library(descr) # Produção de tabela cruzada
library(scales) # Permite modificar formato dos núemros para relatório
library(ggplot2) # Produção de gráficos
library(ggthemes) # Temas de gráficos

#Tabelas iniciais
# Tabela do gráfico
fisdes <- enade14 %>%
  tbl_df() %>%
  filter(co_grupo==1402, qe_i69!="", 
         nt_fg!=0, nt_ce!=0, nt_ger!=0) %>% 
  select(nt_fg, nt_ce, nt_ger, qe_i69) 

# Tabela de resumo para o gráfico
# Permite colocar os pontos das médias
fisdes2 <- fisdes %>%
  group_by(qe_i69) %>%
  summarise(mfg=mean(nt_fg), mce=mean(nt_ce), mger=mean(nt_ger))

# Gráfico p6 de desempenho
p6 <- ggplot(fisdes, aes(x=qe_i69,y=nt_ger, fill=qe_i69))+
  geom_violin(alpha=0.6)+
  geom_boxplot(width=0.2, alpha=0.4, fill="white")+
  geom_point(data=fisdes2, aes(y=mfg), fill="yellow", shape=21, size=3,  colour="black")+
  geom_point(data=fisdes2, aes(y=mce), fill="red", shape=23, size=3,  colour="white")+
  scale_fill_brewer(palette='Set1', name="Respostas", 
                    labels=c("Sim, como atuação \nprofissional principal.", "Sim, mas esta não será a minha \natuação profissional principal. ", "Não", "Ainda não decidi"))+
  labs(x=NULL, y=NULL, title="Resposta à questão 1 - item 69 \nDistribuição por desempenho")+
  theme_tufte(base_family="Helvetica")+
  annotate("text", x = 1.1, y = 55.82, label = "Média da nota \nformação geral", hjust=0, size=3)+
  annotate("text", x = 1.1, y = 34.74, label = "Média da nota \ncomponente específica", hjust=0, size=3)+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(legend.position="bottom")

p6

# Testes t-student (não ser professor)

t.ger <- t.test(as.data.frame(filter(fisdes, qe_i69=="c") %>% select(nt_ger)),as.data.frame(filter(fisdes, qe_i69!="c")%>% select(nt_ger)), alternative ="two.sided")

t.fg <- t.test(as.data.frame(filter(fisdes, qe_i69=="c") %>% select(nt_fg)),as.data.frame(filter(fisdes, qe_i69!="c")%>% select(nt_fg)), alternative ="two.sided")

t.ce <- t.test(as.data.frame(filter(fisdes, qe_i69=="c") %>% select(nt_ce)),as.data.frame(filter(fisdes, qe_i69!="c")%>% select(nt_ce)), alternative ="two.sided")

