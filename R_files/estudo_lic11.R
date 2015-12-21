###########################
####### Estudo ENADE 2011
####### 
##########################

#Carrega bibliotecas
library("data.table")
library("ggplot2")

# Tansforma df em data.table
enade11<-as.data.table(enade11)

# Seleciona o curso de licenciatura em Física
# 1402 - Lic  | 1401 - Bac
lic11<-enade11[CO_GRUPO==1402]



### Gráficos

#Grafico ano de conlcusão do EM por Idade; seprado por Sexo
p1<- ggplot(lic11, aes(x=ANO_FIM_2G, y=NU_IDADE))+
  geom_jitter(aes(colour = TP_SEXO))+
  ggtitle("ENADE - 2G vs Idade")+
  xlab("Ano de conclusão do 2 grau")+
  ylab("Idade")+
  theme_minimal()
p1

#Grafico com destaque para a Código da categoria administrativa da IES
#Pública(1) e Privado(2)
p2<- ggplot(lic11, aes(x=ANO_FIM_2G, y=NU_IDADE))+
  geom_jitter(aes(colour = factor(CD_CATAD)))+
  ggtitle("ENADE - 2G vs Idade")+
  xlab("Ano de conclusão do 2 grau")+
  ylab("Idade")+
  theme_minimal()
p2
