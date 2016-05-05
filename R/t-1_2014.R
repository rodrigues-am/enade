########################################################
### Construção da tabela I
### Utiliza Enade 14 para questão 69
########################################################

# Carrega bibliotecas
library(tableone)
library(dplyr)

# Define variaveis da tabela
var<-c("nu_idade", "tp_sexo", "REG","in_noturno", "tp_ies", "nt_fg", "nt_ce", "nt_ger")


fis14 <- enade14 %>%
  tbl_df() %>%
  filter(co_grupo==1402, qe_i69!="") %>%
  left_join(.,uf, by=c("co_uf_curso"="FK_COD_ESTADO")) %>%
  mutate(tp_ies=ifelse(co_catad==c(93,116, 10001, 10002, 10003),"público", "privado"))


# Cria e imprime tabela 1
tab1.fis14 <- CreateTableOne(vars=var,data=fis14, factorVars=c("in_noturno") )
tab1.fis14
summary(tab1.fis14)