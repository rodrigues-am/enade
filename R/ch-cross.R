########################################################
### Produção de Chi-quadrado e Tabela Cruzada
### Utiliza Enade 2014 para questão 69 - você deseja exercer o magistério?
########################################################

# Carrega bibliotecas
library(dplyr)
library(data.table)
library(descr)

# Seleciona questões 8 - 10 (renda)
varcat <- colnames(select(xt, qe_i8:qe_i10))

# Seleciona e prepara dados em xt  
xt <- enade14 %>% 
  tbl_df() %>%
  filter(co_grupo==1402, qe_i69!="") %>%
  left_join(.,uf, by=c("co_uf_curso"="FK_COD_ESTADO")) %>%
  mutate(tp_ies=ifelse(co_catad==c(93,116, 10001, 10002, 10003),"público", "privado"))
  
# Produz lista com teste chi-quadrado
chi <- lapply(xt[,varcat], function(x) chisq.test(x, xt$qe_i69))

# Produz lista com tabelas cruzadas
cross <- lapply(xt[,varcat], function(x) CrossTable(x, xt$qe_i69, prop.c=FALSE, prop.t=FALSE, format="SPSS"))
  
  