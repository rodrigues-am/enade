########################################################
### Produção de Chi-quadrado e Tabela Cruzada
### Utiliza Enade 2014 para questão 69 - você deseja exercer o magistério?
########################################################

# Carrega bibliotecas
library(dplyr)
library(data.table)
library(descr)
library(plyr)

# Seleciona questões 70 - 81 (renda)
varcat <- colnames(select(xt, tp_sexo, tp_ies, REG, in_noturno, qe_i1:qe_i81))

# Seleciona e prepara dados em xt  
xt <- enade14 %>% 
  tbl_df() %>%
  filter(co_grupo==1402, qe_i69!="") %>%
  left_join(.,uf, by=c("co_uf_curso"="FK_COD_ESTADO")) %>%
  mutate(tp_ies=ifelse(co_catad==c(93,116, 10001, 10002, 10003),"público", "privado"))


# Organizar questões agrupando opções
# Estágio
xt$qe_i73 <-revalue(xt$qe_i73, c("c"="b", "d"="b", "e"="c", "f"="d"))

xt$qe_i75 <-revalue(xt$qe_i75, c("b"="a","c"="a", "d"="b", 
                                 "e"="c", "f"="c", "g"="c", 
                                 "h"="c", "i"="d"))
# Experiência profissional anterior
xt$qe_i71 <- revalue(xt$qe_i71, c("d"="c","e"="c", "f"="d", 
                                  "g"="e", "h"="f", "i"="g"))

# Ensino médio 
# nota: escola no exterior viraram escolas privadas parcialmente cat "c"
xt$qe_i17 <-revalue(xt$qe_i17, c("e"="c", "f"="c"))

xt$qe_i18 <-revalue(xt$qe_i18, c("e"="d"))

xt$qe_i8 <-revalue(xt$qe_i8, c("g"="f"))



# Produz lista com teste chi-quadrado
chi <- lapply(xt[,varcat], function(x) chisq.test(x, xt$qe_i69))

# Produz lista com tabelas cruzadas
cross <- lapply(xt[,varcat], function(x) CrossTable(x, xt$qe_i69, prop.c=FALSE, prop.t=FALSE, expected=TRUE, format="SPSS"))




test <- enade14 %>% 
  tbl_df() %>%
  filter(co_grupo==1402)
