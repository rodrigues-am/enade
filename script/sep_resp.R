##############################################################
### Separa respostas dos alunos
###
#############################################################

# Carrega Bibliotecas
library(tidyr)
library(dplyr)

#### Separação acertos prova geral ###########################

# Preliminar para formação geral
nome <- c(paste("qg",1:8,sep=""))

# Separa gabarito
gfg <- fis14 %>%
  filter(tp_pr_ob_fg==555) %>%
  select(vt_gab_ofg_fin) %>%
  mutate(t=as.character(vt_gab_ofg_fin)) %>%
  separate(.,t, nome,sep=c(1:7)) %>% 
  select(-1) %>%
  as.data.frame()

# Separa resposta
rfg <- fis14 %>%
  filter(tp_pr_ob_fg==555) %>%
  select(vt_esc_ofg) %>%
  mutate(t=as.character(vt_esc_ofg)) %>%
  separate(.,t, nome,sep=c(1:7)) %>% 
  select(-1) %>%
  as.data.frame()

# Consolida acertos
acfg <- sapply(1:ncol(rfg),function(x) ifelse(gfg[,x]==rfg[,x],1,0)) %>%
  as.data.frame() %>%
  mutate(sumVar = rowSums(.[1:ncol(rfg)])*100/ncol(rfg))

# Encontra nota final
fgeral<-fis14 %>%
  filter(tp_pr_ob_fg==555) %>%
  cbind(.,acfg) %>%
  cbind(., rfg)






#### Separação acertos prova específica ###########################

# Preliminar para formação geral
nome <- c(paste("qe",1:27,sep=""))

# Separa gabarito
gce <- fis14 %>%
  filter(tp_pr_ob_ce==555) %>%
  select(vt_gab_oce_fin) %>%
  mutate(t=as.character(vt_gab_oce_fin)) %>%
  separate(.,t, nome,sep=c(1:26)) %>% 
  select(-1) %>%
  as.data.frame()

# Separa resposta
rce <- fis14 %>%
  filter(tp_pr_ob_ce==555) %>%
  select(vt_esc_oce) %>%
  mutate(t=as.character(vt_esc_oce)) %>%
  separate(.,t, nome,sep=c(1:26)) %>% 
  select(-1) %>%
  as.data.frame()

# Consolida acertos
acce <- sapply(1:ncol(rce),function(x) ifelse(gce[,x]==rce[,x],1,
                                              ifelse(gce[,x]=="X", NA,0))) %>%
  as.data.frame() %>%
  mutate(sumVar = rowSums(.[1:ncol(rce)], na.rm = TRUE)*100/(ncol(rce)-8),
         nt_fis=rowSums(.[1:17], na.rm = TRUE)*100/11,
         nt_ped=rowSums(.[18:27], na.rm = TRUE)*100/8)

# Encontra nota final
compespec<-fis14 %>%
  filter(tp_pr_ob_ce==555) %>%
  cbind(.,acce) %>%
  cbind(., rce)

  
