###########################
####### Carrega dados do ENADE 2011
##########################

#Carrega bibliotecas
library("repmis")

#Carrega ENADE 2011 via Dropbox
enade11<-repmis::source_DropboxData("ENADE_2011.csv","u30mf8145806hub")
