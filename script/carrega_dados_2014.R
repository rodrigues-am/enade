####################################################
### Carrega dados ENADE 2014
### Direto do servidor do INEP
####################################################

# Carrega bibliotecas
library("utils")

# Cria arquivo temporário
temp<-tempfile()

# Faz download do arquivo zip
download.file("http://download.inep.gov.br/microdados/Enade_Microdados/microdados_enade_2014.zip",temp, mode="wb")

# Extrai os arquivos compactados em zip no diretório temporário
unzip(temp, exdir = tempdir())

# Obtém localização do arquivo csv
fns<-list.files(tempdir(), pattern="*.csv",full.names=TRUE, recursive=TRUE)

# Lê arquivo csv
enade14<-read.csv(fns, header=TRUE, sep=";")

# Remove variáveis temporárias
unlink(temp)
rm(temp)
rm(fns)

