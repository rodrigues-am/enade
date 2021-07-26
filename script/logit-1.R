#########################################
### Regressão Logistica
### Interesse em ser professor
#########################################



# Carrega bibliotecas
library(dplyr)
library(dummies)


# Funções
# Verificação de colinearidade
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

# Seleciona dados
l.fis14 <-  enade14 %>% 
  filter(co_grupo==1402, qe_i69!="") %>%
  mutate(opt=ifelse(qe_i69=="a" | qe_i69=="b", 1,0)) %>%
  select(qe_i69, qe_i70) %>%
  as.data.frame() %>%
  dummy.data.frame(sep=".") 

# Verifica colinearidade
test <- vif_func(l.fis14,thresh = 5, trace = T)
test

# Cria modelo de regressão logistica para qe_i69.a (ok)
mlogit <- glm( qe_i69.a~qe_i70.a+qe_i70.b+qe_i70.c+qe_i70.d+qe_i70.e+
                qe_i70.h, family = "binomial", data=l.fis14)
summary(mlogit)
exp(cbind(OR = coef(mlogit), confint(mlogit)))



# Cria modelo de regressão logistica para  qe_i69.c
mlogit <- glm( qe_i69.c~qe_i70.a+qe_i70.b+qe_i70.c+qe_i70.d+qe_i70.e+
                 qe_i70.f+qe_i70.g+qe_i70.h+qe_i70.i+qe_i70.j, family = "binomial", data=l.fis14)
summary(mlogit)
exp(cbind(OR = coef(mlogit), confint(mlogit)))





# Cria modelo de regressão logistica para opt 77:81
mlogit <- glm(opt ~qe_i77.b+qe_i77.c+qe_i77.d+qe_i77.e+qe_i78.b+qe_i78.c+qe_i78.d+
qe_i79.b+qe_i79.c+qe_i79.d+qe_i80.b+qe_i80.c+qe_i80.d+qe_i81.b+qe_i81.c+qe_i81.d, family = "binomial", data=l.fis14)
summary(mlogit)
exp(cbind(OR = coef(mlogit0), confint(mlogit0)))




# Cria modelo de regressão logistica para opt
mlogit <- glm(opt ~qe_i70.b+qe_i70.c+qe_i70.d+qe_i70.e+qe_i70.f+qe_i70.g+qe_i70.h+qe_i70.i+qe_i70.j+qe_i71.b+
qe_i71.c+qe_i71.d+qe_i71.e+qe_i71.f+qe_i71.g+qe_i71.h+qe_i72.b+qe_i72.c+qe_i72.d+qe_i72.e+qe_i72.f+qe_i72.g+
qe_i72.h+qe_i73.b+qe_i73.c+qe_i73.d+qe_i73.e+qe_i73.f+qe_i74.b+qe_i74.c+qe_i74.d+qe_i75.b+qe_i75.c+qe_i75.e+
qe_i75.f+qe_i75.g+qe_i75.h+qe_i76.b+qe_i76.c+qe_i76.d+qe_i76.e+qe_i77.b+qe_i77.c+qe_i77.d+qe_i77.e+qe_i78.b+
qe_i78.c+qe_i78.d+qe_i79.b+qe_i79.c+qe_i79.d+qe_i80.b+qe_i80.c+qe_i80.d+qe_i81.b+qe_i81.c+qe_i81.d, family = "binomial", data=l.fis14)
summary(mlogit)
exp(cbind(OR = coef(mlogit0), confint(mlogit0)))

mlogit0 <- glm(opt~qe_i70.e+qe_i70.f+qe_i70.g+qe_i70.h+qe_i70.i+qe_i70.j+
                 qe_i71.f+qe_i71.h+qe_i72.b+qe_i72.c+qe_i72.d+qe_i72.e+qe_i72.g+
                 qe_i75.e+qe_i77.c+qe_i77.e, family = "binomial", data=l.fis14)

summary(mlogit0)
exp(cbind(OR = coef(mlogit0), confint(mlogit0)))




mlogit1 <- glm(opt~qe_i81.b+qe_i81.c+qe_i81.a, family = "binomial", data=l.fis14)
summary(mlogit1)
exp(cbind(OR = coef(mlogit1), confint(mlogit1)))

test <- anova(mlogit,mlogit0,test="Chisq")
summary(test)
test
table(fis14$qe_i69, useNA="always")

table(l.fis14$opt, useNA="always")
  
levels(fis14$qe_i69)


test <- dummy.data.frame(l.fis14, sep=".")
test1 <- glm(opt~., family = "binomial", data=test)
summary(test1)
