#### grafico comparativo de renda

renda <- enade14 %>%
  mutate(cu=ifelse(co_grupo==c(5710,5806,5809,5814,5902,6008,6009,6208,6306,6307,6405),"eng",
                   ifelse(co_grupo==c(702,904,905,906,1502,1602,2001,2402,2501,3002,3202,3502,4005,4301,5402),"lic",
          ifelse(co_grupo==1402, "fis", NA)))) %>%
  filter((cu=="eng" | cu=="lic" | cu=="fis") & qe_i8!="")
 
crossrenda <- CrossTable(renda$qe_i8, renda$cu, chisq = TRUE)

plot(factor(renda$cu))

ggrenda <- ggplot(data=as.data.frame(crossrenda$prop.col), aes(x=x, y=Freq, fill=y))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values=c("black", "gray50", "gray"), name="Curso", 
                    labels=c("Engenharias", "Licenciaturas", "Licenciatura em Física"))+  
  labs(x="Renda", y=NULL, title="Distribuição de \nrenda familiar total por curso")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.text=element_text())+
  theme(legend.title=element_text())+
  theme(legend.text=element_text())+
  theme(axis.ticks=element_blank())+
  scale_y_continuous(labels =scales::percent)+
  scale_x_discrete(labels=c("Até 1,5 SM","de 1,5 a 3 SM",
                            "de 3 a 4,5 SM", "de 4,5 a 6 SM",
                            "de 6 a 10 SM", "de 10 a 30 SM","acima de 30 SM"))+
  theme(legend.position=c(0.85,0.85))

ggrenda

ggsave("p7_renda_bw.png", path="~/R/enade/plot/", ggrenda, device="png", 
       width=25,height=7, units="cm", dpi=300)

