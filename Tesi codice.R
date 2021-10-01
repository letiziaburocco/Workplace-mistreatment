## CODICE TESI ##


# LIBRARY

library(ggplot2)
library(ggpubr)
library("RColorBrewer")
library(dplyr)
library(LMest)
library(ExPanDaR)
library(purrr)
library(readxl)
library(flextable)
library(table1)
library(ggcorrplot)
library(gridExtra)


################################################################################
################################# DATASET ######################################

# I dati sono accessibili tramite la pagina web https://data.mendeley.com/datasets/5bn23jn2yg/1.

follmer<- read_excel("C:/.../follmer.xlsx") 

follmer<-round(follmer,digits = 2)

follmer<-follmer%>%select(1:6,8:18,7)

save(follmer,file="follmer_wide.Rdata")# dati in formato wide

vnames <- c("INCIVILITY", "UNDERMINE", "OSTRACISM","NSUIC","ENGAGE")

follmer_long <-reshape(as.data.frame(follmer),
                       idvar = "ID",
                       times = c("T1","T2","T3"),
                       timevar = "time",
                       varying = matrix(colnames(follmer)[2:16],nrow = 5,ncol=3),
                       v.names = vnames,
                       direction = "long")

follmer_long$time<-as.factor(follmer_long$time)
follmer_long<-follmer_long[order(follmer_long$ID),]
follmer_long$TIME<-as.numeric(follmer_long$time)
follmer_long$treat<-recode(follmer_long$TREAT,`1` = 1, `2` = 0)
follmer_long$id<-rep(1:279,rep(3,279))
follmer_long<-follmer_long %>% select(c("id","ID","TIME","time","TREAT","treat",
                                        "CENTRAL_T1","INCIVILITY","UNDERMINE",
                                        "OSTRACISM","NSUIC","ENGAGE"))

flextable(head(follmer_long))

save(follmer_long,file="follmer_long.Rdata")# dati in formato long


################################################################################
############################ ANALISI DESCRITTIVA ###############################

##################### SUMMARY

follmer_long$treat <- factor(follmer_long$treat, levels=c(0,1),
                             labels=c("No", "Sì"))

labels <- list(variables=list(NSUIC="Intenzione al suicidio",
                              ENGAGE="Work engagement",
                              INCIVILITY="Inciviltà",
                              UNDERMINE="Social Undermining",
                              OSTRACISM="Ostracismo",
                              CENTRAL_T1= "Stigma centrality",
                              treat="Trattamento"),
                              groups=list("Periodo temporale",""))

levels(follmer_long$time)<-c("1","2","3")
strata<-c(split(follmer_long,follmer_long$time),list(Totale=follmer_long))

table1(strata,labels,groupspan = c(3,1))


#################### CORRELAZIONE


follmer<-follmer[,c(1,2,7,12,3,8,13,4,9,14,5,10,15,6,11,16,17,18)]

colnames(follmer)<-c("ID","Inciviltà 1","Inciviltà 2","Inciviltà 3",
                     " Undermining 1"," Undermining 2","Undermining 3",
                     " Ostracismo 1"," Ostracismo 2","Ostracismo 3",
                     " Intenzione al suicidio 1"," Intenzione al suicidio 2",
                     "Intenzione al suicidio 3",
                     " Engagement 1"," Engagement 2","Engagement 3",
                     "Trattamento","Stigma Centrality")

mcor<-round(cor(follmer[,c(2:18)],method = "pearson"),2)

ggcorrplot(mcor, 
           p.mat = cor_pmat(follmer[,c(2:18)],alternative="two.sided"),
           hc.order = FALSE,
           type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", 
           lab = TRUE)


#################### TREND VALORI MEDI


A<-prepare_trend_graph(follmer_long,
                       "time",
                       c("INCIVILITY","UNDERMINE","OSTRACISM","NSUIC","ENGAGE"))

A$df$variable<-as.factor(A$df$variable)
levels(A$df$variable)<-c("Work engagement","Inciviltà","Intenzione al suicidio",
                         "Ostracismo","Social undermining")
colnames(A$df)<-c("Variabili","time","mean","se")

ggplot(A$df,aes_string(x = "time", color="Variabili", group = "Variabili")) +
  stat_summary(fun = sum, geom = "line", aes(y = mean)) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), width = .1) +
  geom_point(aes(y = mean)) +
  labs(title = "",x="Periodo temporale", y = "Valori medi", 
       color="Variabili", fill = "Variabili") +
  theme_classic()+ 
  ylim(2,3.55)+
  scale_color_manual(values = c("red","#3BB9FF","orange","#1569C7","#0000A0"))


################################################################################
########################## SELEZIONE MODELLO ###################################


m2<-lmestSearch(responsesFormula = NSUIC+ENGAGE ~ NULL,
                latentFormula = ~ INCIVILITY + OSTRACISM+ UNDERMINE+CENTRAL_T1+treat,
                data=follmer_long, index=c("ID","TIME"), k=1:10,
                version =  "continuous", out_se = TRUE,output=TRUE)

m2

save(m2,file="LMS.Rdata")
#contiene l'output di ogni modello LM stimato, mediante
#la funzione lmestSearch, per ciascun numero di stati latenti definiti in input


grafico<-data.frame(criterio=c(m2$Aic,m2$Bic),
              index=c(rep("AIC",10),rep("BIC",10)),
              stati=as.factor(rep(seq(1,10),2)))

ggline(grafico,x="stati",y="criterio",color="index")+
  labs(x="Numero stati latenti", y = "Valore",color="")


################################################################################
############################ STIMA MODELLO #####################################


modello2<-lmestCont(responsesFormula = NSUIC+ENGAGE ~ NULL,
                    latentFormula = ~ INCIVILITY + OSTRACISM+ UNDERMINE+CENTRAL_T1+treat,
                    index = c("ID", "time"),
                    data = follmer_long, 
                    k = 4,
                    output = TRUE,
                    out_se = TRUE)

modello2

summary(modello2)


##################### SIGNIFICATIVITà DEI PARAMETRI


modello2$Be/modello2$seBe

modello2$Ga/modello2$seGa


##################### GRAFICI MODELLO

# density plot

plot(modello2,what = "density",components=c(1,2,3,4))

# averaged transition probabilities

plot(modello2,what="transitions")

PI<-round(apply(modello2$PI[, , , 2:modello2$TT], c(1, 2), mean), 4);PI

# averaged marginal distribution

PMarg <- array(0,c(279,4,3))
PMarg[,,1] <- as.matrix(modello2$Piv)
for(i in 1:279) for(t in 2:3) PMarg[i,,t]= t(modello2$PI[,,i,t])%*%PMarg[i,,t-1]
Pmarg <-apply(PMarg,c(2,3),mean)
pmarg <- apply(PMarg,2,mean)

tab<-data.frame(pp=c(Pmarg[,1],Pmarg[,2],Pmarg[,3]),
                Stati=as.factor(rep(c("1","2","3","4"),3)),
                TT=c(rep("T1",4),rep("T2",4),rep("T3",4)))

ggline(tab,x="TT",y="pp",color="Stati",plot_type="l")+
  labs(x="Periodo temporale", y = "Distribuzione marginale media ",color="Stati latenti")+ 
  ylim(0,1)+ theme_classic()+
  scale_color_manual(values = c("red","orange","#3BB9FF","#1569C7"))+
  scale_fill_manual(values=c("red","orange","#3BB9FF","#1569C7"))

piv1 <- round(colMeans(modello2$Piv[, ]), 4);piv1


########################## DECODING

Ul2<-as.data.frame(modello2$Ul) 
colnames(Ul2)<-c("T1","T2","T3")
Ul2$id<- seq_len(nrow(Ul2))
U_long2<-reshape(Ul2, idvar = "id", times = c("T1","T2","T3"),timevar = "time",
                 varying = matrix(colnames(Ul2)[1:3], nrow = 1, ncol=3),
                 v.names = "Stati",direction = "long")

df<-U_long2 %>% group_by(time,Stati) %>% summarise(countn=n()/279)
df$Stati<-as.factor(df$Stati)


ggplot(df,aes(x=time,y=countn))+
  geom_bar(aes(color=Stati,fill=Stati),stat="identity",position = position_stack(reverse=TRUE))+
  labs(title = "Frequenze stati latenti",
       x="Periodo temporale", 
       y = "Freq (n=279)", 
       color="Stati latenti", 
       fill = "Stati latenti")+
  theme_classic()+
  scale_color_manual(values = brewer.pal(n=7,name="YlGnBu"))+
  scale_fill_manual(values = brewer.pal(n=7,name="YlGnBu"))


apply(modello2$Ul,2,table)


################################################################################
########################## DECODIFICA DEI SOGGETTI #############################

################# PERMANENZA STESSO STATO 

Ul2 %>% subset(id %in% which(Ul2$T1==Ul2$T2 & Ul2$T2==Ul2$T3)) %>% head#225


# Stato 1

allstato1<-follmer_long %>% subset(id %in% which(Ul2$T1==1 & Ul2$T2==1 & Ul2$T3==1))  
allstato1$treat<-as.factor(allstato1$treat)
levels(follmer_long$time)<-c("1","2","3")
labels <- list(variables=list(NSUIC="Intenzione al suicidio",ENGAGE="Work engagement",
                              INCIVILITY="Inciviltà",UNDERMINE="Social Undermining",
                              OSTRACISM="Ostracismo",CENTRAL_T1= "Stigma centrality",
                              treat="Trattamento"),groups=list("Sondaggio",""))

strata<-c(split(allstato1,allstato1$time),list(Totale=allstato1))
table1(strata,labels,groupspan = c(3,1))


# Stato 2

allstato2<-follmer_long %>% subset(id %in% which(Ul2$T1==2 & Ul2$T2==2 & Ul2$T3==2))  
allstato2$treat<-as.factor(allstato2$treat)
strata<-c(split(allstato2,allstato2$time),list(Totale=allstato2))
table1(strata,labels,groupspan = c(3,1))


# Stato 3

allstato3<-follmer_long %>% subset(id %in% which(Ul2$T1==3 & Ul2$T2==3 & Ul2$T3==3))  
allstato3$treat<-as.factor(allstato3$treat)
strata<-c(split(allstato3,allstato3$time),list(Totale=allstato3))
table1(strata,labels,groupspan = c(3,1))


# Stato 4

allstato4<-follmer_long %>% subset(id %in% which(Ul2$T1==4 & Ul2$T2==4 & Ul2$T3==4))  
allstato4$treat<-as.factor(allstato4$treat)
strata<-c(split(allstato4,allstato4$time),list(Totale=allstato4))
table1(strata,labels,groupspan = c(3,1))


########################### CAMBIO DI STATO

# cambio stato a ogni tempo

Ul2[Ul2$id %in% which(!(Ul2$T1==Ul2$T2 | Ul2$T2==Ul2$T3)), ]#16
change<-follmer_long %>% subset(id %in% which(!(Ul2$T1==Ul2$T2 | Ul2$T2==Ul2$T3) )) 
change$treat<-as.factor(change$treat)
strata<-c(split(change,change$time),list(Totale=change))
table1(strata,labels,groupspan = c(3,1))

# cambio stato una volta sola

head(Ul2[Ul2$id %in% which((Ul2$T1==Ul2$T2 & Ul2$T2!=Ul2$T3) |(Ul2$T1!=Ul2$T2 & Ul2$T2==Ul2$T3)), ])
changeone<-follmer_long %>%
  subset(id %in% which((Ul2$T1==Ul2$T2 & Ul2$T2!=Ul2$T3) |(Ul2$T1!=Ul2$T2 & Ul2$T2==Ul2$T3))) 
changeone$treat<-as.factor(changeone$treat)
strata<-c(split(changeone,changeone$time),list(Totale=changeone))
table1(strata,labels,groupspan = c(3,1))


########################## STATO PEGGIORE AL TEMPO 3

stato3<-follmer_long  %>% subset(id %in% which(Ul2$T3==3))

# SOLO TEMPO 3

Ul2[Ul2$T1!=3 & Ul2$T2!=3 & Ul2$T3==3,]
stato3T3<-follmer_long %>% subset(id %in% which(Ul2$T1!=3 & Ul2$T2!=3 & Ul2$T3==3))  
stato3T3$treat<-as.factor(stato3T3$treat)
strata<-c(split(stato3T3,stato3T3$time),list(Totale=stato3T3))
table1(strata,labels,groupspan = c(3,1))

# SOLO TEMPO 2 E TEMPO 3

Ul2[Ul2$T1!=3 & Ul2$T2==3 & Ul2$T3==3,]
stato3T2T3<-follmer_long %>% 
  subset(id %in% which(Ul2$T1!=3 & Ul2$T2==3 & Ul2$T3==3))%>%
  select(c(1,4,6:12))

# SOLO TEMPO 1 E TEMPO 3

Ul2[Ul2$T1==3 & Ul2$T2!=3 & Ul2$T3==3,]
stato3T1T2<-follmer_long %>% 
  subset(id %in% which(Ul2$T1==3 & Ul2$T2!=3 & Ul2$T3==3)) %>%
  select(c(1,4,6:12))


########################## STATO MIGLIORE AL TEMPO 3

stato2<-follmer_long  %>% subset(id %in% which(Ul2$T3==2))

# SOLO TEMPO 3

Ul2[Ul2$T1!=2 & Ul2$T2!=2 & Ul2$T3==2,]
stato2T3<-follmer_long %>% subset(id %in% which(Ul2$T1!=2 & Ul2$T2!=2 & Ul2$T3==2))  
stato2T3$treat<-as.factor(stato2T3$treat)
strata<-c(split(stato2T3,stato2T3$time),list(Totale=stato2T3))
table1(strata,labels,groupspan = c(3,1))

# SOLO TEMPO 2 E TEMPO 3

Ul2[Ul2$T1!=2 & Ul2$T2==2 & Ul2$T3==2,]
stato2T2T3<-follmer_long %>% subset(id %in% which(Ul2$T1!=2 & Ul2$T2==2 & Ul2$T3==2))  
stato2T2T3$treat<-as.factor(stato2T2T3$treat)
strata<-c(split(stato2T2T3,stato2T2T3$time),list(Totale=stato2T2T3))
table1(strata,labels,groupspan = c(3,1))

# SOLO TEMPO 1 E TEMPO 3

Ul2[Ul2$T1==2 & Ul2$T2!=2 & Ul2$T3==2,]
stato2T1T3<-follmer_long %>% subset(id %in% which(Ul2$T1==2 & Ul2$T2!=2 & Ul2$T3==2))  
stato2T1T3 %>% select(c(1,4,6:12)) %>% as.data.frame


########################## TRANSIZIONE DA QUARTO STATO

# VERSO STATO MIGLIORE

quattroU2<-(Ul2$T1==4 & Ul2$T2==2) | ( Ul2$T2==4 & Ul2$T3==2) 
Ul2[quattroU2, ]
quattro2<-follmer_long %>% subset(id %in% which(quattroU2)) 
quattro2$treat<-as.factor(quattro2$treat)
strata<-c(split(quattro2,quattro2$time),list(Totale=quattro2))
table1(strata,labels,groupspan = c(3,1))

# VERSO STATO PEGGIORE

quattroU3<-(Ul2$T1==4 & Ul2$T2==3 ) | ( Ul2$T2==4 & Ul2$T3==3) 
Ul2[quattroU3,]
quattro3<-follmer_long %>% subset(id %in% which(quattroU3) )
quattro3$treat<-as.factor(quattro3$treat)
strata<-c(split(quattro3,quattro3$time),list(Totale=quattro3))
table1(strata,labels,groupspan = c(3,1))


########################## TRANSIZIONE DA SECONDO A PRIMO STATO

dueu<-(Ul2$T1==2 & Ul2$T2==1 ) | ( Ul2$T2==2 & Ul2$T3==1) 
Ul2[dueu,]
due<-follmer_long %>% subset(id %in% which(dueu) )
due$treat<-as.factor(due$treat)
strata<-c(split(due,due$time),list(Totale=due))
table1(strata,labels,groupspan = c(3,1))