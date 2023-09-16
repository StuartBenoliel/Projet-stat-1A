
############################# Importation et packages #############################################


library(haven)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(factoextra)
library(questionr)
library(missMDA)
library(cluster)
library(esquisse)
library(data.table)
library(sf)
library(cowplot)
library(vioplot)
library(ggrepel)
library(ineq)
library(weights)
library(colourpicker)
library(cartogram)

rm(list=ls())
getwd()
mon_repertoire<-"~/Desktop/ENSAI/Projet stat/R"
setwd(mon_repertoire)

T1_2<-read.table("T1_2.csv",sep=",",header=TRUE)
D1<-read.table("D1.csv",sep=",",header=TRUE)
D1<-D1[,c(1:6,10:28,7,8,9)]

Lab1<-c("Très peu denses","Peu denses","Densité intermédiaire","Densément peuplés")
Lab2<-c("Hors attraction","Aire de - de 50 000hab","Aire de 50 000 à - de 200 000 hab",
        "Aire de 200 000 à - de 700 000 hab","Aire de 700 000 hab ou + hors Paris",
        "Aire de Paris")
Lab3<-c("Non centre","Centre local", "Centre intermédiaires", "Centres structurant",
        "Centre majeur")

T1_2[,35]<-factor(T1_2[,35], levels= unique(T1_2[,35]),labels=Lab1,exclude=NULL)
T1_2[,36]<-factor(T1_2[,36], levels= unique(T1_2[,36]),labels=Lab2,exclude=NULL)
T1_2[,37]<-factor(T1_2[,37], levels= unique(T1_2[,37]),labels=Lab3,exclude=NULL)
T1_2$Département<- as.factor(T1_2$Département)


FQuantile1_2<-function(var,quant){
  Quantile<-as_tibble(quantile(T1_2[[var]], probs=seq(0,1,1/quant),na.rm=TRUE))
  num<-which(colnames(T1_2)==var)
  A<-subset(T1_2, T1_2[,num]<=as.numeric(Quantile[2,1]) & T1_2[,num]>=as.numeric(Quantile[1,1]))
  T1_2$Tranche<<- ifelse(T1_2$Code %in% A$Code,"1","0")
  r<-c(weighted.mean(A$Voix_Le_Pen_T1,A$Exprimés_T1),weighted.mean(A$Voix_Le_Pen_T2,A$Exprimés_T2))
  for (i in 2:quant) {
    A<-subset(T1_2, T1_2[,num]<=as.numeric(Quantile[i+1,1]) & T1_2[,num]>as.numeric(Quantile[i,1]))
    T1_2$Tranche<<- ifelse(T1_2$Code %in% A$Code,i,T1_2$Tranche)
    r<-rbind(r,c(weighted.mean(A$Voix_Le_Pen_T1,A$Exprimés_T1),weighted.mean(A$Voix_Le_Pen_T2,A$Exprimés_T2)))
  }
  Quantile<-Quantile[-1,]
  Quantile<-cbind(Quantile,r,seq(1:quant))
  ifelse(quant==100,colnames(Quantile)<-c("Centiles","Moyenne_Lepen_T1","Moyenne_Lepen_T2",paste("Tranches_Centiles_",var,sep="")),
         colnames(Quantile)<-c("Déciles","Moyenne_Lepen_T1","Moyenne_Lepen_T2",paste("Tranches_Déciles_",var,sep="")))
  return(Quantile)
}


#FDépartement1_2<-function(){
#  nlevels<-nlevels(T1_2$Département)
#  T1_2Dep<- data.frame(matrix(0, ncol = 30, nrow = nlevels))
#  names(T1_2Dep)<-names(T1_2)[c(2,3,5:32)]
#  for (i in 1:nlevels){
#    T1_2Dep[i,]<- T1_2 %>% filter(Département==levels(T1_2$Département)[i]) %>% select(c(2,3,5:32))%>%  slice(1)
#    T1_2Dep[i,2]<-levels(T1_2$Département)[i]
#    T1_2Dep[i,c(3,4,6,7)]<- T1_2 %>% filter(Département==levels(T1_2$Département)[i]) %>% select(5,6,8,9) %>% colSums()
#    T1_2Dep[i,c(5,8:30)]<- T1_2 %>% filter(Département==levels(T1_2$Département)[i]) %>% select(7,10:32) %>% colMeans(na.rm=TRUE) %>% round(1)
#  }
#  return(T1_2Dep)
#}


#T1_2Dep<-FDépartement1_2()

rm(Lab1,Lab2,Lab3,mon_repertoire)


############################# Graph Bivariés ######################################################


M1=round(weighted.mean(T1_2$Voix_Le_Pen_T1,T1_2$Exprimés_T1),1)
M2=round(weighted.mean(T1_2$Voix_Le_Pen_T2,T1_2$Exprimés_T2),1)
#summary(T1_2)
#str(T1_2)

################################# Corrélation Tour ################################################


#cor<-round(cor(T1_2[,-c(1:4,35:38)],method="pearson",use="complete"),2)
cor<-round(wtd.cor(T1_2[,-c(1:4,35:38)],weight=T1_2$Exprimés_T1)[[1]],2)
Cor<-tibble(row.names(cor),cor[,4])[-c(1,2,3,4,5,6,7,8,9,10,11,19,21,24,25,26,27,29,30),]
colnames(Cor)<-c("Variable","Corrélation")
Cor$Signe<-ifelse(Cor$Corrélation >0,"+","-")
Cor[10,1]<-"Part diplomés du supérieur"
Cor[9,1]<-"Part non diplomés"
Cor[8,1]<-"Taux de chômage"
Cor[11,1]<-"Médiane du \nrevenu disponible"
Cor[7,1]<-"Part retraités"
Cor[6,1]<-"Part professions \nintermédiaires"
Cor[5,1]<-"Part ouvriers"
Cor[4,1]<-"Part employés"
Cor[3,1]<-"Part cadres et professions \nintellectuelles supérieures"
Cor[2,1]<-"Part artisans, \ncommercants, \nchefs d'entrprise"
Cor[1,1]<-"Part agriculteurs exploitants"

#image(cor(T1_2[,-c(1:4,33:35)],use="complete")[,ncol(T1_2[,-c(1:4,33:35)]):1])
#pairs(T1_2[,-c(1:4,33:35)])


COR_T<-ggplot(Cor,aes(y = Variable, x=Corrélation, fill=Signe))+theme_bw()+
    theme(axis.text=element_text(face="bold"),
          axis.title.x = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, face = "bold.italic"),
          axis.text.y = element_text(face = "bold", hjust = 0.5)  )+
    labs(caption="Champ : France métropolitaine, à l'échelle communale et pondéré par les exprimés \nSource : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022
              Insee-DGFIP-Cnaf-Cnav-CCMSA, Filosofi 2020 ")+
    scale_x_continuous("Coefficient de corrélation de Pearson")+
    scale_y_discrete("")+
    geom_col(show.legend = FALSE)
COR_T

#ggsave("Rprojetgraph/CorN.png", plot = COR_T, width = 8, height = 6, dpi = 300)

cor<-round(cor(T1_2[,-c(1:4,35:37)],method="pearson",use="complete"),2)
Cor<-tibble(row.names(cor),cor[,8])[-c(1,2,3,4,5,6,7,8),]
colnames(Cor)<-c("Variable","Corrélation")
Cor$Signe<-ifelse(Cor$Corrélation >0,"+","-")

rm(cor,Cor,COR_T1,COR_T2)


####################### Diplomés supérieur non scolarisés #########################################

Quantile<-FQuantile1_2("Diplomés_supérieur_non_scolarisés",100)

Gdip<-ggplot(Quantile, aes(x = Quantile[,4])) +
  geom_point(aes(y = Quantile[,2], color = "Blue Points")) +
  geom_point(aes(y = Quantile[,3], color = "Red Points")) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("1er Tour", "2ème Tour")) +
  labs(caption="Méthode : Régression loess / Le point associé au xème centile est calculée sur les communes dont la part
      des diplomés du supérieur est comprise entre le x-1ème et le xème centile\nChamp : France métropolitaine, population des 15 ans et plus non scolarisée ayant obtenu un diplôme type universitaire \nSource : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022",
       color = "") + theme_bw()+
  theme(axis.text=element_text(face="bold"),axis.title=element_text(face="bold"),legend.position = "top",
        legend.box = "horizontal",plot.caption = element_text(hjust = 0, face = "bold.italic"),
        legend.title = element_text(face = "bold"),legend.text = element_text(face = "bold"))+
  scale_x_continuous("Centile de la part des diplomés du supérieur", breaks = seq(0, 100, by = 10)) +
  scale_y_continuous("Moyenne pondérée par les exprimés des voix Le Pen (%)") +
  geom_smooth(aes(y = Quantile[,2], color = "Blue Points"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = Quantile[,3], color = "Red Points"), method = "loess", se = FALSE) +
  geom_hline(yintercept = M1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = M2, linetype = "dashed", color = "black") +
  geom_text(x = 20, y = M1 + 1.5, label = "Moyenne nationale Tour 1 : 23.5", fontface = 3,color="blue")+
  geom_text(x = 20, y = M2 + 1.5, label = "Moyenne nationale Tour 2 : 41.5", fontface = 3,color="red")
Gdip


#ggsave("Rprojetgraph/Gdip.png", plot = Gdip, width = 8, height = 6, dpi = 300)


Q9<- T1_2 %>% filter(Tranche>90 | Tranche==100) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))
Q9<- T1_2 %>% filter(Tranche>0 | Tranche==10) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

Bdip<-ggplot(Q9,aes(x =Département, y = n, fill=Région))+theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
    labs(caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
       fill="Régions")+
    scale_x_discrete("Départements")+
    scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
    geom_col(width = .7)
Bdip

cor(T1_2$Diplomés_supérieur_non_scolarisés,T1_2$Voix_Le_Pen_T1) #-0.49

with(T1_2 %>% filter(Tranche>90 | Tranche==100),cor(Diplomés_supérieur_non_scolarisés,Voix_Le_Pen_T1)) #-0.45
with(T1_2 %>% filter(Tranche<=10 | Tranche==0),cor(Diplomés_supérieur_non_scolarisés,Voix_Le_Pen_T1)) #0
with(T1_2 %>% filter((Tranche>90 | Tranche==100) & Région %in% c("Île-de-France","Auvergne-Rhône-Alpes","Occitanie")),
     cor(Voix_Le_Pen_T1,Diplomés_supérieur_non_scolarisés)) #-0.49

################### Cadres professions intellectuelles supérieures ################################

CPIS<-ggplot(T1_2,aes(x = Cadres_professions_intellectuelles_supérieures , y =Voix_Le_Pen_T1, color=Diplomés_supérieur_non_scolarisés))+
  theme_bw()+theme(axis.title=element_text(face="bold"),plot.title = element_text(hjust = 0.5, face="bold"),legend.position = "top",legend.box = "horizontal",
                   legend.title =element_text(hjust = 0.5, face="bold"),plot.caption = element_text(hjust = 0, face = "bold.italic"))+
  labs(caption="Champ : France métropolitaine, communes ayant un taux supérieur à 0.1% \nSource : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022")+
  scale_color_viridis_c("Taux de diplomés \n de l'enseigment supérieur", option = "plasma")+
  scale_x_continuous("Part des cadres et professions intellectuelles supérieures (%)", limits = c(0.1,40))+
  scale_y_continuous("Voix Le Pen au 1er Tour 2022 (%)", limits = c(0,70))+
  geom_point()
CPIS

#ggsave("Rprojetgraph/CPIS.png", plot = CPIS, width = 8, height = 6, dpi = 300)

cor(T1_2$Diplomés_supérieur_non_scolarisés,T1_2$Cadres_professions_intellectuelles_supérieures,use="complete") #0.6

Quantile<-FQuantile1_2("Cadres_professions_intellectuelles_supérieures",100)

GCPIS<-ggplot(Quantile, aes(x = Quantile[,4])) +
  geom_point(aes(y = Quantile[,2], color = "Blue Points")) +
  geom_point(aes(y = Quantile[,3], color = "Red Points")) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("1er Tour", "2ème Tour")) +
  labs(caption="Méthode : Régression loess \nChamp : France métropolitaine, centiles inférieurs au 14ème identiques et non prises en compte \nSource : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022",
       color = "") + theme_bw() +
  theme(axis.text=element_text(face="bold"),axis.title=element_text(face="bold"),legend.position = "top",
        legend.box = "horizontal",plot.caption = element_text(hjust = 0, face = "bold.italic"),
        legend.title = element_text(face = "bold"),legend.text = element_text(face = "bold"))+
  scale_x_continuous("Centile de la part des cadres et professions intellectuelles supérieures",limits=c(15,100),breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne pondérée par les exprimés des voix Le Pen (%)")+
  geom_smooth(aes(y = Quantile[,2], color = "Blue Points"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = Quantile[,3], color = "Red Points"), method = "loess", se = FALSE) +
  geom_hline(yintercept = M1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = M2, linetype = "dashed", color = "black") +
  geom_text(x = 30, y = M1 - 1, label = "Moyenne nationale Tour 1 : 23.5", fontface = 3,color="blue")+
  geom_text(x = 30, y = M2 - 1, label = "Moyenne nationale Tour 2 : 41.5", fontface = 3,color="red")
GCPIS

#ggsave("Rprojetgraph/GCPIS.png", plot = GCPIS, width = 8, height = 6, dpi = 300)

Q9<-T1_2 %>% filter(Tranche>90 | Tranche==100) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))
Q9<-T1_2 %>% filter(Tranche>0 | Tranche==10) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))


BCPIS<-ggplot(Q9,aes(x =Département, y = n, fill=Région))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
       fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
BCPIS

cor(T1_2$Voix_Le_Pen_T1,T1_2$Cadres_professions_intellectuelles_supérieures,use="complete")#-0.26

with(T1_2 %>% filter((Tranche>90 | Tranche==100)) ,
     cor(Voix_Le_Pen_T1,Cadres_professions_intellectuelles_supérieures,use="complete")) #-0.2

with(T1_2 %>% filter((Tranche<=10 | Tranche==0)) ,
     cor(Voix_Le_Pen_T1,Cadres_professions_intellectuelles_supérieures,use="complete")) #0

with(T1_2 %>% filter((Tranche>90 | Tranche==100) & Région %in% c("Île-de-France")),
     cor(Voix_Le_Pen_T1,Cadres_professions_intellectuelles_supérieures)) #-0.6

with(T1_2 %>% filter((Tranche>90 | Tranche==100) & Région %in% c("Île-de-France","Auvergne-Rhône-Alpes","Occitanie")),
     cor(Voix_Le_Pen_T1,Cadres_professions_intellectuelles_supérieures,use = "complete")) #-0.35


##################################### Ouvriers ####################################################

Quantile<-FQuantile1_2("Ouvriers",100)

GOU<-ggplot(Quantile, aes(x = Quantile[,4])) +
  geom_point(aes(y = Quantile[,2], color = "Blue Points")) +
  geom_point(aes(y = Quantile[,3], color = "Red Points")) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("1er Tour", "2ème Tour")) +
  labs(caption="Méthode : Régression linéaire \nChamp : France métropolitaine \nSource : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022",color = "") +
  theme_bw() +theme(axis.text=element_text(face="bold"),axis.title=element_text(face="bold"),
                    plot.caption = element_text(hjust = 0, face = "bold.italic"),legend.position = "top",
                    legend.box = "horizontal",legend.text = element_text(face = "bold"))+
  scale_x_continuous("Centile de la part des ouvriers",breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne pondérée par les exprimés des voix Le Pen (%)")+
  geom_smooth(aes(y = Quantile[,2], color = "Blue Points"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = Quantile[,3], color = "Red Points"), method = "lm", se = FALSE) +
  geom_hline(yintercept = M1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = M2, linetype = "dashed", color = "black") +
  geom_text(x = 80, y = M1 + 1, label = "Moyenne nationale Tour 1 : 23.5", fontface = 3,color="blue")+
  geom_text(x = 80, y = M2 + 1, label = "Moyenne nationale Tour 2 : 41.5", fontface = 3,color="red")
GOU

#ggsave("Rprojetgraph/GOU.png", plot = GOU, width = 8, height = 6, dpi = 300)

summary(lm(Moyenne_Lepen_T1 ~ Tranches_Centiles_Ouvriers, data=Quantile)) #R^2=0.89

Q9<- T1_2 %>% filter(Tranche>90 | Tranche==100) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))
Q9<-T1_2 %>% filter(Tranche>0 | Tranche==10) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

Test<-T1_2 %>% filter(Tranche>0 | Tranche==10 & Région %in% c("Bourgogne-Franche-Comté","Grand Est","Hauts-de-France"))
weighted.mean(Test$Voix_Le_Pen_T1,Test$Exprimés_T1)

BOU<-ggplot(Q9,aes(x =Département, y =n, fill=Région))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
       fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
BOU

cor(T1_2$Voix_Le_Pen_T1,T1_2$Ouvriers,use="complete")# 0.38

with(T1_2 %>% filter((Tranche>90 | Tranche==100)),cor(Voix_Le_Pen_T1,Ouvriers)) # 0
with(T1_2 %>% filter((Tranche<=10 | Tranche==0)) ,cor(Voix_Le_Pen_T1,Ouvriers,use="complete")) #0
with(T1_2 %>% filter((Tranche>90 | Tranche==100) & Région %in% c("Bourgogne-Franche-Comté","Grand Est","Hauts-de-France")),
     cor(Voix_Le_Pen_T1,Ouvriers)) #0

T1$Tranche<-NULL
T1_2$Tranche<-NULL

rm(Quantile,Q9,Gdip,Bdip,CPIS,GCPIS,BCPIS,GOU,BOU,M1,M2)


############################ Analyse Multi-variée #################################################

D1$GroupeACH<-NULL
D1$Abstentions_Exprimés<- 100*D1$Abstentions/D1$Exprimés

Pauv<-read.csv2("~/Desktop/ENSAI/Projet stat/R/Indicateurs/Départements/pauvrete_d.csv",sep=';',skip=2)
colnames(Pauv)[3]<-"Taux pauvreté"
Pauv$`Taux pauvreté`<-as.numeric(Pauv$`Taux pauvreté`)
D1<-left_join(D1,Pauv[,c(2,3)],join_by(Département==Libellé))

#I<-T1_2[,c(2,3,4,5,6,7,16,18,19,20,25,24,27,12)]
#I<-I[complete.cases(I), ]
I<-D1[,c(1,6,14,18,23,29,30,21)]
colnames(I)[-c(1,7)]<-c("% Le Pen 1er Tour","Taux ouvriers","Taux chômage","Taux immigrés","Taux abstention","Taux diplomés du supérieur")

summary(I)
round(cor(I[,-1],method="pearson",use="complete"),2)
apply(I[,-1], 2, function(x) sd(x,na.rm = TRUE))

ACP <- PCA(I,quali.sup=1,graph = FALSE, scale.unit = TRUE) #Dep et reg en sup
#ACP <- PCA(I[,-3],quali.sup=c(1,2), quanti.sup=c(3,5,12),graph = FALSE, scale.unit = TRUE) #Dep et reg en sup

round(head(ACP$eig[,]),2)#VP
#barplot(ACP$eig[,1], main="Valeurs propres",names.arg = paste0("dim", 1:nrow(ACP$eig)))
VP<-fviz_screeplot(ACP, addlabels = TRUE, ylim = c(0, 50),main="",ylab="Pourcentage de variances expliqués")
VP

#ggsave("Rprojetgraph/VP.png", plot = VP, width = 8, height = 6, dpi = 300)

ACP_var<-fviz_pca_var(ACP, col.var = "cos2", repel = TRUE, title="",
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
ACP_var

#ggsave("Rprojetgraph/ACP_var.png", plot = ACP_var, width = 8, height = 6, dpi = 300)

ACP_ind<-plot.PCA(ACP, choix = "ind",title="",graph.type = "ggplot",
                  col.quali="blue",col.ind = alpha("black", 0.2))
ACP_ind

#ggsave("Rprojetgraph/ACP_ind.png", plot = ACP_ind, width = 8, height = 6, dpi = 300)


head(summary(ACP, nb.dec = 2, nbelements = Inf))
rowSums(ACP$ind$cos2[,1:2]) #qualité représentation observation sur les 2 axes

head(ACP$ind$contrib[,1:2])#contribution relative d une observ a constru axe >> pi =atypique
which.max(ACP$ind$contrib[,2])
max(ACP$ind$contrib[,1:2])

ACP$var$cos2[,1:2]
ACP$var$cor[,1:2]
ACP$var$contrib[,1:2]

rm(ACP,ACP_ind,ACP_var,Pauv)

##################################### Atypique ####################################################

summary(Atypique)
summary(I)
apply(Atypique[,-1], 2, function(x) sd(x,na.rm = TRUE))

rm(I,Atypique)

##################################### DOM-TOM #####################################################

S1_2<-read.table("S1_2.csv",sep=",",header=TRUE)
#S1_2<-S1_2[complete.cases(S1_2$Ouvriers),]

FQuantileS1_2<-function(var,quant){
  Quantile<-as_tibble(quantile(S1_2[[var]], probs=seq(0,1,1/quant),na.rm=TRUE))
  num<-which(colnames(S1_2)==var)
  A<-subset(S1_2, S1_2[,num]<=as.numeric(Quantile[2,1]) & S1_2[,num]>=as.numeric(Quantile[1,1]))
  S1_2$Tranche<<- ifelse(S1_2$Code %in% A$Code,"1","0")
  r<-c(weighted.mean(A$Voix_Le_Pen_T1,A$Exprimés_T1),weighted.mean(A$Voix_Le_Pen_T2,A$Exprimés_T2))
  for (i in 2:quant) {
    A<-subset(S1_2, S1_2[,num]<=as.numeric(Quantile[i+1,1]) & S1_2[,num]>as.numeric(Quantile[i,1]))
    S1_2$Tranche<<- ifelse(S1_2$Code %in% A$Code,i,S1_2$Tranche)
    r<-rbind(r,c(weighted.mean(A$Voix_Le_Pen_T1,A$Exprimés_T1),weighted.mean(A$Voix_Le_Pen_T2,A$Exprimés_T2)))
    rm(A)
  }
  Quantile<-Quantile[-1,]
  Quantile<-cbind(Quantile,r,seq(1:quant))
  ifelse(quant==100,colnames(Quantile)<-c("Centiles","Moyenne_Lepen_T1","Moyenne_Lepen_T2",paste("Tranches_Centiles_",var,sep="")),colnames(Quantile)<-c("Déciles","Moyenne_Lepen",paste("Tranches_Déciles_",var,sep="")))
  return(Quantile)
}

SM1=round(weighted.mean(S1_2$Voix_Le_Pen_T1,S1_2$Exprimés_T1),1)
SM2=round(weighted.mean(S1_2$Voix_Le_Pen_T2,S1_2$Exprimés_T2),1)

cor<-round(cor(S1_2[,c(5,7,12:33)],method="pearson",use="complete"),2)
Cor<-tibble(row.names(cor),cor[,2])[-c(1,2),]
colnames(Cor)<-c("Variable","Corrélation")
Cor$Signe<-ifelse(Cor$Corrélation >0,"+","-")

COR_T<-ggplot(Cor,aes(y = Variable, x=Corrélation, fill=Signe))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Note : Calculée à l'échelle communale avec la nomenclature des PCS de 2003 \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes Emploi")+
  scale_x_continuous("Coefficient de corrélation de Pearson")+
  scale_y_discrete("")+
  geom_col(show.legend = FALSE)
COR_T

summary(T1_2)
summary(S1_2)
str(T1_2)


######################## Classification ascendante hiérarchique ###################################

X<-D1[,c(6,7,21,16,24,15)]

CAH<-agnes(X,metric="euclidian",method = "ward",stand=TRUE)
#Maximiser l inertie intergroupe(grande diff entre classe) et minimiser l'inertie intrag(homogénéité dans un groupe)

D1$GroupeACH <- factor(cutree(CAH, k = 3),levels = 1:3,labels = paste("Groupe", 1: 3))
#D1$GroupeACH <- case_when(
#  D1$Voix_Macron> D1$Voix_Le_Pen & D1$Voix_Macron >D1$Voix_Mélenchon ~ "Macron",
#  D1$Voix_Le_Pen > D1$Voix_Macron& D1$Voix_Le_Pen  >D1$Voix_Mélenchon ~ "Le Pen",
#  D1$Voix_Mélenchon > D1$Voix_Le_Pen  & D1$Voix_Mélenchon > D1$Voix_Macron~ "Mélenchon",
#  TRUE ~ NA_character_  # Gestion des cas où les variables sont égales
#)

sortedHeight <- sort(CAH$height, decreasing = TRUE)
relHeight <- sortedHeight / sum(sortedHeight) * 100
cumHeight <- cumsum(relHeight)

barplot(relHeight[ 1: 15], names.arg = seq(1, 15, 1),col = "black",border = "white",
        xlab = "Noeuds", ylab = "Part de l'inertie totale (%)")



#plot(sortedHeight[1:15],type = "h",xlab = "Noeuds",ylab = "Niveau d'agrégation" )
#barplot(cumHeight[ 1: 15], names.arg = seq(1, 15, 1), col = "black", border = "white",xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)")
#plot(CAH, which.plot= 1)
#plot(T1_2Dep[,c(5,14,16,17,19,23,10,25)], col = c("red", "green", "blue")[cutree(CAH, k = 3)],main = "Clusters obtenus avec la méthode CAH")


#circlize_dendrogram(as.dendrogram(as.hclust(CAH)) %>% color_branches(k=3) %>% color_labels, labels_track_height = NA, dend_track_height = .8,labels=FALSE)
#plot(CAH, which.plot = 3, main = "Dendrogramme de la classification hiérarchique ascendante",xlab="Départements",ylab = "Dissimilarité",labels=FALSE,hang=-1)
Dendo<-fviz_dend(CAH,k=3, show_labels = FALSE, rect = FALSE, main="",
          xlab="Département",ylab = "Dissimilarité",palette = c("#E7B800","#4E84C4", "#FC4E07"))
Dendo


#ggsave("Rprojetgraph/Dendo.png", plot = Dendo, width = 8, height = 6, dpi = 300)


Cluster<-fviz_cluster(list(data = X, cluster = cutree(CAH, k = 3)),ggtheme=theme_bw(),
                      ellipse.alpha=0,as.hclust(CAH), stand = TRUE,ellipse.type = "convex",main="")+
  scale_color_manual(values = c("#E7B800","#4E84C4", "#FC4E07"))
Cluster

#ggsave("Rprojetgraph/Cluster.png", plot = Cluster, width = 8, height = 6, dpi = 300)


silhouette_index <- silhouette(cutree(CAH, k = 3), dist(scale(X)))
plot(silhouette_index, col = c("#E7B800","#4E84C4", "#FC4E07"),main="")
#fviz_silhouette(silhouette_index)


Moy_pond<-function(X,k){
  clusProfile <- aggregate(X,by = list(D1$GroupeACH),mean)
  for (i in names(X)){
    for( j in 1:k){
      clusProfile[j,i]<-weighted.mean(D1[D1$GroupeACH == levels(D1$GroupeACH)[j], ][[i]], D1[D1$GroupeACH == levels(D1$GroupeACH)[j], ]$Exprimés)
    }
  }
  return(clusProfile)
}


multistripplot <- function(data, cat, screen = c(16, 10), col = 1:100, parameters = colnames(data)) {
  cat("Parameters: ", parameters, "\n\n")
  nb_col <- length(parameters)
  nb_graphics <- nb_col
  nb_graphics_y <- floor(nb_graphics / sum(screen) * screen[2])
  nb_graphics_x <- ceiling(nb_graphics / nb_graphics_y)

  # Conversion des couleurs
  col2hex <- function(col, alpha = 1) rgb(t(col2rgb(col)), alpha = alpha * 255, maxColorValue = 255)
  colors <- c("#E7B800", "#4E84C4", "#FC4E07")  # col2hex(col)

  # Affichage
  layout(matrix(1:(nb_graphics_x * nb_graphics_y), nb_graphics_y, nb_graphics_x))
  for (i in 1:nb_col) {
    plot(c(0, (length(unique(cat)) + 1)), c(min(data[, i]), max(data[, i])),
         col = "white", type = "n", axes = FALSE, xlab = "", ylab = "")
    axis(2)  # Ajoute l'axe des y à gauche
    axis(1, at = 1:length(unique(cat)), labels = unique(cat), xlab = "")  # Ajoute l'axe des x en bas
    mtext(parameters[i], side = 1, col = "black", line = 2.5)
    grid()
    for (j in 1:length(unique(cat))) {
      stripchart(data[, i][cat == unique(cat)[j]], at = j, method = "jitter",
                 vertical = TRUE, add = TRUE, col = colors[j])
    }
  }
}


multivioplot <- function(data,cat,screen=c(16,10),col=c(1:100),parameters=colnames(data)) {
  nb_col <- length(parameters)
  nb_graphics <- nb_col
  nb_graphics_y <- floor(nb_graphics/sum(screen)*screen[2])
  nb_graphics_x <- ceiling(nb_graphics/nb_graphics_y)
  # Conversion des couleurs
  # Fonction de conversion des couleurs en hexadécimal
  col2hex <- function(col, alpha=1) rgb(t(col2rgb(col)), alpha=alpha*255, maxColorValue=255)
  colors <-  c("#E7B800","#4E84C4", "#FC4E07") # col2hex(col)
  # Affichage
  layout(matrix(1:(nb_graphics_x*nb_graphics_y),nb_graphics_y,nb_graphics_x))
  for (i in c(1:nb_col)) {
    plot(c(0,(length(unique(cat))+1)),c(min(data[,i]),max(data[,i])),col="white",type="n",axes=F,xlab="",ylab="")
    axis(2) # Ajoute l'axe des y à gauche
    axis(1,at=c(1:length(unique(cat))), labels=unique(cat),xlab="") # Ajoute l'axe des x en bas
    mtext(parameters[i],side=1,col="black",line=2.5)
    grid()
    for (j in c(1:length(unique(cat)))) {vioplot(data[,i][cat==unique(cat)[j]],at=j,horizontal=F,add=T,col=colors[j])  }
  }
}


multivioplot <- function(table, cat, parameters = colnames(data),leg) {
  nb_col <- length(parameters)
  # Conversion des couleurs
  # Fonction de conversion des couleurs en hexadécimal
  col2hex <- function(col, alpha = 1) {
    rgb(t(col2rgb(col)), alpha = alpha * 255, maxColorValue = 255)
  }
  colors <- c("#E7B800", "#4E84C4", "#FC4E07") # col2hex(col)
  # Affichage
  plots <- list()
  for (i in 1:nb_col) {
    plot <- ggplot(table, aes(x = cat, y = .data[[names(table)[i]]])) +
      geom_violin(aes(fill = cat),trim = FALSE) +geom_boxplot(width=0.1)+
      scale_fill_manual(values =c("#E7B800","#4E84C4", "#FC4E07") )+
      labs(x = parameters[i],y="")+ theme_bw() + guides(fill = FALSE) +theme(axis.text=element_text(face="bold"),axis.title=element_text(face="bold"))
    plots[[i]] <- plot
  }
  plot_grid(plotlist = plots)+labs(caption=leg) +
    theme(plot.caption = element_text(h=0,face = "bold.italic",size=10))
}

multivioplot(X, D1$GroupeACH, parameters = c("Pourcentages de voix Le Pen au 1er Tour","Part des individus ayant entre 18 et 24 ans","Part des diplômés du supérieur","Part des retraités","Densité de la population","Part des professions intermédiaires"),leg="Source : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022")

mtext("Source : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022", side = 1, line = 4, adj = 2.5, cex = 0.8)

multistripplot(X,D1$GroupeACH,parameters=c("Pourcentages de voix Le Pen au 1er Tour", "Part des individus ayant entre 18 et 24 ans",
                                         "Part des diplomés du supérieur", "Part des retraités",
                                         "Densité de la population","Part des professions intermédiaires"))



clusProfile <-Moy_pond(X,3)
colnames(clusProfile)[c(1,4)] <- c("Groupe","Diplomés_supérieur")
clusLong <- pivot_longer(clusProfile, cols = -Groupe, names_to = "Variable", values_to = "Valeur")

MACH<-ggplot(clusLong[clusLong$Variable != "Densité_population",]) +geom_bar(aes(x = Variable, y = Valeur, fill = Groupe),stat = "identity") +
  facet_wrap(~ Groupe) +coord_flip() + theme_bw() + guides(fill = FALSE) + xlab("") +
  ylab("Moyenne pondérée par les exprimées (%)")+theme(axis.text=element_text(face="bold"),axis.title=element_text(face="bold"),
                                                 plot.caption = element_text( face = "bold"))+
  scale_fill_manual(values =c("#E7B800","#4E84C4", "#FC4E07") )+
  labs(caption="Champ : France métropolitaine \nSource : Insee 2018 / Ministère de l'intérieur 2022")

Dens<-ggplot(clusLong[clusLong$Variable == "Densité_population",]) +geom_bar(aes(x = Variable, y = Valeur, fill = Groupe),stat = "identity") +
  facet_wrap(~ Groupe) + theme_bw() + guides(fill = FALSE) + xlab("") +scale_y_log10()+coord_flip()+
  ylab("Moyenne pondérée par les exprimées à l'échelle logarithmique")+theme(axis.text=element_text(face="bold"),axis.title=element_text(face="bold"),
                                                       plot.caption = element_text( face = "bold"))+
  scale_fill_manual(values =c("#E7B800","#4E84C4", "#FC4E07") )+
  labs(caption="Champ : France métropolitaine \nSource : Recensement, Insee 2018 ")

MoyACH<-plot_grid( Dens, MACH,nrow = 2,scale=c(0.95,1))
MoyACH

#ggsave("Rprojetgraph/MoyACH.png", plot = MoyACH, width = 8, height = 6, dpi = 300)

rm(sortedHeight,relHeight,cumHeight,silhouette_index ,clusProfile,clusLong,MACH,Dendo,Cluster)


#################################### Kmeans #######################################################

dataCluster<-scale(T1_2Dep[,c(5,14,16,17,19,23,10,25)])
Kmeans<-list()
CPtheta<-rep(0,6)
for (k in 1:6){
  Kmeans[[k]]<-kmeans(dataCluster,k,nstart=50)
  CPtheta[K]<-Kmeans[[k]]$tot.withinss
}

plot(1:6,CPtheta,type="b",xlab="K",ylab="inertie-intra")
plot(silhouette(Kmeans[[3]]$cluster,dist=daisy(dataCluster,metric="euclidean")),col = c("red", "green", "blue"))
by(dataCluster,Kmeans[[3]]$cluster,summary)

fviz_nbclust(dataCluster, FUNcluster =kmeans, method = "silhouette")

fviz_cluster(Kmeans[[3]], dataCluster, stand = FALSE, ellipse = TRUE, geom = "point")

plot(dataCluster, col = Kmeans[[3]]$cluster, main = "Clusters obtenus avec la méthode k-means")

T1_2Dep$GroupeKmeans <-paste("Groupe",Kmeans[[3]]$cluster)

clusProfile <- aggregate(T1_2Dep[,c(5,14,16,17,19,23,10,25)],by = list(T1_2Dep$GroupeKmeans),mean)
colnames(clusProfile)[ 1] <- "Groupe"
clusLong <- pivot_longer(clusProfile, cols = -Groupe, names_to = "Variable", values_to = "Valeur")

MACH<-ggplot(clusLong) +geom_bar(aes(x = Variable, y = Valeur, fill = Groupe),stat = "identity") +
  facet_wrap(~ Groupe) +coord_flip() + theme_bw() + guides(fill = FALSE) + xlab("") +
  ylab("Moyenne exprimée en pourcentages")+theme(plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Champ : France métropolitaine \nSource : Insee 2018 / Ministère de l'intérieur 2022")+
  scale_fill_manual(values = c("red","green","blue"))
MACH

rm(silhouette_index,clusProfile,clusLong,MACH,Kmeans,CPtheta)



######################################### Carte ###################################################


France <- readRDS("gadm36_FRA_2_sf.rds")
France <- left_join(France, D1, by = c("NAME_2" = "Département"))  %>%st_set_crs(st_crs(4326))

France_ACH <- ggplot(France) +
  geom_sf(aes(fill = GroupeACH), show.legend = TRUE, color = "white", size = 0.2) +
  coord_sf(datum = NA, expand = FALSE)+# enlever l'affichage des coordonnés et de la grille
  scale_fill_manual(values =c("#E7B800","#4E84C4", "#FC4E07") )+
  theme( legend.position = "top",legend.box = "horizontal",
         panel.background = element_blank(),plot.caption = element_text(face = "bold.italic"),
         legend.title = element_text(face = "bold"),
         legend.text = element_text(face = "bold"))

France_Le_Pen <- ggplot(France) +
  geom_sf(aes(fill = cut(Voix_Le_Pen, breaks = c(5, 18, 25, 30, 40))),
          show.legend = TRUE, color = "white", size = 0.2) +
  coord_sf(datum = NA, expand = FALSE) +
  scale_fill_manual(values = c("#d8eaf3", "#6baed6","#0062B4FF", "#084081" )) +
  theme(panel.background = element_blank(),legend.position = "top",legend.box = "horizontal",
        plot.caption = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))+ labs(fill = "Pourcentages de voix Le Pen")
France_Le_Pen

Carte_CAH<-plot_grid( France_Le_Pen, France_ACH,ncol = 2,scale=c(1,1))+labs(caption="Méthode : Seuils naturels (Jenks-Fisher) \nSource : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022") +
  theme(plot.caption = element_text(h=0,face = "bold.italic",size=10))
Carte_CAH


France_transformed <- st_transform(France, "+proj=laea +lon_0=0 +lat_0=52 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
Fr_cartogram <- cartogram_cont(France_transformed, "Exprimés")

France_Le_Pen_exprimé <- ggplot(Fr_cartogram) +
  geom_sf(aes(fill = cut(Voix_Le_Pen, breaks = c(5, 18, 25, 30, 40))),
          show.legend = TRUE, color = "white", size = 0.2) +
  coord_sf(datum = NA, expand = FALSE) +
  scale_fill_manual(values = c("#d8eaf3", "#6baed6","#0062B4FF", "#084081" )) +
  theme(panel.background = element_blank(),legend.position = "top",legend.box = "horizontal",
        plot.caption = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))+ labs(fill = "Pourcentages de voix Le Pen")
France_Le_Pen_exprimé


#ggsave("Rprojetgraph/Carte_CAH.png", plot = Carte_CAH , width = 11, height = 6, dpi = 300)



######################################### Oral ####################################################

Quantile<-FQuantile1_2("Médiane_revenu_dispo_par_unité_consommation",100)

Grevenu<-ggplot(Quantile, aes(x = Quantile[,4])) +
  geom_point(aes(y = Quantile[,2], color = "Blue Points")) +
  geom_point(aes(y = Quantile[,3], color = "Red Points")) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("1er Tour", "2ème Tour")) +
  labs(caption="Champ : France métropolitaine \nSource : Insee-DGFIP-Cnaf-Cnav-CCMSA, Filosofi 2020 / Ministère de l'intérieur 2022",
       color = "") + theme_bw()+
  theme(axis.text=element_text(face="bold"),axis.title=element_text(face="bold"),legend.position = "top",
        legend.box = "horizontal",plot.caption = element_text(hjust = 0, face = "bold.italic"),
        legend.title = element_text(face = "bold"),legend.text = element_text(face = "bold"))+
  scale_x_continuous("Centile de la médiane du revenu disponible", breaks = seq(0, 100, by = 10)) +
  scale_y_continuous("Moyenne pondérée par les exprimés des voix Le Pen (%)") +
  geom_hline(yintercept = M1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = M2, linetype = "dashed", color = "black") +
  geom_text(x = 20, y = M1 - 3, label = "Moyenne nationale Tour 1 : 23.5", fontface = 3,color="blue")+
  geom_text(x = 20, y = M2 - 3, label = "Moyenne nationale Tour 2 : 41.5", fontface = 3,color="red")
Grevenu

#ggsave("Rprojetgraph/Grevenu.png", plot = Grevenu , width = 8, height = 6, dpi = 300)

Ville_T1_2<-T1_2 %>% arrange(desc(Exprimés_T1)) %>%  head( 40)
sum(T1_2$Exprimés_T1) /10
sum(Ville_T1_2$Exprimés_T1)

Prop<-T1_2 %>%
  mutate(Catégorie= as_factor(
    case_when(Exprimés_T1<4000 ~ "Commune comportant moins de \n 4000 votes exprimés (96.1%)",
              Exprimés_T1<12000 ~ "Commune comportant entre \n 4000 et 12000 votes exprimés (3.0%)",
              Exprimés_T1<40000 ~ "Commune comportant entre \n 12000 et 40000 votes exprimés (0.8%)",
              TRUE ~ "Commune comportant plus de \n 40000 votes exprimés (0.1%)"))) %>%  group_by(Catégorie) %>%
  summarise(Nombre=n(),Votes_Exprimés_T1= sum(Exprimés_T1)) %>%
  mutate(Pourcentage=round(Nombre/sum(Nombre)*100,1),
         Pourcentage_Exprimés = round(Votes_Exprimés_T1 / sum(Votes_Exprimés_T1) * 100,1)) %>%
  arrange(desc(Catégorie)) %>%
  mutate(lab.ypos = cumsum(Pourcentage_Exprimés) - 0.5*Pourcentage_Exprimés)

Lc(T1_2$Exprimés_T1,plot=TRUE)

PROP<-ggplot(Prop, aes(x = "", y = Pourcentage_Exprimés, fill = Catégorie)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0)+
    geom_text(aes(y = lab.ypos, label = paste0(Pourcentage_Exprimés, "%")), color = "white",size = 8)+
    scale_fill_manual(values = c("#9F9FEC", "#9CEC9C","#F1D450","#FF9A8C")) +
    labs(caption="Champ : France métropolitaine \nSource : Ministère de l'intérieur 2022",fill = "Catégories de communes \ndont la proportion dans l'Hexagone :")+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1, title.position = "top", nrow = 4, byrow = TRUE, label.vjust = 1))+
    theme_void()+theme(legend.text = element_text(size=12,face = "bold"),legend.title =element_text(size=12,hjust = 0.5, face="bold", margin = margin(t = 10, b = 10)),
                     plot.caption = element_text(hjust = 0, face = "bold.italic"))
PROP

#ggsave("Rprojetgraph/PROP.png", plot =PROP, width = 10, height = 6, dpi = 300)

cor<-round(cor(Ville_T1_2[,-c(1:4,35:38)],method="pearson",use="complete"),2)
Cor<-tibble(row.names(cor),cor[,4])[-c(1,2,3,4,5,6,7,8),]
colnames(Cor)<-c("Variable","Corrélation")
Cor$Signe<-ifelse(Cor$Corrélation >0,"+","-")


COR_T<-ggplot(Cor,aes(y = Variable, x=Corrélation, fill=Signe))+theme_bw()+
  theme(axis.text=element_text(face="bold"),
        axis.title.x = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, face = "bold.italic"))+
  labs(caption="Note : Calculée à l'échelle communale avec la nomenclature des PCS de 2003 \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes Emploi")+
  scale_x_continuous("Coefficient de corrélation de Pearson")+
  scale_y_discrete("")+
  geom_col(show.legend = FALSE)
COR_T


Grevenu_ville<-ggplot(Ville_T1_2,aes(x = Médiane_revenu_dispo_par_unité_consommation , y =Voix_Le_Pen_T1, color=Diplomés_supérieur_non_scolarisés, label=Commune))+
  theme_bw()+theme(axis.title=element_text(face="bold"),plot.title = element_text(hjust = 0.5, face="bold"),legend.position = "top",legend.box = "horizontal",
                   legend.title =element_text(hjust = 0.5, face="bold"),plot.caption = element_text(hjust = 0, face = "bold.italic"))+
  labs(caption="Lecture : Le revenu disponible représente 'le niveau de vie' d'un ménage ou plus précisement les ressources à disposition
  du ménage pour consommer et épargner \nNote : Les données pour Nantes et Angers ne sont pas disponibles \nSource : Insee-DGFIP-Cnaf-Cnav-CCMSA, Filosofi 2020 / Ministère de l'intérieur 2022")+
  scale_color_viridis_c("Part des diplomés de l'enseigment supérieur",option='D')+
  scale_x_continuous("Médiane du revenu disponible")+
  scale_y_continuous("Voix Le Pen au 1er Tour 2022 (%)")+
  geom_point()+
  geom_text_repel(size = 4, show.legend = FALSE)
Grevenu_ville

#ggsave("Rprojetgraph/Grevenu_ville.png", plot =Grevenu_ville, width = 8, height = 6, dpi = 300)

Gemployé_ville<-ggplot(Ville_T1_2,aes(x = Employés , y =Voix_Le_Pen_T1,color=Cadres_professions_intellectuelles_supérieures, label=Commune))+
  theme_bw()+theme(axis.title=element_text(face="bold"),plot.title = element_text(hjust = 0.5, face="bold"),legend.position = "top",legend.box = "horizontal",
                   legend.title =element_text(hjust = 0.5, face="bold"),plot.caption = element_text(hjust = 0, face = "bold.italic"))+
  labs(caption="Source : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022")+
  scale_color_viridis_c("Taux de cadres et professions intellectuelles supérieures",option='D')+
  scale_x_continuous("Part des employés (%)")+
  scale_y_continuous("Voix Le Pen au 1er Tour 2022 (%)")+
  geom_point()+
  geom_text_repel(size = 4, show.legend = FALSE)
Gemployé_ville

#ggsave("Rprojetgraph/Gemployé_ville.png", plot =Gemployé_ville, width = 8, height = 6, dpi = 300)

Gretraité_ville<-ggplot(Ville_T1_2,aes(x = Retraités , y =Voix_Le_Pen_T1, color=Indicateur_dépendance_économique, label=Commune))+
  theme_bw()+theme(axis.title=element_text(face="bold"),plot.title = element_text(hjust = 0.5, face="bold"),legend.position = "top",legend.box = "horizontal",
                   legend.title =element_text(hjust = 0.5, face="bold"),plot.caption = element_text(hjust = 0, face = "bold.italic"))+
  labs(caption="Lecture : Le taux de dépendance économique est le rapport entre la population des jeunes et des personnes âgées
     sur la population en âge de travailler. Il est défavorable lorsqu’il est supérieur à 100\nSource : Insee, recensement de la population 2018 / Ministère de l'intérieur 2022")+
  scale_color_gradient("Indicateur de dépendance économique",high = "#FF4C4C", low = "#50C878")+
  scale_x_continuous("Part des retraités (%)")+
  scale_y_continuous("Voix Le Pen au 1er Tour 2022 (%)")+
  geom_point()+
  geom_text_repel(size = 4, show.legend = FALSE)
Gretraité_ville

#ggsave("Rprojetgraph/Gretraité_ville.png", plot =Gretraité_ville, width = 8, height = 6, dpi = 300)
