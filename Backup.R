rm(list=ls())


#####Importation et packages
#getwd()
mon_repertoire<-"~/Desktop/ENSAI/Projet stat/R"
setwd(mon_repertoire)
library(openxlsx)
library(haven)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(sas7bdat)
library(factoextra)
library(questionr)
library(ggpubr)
library(questionr)
library(data.table)
library(missMDA)
library(dbscan)

####
#T1<-read.table("T1.csv",sep=",",header=TRUE)
T1_2<-read.table("T1_2.csv",sep=",",header=TRUE)
D1<-read.table("D1.csv",sep=",",header=TRUE)

Lab1<-c("Très peu denses","Peu denses","Densité intermédiaire","Densément peuplés")
Lab2<-c("Hors attraction","Aire de - de 50 000hab","Aire de 50 000 à - de 200 000 hab",
        "Aire de 200 000 à - de 700 000 hab","Aire de 700 000 hab ou + hors Paris",
        "Aire de Paris")
Lab3<-c("Non centre","Centre local", "Centre intermédiaires", "Centres structurant",
        "Centre majeur")


T1[,30]<-factor(T1[,30], levels= unique(T1[,30]),labels=Lab1,exclude=NULL)
T1[,31]<-factor(T1[,31], levels= unique(T1[,31]),labels=Lab2,exclude=NULL)
T1[,32]<-factor(T1[,32], levels= unique(T1[,32]),labels=Lab3,exclude=NULL)
T1$Département<- as.factor(T1$Département)

###################################################################################################
############################# Importation et packages #############################################
###################################################################################################

library(openxlsx)
library(haven)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(sas7bdat)
library(factoextra)
library(questionr)
library(missMDA)
library(cluster)
library(esquisse)
library(data.table)
library(sf)

rm(list=ls())

getwd()
mon_repertoire<-"~/Desktop/ENSAI/Projet stat/R"
setwd(mon_repertoire)

T1<-read.table("T1.csv",sep=",",header=TRUE)
T1_2<-read.table("T1_2.csv",sep=",",header=TRUE)

Lab1<-c("Très peu denses","Peu denses","Densité intermédiaire","Densément peuplés")
Lab2<-c("Hors attraction","Aire de - de 50 000hab","Aire de 50 000 à - de 200 000 hab",
        "Aire de 200 000 à - de 700 000 hab","Aire de 700 000 hab ou + hors Paris",
        "Aire de Paris")
Lab3<-c("Non centre","Centre local", "Centre intermédiaires", "Centres structurant",
        "Centre majeur")


T1[,30]<-factor(T1[,30], levels= unique(T1[,30]),labels=Lab1,exclude=NULL)
T1[,31]<-factor(T1[,31], levels= unique(T1[,31]),labels=Lab2,exclude=NULL)
T1[,32]<-factor(T1[,32], levels= unique(T1[,32]),labels=Lab3,exclude=NULL)
T1$Département<- as.factor(T1$Département)

T1_2[,33]<-factor(T1_2[,33], levels= unique(T1_2[,33]),labels=Lab1,exclude=NULL)
T1_2[,34]<-factor(T1_2[,34], levels= unique(T1_2[,34]),labels=Lab2,exclude=NULL)
T1_2[,35]<-factor(T1_2[,35], levels= unique(T1_2[,35]),labels=Lab3,exclude=NULL)
T1_2$Département<- as.factor(T1_2$Département)


FQuantile1_2<-function(var,quant){
  Quantile<-as_tibble(quantile(T1_2[[var]], probs=seq(0,1,1/quant),na.rm=TRUE))
  num<-which(colnames(T1_2)==var)
  A<-subset(T1_2, T1_2[,num]<=as.numeric(Quantile[2,1]) & T1_2[,num]>=as.numeric(Quantile[1,1]))
  T1_2$Tranche<<- ifelse(T1_2$Code %in% A$Code,"1","0")
  r<-c(mean(A$Voix_Le_Pen_T1),mean(A$Voix_Le_Pen_T2))
  for (i in 2:quant) {
    A<-subset(T1_2, T1_2[,num]<=as.numeric(Quantile[i+1,1]) & T1_2[,num]>as.numeric(Quantile[i,1]))
    T1_2$Tranche<<- ifelse(T1_2$Code %in% A$Code,i,T1_2$Tranche)
    r<-rbind(r,c(mean(A$Voix_Le_Pen_T1),mean(A$Voix_Le_Pen_T2)))
    rm(A)
  }
  Quantile<-Quantile[-1,]
  Quantile<-cbind(Quantile,r,seq(1:quant))
  ifelse(quant==100,colnames(Quantile)<-c("Centiles","Moyenne_Lepen",paste("Tranches_Centiles_",var,sep="")),colnames(Quantile)<-c("Déciles","Moyenne_Lepen",paste("Tranches_Déciles_",var,sep="")))
  return(Quantile)
}


FDépartement<-function(){
  nlevels<-nlevels(T1$Département)
  T1Dep<- data.frame(matrix(0, ncol = 27, nrow = nlevels))
  names(T1Dep)<-names(T1)[c(2,3,5:29)]
  for (i in 1:nlevels){
    T1Dep[i,]<- T1 %>% filter(Département==levels(T1$Département)[i]) %>% select(c(2,3,5:29))%>%  slice(1)
    T1Dep[i,2]<-levels(T1$Département)[i]
    T1Dep[i,c(3,4)]<- T1 %>% filter(Département==levels(T1$Département)[i]) %>% select(5,6) %>% colSums()
    T1Dep[i,c(5:27)]<- T1 %>% filter(Département==levels(T1$Département)[i]) %>% select(7:29) %>% colMeans(na.rm=TRUE) %>% round(1)
  }
  return(T1Dep)
}

T1Dep<-FDépartement()

rm(Lab1,Lab2,Lab3,mon_repertoire)


###################################################################################################
############################# Graph Bivariés ######################################################
###################################################################################################

M1=round(mean(T1_2$Voix_Le_Pen_T1),1)
M2=round(mean(T1_2$Voix_Le_Pen_T2),1)
summary(T1_2)
str(T1_2)

cor<-round(cor(T1_2[,-c(1:4,33:35)],method="pearson",use="complete"),2)
Cor<-tibble(row.names(cor),cor[,3])[-c(1,3,4,5,6),]
colnames(Cor)<-c("Variable","Corrélation")
Cor$Signe<-ifelse(Cor$Corrélation >0,"+","-")

#image(cor(T1_2[,-c(1:4,33:35)],use="complete")[,ncol(T1_2[,-c(1:4,33:35)]):1])
#pairs(T1_2[,-c(1:4,33:35)])

COR_T1<-ggplot(Cor,aes(y = Variable, x=Corrélation, fill=Signe))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Note : Calculée à l'échelle communale avec la nomenclature des PCS de 2003 \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes Emploi")+
  scale_x_continuous("Coefficient de corrélation de Pearson")+
  scale_y_discrete("")+
  geom_col(show.legend = FALSE)
COR_T1

#ggsave("Rprojetgraph/CorN.png", plot = COR_T1, width = 8, height = 6, dpi = 300)

cor<-round(cor(T1_2[,-c(1:4,33:35)],method="pearson",use="complete"),2)
Cor<-tibble(row.names(cor),cor[,6])[-c(1,2,3,4,6),]
colnames(Cor)<-c("Variable","Corrélation")
Cor$Signe<-ifelse(Cor$Corrélation >0,"+","-")

COR_T2<-ggplot(Cor,aes(y = Variable, x=Corrélation, fill=Signe))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Note : Calculée à l'échelle communale avec la nomenclature des PCS de 2003 \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes Emploi")+
  scale_x_continuous("Coefficient de corrélation de Pearson")+
  scale_y_discrete("")+
  geom_col(show.legend = FALSE)
COR_T2

rm(cor,Cor,COR_T1,COR_T2)


####################### Diplomés_supérieur_non_scolarisés #########################################
###################################################################################################

Quantile<-FQuantile1_2("Diplomés_supérieur_non_scolarisés",100)

Gdip<-ggplot(Quantile, aes(x = Quantile[,4])) +
  geom_point(aes(y = Quantile[,2], color = "Blue Points")) +
  geom_point(aes(y = Quantile[,3], color = "Red Points")) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("1er Tour", "2ème Tour")) +
  labs(caption="Note : Population des 15 ans et plus non scolarisée ayant obtenu un diplôme type universitaire de 1er cycle ou plus\nMéthode : Régression loess \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022",
       color = "Couleur :") +
  theme_bw()+theme(plot.caption = element_text(hjust = 0, face = "italic"))+
  scale_x_continuous("Centiles des parts des diplomés du supérieur", breaks = seq(0, 100, by = 10)) +
  scale_y_continuous("Moyenne des voix Le Pen suivant le Tour 2022 (%)") +
  geom_smooth(aes(y = Quantile[,2], color = "Blue Points"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = Quantile[,3], color = "Red Points"), method = "loess", se = FALSE) +
  geom_hline(yintercept = M1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = M2, linetype = "dashed", color = "black") +
  geom_text(x = 20, y = M1 - 1, label = "Moyenne nationale Tour 1", fontface = 3,color="blue")+
  geom_text(x = 20, y = M2 - 1, label = "Moyenne nationale Tour 2", fontface = 3,color="red")
Gdip

#ggsave("Rprojetgraph/Gdip.png", plot = Gdip, width = 8, height = 6, dpi = 300)


Q9<- T1 %>% filter(Tranche>90 | Tranche==100) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

Bdip<-ggplot(Q9,aes(x =Département, y = n, fill=Région))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant les départements comportant le plus de communes \n ayant un fort taux de diplomé du supérieurs",
    caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
    fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
Bdip

cor(T1$Diplomés_supérieur_non_scolarisés,T1$Voix_Le_Pen) #-0.49
B<-T1 %>% filter((Tranche>90 | Tranche==100) & Région %in% c("Île-de-France","Auvergne-Rhône-Alpes","Occitanie"))
cor(B$Voix_Le_Pen,B$Diplomés_supérieur_non_scolarisés) #-0.49


################### Cadres_professions_intellectuelles_supérieures ################################
###################################################################################################

CPIS<-ggplot(T1_2,aes(x = Cadres_professions_intellectuelles_supérieures , y =Voix_Le_Pen_T1, color=Diplomés_supérieur_non_scolarisés))+
  theme_bw()+theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                   legend.title =element_text(hjust = 0.5),plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(subtitle = "Par commune",
       caption="Note : Les communes ayant un chômage inférieur à 0.1 point non prises en compte \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes Emploi et de recensement / Ministère de l'intérieur 2022")+
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
  labs(caption="Note: Les centiles inférieurs au 14ème centile sont identiques et non prises en compte \nMéthode : Régression loess \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022",
       color = "Couleur :") + theme_bw() +theme(plot.caption = element_text(hjust = 0, face = "italic"))+
  scale_x_continuous("Centiles des parts des parts des cadres et professions intellectuelles supérieures",limits=c(15,100),breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen suivant le Tour 2022 (%)")+
  geom_smooth(aes(y = Quantile[,2], color = "Blue Points"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = Quantile[,3], color = "Red Points"), method = "loess", se = FALSE) +
  geom_hline(yintercept = M1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = M2, linetype = "dashed", color = "black") +
  geom_text(x = 27, y = M1 - 1, label = "Moyenne nationale Tour 1", fontface = 3,color="blue")+
  geom_text(x = 27, y = M2 - 1, label = "Moyenne nationale Tour 2", fontface = 3,color="red")
GCPIS

#ggsave("Rprojetgraph/GCPIS.png", plot = GCPIS, width = 8, height = 6, dpi = 300)

Q9<-T1 %>% filter(Tranche>90 | Tranche==100) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

BCPIS<-ggplot(Q9,aes(x =Département, y = n, fill=Région))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant les départements comportant le plus de communes \n ayant un fort taux de cadres et professions intellectuelles supérieures",
    caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
    fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
BCPIS

cor(T1$Voix_Le_Pen,T1$Cadres_professions_intellectuelles_supérieures,use="complete")#-0.26
B<-T1 %>% filter((Tranche>90 | Tranche==100) & Région %in% c("Île-de-France"))
cor(B$Voix_Le_Pen,B$Cadres_professions_intellectuelles_supérieures) #-0.58
B<-T1 %>% filter((Tranche>90 | Tranche==100)& Région %in% c("Île-de-France","Auvergne-Rhône-Alpes","Hauts-de-France","Occitanie"))
cor(B$Voix_Le_Pen,B$Cadres_professions_intellectuelles_supérieures) #-0.34


##################################### Ouvriers ####################################################
###################################################################################################

Quantile<-FQuantile1_2("Ouvriers",100)

GOU<-ggplot(Quantile, aes(x = Quantile[,4])) +
  geom_point(aes(y = Quantile[,2], color = "Blue Points")) +
  geom_point(aes(y = Quantile[,3], color = "Red Points")) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("1er Tour", "2ème Tour")) +
  labs(caption="Méthode : Régression linéaire \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022",color = "Couleur :") + theme_bw() +theme(plot.caption = element_text(hjust = 0, face = "italic"))+
  scale_x_continuous("Centiles des parts des ouvriers",breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen suivant le Tour 2022 (%)")+
  geom_smooth(aes(y = Quantile[,2], color = "Blue Points"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = Quantile[,3], color = "Red Points"), method = "lm", se = FALSE) +
  geom_hline(yintercept = M1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = M2, linetype = "dashed", color = "black") +
  geom_text(x = 20, y = M1 + 1, label = "Moyenne nationale Tour 1", fontface = 3,color="blue")+
  geom_text(x = 20, y = M2 + 1, label = "Moyenne nationale Tour 2", fontface = 3,color="red")
GOU

#ggsave("Rprojetgraph/GOU.png", plot = GOU, width = 8, height = 6, dpi = 300)

summary(lm(Moyenne_Lepen ~ Tranches_Centiles_Ouvriers, data=Quantile)) #R^2=0.97

Q9<- T1 %>% filter(Tranche>90 | Tranche==100) %>% count(Département,Région) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

BOU<-ggplot(Q9,aes(x =Département, y =n, fill=Région))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant les départements comportant le plus de communes \n ayant un fort taux d'ouvriers",
    caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
    fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
BOU

cor(T1$Voix_Le_Pen,T1$Ouvriers,use="complete")#0.38
B<-T1 %>% filter((Tranche>90 | Tranche==100) & Région %in% c("Bourgogne-Franche-Comté","Grand Est","Hauts-de-France"))
cor(B$Voix_Le_Pen,B$Ouvriers)#0

T1$Tranche<-NULL
T1_2$Tranche<-NULL

rm(Q9,B,Gdip,Bdip,CPIS,GCPIS,BCPIS,GOU,BOU)


###################################################################################################
############################ Analyse Multi-variée #################################################
###################################################################################################

I<-T1[,c(2,3,4,5,7,13,15,16,17,22,21,9,24)]
I<-I[complete.cases(I), ]

summary(I)
cor<-round(cor(I[,-c(1,2,3)],method="pearson",use="complete"),2)
apply(I[,4:11], 2, function(x) sd(x,na.rm = TRUE))

#I<-imputePCA(I[,-3],quali.sup=c(1,2), quanti.sup=c(3,9))$completeObs

ACP <- PCA(I[,-3],quali.sup=c(1,2), quanti.sup=c(3,9),graph = FALSE, scale.unit = TRUE) #Dep et reg en sup
round(head(ACP$eig[,]),2)#VP

Atypique<-I[which(ACP$ind$contrib[,1]>=10*100/32000|ACP$ind$contrib[,2]>=10*100/32000),]
I<-anti_join(I,Atypique)

fviz_screeplot(ACP, addlabels = TRUE, ylim = c(0, 50),main="Diagramme en barre des pourcentages d'inertie des dimensions de l'ACP",ylab="Pourcentage de variances expliqués")

#barplot(ACP$eig[,1], main="Valeurs propres",names.arg = paste0("dim", 1:nrow(ACP$eig)))

head(summary(ACP, nb.dec = 2, nbelements = Inf))
head(ACP$ind$cos2[,1:2])#qualité représentation observation
rowSums(ACP$ind$cos2[,1:2])

head(ACP$ind$contrib[,1:2])#contribution relative d une observ a constru axe >> pi =atypique
which.max(ACP$ind$contrib[,2])
max(ACP$ind$contrib[-13323,1:2])
#Atypique<-Ext1[which(ACP$ind$contrib[,2]>=0.1),]

ACP$var$cos2[,1:3]
ACP$var$cor[,1:3]
ACP$var$contrib

plot.PCA(ACP, choix="var",axes = c(1,2),title="ACP des variables",invisible = "var")
plot.PCA(ACP, choix = "var", invisible = "quanti.sup", cex=0.8) #cex=taille
plot(ACP, choix="var", axes = c(1,3))

plot.PCA(ACP, choix = "ind", invisible = "ind.sup")
plot(ACP, choix="ind",habillage=2, cex=0.5,invisible="quali", select="cos2 1", unselect=1)


##################################### Atypique ####################################################
###################################################################################################

summary(Atypique)
summary(I)
apply(Atypique[,4:11], 2, function(x) sd(x,na.rm = TRUE))
sum(Atypique[,4])

aggregate(Voix_Le_Pen~Niveau_équipements_services,T1,mean)

tapply(seq_along(T1$Voix_Le_Pen),T1$Niveau_équipements_services,
       function(xx){return(weighted.mean(x=T1$Voix_Le_Pen[xx],w=T1$Inscrits[xx]))})

###################################################################################################
############################ Clustering non supervisé #############################################
###################################################################################################



######################## Classification ascendante hiérarchique ###################################
###################################################################################################


CAH<-agnes(T1Dep[,c(5,11,13,14,16,20,7,22)],metric="euclidian",method = "ward",stand=FALSE)
#Maximiser l inertie intergroupe(grande diff entre classe) et minimiser l'inertie intrag(homogénéité dans un groupe)

sortedHeight <- sort(CAH$height, decreasing = TRUE)
relHeight <- sortedHeight / sum(sortedHeight) * 100
cumHeight <- cumsum(relHeight)

#plot(sortedHeight[1:5],type = "h",xlab = "Noeuds",ylab = "Niveau d'agrégation" )

Inerclasse<-barplot(relHeight[ 1: 15], names.arg = seq(1, 15, 1),col = "black",border = "white",
                    xlab = "Noeuds", ylab = "Part de l'inertie totale (%)")

ggsave("Rprojetgraph/Inerclasse.png", plot = Inerclasse, width = 8, height = 6, dpi = 300)

#barplot(cumHeight[ 1: 15], names.arg = seq(1, 15, 1), col = "black", border = "white",xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)")
#plot(CAH, which.plot= 1)
#plot(CAH, which.plot = 2, main = "Dendrogramme de la classification hiérarchique ascendante",xlab="Départements",ylab = "Dissimilarité",labels=FALSE,hang=-1)
Dendo<-fviz_dend(CAH,k=3, show_labels = FALSE, rect = FALSE, main="Dendrogramme de la classification hiérarchique ascendante",
                 xlab="Départements",ylab = "Dissimilarité",)

#plot(T1Dep[,c(5,11,13,14,16,20,7,22)], col = c("red", "green", "blue")[cutree(CAH, k = 3)],main = "Clusters obtenus avec la méthode CAH")
fviz_cluster(list(data = T1Dep[,c(5,11,13,14,16,20,7,22)], cluster = cutree(CAH, k = 3)),
             as.hclust(CAH), stand = TRUE,ellipse.type = "convex",main=""
             #,xlab="",ylab=""
)

silhouette_index <- silhouette(cutree(CAH, k = 3), dist(T1Dep[,c(5,11,13,14,16,20,7,22)]))
#fviz_silhouette(silhouette_index)
plot(silhouette_index, col = c("red", "green", "blue"))

T1Dep$GroupeACH <- factor(cutree(CAH, k = 3),levels = 1:3,labels = paste("Groupe", 1: 3))

clusProfile <- aggregate(T1Dep[, c(5,11,13,14,16,20,19,7,22)],by = list(T1Dep$GroupeACH),mean)
colnames(clusProfile)[ 1] <- "Groupe"
clusLong <- pivot_longer(clusProfile, cols = -Groupe, names_to = "Variable", values_to = "Valeur")

MACH<-ggplot(clusLong) +geom_bar(aes(x = Variable, y = Valeur, fill = Groupe),stat = "identity") +
  facet_wrap(~ Groupe) +coord_flip() + theme_bw() + guides(fill = FALSE) + xlab("") +
  ylab("Moyenne exprimée en pourcentages")+theme(plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Champ : France métropolitaine \nSource : Insee 2018 / Ministère de l'intérieur 2022")
MACH

#ggsave("Rprojetgraph/MoyACH.png", plot = MACH, width = 8, height = 6, dpi = 300)

rm(sortedHeight,relHeight,cumHeight,silhouette_index ,clusProfile,clusLong,MACH)


#################################### Kmeans #######################################################
###################################################################################################

dataCluster<-scale(T1Dep[,c(5,11,13,14,16,20,7,22)])
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

T1Dep$GroupeKmeans <-paste("Groupe",Kmeans[[3]]$cluster)

clusProfile <- aggregate(T1Dep[, c(5,11,13,14,16,20,19,7,22)],by = list(T1Dep$GroupeKmeans),mean)
colnames(clusProfile)[ 1] <- "Groupe"
clusLong <- pivot_longer(clusProfile, cols = -Groupe, names_to = "Variable", values_to = "Valeur")

MACH<-ggplot(clusLong) +geom_bar(aes(x = Variable, y = Valeur, fill = Groupe),stat = "identity") +
  facet_wrap(~ Groupe) +coord_flip() + theme_bw() + guides(fill = FALSE) + xlab("") +
  ylab("Moyenne exprimée en pourcentages")+theme(plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(caption="Champ : France métropolitaine \nSource : Insee 2018 / Ministère de l'intérieur 2022")+
  scale_fill_manual(values = c("red","green","blue"))
MACH

rm(silhouette_index,clusProfile,clusLong,MACH,Kmeans,CPtheta)


###################################################################################################
######################################### Carte ###################################################
###################################################################################################


France <- readRDS("gadm36_FRA_2_sf.rds")
France <- left_join(France, T1Dep, by = c("NAME_2" = "Département"))  %>%st_set_crs(st_crs(4326))


France_ACH <- ggplot(France) +
  geom_sf(aes(fill = GroupeACH), show.legend = TRUE, color = "white", size = 0.2) +
  # enlever l'affichage des coordonnés et de la grille
  coord_sf(datum = NA, expand = FALSE) +
  theme(panel.background = element_blank())
France_ACH

#ggsave("Rprojetgraph/France_ACH.png", plot = France_ACH , width = 8, height = 6, dpi = 300)

France_Kmeans <- ggplot(France) +
  geom_sf(aes(fill = GroupeKmeans), show.legend = TRUE, color = "white", size = 0.2) +
  # enlever l'affichage des coordonnés et de la grille
  coord_sf(datum = NA, expand = FALSE) +
  ggtitle("Carte des départements français selon les groupes de l'ACH") +
  theme(panel.background = element_blank())
France_Kmeans

France_Le_Pen <- ggplot(France) +
  geom_sf(aes(fill = Voix_Le_Pen), show.legend = TRUE, color = "white", size = 0.2) +
  # enlever l'affichage des coordonnées et de la grille
  coord_sf(datum = NA, expand = FALSE) +
  scale_fill_viridis_c() +
  ggtitle("Carte des départements français selon le vote Le Pen") +
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))
France_Le_Pen




aggregate(Voix_Le_Pen_T1~Niveau_équipements_services,T1_2,mean)

tapply(seq_along(T1_2$Voix_Le_Pen_T1),T1_2$Niveau_équipements_services,
       function(xx){return(weighted.mean(x=T1_2$Voix_Le_Pen_T1[xx],w=T1_2$Inscrits_T1[xx]))})


CAH<-agnes(T1Dep[,c(5,11,13,14,16,20,7,22)],metric="euclidian",method = "ward",stand=FALSE)

#I<-T1[,c(2,3,4,5,7,13,15,16,17,22,21,9,24)]
#I<-imputePCA(I[,-3],quali.sup=c(1,2), quanti.sup=c(3,9))$completeObs

FDépartement<-function(){
  nlevels<-nlevels(T1$Département)
  T1Dep<- data.frame(matrix(0, ncol = 27, nrow = nlevels))
  names(T1Dep)<-names(T1)[c(2,3,5:29)]
  for (i in 1:nlevels){
    T1Dep[i,]<- T1 %>% filter(Département==levels(T1$Département)[i]) %>% select(c(2,3,5:29))%>%  slice(1)
    T1Dep[i,2]<-levels(T1$Département)[i]
    T1Dep[i,c(3,4)]<- T1 %>% filter(Département==levels(T1$Département)[i]) %>% select(5,6) %>% colSums()
    T1Dep[i,c(5:27)]<- T1 %>% filter(Département==levels(T1$Département)[i]) %>% select(7:29) %>% colMeans(na.rm=TRUE) %>% round(1)
  }
  return(T1Dep)
}



#####On sait jamais

dr<-read.csv2("departements-france.csv", sep=",")

#caf<-read.csv2("CAF.csv", sep=";")
#colnames(caf)<-c("code_commune","","Caf")
#caf<-caf[-c(1,2),-2]
#caf<-caf[-which(caf$Caf=="N/A"),]

#rev<-read.csv2("Medrevdispo.csv", sep=";")
#colnames(rev)<-c("code_commune","","Revdispo")
#rev<-rev[-c(1,2),-2]
#rev<-rev[-which(rev$Revdispo=="N/A"),]

Ext1<-read_sas("ext1.sas7bdat")
Ext1$Code_de_la_commune<-paste(Ext1$Departement,Ext1$Code_de_la_commune,sep="")
Ext1<-Ext1[-which(is.na(Ext1),arr.ind=TRUE)[,1],]
Ext1<-Ext1[Ext1$Departement %in% c("20A","20B","01","02","03","04","05","06","07","08","09",seq(99)),] #-DOM TOM
Ext1<-inner_join(Ext1,dr,join_by(Departement==code_departement))

sum(duplicated(Ext1$Code_de_la_commune))
#Ext1[which(duplicated(Ext1$Code_de_la_commune)),c(1,2,3)]
#Ext1ac<-Ext1[,c(9:20,40,42)]
#Ext1ac$n <- colnames(Ext1ac[,1:12])[apply(Ext1ac[,1:12], 1, which.max)]
#Ext1ac<-Ext1ac[,13:15]
#Ext1ac$n<-as.character(Ext1ac$n)
#Ext1ac$nom_region<-as.character(Ext1ac$nom_region)
Votant<-Ext1[,c(2,3,21)]
Ext1<-Ext1[,-c(1,4:21,31,33,34,35,39,41,50)]
Ext1<-Ext1[,c(1,2,24,25,3:11,13:15,23,16,19:22,17,18,12)]
names(Ext1)[19:22]<-c("45_59","65+","30_44","18_24")
#Ext1$Equip<-as.numeric(Ext1$Equip)

#Ext1R<-inner_join(Ext1,rev,join_by(Code_de_la_commune==code_commune))
#Ext1R$Revdispo<-as.numeric(Ext1R$Revdispo)

rm(dr,mon_repertoire)

##Options

#getwd()
#install.packages("")
#require()
#read.sas7bdat("ext1.sas7bdat")


####Graph Bivariés

M=mean(Ext1$Vlepen)
summary(Ext1)
str(Ext1)
cor<-round(cor(Ext1[,-c(1:4)],method="pearson"),2)
image(cor(Ext1[,-c(1:4)])[,ncol(Ext1[,-c(1:4)]):1])
Cor<-tibble(row.names(cor),cor[,1])[-c(1,11),]
colnames(Cor)<-c("Var","Corrélation")
Cor[9,1]<-"Dipsup"
pairs(Ext1[,-c(1:4)])

COR<-ggplot(Cor,aes(y = Var, x=Corrélation))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant les corrélations des % de voix Le Pen au 1er Tour de 2022 \n avec plusieurs variables socioéconomiques",
    caption="Note : Calculée à l'échelle communale avec la nomenclature des PCS de 2003
       \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018
       \nSource : Insee 2018, enquêtes Emploi")+
  scale_x_continuous("Coefficient de corrélation de Pearson")+
  scale_y_discrete("Variables socioéconomiques")+
  geom_col()
COR

FQuantile<-function(var,quant){
  Quantile<-as_tibble(quantile(Ext1[[var]], probs=seq(0,1,1/quant)))
  num<-which(colnames(Ext1)==var)
  A<-subset(Ext1, Ext1[,num]<=as.numeric(Quantile[2,1]) & Ext1[,num]>=as.numeric(Quantile[1,1]))
  Ext1$box<<- ifelse(Ext1$Code_de_la_commune %in% A$Code_de_la_commune,"1","0")
  r<-mean(A$Vlepen)
  for (i in 2:quant) {
    A<-subset(Ext1, Ext1[,num]<=as.numeric(Quantile[i+1,1]) & Ext1[,num]>as.numeric(Quantile[i,1]))
    Ext1$box<<- ifelse(Ext1$Code_de_la_commune %in% A$Code_de_la_commune,i,Ext1$box)
    r<-rbind(r,mean(A$Vlepen))
    rm(A)
  }
  Quantile<-Quantile[-1,]
  Quantile<-cbind(Quantile,r,seq(1:quant))
  ifelse(quant==100,colnames(Quantile)<-c("Centiles","Moy_Lepen",paste("Tranches_Centiles_",var,sep="")),colnames(Quantile)<-c("Déciles","Moy_Lepen",paste("Tranches_Déciles_",var,sep="")))
  return(Quantile)
}

##Diplôme supérieur

Quantile<-FQuantile("dipsup",100)

Tdip<-ggplot(Quantile,aes(x = Quantile[,3], y =Quantile[,2]))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant la moyenne des % de voix Le Pen au 1er Tour 2022 \n calculée selon le centile auquel appartient la commune",
    caption="Note : Population des 15 ans et plus non scolarisée ayant obtenu un diplôme type universitaire de 1er cycle ou plus \nMéthode : Régression loess \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022")+
  scale_x_continuous("Centiles des parts des diplomés du supérieur",breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen au 1er Tour 2022 (%)")+
  geom_point(size=1.5)+geom_smooth(method="loess",color="red")+
  geom_hline(yintercept=M, linetype="dashed", color = "blue")+
  geom_text(x=90, y=M+1, label="Moyenne nationale",fontface=3)
Tdip

Q9<- Ext1 %>% filter(box>90 | box==100) %>% count(nom_departement,nom_region) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

Bdip<-ggplot(Q9,aes(x =nom_departement, y = n, fill=nom_region))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant les départements comportant le plus de communes \n ayant un fort taux de diplomé du supérieurs",
    caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
    fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
Bdip

cor(Ext1$dipsup,Ext1$Vlepen) #-0.50
B<-Ext1 %>% filter((box>90 | box==100) & nom_region %in% c("Île-de-France","Auvergne-Rhône-Alpes","Occitanie"))
cor(B$Vlepen,B$dipsup) #-0.48


##Cadres et professions intellectuelles supérieures

CPIS<-ggplot(Ext1,aes(x = CA_PIS , y =Vlepen, color=dipsup))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title =element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs( #title = "Graphique montrant les % de voix Le Pen au 1er Tour 2022 \n en fonction de leur part de cadres et de PIS.",
    subtitle = "Par commune",
    caption="Note : Les communes ayant un chômage inférieur à 0.1 point non prises en compte \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes Emploi et de recensement / Ministère de l'intérieur 2022")+
  scale_color_viridis_c("Taux de diplomés \n de l'enseigment supérieur", option = "plasma")+
  scale_x_continuous("Part des cadres et professions intellectuelles supérieures (%)", limits = c(0.1,40))+
  scale_y_continuous("Voix Le Pen au 1er Tour 2022 (%)", limits = c(0,70))+
  geom_point()
CPIS

cor(Ext1$dipsup,Ext1$CA_PIS) #0.6

Quantile<-FQuantile("CA_PIS",100)

TCPIS<-ggplot(Quantile,aes(x = Quantile[,3], y =Quantile[,2]))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant la moyenne des % de voix Le Pen au 1er Tour 2022 \n calculée selon le centile auquel appartient la commune",
    caption="Note: Les centiles inférieurs au 14ème centile sont identiques et non prises en compte \nMéthode : Régression loess \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022")+
  scale_x_continuous("Centiles des parts des parts des cadres et professions intellectuelles supérieures",limits=c(15,100),breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen au 1er Tour 2022 (%)")+
  geom_point(size=1.5)+geom_smooth(color="red")+
  geom_hline(yintercept=M, linetype="dashed", color = "blue")+
  geom_text(x=90, y=M+1, label="Moyenne nationale",fontface=3)
TCPIS

Q9<- Ext1 %>% filter(box>90 | box==100) %>% count(nom_departement,nom_region) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

BCPIS<-ggplot(Q9,aes(x =nom_departement, y = n, fill=nom_region))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant les départements comportant le plus de communes \n ayant un fort taux de cadres et professions intellectuelles supérieures",
    caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
    fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
BCPIS

cor(Ext1$Vlepen,Ext1$CA_PIS)#-0.26
B<-Ext1 %>% filter((box>90 | box==100) & nom_region %in% c("Île-de-France"))
cor(B$Vlepen,B$CA_PIS) #-0.57
B<-Ext1 %>% filter((box>90 | box==100)& nom_region %in% c("Île-de-France","Auvergne-Rhône-Alpes","Hauts-de-France","Occitanie"))
cor(B$Vlepen,B$CA_PIS) #-0.34


##Ouvriers


Quantile<-FQuantile("OU",100)

TOU<-ggplot(Quantile,aes(x = Quantile[,3], y =Quantile[,2]))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant la moyenne des % de voix Le Pen au 1er Tour 2022 \n calculée selon le centile auquel appartient la commune",
    caption="Méthode : Régression linéaire \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022")+
  scale_x_continuous("Centiles des parts des ouvriers",breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen au 1er Tour 2022 (%)")+
  geom_point(size=1.5)+geom_smooth(method = "lm",color="red")+
  geom_hline(yintercept=M, linetype="dashed", color = "blue")+
  geom_text(x=90, y=M+1, label="Moyenne nationale",fontface=3)
TOU

summary(lm(Moy_Lepen ~ Tranches_Centiles_Ouvriers, data=Quantile)) #R^2=0.97

Q9<- Ext1 %>% filter(box>90 | box==100) %>% count(nom_departement,nom_region) %>% filter(n >=quantile(n,probs=0.9)) %>% arrange(desc(n))

BOU<-ggplot(Q9,aes(x =nom_departement, y =n, fill=nom_region))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  labs(#title = "Graphique montrant les départements comportant le plus de communes \n ayant un fort taux d'ouvriers",
    caption="Champ : France métropolitaine \nSource : Insee 2018, enquêtes de recensement",
    fill="Régions")+
  scale_x_discrete("Départements")+
  scale_y_continuous("Nombre de communes appartenant au 9ème décile")+
  geom_col(width = .7)
BOU

cor(Ext1$Vlepen,Ext1$OU)#0.37
B<-Ext1 %>% filter((box>90 | box==100) & nom_region %in% c("Bourgogne-Franche-Comté","Grand Est","Hauts-de-France"))
cor(B$Vlepen,B$OU)#0


##Bonus : Revenu dispo

Quantile<-FQuantile("Revdispo",10,Ext1R)

ggplot(T,aes(x = Quantile[,3], y =Quantile[,2])) + geom_point(size=2)+geom_smooth(color="red")
Test<- Ext1R %>% filter(box==10) %>% group_by(Departement) %>% summarise(Ef=n()) %>% filter(Ef >=quantile(Ef,probs=0.9)) %>% arrange(desc(Ef))
Test <-inner_join(Test,dere,join_by("Departement"=="code_departement"))
ggplot(Test) + geom_col(aes(x =nom_departement, y = Ef, fill=nom_region),width = .7)
B<-Ext1R %>% inner_join(dere,join_by("Departement"=="code_departement")) %>% filter(box==10 & nom_region %in% c("Île-de-France","Grand Est"))
cor(B$Vlepen,B$Revdispo) #-0.43
cor(Ext1R$Revdispo,Ext1R$Vlepen) #-0.18
cor(Ext1R$dipsup,Ext1R$Revdispo) #0.43
cor(Ext1R$CA_PIS,Ext1R$Revdispo) #0.37

Ext1$box<-NULL

#### Analyse multivariée


I<-Ext1[,-c(2,6,7,8,10,16,17,18,19,20,22,23,25)]
names(I)<-c("Code_commune","Département","Région","%Le Pen","cadres/profintellectsup","ouvriers","profintermédiaires","retraités","diplomésup","non diplomé","30-44ans","Immigrés")
cor<-round(cor(I[,-c(1,2,3)],method="pearson"),2)
apply(I[,4:11], 2, sd)

#I<-Ext1[,-c(1:4)]
#ACP <- PCA(I,quanti.sup=c(2,3,4,6,11:16,18,19,21),graph = FALSE, scale.unit = TRUE)

ACP <- PCA(I[,-1],quali.sup=c(1,2), quanti.sup=9,graph = FALSE, scale.unit = TRUE) #Dep et reg en sup
round(head(ACP$eig[,]),2)#VP

Atypique<-I[which(ACP$ind$contrib[,1]>=10*100/32000|ACP$ind$contrib[,2]>=10*100/32000),]
I<-anti_join(I,Atypique)

fviz_screeplot(ACP, addlabels = TRUE, ylim = c(0, 50),main="Diagramme en barre des pourcentages d'inertie des dimensions de l'ACP",ylab="Pourcentage de variances expliqués")

#barplot(ACP$eig[,1], main="Valeurs propres",names.arg = paste0("dim", 1:nrow(ACP$eig)))

head(summary(ACP, nb.dec = 2, nbelements = Inf))
head(ACP$ind$cos2[,1:2])#qualité représentation observation
rowSums(ACP$ind$cos2[,1:2])

head(ACP$ind$contrib[,1:2])#contribution relative d une observ a constru axe >> pi =atypique
which.max(ACP$ind$contrib[,2])
max(ACP$ind$contrib[-13323,1:2])
#Atypique<-Ext1[which(ACP$ind$contrib[,2]>=0.1),]

ACP$var$cos2[,1:3]
ACP$var$cor[,1:3]
ACP$var$contrib

plot.PCA(ACP, choix="var",axes = c(1,2),title="ACP des variables",invisible = "var")
plot.PCA(ACP, choix = "var", invisible = "quanti.sup", cex=0.5) #cex=taille
plot(ACP, choix="var", axes = c(1,3))

plot.PCA(ACP, choix = "ind", invisible = "ind.sup")
plot(ACP, choix="ind",habillage=2, cex=0.5,invisible="quali", select="cos2 1", unselect=1)

###Analyse Atypique

Atypique<-inner_join(Atypique,Votant, by=join_by(Code_commune==Code_de_la_commune))
which(duplicated(Votant$Code_de_la_commune))
summary(Atypique)
summary(I)
sd(I$Vlepen)
apply(I[,3:11], 2, sd)
apply(Atypique[,13],2,sum)

####Test

tabconting<-Ext1ac[,c(2,3)] %>% spread(nom_region,n)

tabconting<-table(Ext1ac$nom_departement , Ext1ac$n)
print(tabconting)
str(tabconting)
str(Ext1ac)
AFC <- CA(tabconting)
dim(donnees)
rowSums(tabconting)
colSums(tabconting)
sum(tabconting)

by(Ext1atyp[,Ext1atyp$Vlepen], INDICES = Ext1atyp$Departement, FUN=summary) # A FINIR STAT DES
Ext2atyp<- Ext2c[Ext2c$Vlepen>quantile(Ext2c$Vlepen, prob=.99),]
#barplot département ou il a y a le plus de communes avec des % Lepen élevés
Ext1atyp<- Ext1[Ext1$Vlepen>quantile(Ext1$Vlepen, prob=.99),]
as_tibble(quantile(Ext1[["OU"]], probs=seq(0,1,0.01)))
t<-Ext1[,"OU"]
t<-Ext1$OU
t<-Ext1[,2:30]
x<-100
1/x

Quantile<-as_tibble(quantile(Ext1[["OU"]], probs=seq(0,1,0.01)))
which(colnames(Ext1)=="dipsup")
A<-Ext1[Ext1[["OU"]]<=as.numeric(Quantile[2,1])&Ext1[["OU"]]==as.numeric(Quantile[1,1])]
A<-subset(Ext1, Ext1[,11]<=as.numeric(Quantile[2,1]) & Ext1[,11]>=as.numeric(Quantile[1,1]))
Ext1$box<- ifelse(Ext1$Code_de_la_commune %in% A$Code_de_la_commune,"1","0")
r<-mean(A$Vlepen)
anyNA(t)
is.na(t)
test<-!is.na(t)
anyNA(test)
summary(test)
a<-t[test]
chisq.test(a)
anyNA(test)
str(Ext1ac)

CPIS<-ggplot(Ext1,aes(x = CA_PIS , y =Vlepen, color=dipsup))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title =element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  scale_color_viridis_c("Taux de diplomés \n de l'enseigment supérieur", option = "plasma")+
  scale_x_continuous("Part des cadres et professions intellectuelles supérieures (%)", limits = c(0.1,40))+
  scale_y_continuous("Voix Le Pen au 1er Tour 2022 (%)", limits = c(0,70))+
  geom_point() +  geom_density2d(bins = 15,color = "black")
CPIS

boxplot(Ext1$Vlepen)
plot(Ext1$OU,Ext1$Vlepen)
ggplot(Ext1) + geom_bin2d(aes(x = dipsup , y =Vlepen))
ggplot(Ext1) +
  geom_point(aes(x = CA_PIS , y =Vlepen, color=dipsup))+
  scale_color_viridis_c("Taux de diplomés de l'enseignement supérieur", option = "plasma")+
  scale_x_continuous("Part des chefs d'entreprises et professions intellectuelles supérieures (%)", limits = c(0,50))
ggplot(Ext1R) +
  geom_point(aes(x = Revdispo , y =Vlepen, color=dipsup))+
  scale_color_viridis_c("Taux d'ouvriers", option = "plasma")+
  scale_x_continuous("Part des non diplômés du supérieur (%)")
ggplot(Ext1) + geom_boxplot(aes(x = box, y = Vlepen))
ggplot(Quantile,aes(x = Tranches_Centiles_Ouvriers, y =Moy_Lepen)) + geom_line()

Ext1 %>% group_by(box) %>% summarise(mean(Vlepen))
cor(Ext1RS$Vlepen,Ext1RS$CAF)
barplot(ACP$eig[,2],main="Valeurs propres",names=paste("Dim",1:nrow(ACP$eig)))

Quantile<-as_tibble(quantile(Ext1$dipsup, probs=seq(0,1,0.01)))
A<-subset(Ext1, dipsup<=as.numeric(Quantile[2,1]) & dipsup>=as.numeric(Quantile[1,1]))
Ext1$box<- ifelse(Ext1$Code_de_la_commune %in% A$Code_de_la_commune,"1","0")
r<-mean(A$Vlepen)
for (i in 2:100) {
  A<-subset(Ext1, dipsup<=as.numeric(Quantile[i+1,1]) & dipsup>as.numeric(Quantile[i,1]))
  Ext1$box<- ifelse(Ext1$Code_de_la_commune %in% A$Code_de_la_commune,i,Ext1$box)
  r<-rbind(r,mean(A$Vlepen))
  rm(A)
}
Quantile<-Quantile[-1,]
Quantile<-cbind(Quantile,r,seq(1:100))
rm(r,i)
colnames(Quantile)<-c("Centiles","Moy_Lepen","Tranches_Centiles_Dipsup")

A<-subset(Ext1, Ext1[,14]<=as.numeric(Quantile[3,1]) & Ext1[,14]>as.numeric(Quantile[2,1]))
B<-subset(Ext1, dipsup<=as.numeric(Quantile[3,1]) & dipsup>as.numeric(Quantile[2,1]))
mean(A$Vlepen)
mean(B$Vlepen)
A[!A==B]

dataCluster<-scale(T1[,c(5,11,13,14,16,20,7,22)])
dataCluster<-dataCluster[complete.cases(dataCluster),]
Kmeans<-list()
CPtheta<-rep(0,6)
for (K in 1:6){
  Kmeans[[K]]<- kmeans(dataCluster,K)
  CPtheta[K]<-Kmeans[[K]]$tot.withinss
}

plot(1:6, CPtheta, type="b", xlab="K", ylab="inertie-intra")



###


FQuantile<-function(var,quant){
  Quantile<-as_tibble(quantile(T1[[var]], probs=seq(0,1,1/quant),na.rm=TRUE))
  num<-which(colnames(T1)==var)
  A<-subset(T1, T1[,num]<=as.numeric(Quantile[2,1]) & T1[,num]>=as.numeric(Quantile[1,1]))
  T1$Tranche<<- ifelse(T1$Code %in% A$Code,"1","0")
  r<-mean(A$Voix_Le_Pen)
  for (i in 2:quant) {
    A<-subset(T1, T1[,num]<=as.numeric(Quantile[i+1,1]) & T1[,num]>as.numeric(Quantile[i,1]))
    T1$Tranche<<- ifelse(T1$Code %in% A$Code,i,T1$Tranche)
    r<-rbind(r,mean(A$Voix_Le_Pen))
    rm(A)
  }
  Quantile<-Quantile[-1,]
  Quantile<-cbind(Quantile,r,seq(1:quant))
  ifelse(quant==100,colnames(Quantile)<-c("Centiles","Moyenne_Lepen",paste("Tranches_Centiles_",var,sep="")),colnames(Quantile)<-c("Déciles","Moyenne_Lepen",paste("Tranches_Déciles_",var,sep="")))
  return(Quantile)
}

cor<-round(cor(T1[,-c(1:4,30:32)],method="pearson",use="complete"),2)
#image(cor(T1[,-c(1:4,30:32)],use="complete")[,ncol(T1[,-c(1:4,30:32)]):1])
Cor<-tibble(row.names(cor),cor[,3])[-c(1,3),]
colnames(Cor)<-c("Variable","Corrélation")
Cor$Signe<-ifelse(Cor$Corrélation >0,"+","-")
#pairs(T1[,-c(1:4,32:34)])

Quantile<-FQuantile("Diplomés_supérieur_non_scolarisés",100)

Tdip<-ggplot(Quantile,aes(x = Quantile[,3], y =Quantile[,2]))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  #labs(#title = "Graphique montrant la moyenne des % de voix Le Pen au 1er Tour 2022 \n calculée selon le centile auquel appartient la commune",
  #  caption="Note : Population des 15 ans et plus non scolarisée ayant obtenu un diplôme type universitaire de 1er cycle ou plus \nMéthode : Régression loess \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022")+
  scale_x_continuous("Centiles des parts des diplomés du supérieur",breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen au 1er Tour 2022 (%)")+
  geom_point(size=1.5)+geom_smooth(method="loess",color="red")+
  geom_hline(yintercept=M, linetype="dashed", color = "blue")+
  geom_text(x=90, y=M+1, label="Moyenne nationale",fontface=3)
Tdip

Quantile<-FQuantile("Cadres_professions_intellectuelles_supérieures",100)

TCPIS<-ggplot(Quantile,aes(x = Quantile[,3], y =Quantile[,2]))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  #labs(#title = "Graphique montrant la moyenne des % de voix Le Pen au 1er Tour 2022 \n calculée selon le centile auquel appartient la commune",
  #    caption="Note: Les centiles inférieurs au 14ème centile sont identiques et non prises en compte \nMéthode : Régression loess \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022")+
  scale_x_continuous("Centiles des parts des parts des cadres et professions intellectuelles supérieures",limits=c(15,100),breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen au 1er Tour 2022 (%)")+
  geom_point(size=1.5)+geom_smooth(color="red")+
  geom_hline(yintercept=M, linetype="dashed", color = "blue")+
  geom_text(x=90, y=M+1, label="Moyenne nationale",fontface=3)
TCPIS


Quantile<-FQuantile("Ouvriers",100)

TOU<-ggplot(Quantile,aes(x = Quantile[,3], y =Quantile[,2]))+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic"))+
  #labs(#title = "Graphique montrant la moyenne des % de voix Le Pen au 1er Tour 2022 \n calculée selon le centile auquel appartient la commune",
  #    caption="Méthode : Régression linéaire \nChamp : France métropolitaine, limite territoriales des communes au 1er janvier 2018 \nSource : Insee 2018, enquêtes de recensement / Ministère de l'intérieur 2022")+
  scale_x_continuous("Centiles des parts des ouvriers",breaks=seq(0,100,by=10))+
  scale_y_continuous("Moyenne des voix Le Pen au 1er Tour 2022 (%)")+
  geom_point(size=1.5)+geom_smooth(method = "lm",color="red")+
  geom_hline(yintercept=M, linetype="dashed", color = "blue")+
  geom_text(x=90, y=M+1, label="Moyenne nationale",fontface=3)
TOU



Luck_plz<-function(nb_var,k=3){
  A<-data.frame()
  for (i in 1:1000){
    random_int <- sample(c(7:28,30), nb_var)
    X<-D1[,c(6,random_int)]  %>% filter(D1$Région != "Île-de-France")
    CAH<-agnes(X,metric="euclidian",method = "ward",stand=TRUE)
    silhouette_index <- silhouette(cutree(CAH, k), dist(scale(X)))
    ifelse(mean(silhouette_index[, 3])>0.35 & table(cutree(CAH, k))[1]< 70 & table(cutree(CAH, k))[2]< 70,
           A<-rbind(A,c(table(cutree(CAH, k)),mean(silhouette_index[, 3]),random_int)),NA)
  }
  return(A)
}
AH<-Luck_plz(10,2)
AH<-Luck_plz(9,2)
AH<-Luck_plz(8,2)
AH<-Luck_plz(7,2)
AH<-Luck_plz(6,2)
AH<-Luck_plz(5,2)
AH<-Luck_plz(4,2)

X<-D1[,c(6,9)]  %>% filter(D1$Région != "Île-de-France")
any(duplicated(AH[,1]))
D1$LP_Z <- rowSums(D1[,c(6,26)])
D1$Abstentions_Exprimés<- 100*D1$Abstentions/D1$Exprimés
str(X)
X<-D1[,c(6,21,14,16,31)] #0.46

X<-D1[,c(6,30,18,20)] #6 28

#16 7 12 15 9
#20 22 7 12
#X<-D1[,c(6,16,23,12,7,22,17,15)]%>% filter(D1$Région != "Île-de-France")
#X<-D1[,c(6,16, 7, 12, 15, 9)]
#X<-D1[,c(6,20, 22, 7 ,12)]
#X<-D1[,c(6,16, 7, 12, 15, 9,24)] %>% filter(D1$Région != "Île-de-France")
X<-D1[,c(6,7,21,16,24,15)]
#X<-D1[,c(6,unlist(AH[16,-c(1:3)]))]%>% filter(D1$Région != "Île-de-France")

#D1mod<-D1%>% filter(D1$Région != "Île-de-France")
#D1mod$GroupeACH<-factor(cutree(CAH, k = 2),levels = 1:2,labels = paste("Groupe", 1: 2))

#I<-imputePCA(I[,-3],quali.sup=c(1,2), quanti.sup=c(3,5,12))$completeObs




