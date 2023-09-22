rm(list=ls())


#####Importation et packages
library(openxlsx)
library(sas7bdat)
library(dplyr)


dr<-read.csv2("~/Desktop/ENSAI/Projet stat/R/departements-france.csv", sep=",")

T1<-read.xlsx("~/Desktop/ENSAI/Projet stat/R/1er Tour/Communes/Subcom_t1.xlsx", sep=",")
T1<-T1[,c(1:4,17,7,52,54)]
names(T1)<-c("Cdépart","Département","Code","Commune","Exprimés","Abstentions","Nb_voix_Le_Pen","Voix_Le_Pen")
T1$Code<-paste(T1$Cdépart,T1$Code,sep="")
S1<-T1[!T1$Cdépart %in% c("2A","2B","01","02","03","04","05","06","07","08","09",seq(99),'ZZ'),]
T1<-T1[T1$Cdépart %in% c("2A","2B","01","02","03","04","05","06","07","08","09",seq(99)),] #-DOM TOM
T1<-inner_join(T1,dr,join_by(Cdépart==code_departement))
T1<-T1[,c(3,11,2,4:8)]

T2<-read.xlsx("~/Desktop/ENSAI/Projet stat/R/2ème Tour/Communes/Subcom_t2.xlsx", sep=",")
T2<-T2[,c(1,3,17,7,31,33)]
names(T2)<-c("Cdépart","Code","Exprimés_T2","Abstentions_T2","Nb_voix_Le_Pen_T2","Voix_Le_Pen_T2")
T2$Code<-paste(T2$Cdépart,T2$Code,sep="")
S2<-T2[!T2$Cdépart %in% c("2A","2B","01","02","03","04","05","06","07","08","09",seq(99),'ZZ'),]
T2<-T2[T2$Cdépart %in% c("2A","2B","01","02","03","04","05","06","07","08","09",seq(99)),] #-DOM TOM
S1_2<-inner_join(S1,S2,join_by(Code==Code))
T1_2<-inner_join(T1,T2,join_by(Code==Code))
S1_2<-S1_2[,-c(1,9)]
S1_2$Code<- paste0("97", substr(S1_2$Code, 3, nchar(S1_2$Code)))
S1_2<-S1_2[,c(2,1,3:11)]
T1_2<-T1_2[,-9]

D1<-read.xlsx("~/Desktop/ENSAI/Projet stat/R/1er Tour/Départements/Dpt_t1.xlsx", sep=",")
D1<-D1[,c(1,2,15,5,45,47,53,35,59)]
names(D1)<-c("Cdépart","Département","Exprimés","Abstentions","Nb_voix_Le_Pen","Voix_Le_Pen","Voix_Zemmour","Voix_Macron","Voix_Mélenchon")
D1<-D1[D1$Cdépart %in% c("2A","2B","01","02","03","04","05","06","07","08","09",seq(99)),] #-DOM TOM
D1<-inner_join(D1,dr,join_by(Cdépart==code_departement))
D1<-D1[,c(12,2:9)]

#getwd()
mon_repertoire<-"~/Desktop/ENSAI/Projet stat/R/Indicateurs/Communes"
setwd(mon_repertoire)

Ag_Ex<-read.csv2("Part_agriculteurs_exploitants_c.csv", sep=";",skip=2)
Ar_Co_Ce<-read.csv2("Part_artisans_commerçants_chefsentreprise_c.csv", sep=";",skip=2)
Ss_Ap<-read.csv2("Part_autressansactprofessionnelle_c.csv", sep=";",skip=2)
Ca_PIS<-read.csv2("Part_cadresprofessionsintellecsup_c.csv", sep=";",skip=2)
Em<-read.csv2("Part_employés_c.csv", sep=";",skip=2)
Ou<-read.csv2("Part_ouvriers_c.csv", sep=";",skip=2)
Pi<-read.csv2("Part_profintermédiaires_c.csv", sep=";",skip=2)
Re<-read.csv2("Part_retaités_c.csv", sep=";",skip=2)
Dip_sup<-read.csv2("Dipsup_c.csv", sep=";",skip=2)
Non_dip<-read.csv2("Nondip_c.csv", sep=";",skip=2)
Chom<-read.csv2("Chom_c.csv", sep=";",skip=2)
Emploi_prec<-read.csv2("16_64emploiprec_c.csv", sep=";",skip=2)
A18_24<-read.csv2("18_24_c.csv", sep=";",skip=2)
A30_44<-read.csv2("30_44_c.csv", sep=";",skip=2)
A45_59<-read.csv2("45_59_c.csv", sep=";",skip=2)
Immigre<-read.csv2("immi_c.csv", sep=";",skip=2)
Qpv<-read.csv2("Part_popmunicipale_QPV_c.csv", sep=";",skip=2)
Caf<-read.csv2("CAF_c.csv", sep=";",skip=2)
Dens_pop<-read.csv2("denspop_c.csv", sep=";",skip=2)
Tranche_aire<-read.csv2("Tranche_aire_c.csv", sep=";",skip=2)
Dep_eco<-read.csv2("depeco_c.csv", sep=";",skip=2)
Equip<-read.csv2("equipement_c.csv", sep=";",skip=2)
Grille_dens<-read.csv2("Grille_communale_densité_c.csv", sep=";",skip=2)
Med_rev_dispo<-read.csv2("Medrevdispo_c.csv", sep=";",skip=2)
Rsa<-read.csv2("RSA_c.csv", sep=";",skip=2)

list<-list(A18_24,A30_44,A45_59,Ag_Ex,Ar_Co_Ce,Ca_PIS,Em,Ou,Pi,Re,Ss_Ap,
           Chom,Emploi_prec,Non_dip,Dip_sup,Dep_eco,Immigre,Qpv,
           Dens_pop,Med_rev_dispo,Caf,Rsa,
           Grille_dens,Tranche_aire,Equip)

for (i in 1:length(list)) {
  list[[i]][,3]<-as.numeric(list[[i]][,3])
  T1<-left_join(T1,list[[i]][,c(1,3)],join_by(Code))
}

for (i in 1:length(list)) {
  list[[i]][,3]<-as.numeric(list[[i]][,3])
  T1_2<-left_join(T1_2,list[[i]][,c(1,3)],join_by(Code))
}

for (i in 1:length(list)) {
  list[[i]][,3]<-as.numeric(list[[i]][,3])
  S1_2<-left_join(S1_2,list[[i]][,c(1,3)],join_by(Code))
}


#getwd()
mon_repertoire<-"~/Desktop/ENSAI/Projet stat/R/Indicateurs/Départements"
setwd(mon_repertoire)

Ag_Ex<-read.csv2("Part_agriculteurs_exploitants_d.csv", sep=";",skip=2)
Ar_Co_Ce<-read.csv2("Part_artisans_commerçants_chefsentreprise_d.csv", sep=";",skip=2)
Ss_Ap<-read.csv2("Part_autressansactprofessionnelle_d.csv", sep=";",skip=2)
Ca_PIS<-read.csv2("Part_cadresprofessionsintellecsup_d.csv", sep=";",skip=2)
Em<-read.csv2("Part_employés_d.csv", sep=";",skip=2)
Ou<-read.csv2("Part_ouvriers_d.csv", sep=";",skip=2) #
Pi<-read.csv2("Part_profintermdiaires_d.csv", sep=";",skip=2)
Re<-read.csv2("Part_retaités_d.csv", sep=";",skip=2)
Dip_sup<-read.csv2("Dipsup_d.csv", sep=";",skip=2)
Non_dip<-read.csv2("Nondip_d.csv", sep=";",skip=2)
Chom<-read.csv2("Chom_d.csv", sep=";",skip=2)
Emploi_prec<-read.csv2("16_64emploiprec_d.csv", sep=";",skip=2)
A18_24<-read.csv2("18_24_d.csv", sep=";",skip=2)
A30_44<-read.csv2("30_44_d.csv", sep=";",skip=2)
A45_59<-read.csv2("45_59_d.csv", sep=";",skip=2)
Immigre<-read.csv2("immi_d.csv", sep=";",skip=2)
Evol_pop_migratoire<-read.csv2("Evol_pop_migratoire_apparent_d.csv", sep=";",skip=2)
Dens_pop<-read.csv2("denspop_d.csv", sep=";",skip=2)
Dep_eco<-read.csv2("depeco_d.csv", sep=";",skip=2)
Med_rev_dispo<-read.csv2("Medrevdispo_d.csv", sep=";",skip=2)
Rsa<-read.csv2("RSA_d.csv", sep=";",skip=2)




list<-list(A18_24,A30_44,A45_59,Ag_Ex,Ar_Co_Ce,Ca_PIS,Em,Ou,Pi,Re,Ss_Ap,
           Chom,Emploi_prec,Non_dip,Dip_sup,Dep_eco,Immigre,
           Dens_pop,Med_rev_dispo,Rsa,Evol_pop_migratoire
           )

for (i in 1:length(list)) {
  list[[i]][,3]<-as.numeric(list[[i]][,3])
  D1<-left_join(D1,list[[i]][,c(2,3)],join_by(Département==Libellé))
}



names(T1)[c(2,9:33)]<-c("Région","Age_18_24ans","Age_30_44ans","Age_45_59ans",
                        "Agriculteurs_exploitants","Artisans_commerçants_chefs_d'entreprise",
                        "Cadres_professions_intellectuelles_supérieures","Employés",
                        "Ouvriers","Professions_intermédiaires","Retraités",
                        "Sans_activité_professionelles_ou_autre","Chômage",
                        "Salariés_emploi_précaire","Non_diplomés_non_scolarisés",
                        "Diplomés_supérieur_non_scolarisés","Indicateur_dépendance_économique",
                        "Immigrés","Vivant_quartier_prioritaire",
                        "Densité_population","Médiane_revenu_dispo_par_unité_consommation",
                        "Ressource_plus_50_pct_Caf","Rsa","Grille_densité",
                        "Tranches_aire_attraction_villes","Niveau_équipements_services")


names(T1_2)[c(2,5:8,13:37)]<-c("Région","Exprimés_T1","Abstentions_T1","Nb_voix_Le_Pen_T1","Voix_Le_Pen_T1",
                           "Age_18_24ans","Age_30_44ans","Age_45_59ans",
                        "Agriculteurs_exploitants","Artisans_commerçants_chefs_d'entreprise",
                        "Cadres_professions_intellectuelles_supérieures","Employés",
                        "Ouvriers","Professions_intermédiaires","Retraités",
                        "Sans_activité_professionelles_ou_autre","Chômage",
                        "Salariés_emploi_précaire","Non_diplomés_non_scolarisés",
                        "Diplomés_supérieur_non_scolarisés","Indicateur_dépendance_économique",
                        "Immigrés","Vivant_quartier_prioritaire",
                        "Densité_population","Médiane_revenu_dispo_par_unité_consommation",
                        "Ressource_plus_50_pct_Caf","Rsa","Grille_densité",
                        "Tranches_aire_attraction_villes","Niveau_équipements_services")

names(S1_2)[c(4:7,12:36)]<-c("Exprimés_T1","Abstentions_T1","Nb_voix_Le_Pen_T1","Voix_Le_Pen_T1",
                               "Age_18_24ans","Age_30_44ans","Age_45_59ans",
                               "Agriculteurs_exploitants","Artisans_commerçants_chefs_d'entreprise",
                               "Cadres_professions_intellectuelles_supérieures","Employés",
                               "Ouvriers","Professions_intermédiaires","Retraités",
                               "Sans_activité_professionelles_ou_autre","Chômage",
                               "Salariés_emploi_précaire","Non_diplomés_non_scolarisés",
                               "Diplomés_supérieur_non_scolarisés","Indicateur_dépendance_économique",
                               "Immigrés","Vivant_quartier_prioritaire",
                               "Densité_population","Médiane_revenu_dispo_par_unité_consommation",
                               "Ressource_plus_50_pct_Caf","Rsa","Grille_densité",
                               "Tranches_aire_attraction_villes","Niveau_équipements_services")




names(D1)[c(1,10:30)]<-c("Région",
                               "Age_18_24ans","Age_30_44ans","Age_45_59ans",
                               "Agriculteurs_exploitants","Artisans_commerçants_chefs_d'entreprise",
                               "Cadres_professions_intellectuelles_supérieures","Employés",
                               "Ouvriers","Professions_intermédiaires","Retraités",
                               "Sans_activité_professionelles_ou_autre","Chômage",
                               "Salariés_emploi_précaire","Non_diplomés_non_scolarisés",
                               "Diplomés_supérieur_non_scolarisés","Indicateur_dépendance_économique",
                               "Immigrés","Densité_population","Médiane_revenu_dispo_par_unité_consommation",
                               "Rsa","Taux_évolution_pop_solde_migratoire_apparent")




rm(list,A18_24,A30_44,A45_59,Ag_Ex,Ar_Co_Ce,Ca_PIS,Caf,Chom,Dens_pop,
   Dep_eco,Dip_sup,Em,Emploi_prec,Equip,Grille_dens,Immigre,Evol_pop_migratoire,
   Med_rev_dispo,Non_dip,Ou,Pi,Qpv,Re,Rsa,Ss_Ap,Tranche_aire,dr,mon_repertoire,i,T2,S1,S2)

str(T1_2)
summary(T1_2)

str(S1_2)
summary(S1_2)


str(T1)
summary(T1)

str(D1)
summary(D1)

write.table(T1, "~/Desktop/ENSAI/Projet stat/R/T1.csv", sep=",",row.names = FALSE)

write.table(T1_2, "~/Desktop/ENSAI/Projet stat/R/T1_2.csv", sep=",",row.names = FALSE)

write.table(D1, "~/Desktop/ENSAI/Projet stat/R/D1.csv", sep=",",row.names = FALSE)

S1_2<-S1_2[, -c(34:36)]

write.table(S1_2, "~/Desktop/ENSAI/Projet stat/R/S1_2.csv", sep=",",row.names = FALSE)

