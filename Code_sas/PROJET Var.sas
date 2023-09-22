LIBNAME SOURCE "\\filer-eleves2\id2237\PROJET\Source" ACCESS=READONLY; 
LIBNAME Resultat "\\filer-eleves2\id2237\PROJET\Résultat";

/*IMPORTATION*/

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Périmètre_ZE.csv" OUT=Perim DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Chom.csv" OUT=Chom DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_agriculteurs_exploitants.csv" OUT=Agriculteurs_exploitants DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_artisans_commerçants_chefsentreprise.csv" OUT=Art_comm_chefentrep DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_autressansactprofessionnelle.csv" OUT=Autres_sans_act_prof DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_cadresprofessionsintellecsup.csv" OUT=Cadres_profintsup DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_employés.csv" OUT=Employes DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_ouvriers.csv" OUT=Ouvriers DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_profintermédiares.csv" OUT=Profinterm DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_retaités.csv" OUT=Retraites DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Grille_communale_densité.csv" OUT=Densite DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Part_popmunicipale_QPV.csv" OUT=Quartprio DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Zone_d'emploi.csv" OUT=Emploi DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\equipement.csv" OUT=Equipement DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\pauvrete.csv" OUT=Pauvrete DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\dipsup.csv" OUT=Dipsup DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\nondip.csv" OUT=Nondip DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

/*FIN IMPORTATION*/

PROC FORMAT;
Value $Grille
"1"="Espaces densément peuplés"
"2"="Espaces de densité intermédiaire"
"3"="Espaces peu denses"
"4"="Espaces très peu denses";
Value $Equip
"0"="Communes non centre"
"1"="Centre local"
"2"="Centre intermédiaire"
"3"="Centre structurant"
"4"="Centre majeur";
Value $Zemploi 
"ZEDIV"="Zones à économie diversifiée"
"ZETOU"="Zones spécialisées dans le tourisme"
"ZEAGG"="Autres grandes agglomérations et dotées de gros employeurs"
"ZEDRES"="Zones résidentielles"
"ZEIND"="Zones spécialisées dans l'industrie"
"ZEAGR"="Spécialisées dans l'agriculture"
"ZEMET"="Grandes agglomérations à forte concentration de fonctions métropolitaines";
Value Chomage
low -< 5 = "Chômage < 5%"
5 -< 10 = "5% < Chômage < 10%"
10 -< 15 = "10% < Chômage < 15%"
15 -< 20 = "15% < Chômage < 20%"
 other = "Chômage > 20%";
Value Categorie
1="Agriculteurs exploitants"
2="Artisans,commerçants et chefs d'entreprise"
3="Autres personnes sans activité professionnelle"
4="Cadres et professions intellectuelles supérieures"
5="Employés"
6="Ouvriers"
7="Professions intermédiaires"
8="Retraités";
RUN;
OPTIONS FMTSEARCH = (work source resultat);


DATA Emploi;
SET Emploi (FIRSTOBS=3);
DROP Observatoire_des_territoires___A;
FORMAT VAR3 $Zemploi.;
LABEL VAR3="Typologie zones emploi selon orientation économique principale";
RENAME VAR2=Zone_emploi VAR3=ZE ;
RUN;

DATA Perim;
SET Perim (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
Zone_emploi=substr(VAR3,8);
LABEL Zone_emploi="Périmètre des zones d'emploi 2020";
RENAME VAR2=Commune;
RUN;

DATA Chom;
SET Chom (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
Chom=INPUT(VAR3,BEST12.);
Cchom=INPUT(VAR3,BEST12.);
Format Cchom Chomage.;
LABEL Chom="Taux de chômage 2018" Cchom="Taux de chômage 2018";
RENAME VAR2=Commune;
RUN;

DATA Densite;
SET Densite (FIRSTOBS=3);
DROP Observatoire_des_territoires___A;
FORMAT VAR3 $Grille.;
LABEL VAR3="Grille communale de densité";
RENAME VAR2=Commune VAR3=Dens;
RUN;

DATA Equipement;
SET Equipement (FIRSTOBS=3) ;
where Var3 not like "null";
DROP Observatoire_des_territoires___A;
FORMAT VAR3 $Equip.;
LABEL VAR3="Niveau de centres d'équipements et de services";
RENAME VAR2=Commune VAR3=Equip;
RUN;

DATA Quartprio;
SET Quartprio (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
Qpv=INPUT(VAR3,BEST12.);
LABEL Qpv="Part population municipale vivant dans quartier prioritaire de la politique ville (QPV)";
RENAME VAR2=Commune;
RUN;

DATA Agriculteurs_exploitants;
SET Agriculteurs_exploitants (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
AG_EX=INPUT(VAR3,BEST12.);
LABEL AG_EX="Part agriculteurs exploitants 2018";
RENAME VAR2=Commune;
RUN;

DATA Art_comm_chefentrep;
SET Art_comm_chefentrep (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
AR_CO_CE=INPUT(VAR3,BEST12.);
LABEL AR_CO_CE="Part artisans, commerçants, chefs d'entreprise 2018";
RENAME VAR2=Commune;
RUN;

DATA Autres_sans_act_prof;
SET Autres_sans_act_prof (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
SS_AP=INPUT(VAR3,BEST12.);
LABEL SS_AP="Part autres sans activité professionnelle 2018";
RENAME VAR2=Commune;
RUN;

DATA Cadres_profintsup;
SET Cadres_profintsup (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
CA_PIS=INPUT(VAR3,BEST12.);
LABEL CA_PIS="Part cadres et professions intellectuelles supérieures 2018";
RENAME VAR2=Commune;
RUN;

DATA Employes;
SET Employes (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
EM=INPUT(VAR3,BEST12.);
LABEL EM="Part employés 2018";
RENAME VAR2=Commune;
RUN;

DATA Ouvriers;
SET Ouvriers (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
OU=INPUT(VAR3,BEST12.);
LABEL OU="Part ouvriers 2018";
RENAME VAR2=Commune;
RUN;

DATA Dipsup;
SET dipsup (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
dipsup=INPUT(VAR3,BEST12.);
LABEL dipsup="Part diplomés du supérieur";
RENAME VAR2=Commune;
RUN;

DATA nondip;
SET nondip (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
nondip=INPUT(VAR3,BEST12.);
LABEL nondip="Part non diplomés";
RENAME VAR2=Commune;
RUN;

DATA Pauvrete;
SET Pauvrete (FIRSTOBS=3);
where Var3 not like "N/A";
DROP Observatoire_des_territoires___A VAR3;
PV=INPUT(VAR3,BEST12.);
LABEL PV="Taux pauvrete 2018";
RENAME VAR2=Commune;
RUN;

DATA Profinterm;
SET Profinterm (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
PI=INPUT(VAR3,BEST12.);
LABEL PI="Part professions intermédiaires 2018";
RENAME VAR2=Commune;
RUN;

DATA Retraites;
SET Retraites (FIRSTOBS=3);
DROP Observatoire_des_territoires___A VAR3;
RE=INPUT(VAR3,BEST12.);
LABEL RE="Part retraités 2018";
RENAME VAR2=Commune;
RUN;

DATA subcom_t2_c;
SET Source.subcom_t2_c;
Vmacron=100*Macron/Exprimes;
Vlepen=100*Le_Pen/Exprimes;
RENAME Code_du_d_partement=Departement;
LABEL Vmacron="% Votes Macron sur les exprimés";
LABEL Vlepen="% Votes Le Pen sur les exprimés";
RUN;

DATA subcom_t1_c;
SET Source.subcom_t1_c;
Vlepen=100*Le_Pen/Exprimes;
/*Varthaud=100*Arthaud/Exprimes;
Vroussel=100*Roussel/Exprimes;
Vmacron=100*Macron/Exprimes;
Vlassalle=100*Lassalle/Exprimes;
Vzemmour=100*Zemmour/Exprimes;
Vmelenchon=100*Melenchon/Exprimes;
Vhidalgo=100*Hidalgo/Exprimes;
Vjadot=100*Jadot/Exprimes;
Vpecresse=100*Pecresse/Exprimes;
Vpoutou=100*Poutou/Exprimes;
Vdupont_aignan=100*Dupont_Aignan/Exprimes;*/
RENAME Code_du_d_partement=Departement;
LABEL Vlepen="% Votes Le Pen sur les exprimés";
/*LABEL Varthaud="% Votes Arthaud sur les exprimés";
LABEL Vroussel="% Votes Roussel sur les exprimés";
LABEL Vmacron="% Votes Macron sur les exprimés";
LABEL Vlassalle="% Votes Lassalle sur les exprimés";
LABEL Vlepen="% Votes Le Pen sur les exprimés";
LABEL Vzemmour="% Votes Zemmour sur les exprimés";
LABEL Vmelenchon="% Votes Mélenchon sur les exprimés";
LABEL Vhidalgo="% Votes Hidalgo sur les exprimés";
LABEL Vjadot="% Votes Jadot sur les exprimés";
LABEL Vpecresse="% Votes Pécresse sur les exprimés";
LABEL Vpoutou="% Votes Poutou sur les exprimés";
LABEL Vdupont_aignan="% Votes Dupont-Aignansur les exprimés";*/
RUN;

/*Zone d'emploi*/

PROC SORT DATA=Emploi;
BY Zone_emploi;
RUN;

PROC SORT DATA=Perim;
BY Zone_emploi;
RUN;

/*Communale*/

PROC SORT DATA=Chom;
BY Commune;
RUN;

PROC SORT DATA=Densite;
BY Commune;
RUN;

PROC SORT DATA=Equipement;
BY Commune;
RUN;

PROC SORT DATA=Quartprio;
BY Commune;
RUN;

PROC SORT DATA=Agriculteurs_exploitants;
BY Commune;
RUN;

PROC SORT DATA=Art_comm_chefentrep;
BY Commune;
RUN;

PROC SORT DATA=Autres_sans_act_prof;
BY Commune;
RUN;

PROC SORT DATA=Cadres_profintsup;
BY Commune;
RUN;

PROC SORT DATA=Employes;
BY Commune;
RUN;

PROC SORT DATA=Ouvriers;
BY Commune;
RUN;

PROC SORT DATA=Profinterm;
BY Commune;
RUN;

PROC SORT DATA=Retraites;
BY Commune;
RUN;

PROC SORT DATA=Dipsup;
BY Commune;
RUN;

PROC SORT DATA=nondip;
BY Commune;
RUN;

PROC SORT DATA=Pauvrete;
BY Commune;
RUN;

PROC SORT DATA=subcom_t2_c;
BY Commune;
RUN;

PROC SORT DATA=subcom_t1_c;
BY Commune;
RUN;

DATA ZE;
MERGE Perim(in=A)Emploi(in=B);
BY Zone_emploi;
IF A and B  then output;
RUN;

PROC SORT DATA=ZE;
BY Commune;
RUN;



DATA Resultat.Ext2;
MERGE subcom_t2_c(in=A)Agriculteurs_exploitants(in=B)Art_comm_chefentrep(in=C) 
Autres_sans_act_prof(in=D)Cadres_profintsup(in=E)Employes(in=F)Ouvriers(in=G)
Profinterm(in=H)Retraites(in=I)Densite(in=J)Quartprio(in=K)ZE(in=L)Equipement(in=M)dipsup(in=N)
nondip(in=O)Chom(in=P);
BY Commune;
IF A and B and C and D and E and F and G and H and I and J and K and L and M and N and O and P then output;
RUN;


DATA Resultat.Ext1;
MERGE subcom_t1_c(in=A)Agriculteurs_exploitants(in=B)Art_comm_chefentrep(in=C) 
Autres_sans_act_prof(in=D)Cadres_profintsup(in=E)Employes(in=F)Ouvriers(in=G)
Profinterm(in=H)Retraites(in=I)Densite(in=J)Quartprio(in=K)ZE(in=L)Equipement(in=M)dipsup(in=N)
nondip(in=O)Chom(in=P);
BY Commune;
IF A and B and C and D and E and F and G and H and I and J and K and L and M and N and O and P then output;
RUN;

Data Resultat.pauvt1;
Merge subcom_t1_c(in=A)Pauvrete(in=B);
BY Commune;
IF A and B then output;
RUN;

Data Resultat.pauvt2;
Merge subcom_t2_c(in=A)Pauvrete(in=B);
BY Commune;
IF A and B then output;
RUN;


Data Ext2p;
Set resultat.ext2;
Where chom > 15 ;
run;

DATA Bigt2;
SET Resultat.Ext2;
DO j=1 to 8;
output;
END;
RUN;

DATA Bigt2;
SET Bigt2;
DROP Vmacron Macron Le_pen AG_EX AR_CO_CE SS_AP CA_PIS EM OU PI RE;
SELECT;
WHEN (j=1) DO;
  Parts=AG_EX;
  END;
WHEN (j=2) DO;
  Parts=AR_CO_CE;
  END;
WHEN (j=3) DO;
  Parts=SS_AP;
  END;
WHEN (j=4) DO;
  Parts=CA_PIS;
  END;
WHEN (j=5) DO;
  Parts=EM;
  END;
WHEN (j=6) DO;
  Parts=OU;
  END;
WHEN (j=7) DO;
  Parts=PI;
  END;
WHEN (j=8) DO;
  Parts=RE;
  END;
END;
FORMAT j Categorie.;
Label j="Professions et catégories socioprofessionnelles";
RUN;

Data Resultat.correl;
Input cat Tour corr;
Format cat Categorie.;
Label cat="Professions et catégories socioprofessionnelles" corr="Coefficient de corrélation de Pearson";
cards;
1 1 -0.07
2 1 -0.12
3 1 0.07
4 1 -0.26
5 1 0.15
6 1 0.38
7 1 -0.06
8 1 -0.14
1 2 0
2 2 -0.07
3 2 0.07
4 2 -0.3
5 2 0.12
6 2 0.3
7 2 -0.12
8 2 -0.06
;
RUN;


/*
DATA Ext1x;
SET Resultat.Ext1;
Tour= 1;
RUN;

DATA Ext2x;
SET Resultat.Ext2;
Tour= 2;
RUN;

DATA Resultat.MLt1_2;
SET Ext1x Ext2x;
DROP Varthaud Vroussel Vlassalle Vzemmour Vmelenchon Vhidalgo
Vjadot Vpecresse Vpoutou Vdupont_aignan Arthaud Roussel Lassalle 
Zemmour Melenchon Hidalgo Jadot Pecresse Poutou Dupont_aignan;
RUN;

PROC SORT DATA=Resultat.MLt1_2;
BY Commune;
RUN;

DATA Bigt1_2;
SET Resultat.MLt1_2;
DO i=1 to 2;
DO j=1 to 8;
output;
END;
END;
RUN;

DATA Bigt1_2;
SET Bigt1_2;
DROP i j Vmacron Vlepen Macron Le_pen AG_EX AR_CO_CE SS_AP CA_PIS EM OU PI RE;
SELECT;
WHEN (i=1) DO;
  Nom="Macron";
  Voix=Vmacron;
  END;
WHEN (i=2) DO;
  Nom="Le Pen";
  Voix=Vlepen;
  END;
END;
SELECT;
WHEN (j=1) DO;
  Cat="Agriculteurs exploit";
  Parts=AG_EX;
  END;
WHEN (j=2) DO;
  Cat="Artis/comm/chefentre";
  Parts=AR_CO_CE;
  END;
WHEN (j=3) DO;
  Cat="Autres ss act prof";
  Parts=SS_AP;
  END;
WHEN (j=4) DO;
  Cat="Cadres/prof intelsup";
  Parts=CA_PIS;
  END;
WHEN (j=5) DO;
  Cat="Employés";
  Parts=EM;
  END;
WHEN (j=6) DO;
  Cat="Ouvriers";
  Parts=OU;
  END;
WHEN (j=7) DO;
  Cat="Professions interméd";
  Parts=PI;
  END;
WHEN (j=8) DO;
  Cat="Retraités";
  Parts=RE;
  END;
END;
RUN;



DATA Bigt2;
SET Resultat.Ext2;
DO i=1 to 2;
DO j=1 to 8;
output;
END;
END;
RUN;

DATA Bigt2;
SET Bigt2;
DROP i j Vmacron Vlepen Macron Le_pen AG_EX AR_CO_CE SS_AP CA_PIS EM OU PI RE;
SELECT;
WHEN (i=1) DO;
  Nom="Macron";
  Voix=Vmacron;
  END;
WHEN (i=2) DO;
  Nom="Le Pen";
  Voix=Vlepen;
  END;
END;
SELECT;
WHEN (j=1) DO;
  Cat="Agriculteurs exploit";
  Parts=AG_EX;
  END;
WHEN (j=2) DO;
  Cat="Artis/comm/chefentre";
  Parts=AR_CO_CE;
  END;
WHEN (j=3) DO;
  Cat="Autres ss act prof";
  Parts=SS_AP;
  END;
WHEN (j=4) DO;
  Cat="Cadres/prof intelsup";
  Parts=CA_PIS;
  END;
WHEN (j=5) DO;
  Cat="Employés";
  Parts=EM;
  END;
WHEN (j=6) DO;
  Cat="Ouvriers";
  Parts=OU;
  END;
WHEN (j=7) DO;
  Cat="Professions interméd";
  Parts=PI;
  END;
WHEN (j=8) DO;
  Cat="Retraités";
  Parts=RE;
  END;
Label Cat="Professions et catégories socioprofessionnelles"
END;
RUN;


DATA Bigt1;
SET Resultat.Ext1;
DO i=1 to 12;
output;
END;
RUN;


DATA Bigt1;
SET Bigt1;
DROP i Varthaud Vroussel Vmacron Vlassalle Vlepen Vzemmour Vmelenchon Vhidalgo
Vjadot Vpecresse Vpoutou Vdupont_aignan Arthaud Roussel Macron Lassalle
Le_pen Zemmour Melenchon Hidalgo Jadot Pecresse Poutou Dupont_aignan;
ATTRIB Nom Format=$15.;
SELECT;
WHEN (i=1) DO;
  Nom="Arthaud";
  Voix=Varthaud;
  END;
WHEN (i=2) DO;
  Nom="Roussel";
  Voix=Vroussel;
  END;
WHEN (i=3) DO;
  Nom="Macron";
  Voix=Vmacron;
  END;
WHEN (i=4) DO;
  Nom="Lassalle";
  Voix=Vlassalle;
  END;
WHEN (i=5) DO;
  Nom="Le Pen";
  Voix=Vlepen;
  END;
WHEN (i=6) DO;
  Nom="Zemmour";
  Voix=Vzemmour;
  END;
WHEN (i=7) DO;
  Nom="Mélenchon";
  Voix=Vmelenchon;
  END;
WHEN (i=8) DO;
  Nom="Hidalgo";
  Voix=Vhidalgo;
  END;
WHEN (i=9) DO;
  Nom="Jadot";
  Voix=Vjadot;
  END;
WHEN (i=10) DO;
  Nom="Pécresse";
  Voix=Vpecresse;
  END;
WHEN (i=11) DO;
  Nom="Poutou";
  Voix=Vpoutou;
  END;
WHEN (i=12) DO;
  Nom="Dupont-Aignan";
  Voix=Vdupont_aignan;
  END;
END;
RUN;




