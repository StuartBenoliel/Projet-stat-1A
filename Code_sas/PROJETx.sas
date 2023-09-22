LIBNAME SOURCE "\\filer-eleves2\id2237\PROJET\Source" ACCESS=READONLY; 
LIBNAME Resultat "\\filer-eleves2\id2237\PROJET\Résultat";

/*IMPORTATION*/

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Périmètre_ZE.csv" OUT=Perim DBMS=csv  REPLACE;
DELIMITER=";";
RUN;

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\Chomtrim1_2020.csv" OUT=Chom DBMS=csv  REPLACE;
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

PROC IMPORT DATAFILE="\\filer-eleves2\id2237\PROJET\Source\data.csv" OUT=Equipement DBMS=csv  REPLACE;
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
LABEL Chom="Taux chômage 2020";
RENAME VAR2=Zone_emploi;
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
Varthaud=100*Arthaud/Exprimes;
Vroussel=100*Roussel/Exprimes;
Vmacron=100*Macron/Exprimes;
Vlassalle=100*Lassalle/Exprimes;
Vlepen=100*Le_Pen/Exprimes;
Vzemmour=100*Zemmour/Exprimes;
Vmelenchon=100*Melenchon/Exprimes;
Vhidalgo=100*Hidalgo/Exprimes;
Vjadot=100*Jadot/Exprimes;
Vpecresse=100*Pecresse/Exprimes;
Vpoutou=100*Poutou/Exprimes;
Vdupont_aignan=100*Dupont_Aignan/Exprimes;
RENAME Code_du_d_partement=Departement;
LABEL Varthaud="% Votes Arthaud sur les exprimés";
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
LABEL Vdupont_aignan="% Votes Dupont-Aignansur les exprimés";
RUN;

/*Zone d'emploi*/

PROC SORT DATA=Emploi;
BY Zone_emploi;
RUN;

PROC SORT DATA=Perim;
BY Zone_emploi;
RUN;

PROC SORT DATA=Chom;
BY Zone_emploi;
RUN;

/*Communale*/

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

PROC SORT DATA=subcom_t2_c;
BY Commune;
RUN;

PROC SORT DATA=subcom_t1_c;
BY Commune;
RUN;

DATA ZE;
MERGE Perim(in=A)Emploi(in=B)Chom(in=C);
BY Zone_emploi;
IF A and B and C then output;
RUN;

PROC SORT DATA=ZE;
BY Commune;
RUN;



DATA Ext2;
MERGE subcom_t2_c(in=A)Agriculteurs_exploitants(in=B)Art_comm_chefentrep(in=C) 
Autres_sans_act_prof(in=D)Cadres_profintsup(in=E)Employes(in=F)Ouvriers(in=G)
Profinterm(in=H)Retraites(in=I)Densite(in=J)Quartprio(in=K)ZE(in=L)Equipement(in=M);
BY Commune;
/*IF A then output;*/
IF A and B and C and D and E and F and G and H and I and J and K and L and M then output;
RUN;

DATA Resultat.Ext2;
SET Ext2 (FIRSTOBS=3);
RUN;

DATA Resultat.Ext1;
MERGE subcom_t1_c(in=A)Agriculteurs_exploitants(in=B)Art_comm_chefentrep(in=C) 
Autres_sans_act_prof(in=D)Cadres_profintsup(in=E)Employes(in=F)Ouvriers(in=G)
Profinterm(in=H)Retraites(in=I)Densite(in=J)Quartprio(in=K)ZE(in=L)Equipement(in=M);
BY Commune;
/*IF A then output;*/
IF A and B and C and D and E and F and G and H and I and J and K and L and M then output;
RUN;

DATA Bigt2;
SET Resultat.Ext2;         /* exemple avec cette fois avec une autre variable qui me donnera le nom de la profession et une autre donnant le % dans la commune */
DO i=1 to 2;
DO j=1 to 8;
output;
end;
end;
RUN;

DATA Bigt2;
SET Bigt2;
DROP i j Vmacron Vlepen Macron Le_pen AG_EX AR_CO_CE SS_AP CA_PIS EM OU PI RE;
IF i=1 then Nom="Macron";
IF i=2 then Nom="Le Pen";
IF j=1 then Cat="Agriculteurs exploit";
IF j=2 then Cat="Artis/comm/chefentre";
IF j=3 then Cat="Autres ss act prof";
IF j=4 then Cat="Cadres/prof intelsup";
IF j=5 then Cat="Employés";
IF j=6 then Cat="Ouvriers";
IF j=7 then Cat="Professions interméd";
IF j=8 then Cat="Retraités";
IF Nom="Macron" then Voix=Vmacron;
IF Nom="Le Pen" then Voix=Vlepen;
IF Cat="Agriculteurs exploit" then Parts=AG_EX;
IF Cat="Artis/comm/chefentre" then Parts=AR_CO_CE;
IF Cat="Autres ss act prof" then Parts=SS_AP;
IF Cat="Cadres/prof intelsup" then Parts=CA_PIS;
IF Cat="Employés" then Parts=EM;
IF Cat="Ouvriers" then Parts=OU;
IF Cat="Professions interméd" then Parts=PI;
IF Cat="Retraités" then Parts=RE;
RUN;

/*
proc sql ;
select *
from Work.Equipement where equip="null";
quit;
*/
DATA Bigt1; /* Objectif : créer une variable nom qui prendra le nom du candidat pour pouvoir regroupé comme on le fait avec des variables qualitatives*/
SET Resultat.Ext1;
DO i=1 to 12;   /* Je multiplie chaque ligne par 12 (nb=candidats) */
output;
end;
RUN;

DATA Bigt1;
SET Bigt1;
ATTRIB Nom Format=$15.;         /* Je créer une nouvelle variable Nom qui va prendre le nom d'un candidat.Ainsi au lieu d'avoir une ligne contenant les 12 valeurs des candidats (12 colonnes)*/
IF i=1 then Nom="Arthaud";      /* je veux 12 lignes contenant chacune l'information d'un candidat (%vote et son nom)*/
IF i=2 then Nom="Roussel";
IF i=3 then Nom="Macron";
IF i=4 then Nom="Lassalle";
IF i=5 then Nom="Le Pen";
IF i=6 then Nom="Zemmour";
IF i=7 then Nom="Mélenchon";
IF i=8 then Nom="Hidalgo";
IF i=9 then Nom="Jadot";
IF i=10 then Nom="Pécresse";
IF i=11 then Nom="Poutou";
IF i=12 then Nom="Dupont-Aignan";
RUN;

DATA Bigt1;
SET Bigt1;
DROP i Varthaud Vroussel Vmacron Vlassalle Vlepen Vzemmour Vmelenchon Vhidalgo
Vjadot Vpecresse Vpoutou Vdupont_aignan Arthaud Roussel Macron Lassalle
Le_pen Zemmour Melenchon Hidalgo Jadot Pecresse Poutou Dupont_aignan;
IF Nom="Arthaud" then Voix=Varthaud;
IF Nom="Roussel" then Voix=Vroussel;    /* j'enlève les 24 colonnes ( 12 contenant le % et  12 la valeur absolu ...), ma variable temporaire i. J'aurai ainsi une seule variable contenant*/
IF Nom="Macron" then Voix=Vmacron;      /* les pourcentages des candidats */
IF Nom="Lassalle" then Voix=Vlassalle;
IF Nom="Le Pen" then Voix=Vlepen;
IF Nom="Zemmour" then Voix=Vzemmour;
IF Nom="Mélenchon" then Voix=Vmelenchon;
IF Nom="Hidalgo" then Voix=Vhidalgo;
IF Nom="Jadot" then Voix=Vjadot;
IF Nom="Pécresse" then Voix=Vpecresse;
IF Nom="Poutou" then Voix=Vpoutou;
IF Nom="Dupont-Aignan" then Voix=Vdupont_aignan;
RUN;




/*Analyse*/

%MACRO vbox(table,candidat,groupe);           /* Des Macros permettant comme une fonction sur python juste en donnant les paramètres d'obtenir le graphique sans faire plein De Copiercoller*/                 
PROC SGPLOT DATA=&table;                      /* Il faut bien lancer le programme de la macro (selectionner+soumettre) avant de l'utiliser (ex d'utilisation apres)*/
VBOX &candidat / category=&groupe;            /* sinon vous pouvre voir une structure possible avec des fois des fonctionnalités suplémentaires mis entre /**/ */
XAXIS display=(nolabel);
/*TITLE "Boxplot des % votes en fonction de la densité des communes";
FOOTNOTE "Données : data.gouv"*/
RUN;
%MEND;

%MACRO hist(table,candidat,groupe);
PROC SGPLOT DATA=&table;
HISTOGRAM &candidat /  Group=&groupe;
/*DENSITY &candidat / Type=Normal; /*Kernel*/
/*TITLE "Histogramme montrant les parts de chaque % votes en fonction de la densité des communes";*/
RUN;
%MEND;

%MACRO vbar(table,var1,var2);
PROC SGPLOT DATA=&table;
VBAR &var1  / response=&var2 stat=mean  DATALABEL; /*STAT = FREQ MEAN MEDIAN PERCENT SUM*/
XAXIS display=(nolabel);
RUN;
%MEND;

%MACRO vbarb(table,var);
PROC SGPLOT DATA=&table;
VBAR &var  / response=voix stat=Mean group=nom SEGLABEL DATALABEL; /*STAT = FREQ MEAN MEDIAN PERCENT SUM*/
XAXIS display=(nolabel);
RUN;
%MEND;

%MACRO corP(table,varQN1,varQN2);/*Quanti*/
PROC CORR DATA=&table;
VAR &varQN1 &varQN2;
/*WEIGHT inscrit;*/
RUN;
%MEND;

%MACRO Regl(table,varQN1,varQN2);/*Quanti*/
PROC REG DATA=&table PLOTS=NONE;
MODEL &varQN1=&varQN2;
RUN;
%MEND;

%MACRO Anova(table,varQL,varQN);/*Quali-Quanti*/
PROC ANOVA DATA=&table;
CLASS &VarQL;
MODEL &VarQN=&VarQL;
RUN;
%MEND;

%MACRO khi(table,varQL1,varQL2);/*Quali*/
PROC FREQ DATA=&table;
TABLE &VarQL1 * &VarQL2 / chisq;
RUN;
%MEND;

%vbox(Bigt1,Nom,equip);           /* Eemples -> il suffit après de juste selectionner et lancer la ligne*/
%vbox(Resultat.Bigt2,parts,cat);
%vbox(Resultat.Ext2,Vmacron,ze); 
%hist(Resultat.Ext2,Vlepen,equip); 
%vbar(Resultat.Ext1,dens,vmacron);
%vbarb(Work.Bigt2,equip); 
%corP(Resultat.Ext1,OU,Vlepen);/*Quanti*/
%Regl(Resultat.Ext1,OU,Vlepen);/*Quanti*/
%khi(Resultat.Ext1,ZE,dens);/*Quali*/
%Anova(Resultat.Ext1,dens,Vmacron); /*Quali-Quanti*/

/*%corP(Resultat.Ext1,OU,Vlepen); 0.37*/

/*TEST*/

PROC SGPLOT DATA=Bigt1;
VBOX voix/ category=nom;
XAXIS display=(nolabel);
/*TITLE "Boxplot des % votes en fonction de la densité des communes";
FOOTNOTE "Données : data.gouv"*/
RUN;

PROC SGPLOT DATA=Resultat.Ext2;
VBAR dens  / response=vlepen stat=Mean DISCRETOFFSET=-0.1 ;
VBAR dens  / response=vmacron stat=Mean DISCRETOFFSET=0.1 ;
XAXIS display=(nolabel);
RUN;

DATA Big;
SET Resultat.Bigt2;
A=voix*parts/50;
RUN;

PROC SGPLOT DATA=Big;
VBAR nom  / response=A stat=MEAN group=cat SEGLABEL DATALABEL; /*FREQ MEAN MEDIAN PERCENT SUM*/
XAXIS display=(nolabel);
RUN;

PROC SGPLOT DATA=Resultat.Ext1;
VBOX OU ;
/*TITLE "Boxplot des % votes en fonction de la densité des communes";
FOOTNOTE "Données : data.gouv";*/
RUN;

PROC SGPLOT DATA=Resultat.Ext2;
HISTOGRAM Vlepen /Freq=AG_EX TYPE=MEAN;
DENSITY Vlepen / Type=Normal; /*Kernel*/
RUN;





/* région */
Data Reg_t2_c;
SET Source.Reg_t2_c;
Vmacron=100*Macron/Exprimes;
Vlepen=100*Le_Pen/Exprimes;
if inscrits >1000000 then code=1;
else code=2;
LABEL Vmacron="% Votes Macron sur les exprimés";
LABEL Vlepen="% Votes Le Pen sur les exprimés";
RUN;

PROC GCHART DATA=Reg_t2_c;
HBAR Region / SUMVAR=Vmacron TYPE=SUM ;
RUN;

PROC SGPLOT DATA=Reg_t2_c;
VBAR Region / FREQ=Vmacron;
REFLINE 58.55 / AXIS=Y LABEL="58,55 Moyenne Nationale" ;
LABEL Region = "% des votes pour Macron au 2nd tour parmi les suffrages exprimés";
RUN;

PROC SGPLOT DATA=Reg_t2_c;
VBAR Region / FREQ=Vlepen;
REFLINE 41.45 / AXIS=Y LABEL="41,45 Moyenne Nationale" ;
LABEL Region  = "% des votes pour Le Pen au 2nd tour parmi les suffrages exprimés";
RUN;

PROC SGPLOT DATA=Reg_t2_c;
VBOX Vmacron;
REFLINE 58.55 / AXIS=Y LABEL="58,55 Moyenne Nationale";
yaxis label="% des votes par région pour Macron au 2nd tour parmi les suffrages exprimés ";
RUN;

PROC SGPLOT DATA=Reg_t2_c;
VBOX Vlepen /category=code;
REFLINE 41.45 / AXIS=Y LABEL="41,45 Moyenne Nationale" ;
yaxis label="% des votes par région pour Le Pen au 2nd tour parmi les suffrages exprimés ";
RUN;




/*
DATA Bigt2;
SET Bigt2;
DROP i j Vmacron Vlepen Macron Le_pen AG_EX AR_CO_CE SS_AP CA_PIS EM OU PI RE;
IF i=1 then Nom="Macron";
IF i=2 then Nom="Le Pen";
IF j=1 then Cat="Agriculteurs exploit";
IF j=2 then Cat="Artis/comm/chefentre";
IF j=3 then Cat="Autres ss act prof";
IF j=4 then Cat="Cadres/prof intelsup";
IF j=5 then Cat="Employés";
IF j=6 then Cat="Ouvriers";
IF j=7 then Cat="Professions interméd";
IF j=8 then Cat="Retraités";
IF Nom="Macron" then Voix=Vmacron;
IF Nom="Le Pen" then Voix=Vlepen;
IF Cat="Agriculteurs exploit" then Parts=AG_EX;
IF Cat="Artis/comm/chefentre" then Parts=AR_CO_CE;
IF Cat="Autres ss act prof" then Parts=SS_AP;
IF Cat="Cadres/prof intelsup" then Parts=CA_PIS;
IF Cat="Employés" then Parts=EM;
IF Cat="Ouvriers" then Parts=OU;
IF Cat="Professions interméd" then Parts=PI;
IF Cat="Retraités" then Parts=RE;
RUN;

proc sql ;
select *
from Work.Equipement where equip="null";
quit;
*/
