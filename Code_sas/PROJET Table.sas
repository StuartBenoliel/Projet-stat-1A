LIBNAME SOURCE "\\filer-eleves2\id2237\PROJET\Source"; 


/*1er Tour*/

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\Régions\Reg_t1"
Out=Reg_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Reg_t1;
SET Reg_t1;
Keep Code_de_la_r_gion Libell__de_la_r_gion Inscrit Abstentions Votants Blancs Nuls Exprim_s Voix AA AG AM AS AY BE BK BQ BW CC CI;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AA="Voix Roussel" AG="Voix Macron" AM="Voix Lassalle" AS="Voix Le Pen" AY="Voix Zemmour" BE="Voix Mélenchon"
BK="Voix Hidalgo" BQ="Voix Jadot" BW="Voix Pécresse" CC="Voix Poutou" CI="Voix Dupont-Aignan" ;
Rename Libell__de_la_r_gion=Region Exprim_s=Exprimes Voix=Arthaud AA=Roussel AG=Macron AM=Lassalle AS=Le_Pen AY=Zemmour BE=Melenchon
BK=Hidalgo BQ=Jadot BW=Pecresse CC=Poutou CI=Dupont_Aignan;
RUN;

DATA Source.Reg_t1_c;
SET Reg_t1;
RENAME Inscrit=Inscrits;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\France_entière\Fe_t1"
Out=Fe_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Fe_t1;
SET Fe_t1;
Keep Code_du_niveau Libell__du_niveau Inscrit Abstentions Votants Blancs Nuls Exprim_s Voix AC AJ AQ AX BE BL BS BZ CG CN CU;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AC="Voix Roussel" AJ="Voix Macron" AQ="Voix Lassalle" AX="Voix Le Pen" BE="Voix Zemmour" BL="Voix Mélenchon"
BS="Voix Hidalgo" BZ="Voix Jadot" CG="Voix Pécresse" CN="Voix Poutou" CU="Voix Dupont-Aignan";
Rename Libell__du_niveau=Niveau Exprim_s=Exprimes Voix=Arthaud AC=Roussel AJ=Macron AQ=Lassalle AX=Le_Pen BE=Zemmour BL=Melenchon
BS=Hidalgo BZ=Jadot CG=Pecresse CN=Poutou CU=Dupont_Aignan;
RUN;

DATA Fe_t1_c;
SET Fe_t1;
RENAME Inscrit=Inscrits;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\France_entière\Fe_metrop_t1"
Out=Fe_metrop_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Fe_metrop_t1;
SET Fe_metrop_t1;
Keep Code_du_niveau Libell__du_niveau Inscrit Abstentions Votants Blancs Nuls Exprim_s Voix AC AJ AQ AX BE BL BS BZ CG CN CU;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AC="Voix Roussel" AJ="Voix Macron" AQ="Voix Lassalle" AX="Voix Le Pen" BE="Voix Zemmour" BL="Voix Mélenchon"
BS="Voix Hidalgo" BZ="Voix Jadot" CG="Voix Pécresse" CN="Voix Poutou" CU="Voix Dupont-Aignan";
Rename Libell__du_niveau=Niveau Exprim_s=Exprimes Voix=Arthaud AC=Roussel AJ=Macron AQ=Lassalle AX=Le_Pen BE=Zemmour BL=Melenchon
BS=Hidalgo BZ=Jadot CG=Pecresse CN=Poutou CU=Dupont_Aignan;
RUN;

DATA Fe_metrop_t1_c;
SET Fe_metrop_t1;
RENAME Inscrit=Inscrits;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\France_entière\Fe_outmer_t1"
Out=Fe_outmer_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Fe_outmer_t1;
SET Fe_outmer_t1;
Keep Code_du_niveau Libell__du_niveau Inscrit Abstentions Votants Blancs Nuls Exprim_s Voix AC AJ AQ AX BE BL BS BZ CG CN CU;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AC="Voix Roussel" AJ="Voix Macron" AQ="Voix Lassalle" AX="Voix Le Pen" BE="Voix Zemmour" BL="Voix Mélenchon"
BS="Voix Hidalgo" BZ="Voix Jadot" CG="Voix Pécresse" CN="Voix Poutou" CU="Voix Dupont-Aignan";
Rename Libell__du_niveau=Niveau Exprim_s=Exprimes Voix=Arthaud AC=Roussel AJ=Macron AQ=Lassalle AX=Le_Pen BE=Zemmour BL=Melenchon
BS=Hidalgo BZ=Jadot CG=Pecresse CN=Poutou CU=Dupont_Aignan;
RUN;

DATA Fe_outmer_t1_c;
SET Fe_outmer_t1;
RENAME Inscrit=Inscrits;
RUN;

DATA Source.Fe_tot_t1;
SET Fe_t1_c Fe_metrop_t1_c Fe_outmer_t1_c;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\Départements\Dpt_t1"
Out=Dpt_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Dpt_t1;
SET Dpt_t1;
Keep Code_du_d_partement Libell__du_d_partement Inscrit Abstentions Votants Blancs Nuls Exprim_s Voix AA AG AM AS AY BE BK BQ BW CC CI;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AA="Voix Roussel" AG="Voix Macron" AM="Voix Lassalle" AS="Voix Le Pen" AY="Voix Zemmour" BE="Voix Mélenchon"
BK="Voix Hidalgo" BQ="Voix Jadot" BW="Voix Pécresse" CC="Voix Poutou" CI="Voix Dupont-Aignan" ;
Rename Libell__du_d_partement=Departement Exprim_s=Exprimes Voix=Arthaud AA=Roussel AG=Macron AM=Lassalle AS=Le_Pen AY=Zemmour 
BE=Melenchon BK=Hidalgo BQ=Jadot BW=Pecresse CC=Poutou CI=Dupont_Aignan;
RUN;

DATA Source.Dpt_t1_c;
SET Dpt_t1;
RENAME Inscrit=Inscrits;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\Communes\Subcom_t1"
Out=Subcom_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Subcom_t1;
SET Subcom_t1;
Keep Code_du_d_partement Libell__du_d_partement Code_de_la_commune Libell__de_la_commune Inscrit Abstentions Votants Blancs Nuls
Exprim_s Voix AE AL AS AZ BG BN BU CB CI CP CW;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AE="Voix Roussel" AL="Voix Macron" AS="Voix Lassalle" AZ="Voix Le Pen" BG="Voix Zemmour" BN="Voix Mélenchon"
BU="Voix Hidalgo" CB="Voix Jadot" CI="Voix Pécresse" CP="Voix Poutou" CW="Voix Dupont-Aignan" ;
Rename Libell__du_d_partement=Departement Libell__de_la_commune=Commune Exprim_s=Exprimes Voix=Arthaud AE=Roussel AL=Macron
AS=Lassalle AZ=Le_Pen BG=Zemmour BN=Melenchon BU=Hidalgo CB=Jadot CI=Pecresse CP=Poutou CW=Dupont_Aignan;
RUN; 

DATA Source.Subcom_t1_c;
SET Subcom_t1;
RENAME Inscrit=Inscrits;
RUN;


PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\Circonscriptions_législatives\Cirlg_t1"
Out=Cirlg_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Cirlg_t1;
SET Cirlg_t1;
Keep Code_du_d_partement Libell__du_d_partement Code_de_la_circonscription Libell__de_la_circonscription Inscrit Abstentions Votants
Blancs Nuls Exprim_s Voix AE AL AS AZ BG BN BU CB CI CP CW;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AE="Voix Roussel" AL="Voix Macron" AS="Voix Lassalle" AZ="Voix Le Pen" BG="Voix Zemmour" BN="Voix Mélenchon"
BU="Voix Hidalgo" CB="Voix Jadot" CI="Voix Pécresse" CP="Voix Poutou" CW="Voix Dupont-Aignan" ;
Rename Libell__du_d_partement=Departement Libell__de_la_circonscription=Circonscription Exprim_s=Exprimes Voix=Arthaud AE=Roussel 
AL=Macron AS=Lassalle AZ=Le_Pen BG=Zemmour BN=Melenchon BU=Hidalgo CB=Jadot CI=Pecresse CP=Poutou CW=Dupont_Aignan;
RUN;

DATA Source.Cirlg_t1_c;
SET Cirlg_t1;
RENAME Inscrit=Inscrits;
RUN;


PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\1er Tour\Bureaux_de_vote\Burvot_t1"
Out=Burvot_t1
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Source.Burvot_t1;
SET Burvot_t1;
Keep Code_du_d_partement Libell__du_d_partement Code_de_la_circonscription Libell__de_la_circonscription Code_de_la_commune 
Libell__de_la_commune Code_du_b_vote Inscrit Abstentions Votants Blancs Nuls Exprim_s Voix AG AN AU BB BI BP BW CD CK CR CY;
Inscrit=INPUT(Inscrits,12.);
LABEL Voix="Voix Arthaud" AG="Voix Roussel" AN="Voix Macron" AU="Voix Lassalle" BB="Voix Le Pen" BI="Voix Zemmour" BP="Voix Mélenchon"
BW="Voix Hidalgo" CD="Voix Jadot" CK="Voix Pécresse" CR="Voix Poutou" CY="Voix Dupont-Aignan";
Rename Libell__du_d_partement=Departement Libell__de_la_circonscription=Circonscription Libell__de_la_commune=Commune Exprim_s=Exprimes
Voix=Arthaud AG=Roussel AN=Macron AU=Lassalle BB=Le_Pen BI=Zemmour BP=Melenchon BW=Hidalgo CD=Jadot CK=Pecresse CR=Poutou CY=Dupont_Aignan;
RUN;

DATA Source.Burvot_t1_c;
SET Burvot_t1;
RENAME Inscrit=Inscrits;
RUN;


/*2ème Tour*/

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\Régions\Reg_t2"
Out=Reg_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Reg_t2_c;
SET Source.Reg_t2;
Keep Code_de_la_r_gion Libell__de_la_r_gion Inscrits Abstentions Votants Blancs Nuls Exprim_s Voix AA;
LABEL Voix="Voix Macron" AA="Voix Le Pen";
Rename Libell__de_la_r_gion=Region Exprim_s=Exprimes Voix=Macron AA=Le_Pen;
RUN;

/*Test Creation d'une variable ...
DATA Reg_t2_c;
SET Source.Reg_t2_c;
TEST= macron/exprimes;
RUN;
*/

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\France_entière\Fe_t2"
Out=Fe_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Fe_t2_c;
SET Fe_t2;
Keep Code_du_niveau Libell__du_niveau Inscrits Abstentions Votants Blancs Nuls Exprim_s Voix AC;
LABEL Voix="Voix Macron" AC="Voix Le Pen";
Rename Libell__du_niveau=Niveau Exprim_s=Exprimes Voix=Macron AC=Le_Pen;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\France_entière\Fe_metrop_t2"
Out=Fe_metrop_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Fe_metrop_t2_c;
SET Fe_metrop_t2;
Keep Code_du_niveau Libell__du_niveau Inscrits Abstentions Votants Blancs Nuls Exprim_s Voix AC;
LABEL Voix="Voix Macron" AC="Voix Le Pen";
Rename Libell__du_niveau=Niveau Exprim_s=Exprimes Voix=Macron AC=Le_Pen;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\France_entière\Fe_outmer_t2"
Out=Fe_outmer_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Fe_outmer_t2_c;
SET Fe_outmer_t2;
Keep Code_du_niveau Libell__du_niveau Inscrits Abstentions Votants Blancs Nuls Exprim_s Voix AC;
LABEL Voix="Voix Macron" AC="Voix Le Pen";
Rename Libell__du_niveau=Niveau Exprim_s=Exprimes Voix=Macron AC=Le_Pen;
RUN;

DATA Source.Fe_tot_t2;
SET Fe_t2_c Fe_metrop_t2_c Fe_outmer_t2_c;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\Départements\Dpt_t2"
Out=Dpt_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Source.Dpt_t2_c;
SET Dpt_t2;
Keep Code_du_d_partement Libell__du_d_partement Inscrits Abstentions Votants Blancs Nuls Exprim_s Voix AA;
LABEL Voix="Voix Macron" AA="Voix Le Pen";
Rename Libell__du_d_partement=Departement Exprim_s=Exprimes Voix=Macron AA=Le_Pen;
RUN;

PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\Communes\Subcom_t2"
Out=Subcom_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Source.Subcom_t2_c;
SET Subcom_t2;
Keep Code_du_d_partement Libell__du_d_partement Code_de_la_commune Libell__de_la_commune Inscrits Abstentions Votants Blancs Nuls
Exprim_s Voix AE;
LABEL Voix="Voix Macron" AE="Voix Le Pen";
Rename Libell__du_d_partement=Departement Libell__de_la_commune=Commune Exprim_s=Exprimes Voix=Macron AE=Le_Pen;
RUN; 


PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\Circonscriptions_législatives\Cirlg_t2"
Out=Cirlg_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Source.Cirlg_t2_c;
SET Cirlg_t2;
Keep Code_du_d_partement Libell__du_d_partement Code_de_la_circonscription Libell__de_la_circonscription Inscrits Abstentions Votants
Blancs Nuls Exprim_s Voix AE;
LABEL Voix="Voix Macron" AE="Voix Le Pen";
Rename Libell__du_d_partement=Departement Libell__de_la_circonscription=Circonscription Exprim_s=Exprimes Voix=Macron AE=Le_Pen;
RUN;


PROC import datafile="\\filer-eleves2\id2237\PROJET\Source\2eme Tour\Bureaux_de_vote\Burvot_t2"
Out=Burvot_t2
Dbms=xlsx
replace;
getnames=yes;
run;

DATA Source.Burvot_t2_c;
SET Burvot_t2;
Keep Code_du_d_partement Libell__du_d_partement Code_de_la_circonscription Libell__de_la_circonscription Code_de_la_commune
Libell__de_la_commune Code_du_b_vote Inscrits Abstentions Votants Blancs Nuls Exprim_s Voix AG;
LABEL Voix="Voix Macron" AG="Voix Le Pen";
Rename Libell__du_d_partement=Departement Libell__de_la_circonscription=Circonscription Libell__de_la_commune=Commune
Exprim_s=Exprimes Voix=Macron AG=Le_Pen;
RUN;

