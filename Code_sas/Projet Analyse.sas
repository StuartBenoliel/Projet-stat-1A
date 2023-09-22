/*Analyse*/

%MACRO vboxw(table,y,groupe); /* y=axe ordonné*/
PROC SGPLOT DATA=&table;
VBOX &y / category=&groupe WEIGHT=Inscrits;
XAXIS display=(nolabel);
/*TITLE "Boxplot des % votes en fonction de la densité des communes";
FOOTNOTE "Données : data.gouv";*/
RUN;
%MEND;

%MACRO vbox(table,y,groupe); /* y=axe ordonné*/
PROC SGPLOT DATA=&table;
HBOX &y / category=&groupe ;
XAXIS display=(nolabel);
/*TITLE "Boxplot des % votes en fonction de la densité des communes";
FOOTNOTE "Données : data.gouv";*/
RUN;
%MEND;


%MACRO hist(table,candidat,groupe);
PROC SGPLOT DATA=&table;
HISTOGRAM &candidat /  Group=&groupe;
DENSITY &candidat / Type=Normal; /*Kernel*/
/*TITLE "Histogramme montrant les parts de chaque % votes en fonction de la densité des communes";*/
RUN;
%MEND;

%MACRO vbar(table,var1,var2);
PROC SGPLOT DATA=&table;
VBAR &var1 / response=&var2 stat=mean  DATALABEL; /*FREQ MEAN MEDIAN PERCENT SUM*/
XAXIS display=(nolabel);
RUN;
%MEND;

%MACRO vbarb(table,var);
PROC SGPLOT DATA=&table;
VBAR &var  / response=voix stat=Mean group=nom SEGLABEL DATALABEL; /*FREQ MEAN MEDIAN PERCENT SUM*/
XAXIS display=(nolabel);
RUN;
%MEND;

%MACRO corPw(table,varQN1,varQN2);/*Quanti*/
PROC CORR DATA=&table;
VAR &varQN1 &varQN2;
WEIGHT Inscrits;
RUN;
%MEND;

%MACRO Regl(table,varQN1,varQN2);/*Quanti*/
PROC REG DATA=&table PLOTS=NONE;
WEIGHT Inscrits;
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

DATA Ext1b;
SET Resultat.ext1;
If qpv not in(0) then output;
RUN;


%vboxw(Bigt1,Voix,Nom);
%vboxw(Bigt2,parts,cat);
%vbox(Resultat.Ext2,Vmacron,ze); 
%hist(Resultat.Ext1,Vmacron,dens); 
%vbar(Resultat.Ext1,equip,vlepen);
%vbarb(Bigt2,ze); 
%corPw(Resultat.Ext1,qpv,Vmacron);/*Quanti*/
%Regl(Resultat.Ext1,chom,Vlassalle);/*Quanti*/
%khi(Resultat.Ext1,ZE,dens);/*Quali*/
%Anova(Resultat.Ext1,ze,Varthaud); /*Quali-Quanti*/

/*%corP(Resultat.Ext1,OU,Vlepen); 0.37*/

/*TEST*/




PROC SGPLOT DATA=Bigt1_2;
VBOX voix/ category=nom group=Tour weight=inscrits;
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
