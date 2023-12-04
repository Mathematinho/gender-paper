PROC IMPORT OUT = all
    DATAFILE = "C:\Users\yxi\University of Texas Southwestern\Summer Student Projects - General\Project 2 Sex disparity in\data\alldata_final.xlsx" 
 dbms = xlsx replace;
 sheet = 'Sheet1';
 getnames = yes;
 datarow = 2;
RUN;

ods select none;
proc logistic data=all;
ods output lsmeans=lsmeans1 diffs=diffs1 LSMEstimates=did1;
class subtype qt source senior_author/ param=glm;
model first_author(event='female') = subtype|source|qt|senior_author @2/hier=single details selection=backward;
lsmeans subtype qt*source senior_author*source/pdiff ilink exp cl plot=meanplot(ilink);
LSMESTIMATE  senior_author*source 1 -1 -1 1 / cl exp;
run;
ods select all;
data diffs1;set diffs1;
if qt ne _qt and source ne _source then delete;
if senior_author ne _senior_author and source ne _source then delete;
keep effect subtype _subtype qt _qt source _source Probz senior_author _senior_author ExpEstimate lowerexp upperexp;
run;
proc sort data=diffs1;by effect;run;
proc multtest inpvalues=diffs1(rename=(Probz=raw_p)) holm noprint out=diffs1;
by effect;
run;


data lsmeans1;set lsmeans1;
keep effect subtype qt source senior_author mu lowermu uppermu ;
run;
data did1;set did1;
keep effect exp lowerexp upperexp Probz ExpEstimate lowerexp upperexp;
run;


proc export data=lsmeans1
  outfile='C:\Users\yxi\University of Texas Southwestern\Summer Student Projects - General\Project 2 Sex disparity in\tables\first lsmeans.xlsx'
  dbms=xlsx
  replace;
  sheet='Sheet1'; /* Specify the sheet name */
run;
proc export data=diffs1
  outfile='C:\Users\yxi\University of Texas Southwestern\Summer Student Projects - General\Project 2 Sex disparity in\tables\first diffs.xlsx'
  dbms=xlsx
  replace;
  sheet='Sheet1'; /* Specify the sheet name */
run;
proc export data=did1
  outfile='C:\Users\yxi\University of Texas Southwestern\Summer Student Projects - General\Project 2 Sex disparity in\tables\first did.xlsx'
  dbms=xlsx
  replace;
  sheet='Sheet1'; /* Specify the sheet name */
run;


