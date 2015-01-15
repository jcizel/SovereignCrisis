X "cd ~/PACKAGES/SovereignCrisis";

%INCLUDE "./sas/SETENVIR.sas";
%INCLUDE "./sas/0.CreateDataset.sas";
%INCLUDE "../WRDSHelpers/sas/quickfunctions.sas";
%INCLUDE "/wrds/wrdsmacros/winsorize.sas";
%INCLUDE "/wrds/wrdsmacros/nwords.sas";
%INCLUDE "/wrds/wrdsmacros/csv.sas";


PROC DATASETS LIB = BS; QUIT;

%PREPROCESS_BANKSCOPE(OUT = BSFIN);

%CSV(
    INSET = BSFIN,
    OUTSET = /scratch/uvanl/BSFIN);

PROC CONTENTS DATA = BSFIN
    OUT = __VARLIST__ (KEEP = NAME LABEL) VARNUM;
RUN;

%CSV(
    INSET = __VARLIST__,
    OUTSET = /scratch/uvanl/BSFIN_LOOKUP);
