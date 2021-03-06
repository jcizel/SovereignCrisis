X "cd ~/PACKAGES/SovereignCrisis";

%INCLUDE "./sas/SETENVIR.sas";
%INCLUDE "./sas/0.CreateDataset.sas";
%INCLUDE "../WRDSHelpers/sas/quickfunctions.sas";
%INCLUDE "/wrds/wrdsmacros/winsorize.sas";
%INCLUDE "/wrds/wrdsmacros/nwords.sas";
%INCLUDE "/wrds/wrdsmacros/csv.sas";


PROC DATASETS LIB = BS; QUIT;


%PREPROCESS_BANKSCOPE(OUT = BSFIN);

%CONTENTS(DATA = BSFIN);
PROC FREQ DATA = BSFIN; TABLE DATE; RUN;


%AGGREGATE_BANKSCOPE(OUT = BSFIN_AGGREGATED);    

%CSV(
    INSET = BSFIN_AGGREGATED,
    OUTSET = /scratch/uvanl/BSFIN_AGGREGATED);


%BANK_PD_DATASET(
    OUT = PD_DATASET,
    WEIGHT = TOTASSUSD);

%CSV(
    INSET = PD_DATASET,
    OUTSET = /scratch/uvanl/BANK_PD_DATASET);

%CONTENTS(DATA = PD_DATASET);
PROC PRINT DATA = PD_DATASET (OBS = 100); RUN;
