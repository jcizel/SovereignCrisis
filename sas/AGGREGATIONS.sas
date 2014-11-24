%MACRO AGGREGATE
    (IN=,
     OUT=,
     BY= FIC DATE,
     VAR = PD_PU60_US_SEL3);

    PROC SORT DATA = &IN. OUT = __T; BY &BY; RUN; 
    PROC UNIVARIATE
        DATA = __T
        OUTTABLE = &OUT.
        NOPRINT;
        BY &BY.;
        VAR &VAR.;
    RUN;

    PROC SORT DATA = &OUT.; BY &BY; RUN;
    %MEND;
