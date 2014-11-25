OPTIONS OBS=MAX NOSYNTAXCHECK;
OPTIONS PS=MAX;

%MACRO PREPROCESS_BANKSCOPE(
    OUT =
    );
    
    %LET RATIOS = DATA2125--DATA4036  DATA18040--DATA18255 DATA30680 DATA30690 DATA30712 DATA30715 DATA30717 DATA38200--DATA38350;

    PROC CONTENTS DATA = BS.BS_FINANCIALS (DROP = &RATIOS INDEX--CPIRATE DATA2025) OUT = __TEMP__ VARNUM NOPRINT; RUN;

    PROC SQL NOPRINT; SELECT NAME INTO :LEVELS SEPARATED BY " " FROM __TEMP__; QUIT;

        /* ------------------------------------------------------------------ */
        /* CONVERT LEVEL VARIABLES TO RATIOS BY DIVIDING BY TOTAL ASSETS      */
        /* ------------------------------------------------------------------ */

    DATA __BSFIN__;
        SET BS.BS_FINANCIALS;

        ARRAY LEVELS {*} &LEVELS;

        DO I = 1 TO DIM(LEVELS);
            LEVELS{I} = LEVELS{I} / DATA2025;
            END;
    RUN;

    PROC SQL;
        CREATE TABLE __TEMP2__ AS
            SELECT
            A.INDEX,
            B.CLOSDATE AS DATE,
            B.CLOSDATE_YEAR AS YEAR,
            A.NAME,
            A.CONSOL,
            A.CTRYCODE,
            B.*

            FROM
            BS.BS_BANK AS A,
            __BSFIN__ AS B

            WHERE
            A.INDEX = B.INDEX AND
            A.CONSOL IN ('U1','U2','C1') AND
            UPCASE(TRIM(A.SPECIAL)) IN
            ('BANK HOLDING & HOLDING COMPANIES',
            'COMMERCIAL BANKS',
            'COOPERATIVE BANK',
            'SAVINGS BANK',
            'GROUP FINANCE COMPANIES',
            'MULTI-LATERAL GOVERNMENT BANKS',
            'REAL ESTATE & MORTGAGE BANK') AND

            DATE IS NOT NULL AND
            A.INDEX IS NOT NULL;
    QUIT;

    /* ====================================================================== */
    /*                          NORMALIZE                                     */
    /* ====================================================================== */
    PROC SORT DATA = __TEMP2__; BY INDEX YEAR; RUN;

    DATA __TEMP2__ ;
        SET __TEMP2__;

        BY INDEX YEAR;

        IF LAST.YEAR THEN
            DO;
            OUTPUT;
            END;    
    RUN;
    
    FILENAME LOGOUT "/scratch/uvanl/temp.log"; 
    PROC PRINTTO LOG=LOGOUT;RUN; 
    PROC EXPAND
    DATA = __TEMP2__ (WHERE = (DATE IS NOT NULL))
        OUT = __TEMP3__
        FROM = YEAR
        TO = YEAR
        METHOD = STEP;

        BY INDEX;
        ID DATE;                            /*MAKE SURE NONE IS MISSING !!!*/
    RUN;
    PROC PRINTTO; 
    RUN; 

    
    /* ====================================================================== */
    /*                    WINSORIZE  VARIABLES                                */
    /* ====================================================================== */
    PROC CONTENTS DATA = BS.BS_FINANCIALS (KEEP = &LEVELS &RATIOS) VARNUM OUT = __VARLIST__ NOPRINT; RUN;
    PROC SQL NOPRINT; SELECT NAME INTO :SELVARS SEPARATED BY " " FROM __VARLIST__; QUIT;
        
        %WINSORIZE(
            INSET = __TEMP3__,
            OUTSET = &OUT.,
            VARS = &SELVARS.,
            SORTVAR = CTRYCODE DATE,
            PERC1 = 1,
            TRIM = 0
            );

    /* %CONTENTS(DATA = __TEMP3__); */
            

    %MEND;

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


%MACRO AGGREGATE_BANKSCOPE(
    OUT=
    );
    %PREPROCESS_BANKSCOPE(OUT = __BSFIN__);

    %CONTENTS(DATA = __BSFIN__);

    PROC SORT DATA = __BSFIN__; BY CTRYCODE INDEX DATE; RUN;
    PROC TRANSPOSE
    DATA = __BSFIN__ 
        OUT = __BSFIN_LONG__;
        BY CTRYCODE INDEX DATE;
        VAR DATA2000 -- DATA38430;
    RUN;

    %CONTENTS(DATA = __BSFIN_LONG__);

    %AGGREGATE(
        IN = __BSFIN_LONG__,
        OUT = &OUT.,
        BY = CTRYCODE DATE,
        VAR = COL1
        );

    %MEND;
