OPTIONS OBS=MAX NOSYNTAXCHECK;
OPTIONS PS=MAX;

%MACRO PREPROCESS_BANKSCOPE(
    OUT =
    );
    
    %LET RATIOS = DATA2125--DATA4036  DATA18040--DATA18255 DATA30680 DATA30690 DATA30712 DATA30715 DATA30717 DATA38200--DATA38350;

    PROC CONTENTS DATA = BS.BS_FINANCIALS (DROP = &RATIOS INDEX--CPIRATE DATA2025) OUT = __TEMP__ VARNUM; RUN;

    PROC SQL; SELECT NAME INTO :LEVELS SEPARATED BY " " FROM __TEMP__; QUIT;

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
            'REAL ESTATE & MORTGAGE BANK');
    QUIT;

    /* ====================================================================== */
    /*                    WINSORIZE  VARIABLES                                */
    /* ====================================================================== */
    PROC CONTENTS DATA = BS.BS_FINANCIALS (KEEP = &LEVELS &RATIOS) VARNUM OUT = __VARLIST__; RUN;
    PROC SQL; SELECT NAME INTO :SELVARS SEPARATED BY " " FROM __VARLIST__; QUIT;
        
        %WINSORIZE(
            INSET = __TEMP2__,
            OUTSET = &OUT.,
            VARS = &SELVARS.,
            SORTVAR = CTRYCODE DATE,
            PERC1 = 1,
            TRIM = 0
            );

    /* %CONTENTS(DATA = __TEMP3__); */
            

    %MEND;


