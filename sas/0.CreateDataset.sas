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
            B.EXCHRATE,
            B.*

            FROM
            BS.BS_BANK AS A,
            __BSFIN__ AS B

            WHERE
            A.INDEX = B.INDEX AND
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
     VAR = PD_PU60_US_SEL3,
     WEIGHT = );

    PROC SORT DATA = &IN. OUT = __T; BY &BY; RUN; 
    PROC UNIVARIATE
        DATA = __T
        OUTTABLE = &OUT.
        NOPRINT;
        BY &BY.;
        VAR &VAR.;

        %IF "&WEIGHT." NE "" %THEN
            %DO;
            WEIGHT &WEIGHT.;
            %END;
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
        BY = _NAME_ _LABEL_ CTRYCODE DATE,
        VAR = COL1
        );

    %MEND;

%MACRO BANK_DATASET_SELECTION(
    OUT=
    );
    %PREPROCESS_BANKSCOPE(OUT = __BSFIN__);

    PROC SQL;
        CREATE TABLE &OUT. AS
            SELECT
            INDEX,
            DATE,
            YEAR,
            NAME,
            CONSOL,
            CTRYCODE,
            EXCHRATE,

            /* ============================================================== */
            /* SIZE                                                           */
            /* ============================================================== */
            DATA2055,
            CASE
            WHEN UNIT = "bil" THEN (EXCHRATE * DATA2025 * 1000)
            WHEN UNIT = "mil" THEN (EXCHRATE * DATA2025)
            WHEN UNIT = "th" THEN (EXCHRATE * DATA2025 / 1000) 
            ELSE (EXCHRATE * DATA2025)
            END AS TOTASSUSD,            
            LOG(CALCULATED TOTASSUSD)    AS LOGASS       LABEL "Logarithm of Total Book Assets",

            /* ============================================================== */
            /* CAPITAL                                                        */
            /* ============================================================== */
            DATA2055 / DATA2025          AS DATA2055_R   LABEL "Equity / Total Assets",
            DATA30680/100                AS C16          LABEL "Regulatory Tier 1 Capital Ratio",
            (DATA30690 - DATA30680)/100  AS C17          LABEL "Regulatory Tier 2 Capital Ratio" ,
            DATA18342/DATA2025           AS C15          LABEL "Risk-Weighted Assets / Total Book Assets",
            /* ============================================================== */
            /* ASSET QUALITY                                                  */
            /* ============================================================== */
            (DATA18215)/100              AS DATA18215    LABEL "Unreserved Impaired Loans / Equity",
            DATA2095 / DATA2001          AS C26          LABEL "Loan Loss Provisions / Gross Loans",        
            /* ============================================================== */
            /* MANAGEMENT QUALITY                                             */
            /* ============================================================== */
            (DATA18070)/100              AS DATA18070    LABEL "Non-Interest Expense/ Gross Revenues",
            DATA18045 / DATA2025         AS DATA18045_R  LABEL "Total Non-Interest Expenses / Total Assets",
            /* ============================================================== */
            /* EARNING QUALITY                                                */
            /* ============================================================== */
            DATA4024/100                 AS DATA4024     LABEL "Return On Avg Assets (ROA)" ,
            DATA4025/100                 AS DATA4025     LABEL "Return On Avg Equity (ROE)",
            DATA4018 / DATA2025          AS DATA4018_R   LABEL "Net Interest Margin / Total Assets",
            DATA18045/100                AS DATA18045    LABEL "Interest Expense / Interest-Bearing Liab.",
            /* ============================================================== */
            /* LIQUIDITY                                                      */
            /* ============================================================== */
            DATA4034/100                 AS DATA4034     LABEL "Net Loans / Tot Dep and Bor",
            DATA4035/100                 AS DATA4035     LABEL "Liquid Assets / Dep and ST Funding"

            FROM __BSFIN__;
    QUIT;

    %MEND;

%MACRO BANK_PD_DATASET(
    OUT=,
    WEIGHT= 
    );
    %BANK_DATASET_SELECTION(OUT = __BSSEL__);

    /* ====================================================================== */
    /* STANDARDIZE VARIABLES (BANKING MODEL WAS RAN ON STANDARDIZED           */
    /* VARIABLES)                                                             */
    /* ====================================================================== */
    PROC STDIZE
    DATA = __BSSEL__
        OUT = __T__
        METHOD = STD;
       VAR DATA2055--DATA4035;
    RUN;    
    

    DATA __T2__;
        SET __T__;

        SC_CLOSURE_ALL = -3.21 * C16 + 0.365 * C15 + 0.199 * DATA18215 + 0.284 * C26 + 0.724 * DATA18045 - 0.33 * DATA4018_R + 0.145 * DATA18070;
        SC_OBR_EU = -0.715 * C16 + 0.369 * C15 + 0.087 * DATA18215 + 0.271 * C26;
    RUN;

    PROC SORT DATA = __T2__; BY CTRYCODE INDEX DATE; RUN;
    PROC TRANSPOSE
    DATA = __T2__ 
        OUT = __T2_LONG__;
        BY CTRYCODE INDEX DATE;
        VAR DATA2055 -- SC_OBR_EU;
    RUN;

    %IF "&WEIGHT." NE "" %THEN
        %DO;
        PROC SQL;
            CREATE TABLE __T2_LONG__ AS
                SELECT
                A.*,
                B.&WEIGHT.
                FROM
                __T2_LONG__ AS A,
                __BSSEL__ (KEEP = INDEX DATE &WEIGHT.) AS B
                WHERE
                A.INDEX = B.INDEX AND
                A.DATE = B.DATE;
        QUIT;
        %END;

    %AGGREGATE(
        IN = __T2_LONG__,
        OUT = &OUT.,
        BY = _NAME_ _LABEL_ CTRYCODE DATE,
        VAR = COL1,
        WEIGHT = &WEIGHT.
        );
    
    
    %MEND;



%MACRO BANK_PD_DATASET_INDIVIDUAL(
    OUT=
    );
    %BANK_DATASET_SELECTION(OUT = __BSSEL__);

    /* ====================================================================== */
    /* STANDARDIZE VARIABLES (BANKING MODEL WAS RAN ON STANDARDIZED           */
    /* VARIABLES)                                                             */
    /* ====================================================================== */
    PROC STDIZE
    DATA = __BSSEL__
        OUT = __T__
        METHOD = STD;
       VAR DATA2055_R--DATA4035;
    RUN;    
    

    DATA __T2__;
        SET __T__;

        SC_CLOSURE_ALL = -3.21 * C16 + 0.365 * C15 + 0.199 * DATA18215 + 0.284 * C26 + 0.724 * DATA18045 - 0.33 * DATA4018_R + 0.145 * DATA18070;
        SC_OBR_EU = -0.715 * C16 + 0.369 * C15 + 0.087 * DATA18215 + 0.271 * C26;
    RUN;

    PROC SORT DATA = __T2__ OUT = &OUT.; BY CTRYCODE INDEX DATE; RUN;

    %MEND;
