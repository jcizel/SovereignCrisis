%LET ROOTPATH = ~;

X "cd &ROOTPATH/PACKAGES/SovereignCrisis";

OPTIONS
    OBS=MAX
    NOSYNTAXCHECK
    LS = 100
    PS = MAX;

/* -------------------------------------------------------------------------- */
/* SET LIBRARIES                                                              */
/* -------------------------------------------------------------------------- */
LIBNAME HOME "/scratch/uvanl/";
LIBNAME BS "/wrds/bvd/sasdata/bs";

