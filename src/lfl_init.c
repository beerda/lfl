#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP goedelImpl(SEXP, SEXP);
extern SEXP goguenImpl(SEXP, SEXP);
extern SEXP involNeg(SEXP);
//extern SEXP isSpecific(SEXP);
extern SEXP lukasImpl(SEXP, SEXP);
extern SEXP lukConorm(SEXP, SEXP);
extern SEXP lukNorm(SEXP, SEXP);
extern SEXP maxConorm(SEXP, SEXP);
extern SEXP minNorm(SEXP, SEXP);
//extern SEXP multCpp(SEXP, SEXP, SEXP);
//extern SEXP perceiveGlobal(SEXP);
extern SEXP prodConorm(SEXP, SEXP);
extern SEXP prodNorm(SEXP, SEXP);
//extern SEXP reduceCpp(SEXP);
//extern SEXP search(SEXP, SEXP);
extern SEXP strictNeg(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"goedelImpl",     (DL_FUNC) &goedelImpl,     2},
    {"goguenImpl",     (DL_FUNC) &goguenImpl,     2},
    {"involNeg",     (DL_FUNC) &involNeg,         1},
    //{"isSpecific",     (DL_FUNC) &isSpecific,     1},
    {"lukasImpl",      (DL_FUNC) &lukasImpl,      2},
    {"lukConorm",      (DL_FUNC) &lukConorm,      2},
    {"lukNorm",        (DL_FUNC) &lukNorm,        2},
    {"maxConorm",      (DL_FUNC) &maxConorm,      2},
    {"minNorm",        (DL_FUNC) &minNorm,        2},
    //{"multCpp",           (DL_FUNC) &multCpp,           3},
    //{"perceiveGlobal", (DL_FUNC) &perceiveGlobal, 1},
    {"prodConorm",     (DL_FUNC) &prodConorm,     2},
    {"prodNorm",       (DL_FUNC) &prodNorm,       2},
    //{"reduceCpp",         (DL_FUNC) &reduceCpp,         1},
    //{"search",         (DL_FUNC) &search,         2},
    {"strictNeg",     (DL_FUNC) &strictNeg,       1},
    {NULL, NULL, 0}
};

void R_init_lfl(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
