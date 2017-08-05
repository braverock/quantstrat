#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Function definitions */
SEXP firstCross(SEXP x, SEXP th, SEXP rel, SEXP start);

static const
R_CallMethodDef callMethods[] = {
  {"firstCross", (DL_FUNC) &firstCross, 4},
  {NULL, NULL, 0}
};

void R_init_quantstrat(DllInfo *info)
{
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     NULL);

  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);  /* C code not callable outside blotter */
}
