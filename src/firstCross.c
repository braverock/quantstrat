#include <R.h>
#include <Rinternals.h>

SEXP firstCross(SEXP x, SEXP th, SEXP rel, SEXP start)
{
    int i, int_rel, int_start, P=0;
    double *real_x=NULL, real_th;

    if(ncols(x) > 1)
        error("only univariate data allowed");

    /* this currently only works for real x and th arguments
     * support for other types may be added later */
    PROTECT(x = coerceVector(x, REALSXP)); P++;
    real_th = asReal(th);
    int_rel = asInteger(rel);
    int_start = asInteger(start)-1;

    /* return number of observations if relationship is never TRUE */
    SEXP result = ScalarInteger(nrows(x));

    switch(int_rel) {
        case 1:  /* >  */
            real_x = REAL(x);
            for(i=int_start; i<nrows(x); i++)
                if(real_x[i] >  real_th) {
                    result = ScalarInteger(i+1);
                    break;
                }
            break;
        case 2:  /* <  */
            real_x = REAL(x);
            for(i=int_start; i<nrows(x); i++)
                if(real_x[i] >  real_th) {
                    result = ScalarInteger(i+1);
                    break;
                }
            break;
        case 3:  /* == */
            real_x = REAL(x);
            for(i=int_start; i<nrows(x); i++)
                if(real_x[i] >  real_th) {
                    result = ScalarInteger(i+1);
                    break;
                }
            break;
        case 4:  /* >= */
            real_x = REAL(x);
            for(i=int_start; i<nrows(x); i++)
                if(real_x[i] >  real_th) {
                    result = ScalarInteger(i+1);
                    break;
                }
            break;
        case 5:  /* <= */
            real_x = REAL(x);
            for(i=int_start; i<nrows(x); i++)
                if(real_x[i] >  real_th) {
                    result = ScalarInteger(i+1);
                    break;
                }
            break;
        default:
            error("unsupported relationship operator");
  }
  UNPROTECT(P);
  return(result);
}

