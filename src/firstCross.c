#include <R.h>
#include <Rinternals.h>

SEXP firstCross(SEXP x, SEXP th, SEXP rel, SEXP start)
{
    int i, int_rel, int_start, P=0;
    double *real_x=NULL, real_th;

    if(ncols(x) > 1)
        error("only univariate data allowed");

    /* return number of observations if relationship is never TRUE */
    SEXP result = ScalarInteger(nrows(x));

    /* Use integers if both x and th are integers */
    int *int_x=NULL, int_th;
    if (TYPEOF(x) == INTSXP && TYPEOF(th) == INTSXP) {
        int_x = INTEGER(x);
        int_th = asInteger(th);
        int_rel = asInteger(rel);
        int_start = asInteger(start)-1;

        switch(int_rel) {
            case 1:  /* >  */
                for(i=int_start; i<nrows(x); i++)
                    if(int_x[i] >  int_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 2:  /* <  */
                for(i=int_start; i<nrows(x); i++)
                    if(int_x[i] <  int_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 3:  /* == */
                for(i=int_start; i<nrows(x); i++)
                    if(int_x[i] == int_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 4:  /* >= */
                for(i=int_start; i<nrows(x); i++)
                    if(int_x[i] >= int_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 5:  /* <= */
                for(i=int_start; i<nrows(x); i++)
                    if(int_x[i] <= int_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 6:  /* != */
                for(i=int_start; i<nrows(x); i++)
                    if(int_x[i] != int_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            default:
                error("unsupported relationship operator");
      }
    } else {
        /* this currently only works for real x and th arguments
         * support for other types may be added later */
        PROTECT(x = coerceVector(x, REALSXP)); P++;
        real_x = REAL(x);
        real_th = asReal(th);
        int_rel = asInteger(rel);
        int_start = asInteger(start)-1;

        switch(int_rel) {
            case 1:  /* >  */
                for(i=int_start; i<nrows(x); i++)
                    if(real_x[i] >  real_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 2:  /* <  */
                for(i=int_start; i<nrows(x); i++)
                    if(real_x[i] <  real_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 3:  /* == */
                for(i=int_start; i<nrows(x); i++)
                    if(real_x[i] == real_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 4:  /* >= */
                for(i=int_start; i<nrows(x); i++)
                    if(real_x[i] >= real_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 5:  /* <= */
                for(i=int_start; i<nrows(x); i++)
                    if(real_x[i] <= real_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            case 6:  /* != */
                for(i=int_start; i<nrows(x); i++)
                    if(real_x[i] != real_th) {
                        result = ScalarInteger(i+1);
                        break;
                    }
                break;
            default:
                error("unsupported relationship operator");
      }
  }
  UNPROTECT(P);
  return(result);
}

