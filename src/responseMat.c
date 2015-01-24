// Quickly builds the response matrix, where each column of the matrix corresponds to a window
// of the vector X.

// Landon Sego, November 2007

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

void responseMat(int *ntu,
                 int *winLen,
                 int *lenX,
                 double *X,
                 double *Xnew)
{
  int i, origin;
  int j;

  for (i=1; i <= *lenX; i++) {

    origin = i * *winLen - *ntu;

    for (j=(-1 * *ntu); j <= *ntu; j++) {

      if ((j + i >= 1) & (j + i <= *lenX))
        Xnew[j+origin-1] = X[j+i-1];
      else
        Xnew[j+origin-1] = NA_REAL;

      //Rprintf("i: %i, origin: %i, j: %i, j+i: %i, j+origin-1: %i, Xnew[j+origin-1]: %f\n",
               //i, origin, j, j+i, j+origin-1, Xnew[j+origin-1]);

    } // for j

  } // for i

} // void responseMat
