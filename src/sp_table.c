// This C method works a lot like the 'table' function in R
// It simply counts the number of values of 'y' that correspond to
// 'unique'.  

// Landon Sego, January 30, 2008

#include <R.h>

void sp_table(int *y,
              int *nY,
              int *unique,
              int *nU,
              int *totals) 
{
  int i, j;

  for (i=0; i < *nY; i++) {

    for (j=0; j < *nU; j++) {

      if (y[i] == unique[j]) {

        totals[j] += 1;
        break;

      }

    }

  }

}
