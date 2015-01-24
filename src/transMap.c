// This C routine applies a discrete, 1-1 map, say m, to the vector Y
// such that the first differences of m(Y) uniquely identify
// the transitions between all the possible values of Y.
// It creates the map, calculates the first differences of m(Y),
// and produces a set of transition labels that indicate which
// value of m(Y) corresponds to each of the possible transitions
// among the unique states of Y.

//  Landon Sego, January 30, 2008

#include <R.h>

void transMap(int *Y,
              int *nY,
              int *unique,
              int *nU,
              int *map,
              int *ntrans,
              int *diffY,
              int *from,
              int *to,
              int *transValue)
{
  int i, j, k;

  // Assign the power mapping
  for (i=0; i < *nY; i++) {

    for (j=0; j < *nU; j++) {

      if (Y[i] == unique[j]) {

        Y[i] = map[j];
        break;

      }
    }
  }

  // First differences (today - tomorrow)
  for (i=0; i < *nY - 1; i++)
    diffY[i+1] = Y[i] - Y[i+1];


  // Full set of transition labels
  for (i=0; i < *nU; i++) {
    for (j=0; j < *nU; j++) {
      k = *nU * i + j;
      from[k] = unique[i];
      to[k] = unique[j];
      transValue[k] = map[i] - map[j];
    }
  }

} // transMap
