// This code fits a quadratic regression model to the
// a moving window that is passed over an array (vector) of data
// It does so by fitting an orthogonal or near-orthogonal model and then making
// simple calculations to "correct" the parameter estimates so that the end
// result is the same as a regular quadratic regression model

// Landon Sego, 2008-01-10

/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>

void fitQ(double *y,   // The data array
          int *nNAy,   // Indicator array corresponding to the data array:  1 is not missing, 0 is missing
          int *n,      // The length of y
          double *x1,  // The original linear predictor (uncentered), assumed to be strictly monotonic (increasing or decreasing)
          double *x1c, // The _centered_ linear predictor, assumed to be strictly monotonic (increasing or decreasing)
          double *x2c, // The _centered_ quadratic predictor
          double *mX1, // The mean of the linear predictor (before centering)
          double *mX2, // The mean of the quadratic precitor (before centering)
          int *orthog, // Indicator of whether x1 and x2 are orthogonal
          int *bwidth, // The bandwidth of the window. The length of window is (2 * *bw + 1). Must be >= 1
          int *mwin,   // The minimum number of data points allowed for the model to be fit to the window (must be <= window length)
          int *winCen, // The indexes of y that indicate the center of the windows
          int *numWin, // The number of windows to fit
          double *a,   // The intercept coefficients of the regression model (= the mean of window)
          double *b,   // The linear coefficients
          double *c,   // The quadratic coefficients
          double *d)   // The RMSE values
{
    int win_i,           // Index of the elements in *winCen
        i,               // Index for the corresponding elements of the data array
        j,               // Index the elements in the moving window contained in 'win'
        k,               // A convenience index, a function of i, j, and bw
        bw = *bwidth,    // The bandwidth of the window
        nw = 2 * bw + 1, // The length of the window contained in 'win'
        nn = *n,         // The total length of y, the data array
        indMissing,      // Bernoulli indicator of whether there are any missing data in the window
        winC[nw],        // Bernoulli array of the elements of the window that are missing (1=present, 0=missing)
        first_x1,        // Window index of first non-missing data point in the window
        last_x1,         // Window index of last non-missing data point in the window
        getfirstx1,      // A flag to aid in finding the first_x1
        ortho = *orthog, // A flag indicating whether the model is fully orthogonal
        countI;          // An integer count of the number of non-missing data points in the window

    double win[nw],      // Array containing the moving window of data
           sumX1sq = 0,  // sum(x1_j^2)
           sumX2sq = 0,  // sum(x2_j^2)
           N = nw,       // double version of the number of data points in the window
           A,            // Value of the intercept for i_th element of y
           B,            // Value of the slope for i_th element of y
           C,            // Value of the quadratic term for i_th element of y
           resid,        // Value of the residual for the i_th element of y and the j_th element of the window
           SSE,          // Sum of squared residuals
           mx1 = *mX1,   // Non-pointer version of *mX1
           mx2 = *mX2,   // Non-pointer version of *mX2

           //  The following are used as temporary variables to calculate the parameter estimates
           count,        // Number of non-missing data points in the window
           sY,           // sum(y_j)
           sX1sq,        // sum(x1_j^2)
           sX2sq,        // sum(x2_j^2)
           sX1X2,        // sum(x1_j * x2_j)
           sX1Y,         // sum(x1_j * y_j)
           sX2Y,         // sum(x2_j * y_j)
           x1new[nw],    // New linear predictor, centered to reflect the NA's in the window
           x2new[nw],    // New quadratic predictor, centered to reflect the NA's in the window
           sx1new,       // sum(x1new_j)
           sx2new,       // sum(x2new_j)
           mx1new,       // mean(x1new_j)
           mx2new,       // mean(x2new_j)
           det;          // Suppose Z were the model matrix and X is a partition of Z such that
                         // X = Z[2:3,2:3].  Then 'det' is the determinant of X'X.

    // Sanity checks
    if (bw < 1)
      error("The bandwidth must be >= 1\n");
    if (*mwin < 3)
      error("The minimum number of non-missing data points in the window must be >= 3\n (since >= 3 points are required to fit a quadratic model)\n)");
    if (*mwin > nw)
      error("The minimum number of non-missing data points in the window\n  must be <= window length (2*bandwidth + 1)\n");
    if (nn < *mwin)
      error("The length of the data vector must be >= minimum number of\n  non-missing data points in the window\n");

    // Calculate Sum of x1^2, x2^2.  These are the only quanitites in the calculations
    // that don't depend on y (or on the missing values in y)
    for (j=0; j < nw; j++) {
      sumX1sq += x1c[j] * x1c[j];
      sumX2sq += x2c[j] * x2c[j];
    }

    // Loop over the center of each window that will be grabbed from y
    for (win_i = 0; win_i < *numWin; win_i++) {

      // Identify the index for the center of this window
      i = winCen[win_i];

      // Create the win array that contains the window of interest
      // If not near the edge of the series
      if (!(i - bw < 0 || i + bw >= nn)) {
        for (j = 0; j < nw; j++) {
          k = i - bw + j;
          win[j] = y[k];
          winC[j] = nNAy[k];
        }
      }

      // else near the edge of the series:  pad the edges with NA's as necessary
      else {
        for (j = 0; j < nw; j++) {
          k = i - bw + j;
          if (!(k < 0 || k >= nn)) {
            win[j] = y[k];
            winC[j] = nNAy[k];
          }
          else {
            win[j] = NA_REAL;
            winC[j] = 0;
          }
        }
      }

      // Initialize the variable which indicates if any of the
      // data in the window are missing
      indMissing = 0;

      // Initialize the needed variables
      sY = 0; sX1Y = 0; sX2Y = 0; SSE = 0;

      // If ortho
      if (ortho) {

        // Loop over the window to calculate sum(Y_i), sum(X1_i * Y_i), sum(X2_i * Y_i)
        // Which are the components of the least squares estimates when the predictors are orthogonal
        for (j = 0; j < nw; j++) {
          if (winC[j]) {
            sY += win[j];
            sX1Y += x1c[j] * win[j];
            sX2Y += x2c[j] * win[j];
          }
          else {
            indMissing = 1;
            break;
          }

        } // for (j = 0; ... Finish looping over the window

      } // if x1 and x2 orthogonal

      // else x1 and x2 not orthogonal
      else {

        sX1X2 = 0; det = 0;

        // Loop through the window again calculating x2new and summary statistics used
        // to calculate the coefficients
        for (j = 0; j < nw; j++) {
          if (winC[j]) {
            sY += win[j];
            sX1X2 += x1c[j] * x2c[j];
            sX1Y += x1c[j] * win[j];
            sX2Y += x2c[j] * win[j];
          }
          else {
            indMissing = 1;
            break;
          }
        }

      } // else not ortho

      // Assign the values of the coefficients if there were no missing points
      if (!indMissing) {

        // Calculate parameter estimates
        if (ortho) {
          // Parameter values needed for residuals estimates
          A = sY / N;
          B = sX1Y / sumX1sq;
          C = sX2Y / sumX2sq;
        }
        else {
          // Parameter values needed for residuals estimates
          A = sY / N;
          det = sumX1sq * sumX2sq - sX1X2 * sX1X2;
          B = (sumX2sq * sX1Y - sX1X2 * sX2Y) / det;
          C = (sumX1sq * sX2Y - sX1X2 * sX1Y) / det;
        }

        // Assign to output arrays, 'correcting' the orthogonal parameter estimates
        // so that they end up being the same as the estimates for the
        // model y = a + b*x1 + c*x1^2 + e where x1 is not centered
        a[win_i] = A - B * mx1 + C * (mx1 * mx1 - mx2);
        b[win_i] = B - 2 * C * mx1;
        c[win_i] = C;

        // Calculate sqrt(MSE) = RMSE
        if (nw > 3) {

          for (j = 0; j < nw; j++) {
            resid = win[j] - A - B * x1c[j] - C * x2c[j];
            SSE += resid * resid;
          }

          d[win_i] = sqrt(SSE / (N - 3));

        } // If nw > 3

        // when there are only 3 data points, the residual is 0.
        else if (nw == 3)
          d[win_i] = 0;
        else
          error("1: There were fewer than 3 data points in the window. This error should not have happened.\n");

      } // if there were no missing values in the window

      // If there are missing values in window re-calculate the solutions to the
      // normal equations
      else {

        // Initialize the needed variables
        countI = 0; sY = 0; sX1sq = 0; sX2sq = 0; sX1X2 = 0; sX1Y = 0; sX2Y = 0;
        sx1new = 0; sx2new = 0; first_x1 = bw + 1; last_x1 = bw - 1; getfirstx1 = 1;

        // Create the array that indicates the location of missing values,
        // count the number of values present in the window,
        // sum the x1 values in the window.  Will assume that x1 is ordered smallest to largest
        // Identify the min and max x1 values
        for (j = 0; j < nw; j++) {
          if (winC[j]) {
            sx1new += x1[j];
            countI += 1;
            last_x1 = j;
            if (getfirstx1) {
              first_x1 = j;
              getfirstx1 = 0;
            }
          }
        }

        // Get the double version of countI
        count = countI;

        // Verify that window is of sufficient size and that there is at least one data point on each side
        // of the window, where the center point qualifies as a 'side'. It ensures there won't be a window that
        // looks like this:  NA NA NA NA NA NA NA NA NA 7 2 3 l 4 7

        // The following 3 conditions are being checked:
        // 1. Num of nonNA is at least as big as the min window size
        // 2. First nonNA occurs on or before center point of window
        // 3. Last nonNA occurs on or after center point of window

        if ((countI >= *mwin) & (first_x1 <= bw) & (last_x1 >= bw)) {

          // Calculate the mean of the new x1's
          mx1new = sx1new / count;

          // Re-center the linear and quadratic predictors.  They will be orthogonal to the intercept,
          // but not to each other

          // Center the x1new and begin creating x2new
          for (j = 0; j < nw; j++) {
            if (winC[j]) {
              x1new[j] = x1[j] - mx1new;
              x2new[j] = x1new[j] * x1new[j];
              sx2new += x2new[j];
            }
          }

          // Calculate the mean of the new x2's
          mx2new = sx2new / count;

          // Loop through the window again calculating x2new and summary statistics used
          // to calculate the coefficients
          for (j = 0; j < nw; j++) {
            if (winC[j]) {
              x2new[j] = x2new[j] - mx2new;
              sY += win[j];
              sX1sq += x1new[j] * x1new[j];
              sX2sq += x2new[j] * x2new[j];
              sX1X2 += x1new[j] * x2new[j];
              sX1Y += x1new[j] * win[j];
              sX2Y += x2new[j] * win[j];
            }
          }

          // Now calculate the parameter estimates (coefficients)
          // A is orthogonal to B & C, but B & C are not orthogonal to each other
          A = sY / count;
          det = sX1sq * sX2sq - sX1X2 * sX1X2;
          B = (sX2sq * sX1Y - sX1X2 * sX2Y) / det;
          C = (sX1sq * sX2Y - sX1X2 * sX1Y) / det;

          // Assign to output arrays, 'correcting' the orthogonal parameter estimates
          // so that they end up being the same as the estimates for the
          // model y = a + b*x1 + c*x1^2 + e where x1 is not centered
          a[win_i] = A - B * mx1new + C * (mx1new * mx1new - mx2new);
          b[win_i] = B - 2 * C * mx1new;
          c[win_i] = C;

          // Calculate sqrt(MSE) = RMSE
          if (countI > 3) {

            for (j = 0; j < nw; j++) {
              if (winC[j]) {
                resid = win[j] - A - B * x1new[j] - C * x2new[j];
                SSE += resid * resid;
              }
            }
            d[win_i] = sqrt(SSE / (count - 3));

          } // If count > 3

          // when there are only 3 data points, the residual is 0.
          else if (countI == 3)
            d[win_i] = 0;
          else
            error("2: There were fewer than 3 data points in the window. This error should not have happened.\n");

        } // if the window did have sufficient data points to fit the quadratic model

        // else the window did not have sufficient data points to fit the quadratic model
        else {
          a[win_i] = NA_REAL;
          b[win_i] = NA_REAL;
          c[win_i] = NA_REAL;
          d[win_i] = NA_REAL;
        }

      } // else if (indMissing) : if there were missing values in the window

    } // for (win_i -- (initial loop)

} // end fitQ

