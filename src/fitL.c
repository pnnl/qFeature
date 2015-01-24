// This code fits a simple linear regression model to the
// a moving window that is passed over an array (vector) of data

// Landon Sego, 2009-12-19


/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>
#include <R.h>
#include <Rmath.h>

void fitL(double *y,   // The data array
          int *nNAy,   // Indicator array corresponding to the data array:  1 is not missing, 0 is missing
          int *n,      // The length of y
          double *x,   // The original linear predictor (uncentered)
          double *mX,  // The mean of the linear predictor 
          double *ssX, // The sum of squares of the linear predictor (the dot product)
          int *bwidth, // The bandwidth of the window. The length of window is (2 * *bw + 1). Must be >= 1
          int *mwin,   // The minimum number of data points allowed for the model to be fit to the window (must be <= window length)
          int *winCen, // The indexes of y that indicate the center of the windows
          int *numWin, // The number of windows to fit
          double *a,   // The intercept coefficients of the regression model (= the mean of window)
          double *b,   // The linear coefficient
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
        first_x,         // Window index of first non-missing data point in the window
        last_x,          // Window index of last non-missing data point in the window
        getfirstx,       // A flag to aid in finding the first_x
        countI;          // An integer count of the number of non-missing data points in the window

    double win[nw],      // Array containing the moving window of data
           N = nw,       // double version of the number of data points in the window
           A,            // Temporary storage for the intercept for i_th element of y
           B,            // Temporary storage for the slope for i_th element of y
           
           resid,        // Value of the residual for the i_th element of y and the j_th element of the window
           SSE,          // Sum of squared residuals
           mx = *mX,     // Non-pointer version of *mX
           sxx = *ssX,   // Non-pointer version of *ssX
           sY,           // sum(y_j)
           sXY,          // sum(x_j * y_j)

           // These variables are used when data are missing in the window
           count,        // Number of non-missing data points in the window
           sX,           // sum(x_j)
           sXX;          // sum(x_j * x_j)

           

    // Sanity checks
    if (bw < 1)
      error("The bandwidth must be >= 1\n");
    if (*mwin < 2)
      error("The minimum number of non-missing data points in the window must be >= 2\n (since >= 2 points are required to fit a linear model)\n)");
    if (*mwin > nw)
      error("The minimum number of non-missing data points in the window\n  must be <= window length (2*bandwidth + 1)\n");
    if (nn < *mwin)
      error("The length of the data vector must be >= minimum number of\n  non-missing data points in the window\n");

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
      sY = 0; sXY = 0; SSE = 0;

      // Loop over the window to calculate sum(Y_i), sum(X_i * Y_i)
      for (j = 0; j < nw; j++) {
        if (winC[j]) {
          sY += win[j];
          sXY += x[j] * win[j];
        }
        else 
          indMissing = 1;

      } // for (j = 0; ... Finish looping over the window

      // Assign the values of the coefficients if there were no missing points
      if (!indMissing) {

        // Assign the least squares output to the arrays, 
	// can used the pre-calculated values of mx and sxx since none where missing
        B = (sXY - mx * sY) / (sxx - N * mx * mx);
        A = sY / N - B * mx;
        a[win_i] = A;
        b[win_i] = B;

        // Calculate sqrt(MSE) = RMSE
        if (nw > 2) {

          for (j = 0; j < nw; j++) {
            resid = win[j] - A - B * x[j];
            SSE += resid * resid;
          }

          d[win_i] = sqrt(SSE / (N - 2));

        } // If nw > 2

        // when there are only 2 data points, the residual is 0.
        else if (nw == 2)
          d[win_i] = 0;
        else
          error("1: There were fewer than 2 data points in the window. This error should not have happened.\n");

      } // if there were no missing values in the window

      // If there are missing values in window re-calculate the solutions to the
      // normal equations by calculating sXX, sX
      else {

        // Initialize the needed variables
        countI = 0; sX = 0; sXX = 0; 
        first_x = bw + 1; last_x = bw - 1; getfirstx = 1;

        // Create the array that indicates the location of missing values,
        // count the number of values present in the window,
        // sum the x values in the window.  Will assume that x is ordered smallest to largest
        // Identify the min and max x values (this is the ordering assumption)
        for (j = 0; j < nw; j++) {
          if (winC[j]) {
            sX += x[j];
            sXX += x[j] * x[j];
            countI += 1;
            last_x = j;
            if (getfirstx) {
              first_x = j;
              getfirstx = 0;
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

        if ((countI >= *mwin) & (first_x <= bw) & (last_x >= bw)) {

          // Assign the least squares output to the arrays, 
          // can used the pre-calculated values of mx and sxx since none where missing
          B = (sXY - sX * sY / count) / (sXX - sX * sX / count);
          A = (sY - B * sX) / count;
          a[win_i] = A;
          b[win_i] = B;
  
          // Calculate sqrt(MSE) = RMSE
          if (nw > 2) {
  
            for (j = 0; j < nw; j++) {
              if (winC[j]) {
                resid = win[j] - A - B * x[j];
                SSE += resid * resid;
	      }
            }
  
            d[win_i] = sqrt(SSE / (count - 2));
  
          } // If nw > 2
  
          // when there are only 2 data points, the residual is 0.
          else if (nw == 2)
            d[win_i] = 0;
          else
            error("2: There were fewer than 2 data points in the window. This error should not have happened.\n");

        } // if the window did have sufficient data points to fit the linear model

        // else the window did not have sufficient data points to fit the linear model
        else {
          a[win_i] = NA_REAL;
          b[win_i] = NA_REAL;
          d[win_i] = NA_REAL;
        }

      } // else if (indMissing) : if there were missing values in the window

    } // for (win_i -- (initial loop)

} // end fitL

