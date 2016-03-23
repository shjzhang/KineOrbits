/******************************************************************************
*      RTG Source Code,                                                       *
*      Copyright (C) 1996, California Institute of Technology                 *
*      U.S. Government Sponsorship under NASA Contract NAS7-1260              *
*                    (as may be time to time amended)                         *
*                                                                             *
*      RTG is a trademark of the California Institute of Technology.          *
*                                                                             *
*                                                                             *
*      written by Yoaz Bar-Sever, Willy Bertiger, Bruce Haines,               *
*                 Angelyn Moore, Ron Muellerschoen, Tim Munson,               *
*                 Larry Romans, and Sien Wu                                   *
*                                                                             *
*      modified by Gerhard L.H. Kruizinga for stand alone use                 *
*                 7/22/98                                                     *
******************************************************************************/
/* Interpolates an ECI file to retrieve satellite state at the requested time*/
/* Yoaz Bar-Sever. May, 1996 */

#include <math.h>
#include <stdio.h>
#include "GRACEdefs.h"

#define MAX_DEG 20

static char SccsId[] = "$Id: lagrange_int.c,v 1.5 2004/08/30 22:19:43 wib Exp $";

/* Performs straightforward Lagrange interpolation,
  to get value y(x) and (if requested) derivative y'(x),
  given tables of x-y points xt[] and yt[].  Tables should
  be equally spaced in x; will still work otherwise, but
  search used is stupidest possible.

    ntab = size of tables
    ndeg = degree of polynomial (uses ndeg+1 points around target x,
           symmetrically distributed if possible)
*/

int lagrange_int( double x, double *y, int ntab, double *xt, double *yt, 
                  int ndeg, int compute_deriv, double *yd)
{

  static char SccsId[] = "$Id: lagrange_int.c,v 1.5 2004/08/30 22:19:43 wib Exp $";

  double i_r, yyd;
  int i_shift, i, i1, i2, j, k, n, n2;

  static double w[MAX_DEG], df[MAX_DEG], x0_save, xn_save, x_save;
  double *xi, *yi;
  static int n_save = -1;
  static int ntab_save = -1;
  static int i1_save;
 /*  double ddif[MAX_DEG][MAX_DEG]; */

  /* check if time series is the same and at the same location */

  if (n_save == ndeg + 1 && x == x_save && xt[0] == x0_save
       && xt[ntab-1] == xn_save && ntab == ntab_save && compute_deriv == 0) {
    *y = 0.0;
    yi = yt + i1_save;
    loop(i, n_save) *y += w[i]*yi[i];
    return (int) 0;
  }

  if (x < xt[0] ) { return (int) -1;}
  if (x > xt[ntab-1]) { return (int) 1; }

/*
    fprintf(stderr, "filup: time %.16g out of table range [ %.16g , %.16g ]\n",
      x, xt[0], xt[ntab-1]);
    exit(1);
*/


  if (ntab <= ndeg) {
/*
    fprintf(stderr, "filup: table size = %d, not big enough for degree = %d\n",
      ntab, ndeg);
    exit(1);
*/
    return (int) 2;
  }

  i_r = (ntab - 1) * (x - xt[0])/(xt[ntab-1] - xt[0]);
  i = (int) floor(i_r);
  if (i == ntab - 1) i--;
  i_r = i_r - i;

  if (i == ntab-1) {
    i--;
    i_r++;
  }

  if (x < xt[i] || x > xt[i+1]) {
    i_shift = 0;
    if (x < xt[i]) {
      while (x < xt[i]) { i_shift--; i--; }
    }
    else {
      while (x > xt[i+1]) { i_shift++; i++; }
    }
/*
    EH(EH_warning, "filup: table apparently not evenly spaced;"
      " did dumb search to shift i by %d to %d.", i_shift, i);
*/
    i_r = (x - xt[i])/(xt[i+1] - xt[i]);
  }

  n = ndeg + 1;
  if (n % 2) {
    /* n odd */
    n2 = (n-1)/2;
    if (i_r < 0.5) {
      i1 = i - n2;
      i2 = i + n2;
    }
    else {
      i1 = i - n2 + 1;
      i2 = i + n2 + 1;
    }
  }
  else {
    n2 = n/2;
    i1 = i - n2 + 1;
    i2 = i + n2;
  }
  
  if (i1 < 0) {
    i1 = 0;
    i2 = n - 1;
  }
  if (i2 >= ntab) {
    i2 = ntab - 1;
    i1 = ntab - n;
  }

  i1_save = i1;
  xi = xt + i1;
  yi = yt + i1;

  loop(i,n) {
    df[i] = 1.0;
    loop(j,n) {
      if (j != i) {
        df[i] /= (xi[i] - xi[j]);
      }
    }
  }

  *y = 0.0;
  loop(i,n) {
    w[i] = df[i];
    loop(j,n) {
      if (j != i) {
        w[i] *= (x - xi[j]);
      }
    }
    *y += w[i]*yi[i];
  }

  if (compute_deriv == 1) {
    *yd = 0.0;
    for (i = i1; i <= i2; i++) {
      for (j = i1; j <= i2; j++) {
        if (j != i) {
          yyd = yt[i]/(xt[i] - xt[j]);
          for (k = i1; k <= i2; k++) {
            if (k != i && k != j) yyd *= (x - xt[k])/(xt[i] - xt[k]);
          }
          *yd += yyd;
        }
      }
    }
  }

  n_save    = n;
  ntab_save = ntab;
  x0_save   = xt[0];
  xn_save   = xt[ntab-1];
  x_save    = x;

  return (int) 0;
}

