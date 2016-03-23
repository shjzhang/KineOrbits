/* $Id: cstats.h,v 1.2 2004/08/31 15:45:15 wib Exp $ */

#ifndef _cstats_h_
#define _cstats_h_

typedef struct cstats_internal_t {

  int n;                        /* number of data points seen */
  double s[4];                  /* array to hold accumulated numbers mean, sum_of_sq min max */

} cstats_internal_t;

typedef struct cstats_out_t {

  int n;                        /* number of data points  */
  double stats[5];              /* array mean, sigma, min, max, rms */

} cstats_out_t;


void cstats_new(                 /* initializes and can also be used to reset */
               /* input/output */
               cstats_internal_t *internal
               );

void cstats_add(
 /* input/output */
               cstats_internal_t *internal,
 /* input */
               double val
               );

void cstats_calc(
 /* input */
               cstats_internal_t *internal, /* not modified by cal */

/* output */
               cstats_out_t *out_stats
               );

void cstats_array_calc(
                       /* input */
                       double *v, /* array of n values whose stats are computed */
                       int    n, /* number of elements of v ( v has indices 0..n-1 ) */
                       /* output */
                       cstats_out_t *out_stats
                       );



#endif
