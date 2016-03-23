/* $Id: atti.h,v 1.8 2004/06/29 19:52:34 scw Exp $ */

#ifndef _atti_h_
#define _atti_h_

#define NSTATE	6	/* number of state parameters */
#define NDYN	12	/* number of dynamic parameters */

#define loop(A,B) for(A=0;A<B;A++)

#include "/goa/local/libs/meschach/test_wib/matrix.h"

typedef struct atti_mem_info {
  double *ts_pq_omega[6];       /* array of pointers used for storing time series of pq and omega by atti */
  double *ts_time;              /* pointer to array of time tags associated with ts_pq_omega */
  int    ts_nobs;               /* number of points in ts_pq_omeaga time series */
  double ts_start;              /* start time of time pq and omega time series */
  double ts_final;              /* final time of time pq and omega time series */
  double ts_dT;                 /* time step in pq and omega time series */
  double epoch_state_update[6]; /* epoch state update for pq and omega */
} atti_mem_info;

typedef struct atti_state {
  double t;                     /* current time */
  double t0;                    /* initial condition time */

  double pq[3];
  double pq0[3];                /* initial proj quaterion at t0 */

  double omega[3];
  double omega0[3];             /* initial angular velocity */

  double scale[3];              /* angualar acc scale */
  double bias[3];               /* angualar acc bias */

  double hmin;                  /* min step size (secs), currently fixed and should be mult of data interval */
  double h;                     /* current step size, not used until we have variable step size */

  atti_mem_info *ts_mem_info;    /* memory information on atti time series storage */

} atti_state;


typedef struct atti_vari {
  double t;                     /* time of variational partial */
  double t0;                    /*  initial condition time !!t0 should
                                    be same for state and vari*/
  double p[NSTATE][NDYN];   /* first index state pq, omega, second
                                   partial pq0,omega0,S,B */
  double p0[NSTATE][NDYN];  /* initial condition for variational partials */
  char*  n[NSTATE][NDYN];   /* names of the variational partials, init with vari_nm.c */

} atti_vari;


#include "atti_prototypes.h"


#endif
