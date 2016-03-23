/* $Id: atti_flt_prototypes.h,v 1.7 2004/11/01 22:33:35 wib Exp $ */


#include "atti_flt.h"

#include "atti_flt_utils_prototypes.h" /* for MxRow MaxCol */

int atti_lsq(                   /* using QR decomposition */

/* Input */
         double tstart,         /* start of fit interval */
         double tend,            /* end of fit interval */
         int    dT,             /* measurement time interval should be
                                   a mult of SCA1A sampling */
         atti_flt *flt_info,    /* which are estimated, apriori,
                                   non-dyn state values */
         atti_sim_t        *SimInfo,
/* Input/Output */
         atti_state *state,     /* contains both initial condition,
                                   and previous time point on input;
                                   current values on output */
         atti_vari *vari,       /* same comments as state , see struct */

 /* Output */
         double deltaParm[]    /* an array of dimension flt_info->nDyn + flt_info->n_NonDyn */

         /* returns number of measurements process if successful and < 0 if error */
         

         );


int srif_flt(                   /* atti/pad srif filter loops over all
                                   measurements and times, writes out
                                   smoothing info, current state calls
                                   regres */

/* Input */
             double tstart,         /* start of fit interval */
             double tend,            /* end of fit interval  */
             int    dT,             /* measurement time interval should be
                                       a mult of SCA1A sampling */
             atti_flt *flt_info,    /* which are estimated, apriori,
                                       non-dyn state values */
         
             /* Input/Output */
             atti_state *state,     /* contains both initial condition,
                                       and previous time point on input;
                                       current values on output */
             atti_vari *vari,       /* same comments as state , see struct */

             edit_criteria_t    EditCriteria,
             atti_sim_t        *SimInfo,
             

 /* Input/Output */
             double  R[MaxRow][MaxCol] /* SRIF array ( + residuals
                                   column ) stored in a rectangle ,
                                   note MaxRow >= MaxBatchMeas + Num
                                   param = 6 + 18 = 24 , the number of
                                   columns that are active is 2*Np+Nx
                                   where the extra Np are storage for
                                   smoothing coeffs in the iterations
                                   on output this the SRIF at time N+1
                                   = tend +dT and should be the
                                   appropriate initialization input
                                   for smooth_all*/

/* Output */
             /* smooth coefficient file, empty if there are no stochastics */
                

             /* returns number of measurements process if successful
                and < 0 if error */
         

             );

int smooth_all(

/* Input */

  double tend,                  /* time of input R, R will be smoothed
                                   back to tend - N*dT  <= tstart */
  double tstart,
  double dT,                    /* delta time for smoothing, just a
                                   check for the smooth read */
  int    Np,                   /* number of process-noise parameters,
                                  again just to check consistency with
                                  the ReadSmoothCoef*/
  int    Nx,                    /* number of non-proc. states */

  int	Parm_ord_Inv[],	      /* parameter index mapping from filter/smoother order
				   to standard input order (flt_info->Param_order_inv) */

  atti_mem_info *ts_mem_info, /* pointer to storage information for pq and omega time series */

  output_requests_t *output_requests,

/* Input/Output */
  double  R[MaxRow][MaxCol] /* SRIF array ( + residuals column )
                               stored in a rectangle , note MaxRow >=
                               MaxBatchMeas + Num param = 6 + 18 = 24
                               , the number of columns that are active
                               is 2*Np+Nx where the extra Np are
                               storage for smoothing coeffs in the
                               iterattions*/
  

  );

int regres_prefit_out(
/* input  */
                      double tc, /* time of residual */
                      int SCAid, /* camera ud */
                      double OmC[4],
                      int    iter /* iteration number */
/* output print to standard out */
                      );
