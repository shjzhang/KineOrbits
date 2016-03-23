#ifndef _atti_sim_h_
#define _atti_sim_h_

typedef struct atti_sim_t {
  int sim_on;                   /* = 0 off default, 1 do simulation   */
  double sim_t_start;           /* J2000 GPS sec for the start of sim */
  double sim_t_end;             /* end time                           */
  int    sim_data_interval;     /* default = 1, but set by -dt option */
  char  *sim_sca1a_out_filename;/* filename for simulated SCA1A output*/
  char  GRACE_id;               /* GRACE S/C id (A || B)              */

} atti_sim_t;

#endif
