/* @(#) GRACEreport.h      1.17 11/01/04 */

typedef struct Report_t             /* struct for report file values          */
        {   
          char key[HEADERMAXCHAR];  /* report key = PRID_YYYY-MM-DD_S_RL      */
          double previous_time_tag; /* previous time tag used for this struct */
          double file_time;         /* file time tag (sec 2000) = YYYY-MM-DD  */
          double proc_time;         /* create time of report file (sec 2000)  */
          double start_time;        /* time of first observation              */
          double final_time;        /* time of last observation               */
          long   nobs;              /* number of observations in data file    */
          double avg_time_gap;      /* average of all time gaps in data file  */
          double std_time_gap;      /* standard deviation of all time gaps    */
          double min_time_gap;      /* minimum time gap in data file          */
          double max_time_gap;      /* maximum time gap in data file          */
          long qualflag_nbits;      /* number of quality flag bits            */
          long qualflag_count[32];  /* counter array for quality flag bits    */
          /* ACC1B specific report data */
          long   ACC1B_Nr_nodatagapfill; /* number of data gaps not filled         */
          double ACC1B_CRMS_lin_accl_x; /* compression rms lin_acc_x_axis m/s**2   */
          double ACC1B_CRMS_lin_accl_y; /* compression rms lin_acc_y_axis m/s**2   */
          double ACC1B_CRMS_lin_accl_z; /* compression rms lin_acc_z_axis m/s**2   */
          double ACC1B_CRMS_ang_accl_x; /* compression rms ang_acc_x_axis rad/s**2 */
          double ACC1B_CRMS_ang_accl_y; /* compression rms ang_acc_y_axis rad/s**2 */
          double ACC1B_CRMS_ang_accl_z; /* compression rms ang_acc_z_axis rad/s**2 */
          double CLK1B_bias_start;      /* Time offset at start midnight from      */           
                                        /* linear fit in nano seconds (1e-9 sec)   */         
          double CLK1B_bias_start_sigma;/* Sigma Time offset at start midnight     */        
                                        /* in nano seconds (1e-9 seconds)          */     
          double CLK1B_slope_start;     /* Relative drift at start midnight from   */              
                                        /* linear fit in nano-seconds/sec          */            
          double CLK1B_slope_start_sigma;/* Sigma Relative drift at start midnight */      
                                        /* in nano-seconds/sec                     */
          double CLK1B_rms_zero_start;  /* raw RMS of overlap clock difference     */ 
                                        /* at start midnight                       */
          double CLK1B_rms__fit_start;  /* linear fit  RMS of overlap clock diff.  */
                                        /* at start midnight                       */
          long   CLK1B_npoints_start;   /* linear fit  RMS of overlap clock diff.  */
                                        /* at start midnight                       */
          double CLK1B_bias_end  ;      /* Time offset at end   midnight from      */           
                                        /* linear fit in nano seconds (1e-9 sec)   */         
          double CLK1B_bias_end_sigma;  /* Sigma Time offset at end   midnight     */        
                                        /* in nano seconds (1e-9 seconds)          */     
          double CLK1B_slope_end  ;     /* Relative drift at end   midnight from   */              
                                        /* linear fit in nano-seconds/sec          */            
          double CLK1B_slope_end_sigma; /* Sigma Relative drift at end   midnight  */      
                                        /* in nano-seconds/sec                     */
          double CLK1B_rms_zero_end  ;  /* raw RMS of overlap clock difference     */ 
                                        /* at end   midnight                       */
          double CLK1B_rms__fit_end  ;  /* linear fit  RMS of overlap clock diff.  */
                                        /* at end   midnight                       */
          long   CLK1B_npoints_end;     /* linear fit  RMS of overlap clock diff.  */
                                        /* at end midnight                         */
          long   CLK1B_Nformal_edit;    /* number of tdp solutions edit based on   */
                                        /* formal error in tdp file                */
          long   GNV1B_npoints_start;   /* number of overlap point at Midnight strt*/
          double GNV1B_hpos_rms_start;  /* position overlap rms in H direction for */
                                        /* midnight start (m)                      */
          double GNV1B_cpos_rms_start;  /* position overlap rms in C direction for */
                                        /* midnight start (m)                      */
          double GNV1B_lpos_rms_start;  /* position overlap rms in L direction for */
                                        /* midnight start (m)                      */
          double GNV1B_hvel_rms_start;  /* velocity overlap rms in H direction for */
                                        /* midnight start (m)                      */
          double GNV1B_cvel_rms_start;  /* velocity overlap rms in C direction for */
                                        /* midnight start (m)                      */
          double GNV1B_lvel_rms_start;  /* velocity overlap rms in L direction for */
                                        /* midnight start (m)                      */
          long   GNV1B_npoints_end;     /* number of overlap point at Midnight end */
          double GNV1B_hpos_rms_end;    /* position overlap rms in H direction for */
                                        /* midnight end (m)                        */
          double GNV1B_cpos_rms_end;    /* position overlap rms in C direction for */
                                        /* midnight end (m)                        */
          double GNV1B_lpos_rms_end;    /* position overlap rms in L direction for */
                                        /* midnight end (m)                        */
          double GNV1B_hvel_rms_end;    /* velocity overlap rms in H direction for */
                                        /* midnight end (m)                        */
          double GNV1B_cvel_rms_end;    /* velocity overlap rms in C direction for */
                                        /* midnight end (m)                        */
          double GNV1B_lvel_rms_end;    /* velocity overlap rms in L direction for */
                                        /* midnight end (m)                        */
          double SCA1B_crms_q0_prim;    /* compression rms for q0 for primary SCA  */
          double SCA1B_crms_q1_prim;    /* compression rms for q1 for primary SCA  */
          double SCA1B_crms_q2_prim;    /* compression rms for q2 for primary SCA  */
          double SCA1B_crms_q3_prim;    /* compression rms for q3 for primary SCA  */
          double SCA1B_crms_q0_sec;     /* compression rms for q0 for secondary SCA*/
          double SCA1B_crms_q1_sec;     /* compression rms for q1 for secondary SCA*/
          double SCA1B_crms_q2_sec;     /* compression rms for q2 for secondary SCA*/
          double SCA1B_crms_q3_sec;     /* compression rms for q3 for secondary SCA*/
          double SCA1B_mean_yaw_prim;   /* mean yaw angle (deg) when eci (primary) */
          double SCA1B_rms_yaw_prim;    /* RMS yaw angle (deg) when eci (primary)  */
          double SCA1B_badn_yaw_prim;   /* bad data points yaw angle if eci        */
          double SCA1B_mean_pitch_prim; /* mean pitch angle (deg) if eci (primary) */
          double SCA1B_rms_pitch_prim;  /* RMS pitch angle (deg) if eci (primary)  */
          double SCA1B_badn_pitch_prim; /* bad data points pitch angle if eci      */
          double SCA1B_mean_roll_prim;  /* mean roll angle (deg) if eci (primary)  */
          double SCA1B_rms_roll_prim;   /* RMS roll angle (deg) if eci (primary)   */
          double SCA1B_badn_roll_prim;  /* bad data points roll angle if eci       */
          double SCA1B_mean_yaw_sec;    /* mean yaw angle (deg) when eci (second.) */
          double SCA1B_rms_yaw_sec;     /* RMS yaw angle (deg) when eci (second.)  */
          double SCA1B_badn_yaw_sec;    /* bad data points yaw angle if eci        */
          double SCA1B_mean_pitch_sec;  /* mean pitch angle (deg) if eci (second.) */
          double SCA1B_rms_pitch_sec;   /* RMS pitch angle (deg) if eci (second.)  */
          double SCA1B_badn_pitch_sec;  /* bad data points pitch angle if eci      */
          double SCA1B_mean_roll_sec;   /* mean roll angle (deg) if eci (second.)  */
          double SCA1B_rms_roll_sec;    /* RMS roll angle (deg) if eci (second.)   */
          double SCA1B_badn_roll_sec;   /* bad data points roll angle if eci       */
          double KBR1B_crms_dowr;       /* compression rms for dual-1way range (m) */
          double KBR1B_crms_ion;        /* compression rms for biased Ka-band	   */
					/* ion corection (m) */
          double KBR1B_resid_rms;       /* RMS residual of DOWR minus eci 	   */
					/* predicted range (m) */
          double KBR1B_resid_max;       /* maximum residual of DOWR minus eci      */
					/* predicted range (m) */
          double KBR1B_resid_min;       /* minimum residual of DOWR minus eci      */
					/* predicted range (m) */
          double KBR1B_resid_nobs;      /* number of residuals */
          double GPS1B_crms_CA;         /* compression rms for CA phase (m)        */
          double GPS1B_CA_nobs;         /* number of CA phase points               */
          double GPS1B_crms_L1;         /* compression rms for L1 phase (m)        */
          double GPS1B_L1_nobs;         /* number of L1 phase points               */
          double GPS1B_crms_L2;         /* compression rms for L2 phase (m)        */
          double GPS1B_L2_nobs;         /* number of L2 phase points               */
          double GPS1B_breaks;          /* number of phase breaks in output data   */
          double GPS1B_lowL1_snr;       /* number of discarded data (low L1 SNR)   */
          double GPS1B_lowL2_snr;       /* number of discarded data (low L2 SNR)   */
          double GPS1B_CAmisLock;       /* number of discarded data                */
                                        /* (L1_SNR !< 0.4*CA_SNR^2/1000)           */
          double GPS1B_discards;        /* total number of discarded data          */
          double GPS1B_nobs_in;         /* number of input raw data points         */
          double PCI1A_crms;            /* compression RMS of PC-CG range corr (m) */
          double KBR1B_clkdd_nobs;      /* number of clk dd obs                    */
          double KBR1B_clkdd_mean;      /* mean of clk dd obs    (picosec)         */
          double KBR1B_clkdd_sigma;     /* sigma of clk dd obs   (picosec)         */
          double KBR1B_clkdd_min;       /* min of clk dd obs     (picosec)         */
          double KBR1B_clkdd_max;       /* max of clk dd obs     (picosec)         */
          double KBR1B_clkdd_rms;       /* rms of clk dd obs     (picosec)         */
          double ACC1B_bias_x;          /* relative bias in x-direction (m/sec^2)  */  
          double ACC1B_bias_y;          /* relative bias in y-direction (m/sec^2)  */  
          double ACC1B_bias_z;          /* relative bias in z-direction (m/sec^2)  */  
          double ACC1B_scale_x;         /* relative scale in x-direction           */  
          double ACC1B_scale_y;         /* relative scale in y-direction           */  
          double ACC1B_scale_z;         /* relative scale in z-direction           */  
          double ACC1B_relres_x;        /* relative res in x-direction (m/sec^2)   */  
          double ACC1B_relres_y;        /* relative res in y-direction (m/sec^2)   */  
          double ACC1B_relres_z;        /* relative res in z-direction (m/sec^2)   */  
          double SCA1B_nr_offset;       /* number of offset used in statistics     */
          double SCA1B_q2_avg_dif;      /* daily average x-component of diff quat  */
          double SCA1B_q3_avg_dif;      /* daily average y-component of diff quat  */
          double SCA1B_q4_avg_dif;      /* daily average z-component of diff quat  */
          double SCA1B_q2_stdev_dif;    /* daily stddev  x-component of diff quat  */
          double SCA1B_q3_stdev_dif;    /* daily stddev  y-component of diff quat  */
          double SCA1B_q4_stdev_dif;    /* daily stddev  z-component of diff quat  */
          double SCA1B_sca1_nr_res;     /* number of residual for SCA id = 1       */
          double SCA1B_sca1_x_res_rms;  /* RMS SCA1 x residual (micro radians)     */
          double SCA1B_sca1_y_res_rms;  /* RMS SCA1 y residual (micro radians)     */
          double SCA1B_sca1_z_res_rms;  /* RMS SCA1 z residual (micro radians)     */
          double SCA1B_sca2_nr_res;     /* number of residual for SCA id = 2       */
          double SCA1B_sca2_x_res_rms;  /* RMS SCA2 x residual (micro radians)     */
          double SCA1B_sca2_y_res_rms;  /* RMS SCA2 y residual (micro radians)     */
          double SCA1B_sca2_z_res_rms;  /* RMS SCA2 z residual (micro radians)     */
          long ACC1A_nrec_read;         /* number of records read from file        */
          long ACC1A_nrec_read_used;    /* number of records used for ttag fix     */
          long ACC1A_nrec_written;      /* number of fixed ttag records written    */
          long ACC1A_nrec_nulled;       /* number of records nulled                */
          long ACC1A_nrec_non_incorportated; /* number of non-incorparated records */
          long ACC1A_nrec_filled;       /* number of ang acc records added to fill */
          long ACC1A_nrec_consistency;  /* consitency check sum 0=nominal != 0 problem */
        } Report_t;

void MakeACC1AFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeGNV1AFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeGFD1XFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeSCA1AFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeACC1BFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeGNV1BFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeKBR1BFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeSCA1BFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeXXXVOFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeIOA1BFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeOSCFQFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeCLK1BFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeMAG1XFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeHRT1XFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeTHR1XFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeIHK1XFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeTNK1XFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakeAHK1XFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);
void MakePCI1AFreport(FILE *src, FILE *dst, FILE *accum, char *filename, long *stat_modes);

boolean PrintReport(FILE *dst,Report_t *report, long *stat_modes);
boolean AccumulateTimeGap(double time_tag, Report_t *report);
boolean AccumulateQualFlags(char qual_flag, Report_t *report);
