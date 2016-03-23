#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintGNV1AFRecord.c,v 1.7 2004/08/30 21:03:34 wib Exp $";


void PrintGNV1AFRecord(FILE *dst, GNV1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print an detailed ascii description of GPS Navigation 1A Data 
/          Format Record to file pointer dst
/
/ coded by: Jean E. Patterson                       06/13/00
/
/ input:  *dst    pointer to GPS Navigation 1A Data Format File
/         *record Pointer to GPS Navigation 1A Data struct (GNV1A_t)
<-----------------------------------------------------------------------------*/
{
  int     i;
  char   *ptr_prn_id;
  double *ptr_el_prn;
  double *ptr_az_prn;

 char string[3];

 strcpy(string,"-");

 
/*----------------------------------------------------------------------------->
/ Write Record elements to dst
<-----------------------------------------------------------------------------*/

  /* Note these are not all really doubles, be careful because some are
     chars and will need a different syntax
  */
  fprintf(dst," %-20s = %ld\n","record->rcv_time",record->rcv_time);
  fprintf(dst," %-20s = %d\n","record->n_prns",record->n_prns);
  string[0] = record->GRACE_id;
  fprintf(dst," %-20s = %s\n","record->GRACE_id",string);
  fprintf(dst," %-20s = %f\n","record->chisq",record->chisq);
  fprintf(dst," %-20s = %f\n","record->cov_mult",record->cov_mult);
  fprintf(dst," %-20s = %d\n","record->voltage",record->voltage);
  fprintf(dst," %-20s = %lf\n","record->xpos",record->xpos);
  fprintf(dst," %-20s = %lf\n","record->ypos",record->ypos);
  fprintf(dst," %-20s = %lf\n","record->zpos",record->zpos);
  fprintf(dst," %-20s = %lf\n","record->xpos_err",record->xpos_err);
  fprintf(dst," %-20s = %lf\n","record->ypos_err",record->ypos_err);
  fprintf(dst," %-20s = %lf\n","record->zpos_err",record->zpos_err);
  fprintf(dst," %-20s = %lf\n","record->xvel",record->xvel);
  fprintf(dst," %-20s = %lf\n","record->yvel",record->yvel);
  fprintf(dst," %-20s = %lf\n","record->zvel",record->zvel);
  fprintf(dst," %-20s = %lf\n","record->xvel_err",record->xvel_err);
  fprintf(dst," %-20s = %lf\n","record->yvel_err",record->yvel_err);
  fprintf(dst," %-20s = %lf\n","record->zvel_err",record->zvel_err);
  fprintf(dst," %-20s = %lf\n","record->time_offset",record->time_offset);
  fprintf(dst," %-20s = %lf\n","record->time_offset_err",record->time_offset_err);
  fprintf(dst," %-20s = %lf\n","record->time_drift",record->time_drift);
  fprintf(dst," %-20s = %lf\n","record->err_drift",record->err_drift);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


  /* This is not correct in form or syntax.  Fix this up once we decide 
     how to declare these data 
*/

  ptr_prn_id   = record->prn_id;
  ptr_el_prn   = record->el_prn;
  ptr_az_prn   = record->az_prn;

  for (i=0; i<record->n_prns; i++,ptr_prn_id++,ptr_el_prn++,ptr_az_prn++)
  {
    fprintf(dst," %-20s = %d\n","record->prn_id",*ptr_prn_id);
    fprintf(dst," %-20s = %lf\n","record->ptr_el_prn",*ptr_el_prn);
    fprintf(dst," %-20s = %lf\n","record->ptr_az_prn",*ptr_az_prn);
  }
}
