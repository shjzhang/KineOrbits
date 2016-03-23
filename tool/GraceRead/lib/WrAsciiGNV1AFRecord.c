#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1
#define Failure 0

static char SccsId[] = "$Id: WrAsciiGNV1AFRecord.c,v 1.2 2004/08/30 21:03:37 wib Exp $";

boolean WrAsciiGNV1AFRecord(FILE *dst, GNV1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of GPS Navigation Level 1A data 
/          from the file pointed to by dst 
/
/ coded by: Gerhard L.H. Kruizinga             09/10/01
/
/ input:  *dst    pointer to GPS Navigation 1A Data Format File
/         *record Pointer to GPS Navigation 1A Data struct (GNV1A_t)
<-----------------------------------------------------------------------------*/
{
  int     i;
  char   *ptr_prn_id,bits8[8];
  double *ptr_elev_prn;
  double *ptr_az_prn;
 
/*----------------------------------------------------------------------------->
/ Write Record elements to dst
<-----------------------------------------------------------------------------*/
  fprintf(dst,"%d %d %c %.16g %.16g %d %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g ",
              record->rcv_time ,record->n_prns ,record->GRACE_id ,record->chisq,
              record->cov_mult ,record->voltage ,record->xpos ,record->ypos, 
              record->zpos ,record->xpos_err ,record->ypos_err ,record->zpos_err,
              record->xvel ,record->yvel ,record->zvel ,record->xvel_err,
              record->yvel_err ,record->zvel_err ,record->time_offset,
              record->time_offset_err ,record->time_drift ,record->err_drift);

  GetCharBits(record->qualflg,bits8);

  fprintf(dst,"  ");
  loop(i,8)fprintf(dst,"%d",bits8[7-i]);
  fprintf(dst," ");

  ptr_prn_id   = record->prn_id;
  ptr_elev_prn = record->el_prn;
  ptr_az_prn   = record->az_prn;
  
  for (i=0; i<record->n_prns; i++,ptr_prn_id++,ptr_elev_prn++,ptr_az_prn++)
  {
    fprintf(dst," %d",*ptr_prn_id);
    fprintf(dst," %.16g",*ptr_elev_prn);
    fprintf(dst," %.16g",*ptr_az_prn);
  }

  fprintf(dst,"\n");

  return Success;
}
