#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintGNV1BFRecord.c,v 1.6 2004/08/30 21:03:34 wib Exp $";



void PrintGNV1BFRecord(FILE *dst, GNV1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of GPS Navigation Level 1B 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                       06/14/00
/
/ input:  *dst    Pointer to GNV1B Data Format File
/         *record Pointer to GNV1B Data struct (GNV1B_t)
<-----------------------------------------------------------------------------*/
{
 char string[3];

/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  fprintf(dst," %-20s = %ld\n","record->gps_time",record->gps_time);

  strcpy(string,"-");
  string[0] = record->GRACE_id;
  fprintf(dst," %-20s = %s\n","record->GRACE_id",string);

  strcpy(string,"-");
  string[0] = record->coord_ref;
  fprintf(dst," %-20s = %s\n","record->coord_ref",string);

  fprintf(dst," %-20s = %le\n","record->xpos",record->xpos);
  fprintf(dst," %-20s = %le\n","record->ypos",record->ypos);
  fprintf(dst," %-20s = %le\n","record->zpos",record->zpos);

  fprintf(dst," %-20s = %le\n","record->xpos_err",record->xpos_err);
  fprintf(dst," %-20s = %le\n","record->ypos_err",record->ypos_err);
  fprintf(dst," %-20s = %le\n","record->zpos_err",record->zpos_err);

  fprintf(dst," %-20s = %le\n","record->xvel",record->xvel);
  fprintf(dst," %-20s = %le\n","record->yvel",record->yvel);
  fprintf(dst," %-20s = %le\n","record->zvel",record->zvel);

  fprintf(dst," %-20s = %le\n","record->xvel_err",record->xvel_err);
  fprintf(dst," %-20s = %le\n","record->yvel_err",record->yvel_err);
  fprintf(dst," %-20s = %le\n","record->zvel_err",record->zvel_err);

  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);

}
