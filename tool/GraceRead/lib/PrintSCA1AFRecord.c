#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

static char SccsId[] = "$Id: PrintSCA1AFRecord.c,v 1.8 2004/08/30 21:03:35 wib Exp $"; 



void PrintSCA1AFRecord(FILE *dst, SCA1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of the Star Camera Assembly 
/          Level 1A Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                     06/14/00
/
/ input:  *dst    Pointer to SCA1A Data Format File
/         *record Pointer to SCA1A Data struct (SCA1A_t)
<-----------------------------------------------------------------------------*/
{
 
/*----------------------------------------------------------------------------->
/ Write record elements to dst
<-----------------------------------------------------------------------------*/

  fprintf(dst," %-20s = %ld\n","record->rcv_time",record->rcv_time);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-20s = %d\n","record->sca_id",record->sca_id);
  fprintf(dst," %-20s = %c\n","record->sca_desig",record->sca_desig);
  fprintf(dst," %-20s = %lf\n","record->quatangle",record->quatangle);
  fprintf(dst," %-20s = %lf\n","record->quaticoeff",record->quaticoeff);
  fprintf(dst," %-20s = %lf\n","record->quatjcoeff",record->quatjcoeff);
  fprintf(dst," %-20s = %lf\n","record->quatkcoeff",record->quatkcoeff);
  fprintf(dst," %-20s = %d\n","record->nlocks",(int)record->nlocks);
  fprintf(dst," %-20s = %d\n","record->nstars",(int)record->nstars);
  fprintf(dst," %-20s = %d\n","record->sca_config[0]",record->sca_config[0]); 
  fprintf(dst," %-20s = %d\n","record->sca_config[1]",record->sca_config[1]); 
  fprintf(dst," %-20s = %d\n","record->sca_config[2]",record->sca_config[2]); 
  fprintf(dst," %-20s = %d\n","record->sca_mode",record->sca_mode);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


}
