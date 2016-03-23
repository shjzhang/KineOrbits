#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintSCA1BFRecord.c,v 1.8 2004/08/30 21:03:35 wib Exp $";




void PrintSCA1BFRecord(FILE *dst, SCA1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of the Star Camera Assembly 
/          Level 1B Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                     06/21/00
/
/ input:  *dst    Pointer to SCA1B Data Format File
/         *record Pointer to SCA1B Data struct (SCA1B_t)
<-----------------------------------------------------------------------------*/
{
 
/*----------------------------------------------------------------------------->
/ Write record elements to dst
<-----------------------------------------------------------------------------*/

  fprintf(dst," %-20s = %ld\n","record->gps_time",record->gps_time);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-20s = %d\n","record->sca_id",record->sca_id);
  fprintf(dst," %-20s = %lf\n","record->quatangle",record->quatangle);
  fprintf(dst," %-20s = %lf\n","record->quaticoeff",record->quaticoeff);
  fprintf(dst," %-20s = %lf\n","record->quatjcoeff",record->quatjcoeff);
  fprintf(dst," %-20s = %lf\n","record->quatkcoeff",record->quatkcoeff);
  fprintf(dst," %-20s = %lf\n","record->qual_rss",record->qual_rss);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


}
