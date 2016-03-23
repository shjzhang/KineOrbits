#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: PrintIHK1XFRecord.c,v 1.2 2004/08/30 21:03:34 wib Exp $";

void PrintIHK1XFRecord(FILE *dst, IHK1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print the IPU HK 1X Data Format record to file pointer dst
/
/ coded by: Gerhard L.H. Kruizinga           09/25/01
/
/ input:  *dst    Pointer to IHK1XF Data Format File
/         *record Pointer to IHK1XF Data struct (IHK1X_t)
<-----------------------------------------------------------------------------*/
{
  int n_len,i;
  char bits[8];
 
/*----------------------------------------------------------------------------->
/ Decode Product flag 
<-----------------------------------------------------------------------------*/
  GetCharBits(record->qualflg,bits);
 
  fprintf(dst," %-20s = %d\n","record->time_intg",record->time_intg);
  fprintf(dst," %-20s = %d\n","record->time_frac",record->time_frac);
  fprintf(dst," %-20s = %c\n","record->time_ref",record->time_ref);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);

  fprintf(dst," %-20s = %d [","record->qualflg",record->qualflg);
  loop(i,8)fprintf(dst,"%d",bits[7-i]);  
  fprintf(stderr,"]\n");
    
  fprintf(dst," %-20s = %c\n","record->sensortype",record->sensortype);
  fprintf(dst," %-20s = %.16g\n","record->sensorvalue",record->sensorvalue);
  fprintf(dst," %-20s = %s\n","record->sensorname",record->sensorname);
}
