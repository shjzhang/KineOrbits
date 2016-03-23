#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintTHR1XFRecord.c,v 1.6 2004/08/30 21:03:35 wib Exp $";

void PrintTHR1XFRecord(FILE *dst, THR1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of Thruster activation Level 1B 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to THR1X Data Format File
/         *record Pointer to THR1X Data struct (THR1X_t)
<-----------------------------------------------------------------------------*/
{

  long i; 

  char label[100];
  char string[3];


/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/

  fprintf(dst," %-21s = %ld\n","record->time_intg",record->time_intg);
  fprintf(dst," %-21s = %ld\n","record->time_frac",record->time_frac);
  
  strcpy(string,"-");
  string[0] = record->time_ref;
  fprintf(dst," %-21s = %s\n","record->time_ref",string);

  strcpy(string,"-");
  string[0] = record->GRACE_id;
  fprintf(dst," %-21s = %s\n","record->GRACE_id",string);

  loop(i,MAXTHRSTRS)
  {
    fprintf(dst," %-18s%02d%1s = %ld\n","record->thrust_count[",i+1,"]",
                record->thrust_count[i]);
  }

  loop(i,MAXTHRSTRS)
  {
    fprintf(dst," %-18s%02d%1s = %ld\n","record->on_time[",i+1,"]",
                record->on_time[i]);
  }

  loop(i,MAXTHRSTRS)
  {
    fprintf(dst," %-18s%02d%1s = %ld\n","record->accum_dur[",i+1,"]",
                record->accum_dur[i]);
  }

  fprintf(dst," %-21s = %d\n","record->qualflg",record->qualflg);

}
