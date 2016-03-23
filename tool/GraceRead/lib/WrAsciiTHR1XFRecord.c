#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1 
#define Failure 0


static char SccsId[] = "$Id: WrAsciiTHR1XFRecord.c,v 1.7 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiTHR1XFRecord(FILE *dst, THR1X_t *record)
/*-----------------------------------------------------------------------------'
/ purpose: Dump ascii records of Thruster data 
/          from the file pointed to by dst 
/
/ coded by: J. E. Patterson                  07/18/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to THR1XF Data Format File
/         *record Pointer to THR1XF Data struct (THR1X_t)
'-----------------------------------------------------------------------------*/
{
 long i;

 char string1[3];
 char string2[3];
 char bits8[8];                                                                         

 GetCharBits(record->qualflg,bits8);                                                    

 strcpy(string1,"-");
 strcpy(string2,"-");
 string1[0] = record->GRACE_id;
 string2[0] = record->time_ref; 

 fprintf(dst,"%d %d %s %s ",
         record->time_intg, record->time_frac, 
         string2, string1);

 fprintf(dst,"  ");                                                                     

 loop(i,MAXTHRSTRS)
 {
   fprintf(dst,"%d ",record->thrust_count[i],dst);
 }

 loop(i,MAXTHRSTRS)
 {
   fprintf(dst,"%d ",record->on_time[i],dst);
 }

 loop(i,MAXTHRSTRS)
 {
   fprintf(dst,"%d ",record->accum_dur[i],dst);
 }

 fprintf(dst,"  ");                                                                     
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);                                                 
 fprintf(dst,"\n");
  
 return Success;
}
