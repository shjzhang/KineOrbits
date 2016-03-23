#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintXXXVOFRecord.c,v 1.4 2004/08/30 21:03:35 wib Exp $";


void PrintXXXVOFRecord(FILE *dst, XXXVO_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print an detailed ascii description of Vector Orientation 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01                           
/
/ input:  *dst    Pointer to XXXVO Data Format File
/         *record Pointer to XXXVO Data struct (XXXVO_t)
<-----------------------------------------------------------------------------*/
{
 char string[3];

 strcpy(string,"-");

/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/

 fprintf(dst," %-20s = %ld\n","record->gps_time",record->gps_time);
 string[0] = record->GRACE_id;
 fprintf(dst," %-20s = %s\n","record->GRACE_id",string);
 fprintf(dst," %-20s = %le\n","record->mag",record->mag);
 fprintf(dst," %-20s = %le\n","record->cosx",record->cosx);
 fprintf(dst," %-20s = %le\n","record->cosy",record->cosy);
 fprintf(dst," %-20s = %le\n","record->cosz",record->cosz);
 fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


}
