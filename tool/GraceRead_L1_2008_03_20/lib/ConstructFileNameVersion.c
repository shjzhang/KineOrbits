#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

static char SccsId[] = "$Id: ConstructFileNameVersion.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

#define MAXCHAR 1000

void ConstructFileNameVersion(char *Satellite , double Time, char *Version,
                       char *FileTypeName, char *Filename, char *ext)
/*----------------------------------------------------------------------------->
/ purpose:  Construct filename based on input information
/
/ coded by: Gerhard L.H. Kruizinga             08/22/00
/ modified: Gerhard L.H. Kruizinga             06/06/01 
/           change Version Number to Version String
/
/ input: char satellite       Satellite indicator (A = GRACE A and B = GRACE B"
/        double Time          time for filename time tag (sec past 2000)
/        char VersionNumber   Version string
/        long FileTypeName    Standard file type name(aka filekey,eg ACC1B);
/        char ext             file extension
/ output:
/        char Filename        Filename constructed based on input according to
/                             TTTTT_yyyy_mm_dd_S_vn.ext
/                             where
/                             TTTTT file label based on File Type Pointer
/                             yyyy  year
/                             mm    month
/                             dd    day of month
/                             S     satellite indicator A=(GRACE A) B=(GRACE B)
/                             vn    version number
<-----------------------------------------------------------------------------*/
{
   char Date[MAXCHAR];

   int  year,month,day,hour,minute,second;

   double frac;

   seccal(Time,&year,&month,&day,&hour,&minute,&second,&frac);

   sprintf(Date,"%4d-%02d-%02d",year,month,day);

   sprintf(Filename,"%c%c%c%c%c_%s_%s_%s.%s",
           FileTypeName[0],FileTypeName[1],FileTypeName[2],
           FileTypeName[3],FileTypeName[4],Date,Satellite,Version,ext);


}
