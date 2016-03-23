#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include "TimeLib.h"

#define MAXCHAR 1000

static char SccsId[] = "$Id: MakeL1bOuputFilename.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

void MakeL1BOutputFilename (char *l1a_inputfilename, char *l1b_inputfilename,
                            char *l1b_outputfilename, char *l1b_reportfilename,
                            char *l1b_versionchar,char *l1b_filesat)
/*----------------------------------------------------------------------------->
/ purpose: create Level1B output filename based in L1a input filename
/          or construct input filename.out if input file is not specified
/          in the standard PRDID_YYYY-MM-DD_S_VS.ext. Furthermore determine
/          report filename as well.
/          
/
/ coded by: Gerhard L.H. Kruizinga                06/25/01
/
/ input:
/        l1a_inputfilename  level 1a input filename (from argument list)
/        l1b_inputfilename  level 1b input filename (null or from argument list)
/        l1b_charversion    version string to be used for creation of filenames
/        l1b_filesat        satellite id to be used, ignore if '\0'
/ output:
/        l1b_outputfilename level 1b output filename
/        l1b_reportfilename level 1b report filename
<-----------------------------------------------------------------------------*/
{ 

 int year,month,day;
 long VSNumber;

 char basefilename[MAXCHAR];
 char FileKey[MAXCHAR],FileSat[MAXCHAR],VSChar[MAXCHAR],extension[MAXCHAR];
 char VersionChar[MAXCHAR];

 strcpy (VersionChar,l1b_versionchar);

 if (l1b_inputfilename[0])
 {
   if (VerifyFilename (l1b_inputfilename, basefilename,FileKey,&year,&month,
       &day,FileSat,&VSNumber,VSChar,extension) == 0)
   {
     strcpy(l1b_outputfilename,l1b_inputfilename);

     strcpy(VersionChar,VSChar);

     if (l1b_filesat[0]) strcpy(FileSat,l1b_filesat);

     sprintf(l1b_reportfilename,"%s_%04d-%02d-%02d_%s_%s.%s",FileKey,year,
             month,day, FileSat, VersionChar,"rpt");
/*
     fprintf(stderr,"hallo3 %s %s\n",l1b_outputfilename,l1b_reportfilename);
*/
   }
   else
   {
     strcpy(l1b_outputfilename,basefilename);
     strcpy(l1b_reportfilename,basefilename);
     strcat(l1b_reportfilename,".rpt");
/*
     fprintf(stderr,"hallo4 %s %s\n",l1b_outputfilename,l1b_reportfilename);
*/
   }
 }
 else
 {
   if (VerifyFilename (l1a_inputfilename, basefilename,FileKey,&year,&month,
       &day,FileSat,&VSNumber,VSChar,extension) == 0)
   {
     FileKey[4] = 'B';

     if (VSNumber  < 0) strcpy(VersionChar,VSChar);
  
     if (l1b_filesat[0]) strcpy(FileSat,l1b_filesat);

     sprintf(l1b_outputfilename,"%s_%04d-%02d-%02d_%s_%s",FileKey,year,month,
             day,FileSat, VersionChar);
     strcpy(l1b_reportfilename,l1b_outputfilename);
     strcat(l1b_outputfilename,".");
     strcat(l1b_outputfilename,extension);
     strcat(l1b_reportfilename,".rpt");
/*
     fprintf(stderr,"hallo1 %s %s\n",l1b_outputfilename,l1b_reportfilename);
*/
   }
   else
   {
     GetBaseFilename(l1a_inputfilename,basefilename);
     strcpy(l1b_outputfilename,basefilename);
     strcpy(l1b_reportfilename,basefilename);
     strcat(l1b_outputfilename,".out");
     strcat(l1b_reportfilename,".rpt");
/*
     fprintf(stderr,"hallo2 %s %s\n",l1b_outputfilename,l1b_reportfilename);
*/
   }
 }
} 
