#include <string.h>
#include <stdlib.h>
#include "GRACEiolib.h"
#include "GRACEfiletype.h"
#include "GRACEio_prototypes.h"
#include "TimeLib.h"

#define MAXLINECHAR 1000

static char SccsId[] = "$Id: PutFileHeader.c,v 1.11 2005/04/29 20:51:56 glk Exp $";

boolean PutFileHeader(FILE *dst,                /* pointer to destination file*/
                      FileHeader_t *header,     /* pointer to header struct   */
                      long FileType,            /* file type integer pointer  */
                      long FormatType,          /* format type binary/ascii   */
                      long NHeaderRecord,       /* number of header records   */
                      long init_flag,           /* initialization flag 0,1    */
                      char *ProdAgency,         /* producer agency label      */
                      char *ProdInstitution,    /* producer institution label */
                      char *SoftwareVersion,    /* software version label     */
                      char *SoftwareLinkTime,   /* software link time         */
                      char *Documentation,      /* documentation label        */
                      char *SatName,            /* satellite name label       */
                      char *SensorName,         /* sensor name label          */
                      char *TimeEpoch,          /* Time epoch label           */
                      double TfirstObs,         /* Time first observation     */
                      double TlastObs,          /* Time last observation      */
                      long   Nobs,              /* number of observations     */
                      char *TimeTag,            /* time tag label FIRST,FINAL */
                      char *FileName,           /* Filename                   */
                      char *ProcessLevel        /* Processing level (1A or 1B)*/
                     )
/*----------------------------------------------------------------------------->
/ purpose: Take header arguments, update header struct FileHeader_t and 
/          write header information to file src
/
/ coded by: Gerhard L.H. Kruizinga                           09/12/2000
/
/ input:  *src    Pointer to data file
/ output: *record Pointer to header struct
/
/ return:      1       normal return
/              0       End Of File reached before header could be read
/ notes:
/              if any labels are set to "NONE" then default values will be used
/              as specified in file HeaderFile.txt
/
<-----------------------------------------------------------------------------*/
{
  char               TimeTagUTC[HEADERMAXCHAR],FinalTime[HEADERMAXCHAR];
  char               StartTime[HEADERMAXCHAR],NobsChar[HEADERMAXCHAR];
  char               file_type[HEADERMAXCHAR],file_format[HEADERMAXCHAR];
  char               HeadNrecord[HEADERMAXCHAR],NrBytes[HEADERMAXCHAR];
  char               none[HEADERMAXCHAR];

  int  year,month,day,hour,minute,second;

  double frac,secs;        

  GetUTCTimeTag(TimeTagUTC);

  strcat(TimeTagUTC," by ");
  strcat(TimeTagUTC,getenv("LOGNAME"));

  header->filetype   = FileType;
  header->formattype = FormatType;


  if ( header->nrecord < NRHEADERLABELS ) 
  {
    header->nrecord         = NRHEADERLABELS;
    header->NinputFileLabel = 0;
  }

  header->init_flag  = init_flag;

  sprintf(file_type,  "%d",header->filetype);
  sprintf(file_format,"%d",header->formattype);
  sprintf(HeadNrecord,"%d",header->nrecord);

  seccal(TfirstObs,&year,&month,&day,&hour,&minute,&second,&frac);

  frac = (double) (long) (frac * 100);
  frac /= 100;
  secs = (double)second + frac;
  sprintf(StartTime,"%f (%4d-%02d-%02d %02d:%02d:%05.2f)",TfirstObs,
                     year,month,day,hour,minute,secs);

  seccal(TlastObs,&year,&month,&day,&hour,&minute,&second,&frac);

  frac = (double) (long) (frac * 100);
  frac /= 100;
  secs = (double)second + frac;

  sprintf(FinalTime,"%f (%4d-%02d-%02d %02d:%02d:%05.2f)",TlastObs,
                     year,month,day,hour,minute,secs);

  sprintf(NobsChar,"%d",Nobs);

  sprintf(NrBytes,"%d",ftell(dst));

  strcpy(none,"NONE");

  ModifyFileHeader(header,GetHeaderLabel("iphProducerAgency")        , ProdAgency);
  ModifyFileHeader(header,GetHeaderLabel("iphProducerInstitution")   , ProdInstitution);
  ModifyFileHeader(header,GetHeaderLabel("iphFileType")              , file_type);
  ModifyFileHeader(header,GetHeaderLabel("iphFileFormat")            , file_format);
  ModifyFileHeader(header,GetHeaderLabel("iphHeadNrecord")           , HeadNrecord);
  ModifyFileHeader(header,GetHeaderLabel("iphSoftwareVersion")       , SoftwareVersion);
  ModifyFileHeader(header,GetHeaderLabel("iphSoftwareLinkTime")      , SoftwareLinkTime);
  ModifyFileHeader(header,GetHeaderLabel("iphDocumentation")         , Documentation);
  ModifyFileHeader(header,GetHeaderLabel("iphSatelliteName")         , SatName);
  ModifyFileHeader(header,GetHeaderLabel("iphSensorName")            , SensorName);
  ModifyFileHeader(header,GetHeaderLabel("iphTimeEpoch")             , TimeEpoch);
  ModifyFileHeader(header,GetHeaderLabel("iphTimeFirstObs")          , StartTime);
  ModifyFileHeader(header,GetHeaderLabel("iphTimeLastObs")           , FinalTime);
  ModifyFileHeader(header,GetHeaderLabel("iphNumberObs")             , NobsChar);
  ModifyFileHeader(header,GetHeaderLabel("iphNumberBytes")           , NrBytes);
  ModifyFileHeader(header,GetHeaderLabel("iphFileName")              , FileName);
  ModifyFileHeader(header,GetHeaderLabel("iphProcessLevel")          , ProcessLevel);

  strcpy(header->ProducerAgency      , ProdAgency);
  strcpy(header->ProducerInstitution , ProdInstitution);
  strcpy(header->SoftwareVersion     , SoftwareVersion);
  strcpy(header->Documentation       , Documentation);
  strcpy(header->SatelliteName       , SatName);
  strcpy(header->SensorName          , SensorName);
  strcpy(header->TimeEpoch           , TimeEpoch);
  header->TimeFirstObs = TfirstObs;
  header->TimeLastObs  = TlastObs;
  header->NumberObs    = Nobs;
  sscanf("%ld",NrBytes,&header->NumberBytes);
  strcpy(header->FileName            , FileName);
  strcpy(header->ProcessLevel        , ProcessLevel);

  if (strcmp(TimeTag,"FINAL") == 0)
  {
    ModifyFileHeader(header,GetHeaderLabel("iphProductCreateEndTime")  , TimeTagUTC);
    strcpy(header->ProductCreateEndTime, TimeTagUTC);
  }
  else
  {
    ModifyFileHeader(header,GetHeaderLabel("iphProductCreateStartTime"), TimeTagUTC);
    ModifyFileHeader(header,GetHeaderLabel("iphProductCreateEndTime")  , none);
    strcpy(header->ProductCreateStartTime, TimeTagUTC);
    strcpy(header->ProductCreateEndTime  , none);
  }

  WriteFileHeader(dst,header);

  return True;
}
