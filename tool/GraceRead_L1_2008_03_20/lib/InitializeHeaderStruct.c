#include <stdlib.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define MAXLINECHAR 1000

static char SccsId[] = "$Id: InitializeHeaderStruct.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

void InitializeHeaderStruct(FileHeader_t *header)
/*----------------------------------------------------------------------------->
/ purpose: Initialize header struct 
/
/ coded by: Gerhard L.H. Kruizinga                           09/07/2001
/
/ input:  *header    Pointer to header struct 
/
<-----------------------------------------------------------------------------*/
{
  long i;

  header->filetype = -1;
  header->formattype = -1;
  header->nrecord    = 0;
  header->init_flag  = 0;
  strcpy(header->ProducerAgency,"Not Defined");
  strcpy(header->ProducerInstitution,"Not Defined");
  strcpy(header->SoftwareVersion,"Not Defined");
  strcpy(header->Documentation,"Not Defined");
  strcpy(header->SatelliteName,"Not Defined");
  strcpy(header->SensorName,"Not Defined");
  strcpy(header->TimeEpoch,"Not Defined");
  header->TimeFirstObs = 0.0;
  header->TimeLastObs  = 0.0;
  header->NumberObs    = 0;
  header->NumberBytes  = 0;
  strcpy(header->ProductCreateStartTime,"Not Defined");
  strcpy(header->ProductCreateEndTime,"Not Defined");
  strcpy(header->ProcessLevel,"Not Defined");

  loop(i,NMAXHEADERREC) strcpy(&header->HeaderCards[i][0],"Not Defined");

  header->NinputFileLabel = 0;

  loop(i,MAXINPUTFILELABEL)
  {
    strcpy(header->InputFileLabel[i].filekey,"Not Defined");
    strcpy(header->InputFileLabel[i].name,"Not Defined");
    strcpy(header->InputFileLabel[i].time_tag,"Not Defined");
    strcpy(header->InputFileLabel[i].software_version,"Not Defined");
    strcpy(header->InputFileLabel[i].linktime,"Not Defined");
  }
}
