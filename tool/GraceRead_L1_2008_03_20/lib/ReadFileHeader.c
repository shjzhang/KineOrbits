#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define MAXLINECHAR 1000

static char SccsId[] = "$Id: ReadFileHeader.c,v 1.14 2004/08/30 21:03:35 wib Exp $";


boolean ReadFileHeader(FILE *src,FileHeader_t *header)
/*----------------------------------------------------------------------------->
/ purpose: Read Header information from file src and put pertinent information
/          in FileHeader_t struct
/
/ coded by: Gerhard L.H. Kruizinga                           08/09/2000
/
/ input:  *src    Pointer to data file
/ output: *record Pointer to header struct
/
/ return:      true    normal return
/              false   problem reading complete header information from file
/
/ note:   After ReadFileHeader has read the header of file src, the pointer
/         in file src will be located at the first record of the data section
/         of the file
<-----------------------------------------------------------------------------*/
{
  char line[MAXLINECHAR];
  char String[MAXLINECHAR],filekey[MAXLINECHAR],ttag[MAXLINECHAR];
  char version[MAXLINECHAR],linktime[MAXLINECHAR];

  long i,j,n;
  int  len;

  HeaderInputFileLabel_t *pHIF;

  char *pS;

  InitializeHeaders();

  n = 0;

  if (fgets(line,MAXLINECHAR,src) == NULL)
  {
    fprintf(stderr,"\n Empty File in ReadFileHeader!!\n\n");
    return false;
  }

  if (strlen(line) > HEADERMAXCHAR+2)
  {
    fprintf(stderr,"\n Header line exceeds %d characters!\n",HEADERMAXCHAR);
    fprintf(stderr,"\n Current line : %s\n",line);
    fprintf(stderr," Check file header or change parameter HEADERMAXCHAR\n\n");
    return false;
  }
  if (strlen(line) <= 1)
  {
    fprintf(stderr,"\n Header line is less than 1 character!\n");
    fprintf(stderr," Input file is probably not a standard Level 1A or 1B file!\n\n");
    return false;
  }

  /* copy strlen-1 to remove \n from input line */
  len = strlen(line)-1;
  strncpy(&header->HeaderCards[n][0],line,len);
  header->HeaderCards[n][len] = '\0';
  
  while(strncmp(&header->HeaderCards[n][0],"END OF HEADER",13) != 0)
  {
    n++;
    if (fgets(line,MAXLINECHAR,src) == NULL)
    {
      fprintf(stderr,"\n Header in input file is incomplete or not a standard input file in ReadFileHeader!!\n\n");
      return false;
    }
    if (strlen(line) > HEADERMAXCHAR+2)
    {
      fprintf(stderr,"\n Header line exceeds %d characters!\n",HEADERMAXCHAR);
      fprintf(stderr,"\n Current line : %s\n",line);
      fprintf(stderr," Check file header or change parameter HEADERMAXCHAR\n\n");
      return false;
    }
    if (strlen(line) <= 1)
    {
      fprintf(stderr,"\n Header line is less than 1 character!\n");
      fprintf(stderr," Input file is probably not a standard Level 1A or 1B file!\n\n");
      return false;
    }
    /* copy strlen-1 to remove \n from input line */
    len = strlen(line)-1;
    strncpy(&header->HeaderCards[n][0],line,len);
    header->HeaderCards[n][len] = '\0';
  } 

/*----------------------------------------------------------------------------->
/ Set filetype, formattype, nrecord and init_flag in struct based on header
<-----------------------------------------------------------------------------*/

  header->nrecord = n; 
  header->init_flag = 1L;

  header->filetype = -1L;
  header->formattype = -1L;

  loop(i,header->nrecord)
  {
    if (strncmp(&header->HeaderCards[i][0],"FILE TYPE ",10) == 0)
    {
      sscanf(&header->HeaderCards[i][0]+HEADERLABELMAXCHAR+1,"%ld",&header->filetype);
    }
    if (strncmp(&header->HeaderCards[i][0],"FILE FORMAT 0=BINARY 1=ASCII  ",HEADERLABELMAXCHAR) == 0)
    {
      sscanf(&header->HeaderCards[i][0]+HEADERLABELMAXCHAR+1,"%ld",&header->formattype);
    }
  } 

  if (header->filetype == -1L)
  {
    fprintf(stderr,"File type cannot be determined for input file\n\n");
    return false;
  }
  if (header->formattype == -1L)
  {
    fprintf(stderr,"Format type cannot be determined for input file\n\n");
    return false;
  }

/*----------------------------------------------------------------------------->
/ Set all header entries in the appropriate locations
<-----------------------------------------------------------------------------*/

   SetChar(header->ProducerAgency,"NOT FOUND",9L);
   SetChar(header->ProducerInstitution,"NOT FOUND",9L);
   SetChar(header->SoftwareVersion,"NOT FOUND",9L);
   SetChar(header->Documentation,"NOT FOUND",9L);
   SetChar(header->SatelliteName,"NOT FOUND",9L);
   SetChar(header->SensorName,"NOT FOUND",9L);
   SetChar(header->TimeEpoch,"NOT FOUND",9L);
   header->TimeFirstObs = -1.0;
   header->TimeLastObs  = -1.0;
   header->NumberObs    = -1;
   SetChar(header->ProductCreateStartTime,"NOT FOUND",9L);
   SetChar(header->ProductCreateEndTime,"NOT FOUND",9L);

  loop(i,header->nrecord)
  {
    len = strlen(&header->HeaderCards[i][0]);
    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphProducerAgency")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->ProducerAgency,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphProducerInstitution")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->ProducerInstitution,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphSoftwareVersion")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->SoftwareVersion,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphDocumentation")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->Documentation,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphSatelliteName")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->SatelliteName,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphSensorName")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->SensorName,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphTimeEpoch")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->TimeEpoch,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphTimeFirstObs")][0],
                HEADERLABELMAXCHAR) == 0)
                sscanf(&header->HeaderCards[i][0]+HEADERLABELMAXCHAR+1,"%lf",
                       &header->TimeFirstObs);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphTimeLastObs")][0],
                HEADERLABELMAXCHAR) == 0)
                sscanf(&header->HeaderCards[i][0]+HEADERLABELMAXCHAR+1,"%lf",
                       &header->TimeLastObs);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphNumberObs")][0],
                HEADERLABELMAXCHAR) == 0)
                sscanf(&header->HeaderCards[i][0]+HEADERLABELMAXCHAR+1,"%ld",
                       &header->NumberObs);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphNumberBytes")][0],
                HEADERLABELMAXCHAR) == 0)
                sscanf(&header->HeaderCards[i][0]+HEADERLABELMAXCHAR+1,"%ld",
                       &header->NumberBytes);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphProductCreateStartTime")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->ProductCreateStartTime,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphProductCreateEndTime")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->ProductCreateEndTime,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphFileName")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->FileName,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphProcessLevel")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->ProcessLevel,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);

    if (strncmp(&header->HeaderCards[i][0],
                &FileHeaderLabel[header->filetype][GetHeaderLabel("iphSoftwareLinkTime")][0],
                HEADERLABELMAXCHAR) == 0)
                SetChar(header->LinkTime,
                       &header->HeaderCards[i][HEADERLABELMAXCHAR+2],
                        len-HEADERLABELMAXCHAR-2);


  }

  header->NinputFileLabel = 0;

  /*>>>> extract input filenames <<<<*/

  loop(i,header->nrecord)
  {
    len = strlen(&header->HeaderCards[i][0]);
    if (strncmp(&header->HeaderCards[i][0],"INPUT FILE NAME               ",
                HEADERLABELMAXCHAR) == 0)
    {
      SetChar(String,&header->HeaderCards[i][HEADERLABELMAXCHAR+2],
              len-HEADERLABELMAXCHAR-2);

      loop(j,strlen(String))
      {
        if (strncmp(&String[j],"<-",2) == 0)
        {
          strncpy(filekey,String,j);
          filekey[j] = '\0';
          strcpy(header->InputFileLabel[header->NinputFileLabel].filekey,filekey);
          strcpy(header->InputFileLabel[header->NinputFileLabel].name,&String[j+2]);
        }
      }

      if (! filekey) fprintf(stderr," Filekey not defined in ->%s<-\n",String);
      if (! ttag) fprintf(stderr," TimeTag not defined in ->%s<-\n",String);

      header->NinputFileLabel++;
    }
  }

  /*>>>> extract input file time tags <<<<*/

  loop(i,header->nrecord)
  {
    len = strlen(&header->HeaderCards[i][0]);
    if (strncmp(&header->HeaderCards[i][0],"INPUT FILE TIME TAG (UTC)     ",
                HEADERLABELMAXCHAR) == 0)
    {
      SetChar(String,&header->HeaderCards[i][HEADERLABELMAXCHAR+2],
              len-HEADERLABELMAXCHAR-2);

      filekey[0] = '\0';
      ttag[0]    = '\0';

      loop(j,strlen(String))
      {
        if (strncmp(&String[j],"<-",2) == 0)
        {
          strncpy(filekey,String,j);
          filekey[j] = '\0';
          strcpy(ttag,&String[j+2]);
        }
      }

      if (! filekey) fprintf(stderr," Filekey not defined in ->%s<-\n",String);
      if (! ttag) fprintf(stderr," TimeTag not defined in ->%s<-\n",String);
    
      loop(j,header->NinputFileLabel)
      {
        if (strcmp(filekey,header->InputFileLabel[j].filekey) == 0)
        {
          strcpy(header->InputFileLabel[j].time_tag,ttag);
        }
      }
    }
  }

  /*>>>> extract input file software versions <<<<*/

  loop(i,header->nrecord)
  {
    len = strlen(&header->HeaderCards[i][0]);
    if (strncmp(&header->HeaderCards[i][0],"INPUT FILE SOFTWARE VERSION   ",
                HEADERLABELMAXCHAR) == 0)
    {
      SetChar(String,&header->HeaderCards[i][HEADERLABELMAXCHAR+2],
              len-HEADERLABELMAXCHAR-2);

      filekey[0] = '\0';
      version[0] = '\0';

      loop(j,strlen(String))
      {
        if (strncmp(&String[j],"<-",2) == 0)
        {
          strncpy(filekey,String,j);
          filekey[j] = '\0';
          strcpy(version,&String[j+2]);
        }
      }

      if (! filekey) fprintf(stderr," Filekey    not defined in ->%s<-\n",String);
      if (! version) fprintf(stderr," SW version not defined in ->%s<-\n",String);
    
      loop(j,header->NinputFileLabel)
      {
        if (strcmp(filekey,header->InputFileLabel[j].filekey) == 0)
        strcpy(header->InputFileLabel[j].software_version,version);
      }
    }
  }

  /*>>>> extract input file link time tags <<<<*/

  loop(i,header->nrecord)
  {
    len = strlen(&header->HeaderCards[i][0]);
    if (strncmp(&header->HeaderCards[i][0],"INPUT FILE LINKTIME TAG       ",
                HEADERLABELMAXCHAR) == 0)
    {
      SetChar(String,&header->HeaderCards[i][HEADERLABELMAXCHAR+2],
              len-HEADERLABELMAXCHAR-2);

      filekey[0] = '\0';
      linktime[0] = '\0';

      loop(j,strlen(String))
      {
        if (strncmp(&String[j],"<-",2) == 0)
        {
          strncpy(filekey,String,j);
          filekey[j] = '\0';
          strcpy(linktime,&String[j+2]);
        }
      }

      if (! filekey) fprintf(stderr," Filekey   not defined in ->%s<-\n",String);
      if (! linktime) fprintf(stderr,"LinkTime  not defined in ->%s<-\n",String);
    
      loop(j,header->NinputFileLabel)
      {
        if (strcmp(filekey,header->InputFileLabel[j].filekey) == 0)
        strcpy(header->InputFileLabel[j].linktime,linktime);
      }
    }
  }

  return true;
}
