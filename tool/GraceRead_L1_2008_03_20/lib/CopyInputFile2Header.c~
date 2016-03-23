#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEfiletype.h"

static char SccsId[] = "$Id: CopyInputFile2Header.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

#define MAXCHAR 1000

boolean CopyInputFile2Header(FileHeader_t *src_header,char *filekey, 
                             FileHeader_t *dst_header)
/*----------------------------------------------------------------------------->
/ purpose: Copy Input File header information from src_header for "filekey"
/          to dst_header
/
/ coded by: Gerhard L.H. Kruizinga                           05/05/2003
/
/ input:   *src_header      Pointer to header struct to copy from
/          *filekey         Filekey name of inputfile     
/ output:  *dst_header      Pointer to header struct to copy to
/
/ return:      0       normal return
/              1       error copying header
/
<-----------------------------------------------------------------------------*/
{
  long j,notfound;

  char software_version[MAXCHAR],linktime[MAXCHAR];

  notfound = 1;

  loop(j,src_header->NinputFileLabel)
  {
    if (strncmp(filekey,src_header->InputFileLabel[j].filekey,strlen(filekey)) == 0)
    {
      software_version[0] = '\0';
      linktime[0]         = '\0';

      if (strcmp(src_header->InputFileLabel[j].software_version,"Not Defined") != 0)  
         strcpy(software_version,src_header->InputFileLabel[j].software_version);
      if (strcmp(src_header->InputFileLabel[j].linktime,"Not Defined") != 0)  
         strcpy(linktime,src_header->InputFileLabel[j].linktime);
      
      AddInputFile2Header(dst_header,filekey,
                          src_header->InputFileLabel[j].name,
                          src_header->InputFileLabel[j].time_tag,
                          software_version,
                          linktime);
      notfound = 0L;
    }
  }

  if (notfound) return True;

  return False;
}
