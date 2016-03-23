#include <string.h>
#include "GRACEiolib.h"

static char SccsId[] = "$Id: ModifyFileHeader.c,v 1.4 2004/08/30 21:30:26 glk Exp $";

boolean ModifyFileHeader(FileHeader_t *header,long RecPointer, char *ModString)
/*----------------------------------------------------------------------------->
/ purpose: Change Contents in header struct for RecPointer with Modstring
/
/ coded by: Gerhard L.H. Kruizinga                           08/15/2000
/
/ input:   *header    Pointer to header struct
/          RecPointer Pointer to Header Record entry
           *ModString Pointer to string to be inserted into header
/ output:  *header Pointer to header struct
/
/ return:      1       normal return
/              0       error modifying header
/
<-----------------------------------------------------------------------------*/
{
  long i;

  char write_format[HEADERMAXCHAR];
  char ModString2Write[1000];

  strcpy(ModString2Write,ModString);

  if (RecPointer == GetHeaderLabel("iphSoftwareVersion"))
  {
    ReformatRCStag(ModString,ModString2Write); 
  }

  if (header->filetype < 0 || header->filetype > NFILETYPEMAX-1)
  {
    fprintf(stderr," Filetype = %d does not exist!!\n",header->filetype);
    fprintf(stderr," Check input to ModifyHeader\n\n");
    exit(1);
  }

  sprintf(write_format,"%%-%ds: %%-%ds",HEADERLABELMAXCHAR,
          HEADERMAXCHAR-HEADERLABELMAXCHAR-2);

  if (strcmp(ModString2Write,"NONE") != 0)
  {
    sprintf(&header->HeaderCards[RecPointer][0],write_format,
            &FileHeaderLabel[header->filetype][RecPointer][0],
            ModString2Write);
  }
  else
  {
    sprintf(&header->HeaderCards[RecPointer][0],write_format,
            &FileHeaderLabel[header->filetype][RecPointer][0],
            &FileHeaderContents[header->filetype][RecPointer][0]);
  }
  
  return True;
}
