#include <stdio.h>
#include <string.h>
#include "GRACEfiletype.h"

#define loop(A,B) for(A=0;A<B;A++)

static char SccsId[] = "$Id: PointerLib.c,v 1.2 2004/08/30 21:03:34 wib Exp $";

long GetFileType (char *txt_pointer)
/*----------------------------------------------------------------------------->
/ purpose: return file type pointer based on text version of pointername
/
/ coded by:  Gerhard L.H. Kruizinga                      03/06/01
/
<-----------------------------------------------------------------------------*/
{
  long i;

  loop(i,NRFILETYPES)
  {
    if (strcmp(txt_pointer,FileTypeName[i]) == 0) return FileTypePointer[i];
  }

  fprintf(stderr,"\n FileTypePointer = %s has not been defined!!\n",txt_pointer);
  fprintf(stderr," Check input to GetFileType.\n\n");
  exit(1);
}
long GetAcc1aProd (char *txt_pointer)
/*----------------------------------------------------------------------------->
/ purpose: return acc1a product pointer based on text version of pointername
/
/ coded by:  Gerhard L.H. Kruizinga                      07/26/01
/
<-----------------------------------------------------------------------------*/
{
  long i;

  loop(i,NRACC1APODS)
  {
    if (strcmp(txt_pointer,Acc1aProdName[i]) == 0) return Acc1aProdPointer[i];
  }

  fprintf(stderr,"\n Acc1aProdName = %s has not been defined!!\n",txt_pointer);
  fprintf(stderr," Check input to GetAcc1aProd.\n\n");
  exit(1);
}
long GetHeaderLabel (char *txt_pointer)
/*----------------------------------------------------------------------------->
/ purpose: return file header label pointer based on text version of header
/          label name
/
/ coded by:  Gerhard L.H. Kruizinga                      03/06/01
/
<-----------------------------------------------------------------------------*/
{
  long i;

  loop(i,NRHEADERLABELS)
  {
    if (strcmp(txt_pointer,HeaderLabelName[i]) == 0) return HeaderLabelPointer[i];
  }

  fprintf(stderr,"\n HeaderLabelPointer = %s has not been defined!!\n",txt_pointer);
  fprintf(stderr," Check input to GetHeaderLabel.\n\n");
  exit(1);
}
long GetFileTypeName (long SectorPointer, char* txt_pointer)
/*----------------------------------------------------------------------------->
/ purpose: return file type name based on file type pointer based 
/
/ coded by:  Gerhard L.H. Kruizinga                      03/06/01
/
<-----------------------------------------------------------------------------*/
{
  long i;

  strcpy(txt_pointer,"NOT_DEFINED");

  loop(i,NRFILETYPES)
  {
    if (SectorPointer == FileTypePointer[i]) 
    {
      strcpy(txt_pointer,FileTypeName[i]);
      return 0;
    }
  }

  fprintf(stderr,"\n SectorPointer = %d has not been defined!!\n",SectorPointer);
  fprintf(stderr," Check input to GetFileTypeName.\n\n");
  exit(1);
}
long GetAcc1aProdName (long SectorPointer, char* txt_pointer)
/*----------------------------------------------------------------------------->
/ purpose: return acc1a product name based on pointer based  acc1a pointer
/
/ coded by:  Gerhard L.H. Kruizinga                      07/26/01
/
<-----------------------------------------------------------------------------*/
{
  long i;

  strcpy(txt_pointer,"NOT_DEFINED");

  loop(i,NRACC1APODS)
  {
    if (SectorPointer == Acc1aProdPointer[i]) 
    {
      strcpy(txt_pointer,Acc1aProdName[i]);
      return 0;
    }
  }

  fprintf(stderr,"\n Acc1aProdPointer = %d has not been defined!!\n",SectorPointer);
  fprintf(stderr," Check input to GetAcc1aProdName.\n\n");
  exit(1);
}
long GetHeaderLabelName (long HeaderRecPointer, char *txt_pointer)
/*----------------------------------------------------------------------------->
/ purpose: return header label name based on file header label pointer 
/
/ coded by:  Gerhard L.H. Kruizinga                      03/06/01
/
<-----------------------------------------------------------------------------*/
{
  long i;

  strcpy(txt_pointer,"NOT_DEFINED");

  loop(i,NRHEADERLABELS)
  {
    if (HeaderRecPointer == HeaderLabelPointer[i]) 
    {
      strcpy(txt_pointer,HeaderLabelName[i]);
      return 0;
    }
  }

  fprintf(stderr,"\n HeaderRecPointer = %d has not been defined!!\n",HeaderRecPointer);
  fprintf(stderr," Check input to GetHeaderLabelName.\n\n");
  exit(1);
}
