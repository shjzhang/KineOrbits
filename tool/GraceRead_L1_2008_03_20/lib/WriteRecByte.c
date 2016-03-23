#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: WriteRecByte.c,v 1.4 2004/08/30 21:03:32 wib Exp $";

void WriteRecByte(FILE* dst, long RecBytes)
/*----------------------------------------------------------------------------->
/ purpose: write leading or trailing 4 bytes in fortran unformatted binary file
/
/ coded by: G.L.H. Kruizinga        04/07/01
/
/ input:  *dst      Pointer to fortran unformatted binary file
/ output: *RecBytes Pointer to Integer to return number of bytes in record
/
/-----------------------------------------------------------------------------*/
{
  if (fwrite(&RecBytes,sizeof(long),1,dst) != 1)
  {
    fprintf(stderr,"Error writing RecBytes =%d\n",RecBytes);
    exit(1);
  }
}
