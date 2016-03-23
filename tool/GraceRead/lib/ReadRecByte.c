#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: ReadRecByte.c,v 1.4 2004/08/30 21:03:32 wib Exp $";

void ReadRecByte(FILE* src, long *RecBytes)
/*----------------------------------------------------------------------------->
/ purpose: read leading or trailing 4 bytes in fortran unformatted binary file
/          and return number of bytes
/
/ coded by: G.L.H. Kruizinga        04/06/01
/
/ input:  *src      Pointer to fortran unformatted binary file
/ output: *RecBytes Pointer to Integer to return number of bytes in record
/
/-----------------------------------------------------------------------------*/
{
  fread(RecBytes,sizeof(long),1,src);
}
