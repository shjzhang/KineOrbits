#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define MAXCHAR 1000

main ()
{
  char in[MAXCHAR];
  char out[MAXCHAR];

  strcpy(in,"$Id: Bin2AsciiLevel1.c,v 1.52 2004/08/17 17:45:31 glk Exp glk $");

  ReformatRCStag(in,out);

  fprintf(stdout,"in  ->\"%s\"\n",in);
  fprintf(stdout,"out ->\"%s\"\n",out);
}
