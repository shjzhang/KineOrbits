#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: WriteTdp.c,v 1.2 2004/08/30 21:03:32 wib Exp $";

long WriteTdp(FILE* dst, tdp_t *TdpRec)
/*----------------------------------------------------------------------------->
/ purpose: Write a tdp record to file dst based on data in TdpRec struct
/
/ coded by: G.L.H. Kruizinga        04/16/01
/
/ input:  *src      Pointer to tdp file
/ output: *TdpRec   Pointer to TdpRec Struct
/
/-----------------------------------------------------------------------------*/
{

  fprintf(dst,"%.16g %.16g %.16g %.16g %s\n",TdpRec->time,TdpRec->apriori,
                                   TdpRec->value,TdpRec->sigma,TdpRec->name);
 
  return 0L;
}
