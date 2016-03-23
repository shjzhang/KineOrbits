#include <stdio.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

static char SccsId[] = "$Id: fwrite_grace.c,v 1.4 2004/08/30 21:03:39 wib Exp $";

size_t fwrite_grace(void *ptr, size_t  size,  size_t  nitems,  
                    FILE *stream)
/*----------------------------------------------------------------------------->
/ purpose:  identical function as standard c-function fwrite with the inclusion
/           of endian architecture check. If little-endian then bytes will
/           be swapped
/           endian architectures
/
/ coded by: Gerhard L.H. Kruizinga                08/27/01
/
<-----------------------------------------------------------------------------*/
{

   size_t  out;

   if (little_endian()) swapbyte((char *)ptr,size);

   out = fwrite(ptr,size,nitems,stream); 

   if (little_endian()) swapbyte((char *)ptr,size);

   return out;
}
