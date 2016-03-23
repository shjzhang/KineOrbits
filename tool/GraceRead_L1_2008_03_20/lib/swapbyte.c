#include "GRACEiolib.h"

static char SccsId[] = "$Id: swapbyte.c,v 1.3 2004/08/30 21:03:39 wib Exp $";

long swapbyte(char buf[],size_t num_bytes)
/*----------------------------------------------------------------------------->
/ purpose:  swap bytes depinding on specification to accomadate different
/           endian architectures
/
/ coded by: Gerhard L.H. Kruizinga                08/24/01
/
<-----------------------------------------------------------------------------*/
{
   char            temp;

   long            i,half_num_bytes;

   half_num_bytes = (long) num_bytes/2;
   
   loop(i,half_num_bytes)
   {
     temp               = buf[i];
     buf[i]             = buf[num_bytes-i-1];
     buf[num_bytes-i-1] = temp;
   }

   return 0L;
} 
