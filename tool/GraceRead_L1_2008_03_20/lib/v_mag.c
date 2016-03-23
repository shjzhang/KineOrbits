/* @(#) v_mag.c      1.2 09/18/00

   Purpose: compute magnitude of a vector
*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

real v_mag ( vector a )
{
 static char SccsId[] = "@(#) v_mag.c      1.2 09/18/00";

 real d = 0;		/* accumulates dot product */
 int  i;		/* index */
 for (i = 0 ; i < a.size ; i++ ) d += a.value[i] * a.value[i];
 return sqrt( d );
}
