/* @(#) dot.c      1.3 09/18/00

   Purpose: Return dot product of two vectors.
*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

real dot( vector x, vector y )
{
 static char SccsId[] = "@(#) dot.c      1.3 09/18/00";

 real d = 0;		/* accumulates dot product */
 int  i;		/* index */
 for (i = 0 ; i < x.size ; i++ ) d += x.value[i] * y.value[i];
 return d;
}
