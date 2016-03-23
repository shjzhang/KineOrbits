/* @(#) vdiff.c      1.1 08/03/00

   Purpose: Difference of two vectors.
*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

int vdiff( vector v1, vector v2, vector v3 )
{
 static char SccsId[] = "@(#) vdiff.c      1.1 08/03/00";

 int  i;                       /* index */
 for (i = 0 ; i < v3.size ; i++ ) v3.value[i] = v1.value[i] - v2.value[i];
 return;
}
