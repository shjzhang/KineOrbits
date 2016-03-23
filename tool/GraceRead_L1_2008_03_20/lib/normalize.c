/* Purpose: normalizing a vector */

#include "GRACEdefs.h"
#include "GRACEprototype.h"

void normalize ( vector a )
{
  static char SccsId[] = "@(#) normalize.c      1.1 08/03/00";
  real norm ;
  int  i ;

  norm = sqrt( dot( a, a ) );

  if (norm) for (i=0; i<a.size; i++) a.value[i] /= norm ;

  return ;
}
