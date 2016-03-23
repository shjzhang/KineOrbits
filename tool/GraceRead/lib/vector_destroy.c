/* @(#) vector_destroy.c      1.1 07/18/00

	Purpose: Frees storage for vector structure.  */

#include "GRACEdefs.h"
#include "GRACEprototype.h"

#include <stdlib.h>

void vector_destroy( vector v ) 
{
 static char SccsId[] = "@(#) vector_destroy.c      1.1 07/18/00";
  free ( v.value ) ;
}
