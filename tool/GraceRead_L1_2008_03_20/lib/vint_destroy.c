/* @(#) vint_destroy.c      1.1 07/18/00
	Purpose: Frees storage for vint structure.  */

#include "GRACEdefs.h"
#include "GRACEprototype.h"

#include <stdlib.h>

void vint_destroy( vint v )
{
 static char SccsId[] = "@(#) vint_destroy.c      1.1 07/18/00";
  free ( v.value ) ;
}
