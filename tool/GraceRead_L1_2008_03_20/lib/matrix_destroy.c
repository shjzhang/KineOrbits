/* @(#) matrix_destroy.c      1.1 07/18/00
	Purpose:
           Frees storage for matrix structure. 
*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

#include <stdlib.h>

void matrix_destroy( matrix v ) 
{
 static char SccsId[] = "@(#) matrix_destroy.c      1.1 07/18/00";
  free ( v.value[0] ) ;
  free ( v.value ) ;
}
