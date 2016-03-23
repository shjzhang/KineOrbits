/* @(#) vint_create.c      1.1 07/18/00
	Purpose:
           Creates storage for vint structure. Storage is 
           _not_ initialized to 0.

Initial coding:
   09/29/95 RJM 
*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

#include <stdlib.h>		/* malloc */
#include <stdio.h>		/* printf */

vint vint_create( 
/* Input:    */
          int size
/* Output: function value */
 ){

 static char SccsId[] = "@(#) vint_create.c      1.1 07/18/00";
  vint vi; 

  vi.size = size;
  vi.value = (int * ) malloc( size * sizeof(int) );
  if ( ! vi.value )  fprintf( stderr ,"Error in vint_create\n" ) ;

  return vi;
}
