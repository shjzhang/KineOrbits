/* @(#) vector_create.c      1.1 07/18/00
	Purpose:
           Creates storage for vector structure. Storage is 
           _not_ initialized to 0.

Initial coding:
   09/21/95 Willy Bertiger
*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

#include <stdlib.h>		/* malloc */
#include <stdio.h>		/* printf */

vector vector_create( 
/* Input:    */
          int size
/* Output: function value */
 ){

 static char SccsId[] = "@(#) vector_create.c      1.1 07/18/00";
  vector r; 

  r.size = size;
  r.value = (real * ) malloc( size * sizeof(real) );
  if ( !r.value )  fprintf( stderr ,"Error in vector_create\n" ) ;

  return r;
}
