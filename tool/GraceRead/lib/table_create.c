/****************************************************************************
*      RTG Source Code,                                                     *
*      Copyright (C) 1996, California Institute of Technology               *
*      U.S. Government Sponsorship under NASA Contract NAS7-1260            *
*                    (as may be time to time amended)                       *
*                                                                           *
*      RTG is a trademark of the California Institute of Technology.        *
*                                                                           *
*                                                                           *
*      written by Yoaz Bar-Sever, Willy Bertiger, Bruce Haines,             *
*                 Angelyn Moore, Ron Muellerschoen, Tim Munson,             *
*                 Larry Romans, and Sien Wu                                 *
****************************************************************************/
/* 

   Purpose:
          Create storage for table structure.

   11/09/95  Willy Bertiger

*/

#include <stdio.h>
#include "TimeLib.h"
#include <stdlib.h>


table table_create( 

/* Input: */
		   int size
/* Output: function value */
		   )

{

/* Local: */
  static char SccsId[] = "@(#) table_create.c       1.1 01/08/01";


  table r;

  r.size = size;

  r.x    = (real * ) malloc( size * sizeof(real) );
  if ( !r.x )  {
    fprintf(stderr,"Severe error in allocation r.x in table_create\n");
    exit(0);
  }
  r.y    = (real * ) malloc( size * sizeof(real) );

  if ( !r.y )  {
    fprintf(stderr,"Severe error in allocation r.y in table_create\n");
    exit(0);
  }

  return(r);
}






