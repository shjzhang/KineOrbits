/* @(#) GRACEio_utils.h      1.2 06/25/01 */

#ifndef _GRACEio_utils_h_
#define _GRACEio_utils_h_

#include "GRACEdefs.h"

/*----------------------------------------------------------------------------->
/  Structure definitions
<-----------------------------------------------------------------------------*/
/* None at the moment                                                         */
      
/*----------------------------------------------------------------------------->
/ Function Prototypes 
<-----------------------------------------------------------------------------*/


/*----------------------------------------------------------------------------*/
void SetCharBit(
                unsigned char  *value,   /* set nth-bit to 1 in value         */
                long           nth_bit
               );

/*----------------------------------------------------------------------------*/
int PutCharBits(
                unsigned char x,         /* This function takes n bits        */
                int px,                  /* in the input variable x starting  */
                unsigned char *y,        /* at position px, and puts them into*/
                int py,                  /* the output variable y starting at */
                int n                    /* position py                       */
               );
/* return:	0	normal return
               -1	incorrect arguments used for subroutine
*/

/*----------------------------------------------------------------------------*/
void SetShortBit(
                unsigned short  *value,  /* set nth-bit to 1 in value         */
                long           nth_bit
               );
/*----------------------------------------------------------------------------*/
void UnSetShortBit(
                unsigned short  *value,  /* set nth-bit to 0 in value         */
                long           nth_bit
               );
/*----------------------------------------------------------------------------*/
int PutShortBits(
                unsigned short x,        /* This function takes n bits        */
                int px,                  /* in the input variable x starting  */
                unsigned short *y,       /* at position px, and puts them into*/
                int py,                  /* the output variable y starting at */
                int n                    /* position py                       */
               );
/* return:	0	normal return
               -1	incorrect arguments used for subroutine
*/
/*----------------------------------------------------------------------------*/
void SetLongBit(
                unsigned long  *value,   /* set nth-bit to 1 in value         */
                long           nth_bit
               );
/*----------------------------------------------------------------------------*/
void UnSetLongBit(
                unsigned long  *value,   /* set nth-bit to 0 in value         */
                long           nth_bit
               );
/*----------------------------------------------------------------------------*/
int PutLongBits(
                unsigned long x,         /* This function takes n bits        */
                int px,                  /* in the input variable x starting  */
                unsigned long *y,        /* at position px, and puts them into*/
                int py,                  /* the output variable y starting at */
                int n                    /* position py                       */
               );
/* return:	0	normal return
               -1	incorrect arguments used for subroutine
*/

/*----------------------------------------------------------------------------*/
void GetCharBits
               (
                unsigned char   value,

                char            *bits
               );

/*----------------------------------------------------------------------------*/
void GetShortBits
               (
                short           value,

                char            *bits
               );

/*----------------------------------------------------------------------------*/
void GetLongBits
               (
                long		value,

                char            *bits
               );

/*----------------------------------------------------------------------------*/
long GetLongArgv(                         /* return long after string in args */
/* input */
               int argc,                  /* number of arguments              */
               char *argv[],              /* arguments array                  */
               char *str                  /* search string                    */
               );
/* return:     long after "str" in argument list to program
*/

/*----------------------------------------------------------------------------*/
void Get2LongArgv(                        /* return 2 longs after string      */
/* input */
               int argc,                  /* number of arguments              */
               char *argv[],              /* arguments array                  */
               char *str,                 /* search string                    */
               long *quan1,               /* first long after string          */
               long *quan2                /* second long after string         */
               );
/*----------------------------------------------------------------------------*/
int GetIntArgv(
/* input */
               int argc,                  /* number of arguments              */
               char *argv[],              /* arguments array                  */
               char *str                  /* search string                    */
               );
/* return:     int after "str" in argument list to program
*/

/*----------------------------------------------------------------------------*/
char *GetStrArgv(
/* input */
               int argc,                  /* number of arguments              */
               char *argv[],              /* arguments array                  */
               char *str                  /* search string                    */
               );
/* return:     chars after "str" in argument list to program
*/

/*----------------------------------------------------------------------------*/
double GetDoubleArgv(
/* input */
               int argc,                  /* number of arguments              */
               char *argv[],              /* arguments array                  */
               char *str                  /* search string                    */
               );
/* return:     double after "str" in argument list to program
*/
/*----------------------------------------------------------------------------*/
int FindOpt (
/* input */
               int argc,                  /* number of arguments              */
               char *argv[],              /* arguments array                  */
               char *str                  /* search string                    */
               );
/* return:     index of string "str" in argv[] arguments array
*/

long GetArgEnvGraceVersion(
               int argc,                  /* number of arguments              */
               char *argv[]               /* arguments array                  */
               );
/* return:     return version number 0<= <= 99 or -1 upon errror
*/


#undef _mk_extern_

#endif	/* _GRACEio_utils_h_ */
