#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "%Z% %M%       %I% %G%";

long WriteGoaPosRecord(FILE* dst, pos_goa_t *posgoa)
/*----------------------------------------------------------------------------->
/ purpose: write a pos_goa record to file dst based on information in 
/          posgoa struct
/
/ coded by: G.L.H. Kruizinga        04/18/01
/
/ input:  *dst      Pointer to fortran unformatted binary file
/         *posgoa   Pointer to goapos struct                               
/
/ Return 1L if end of file or error condition
/-----------------------------------------------------------------------------*/
{
  long i;

  loop(i,posgoa->nfield)
  {
    switch (i)
    {
      case 0:
             fprintf(dst,"%d ",posgoa->nfield);
             break;
      case 1:
             fprintf(dst,"%c ",posgoa->frame);
             break;
      case 2:
             fprintf(dst,"%s ",posgoa->satname);
             break;
      case 3:
             fprintf(dst,"%.16g ",posgoa->time);
             break;  
      case 4:
             fprintf(dst,"%.16g ",posgoa->xpos);
             break;
      case 5:
             fprintf(dst,"%.16g ",posgoa->ypos);
             break;
      case 6:
             fprintf(dst,"%.16g ",posgoa->zpos);
             break;
      case 7:
             fprintf(dst,"%.16g ",posgoa->xvel);
             break;          
      case 8:
             fprintf(dst,"%.16g ",posgoa->yvel);
             break;
      case 9:
             fprintf(dst,"%.16g ",posgoa->zvel);
             break;
      case 10:
             fprintf(dst,"%.16g ",posgoa->xpos_sig);
             break;
      case 11:
             fprintf(dst,"%.16g ",posgoa->ypos_sig);
             break;
      case 12:
             fprintf(dst,"%.16g ",posgoa->zpos_sig);
             break;
      case 13:
             fprintf(dst,"%.16g ",posgoa->xvel_sig);
             break;
      case 14:
             fprintf(dst,"%.16g ",posgoa->yvel_sig);
             break;
      case 15:     
             fprintf(dst,"%.16g ",posgoa->zvel_sig);
             break;
             
    } 
  }  

  fprintf(dst,"\n");

  return 0L;
}
