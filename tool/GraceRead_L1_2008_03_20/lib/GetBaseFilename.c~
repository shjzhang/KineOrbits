#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include "TimeLib.h"

static char SccsId[] = "$Id: GetBaseFilename.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

void GetBaseFilename (char *filename, char *BaseFilename)
/*----------------------------------------------------------------------------->
/ purpose:  retrieve base filename from filename (which might include pathname)
/
/ coded by: Gerhard L.H. Kruizinga                01/09/01
/ modified: Gerhard L.H. Kruizinga                05/08/01
/
/ input:
/        filename      character string containing complete path + filename
/        BaseFilename  base filename retrieved from file name
<-----------------------------------------------------------------------------*/
{ 
  long start_counter,i;
  
  start_counter = 0;

  loop(i,strlen(filename)) if (strncmp(&filename[i],"/",1) == 0) start_counter = i;

  if (start_counter != 0) start_counter++;
  
  strcpy(BaseFilename,&filename[start_counter]);
} 
