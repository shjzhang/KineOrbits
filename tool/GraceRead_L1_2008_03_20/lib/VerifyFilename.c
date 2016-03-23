#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include "GRACEreport.h"
#include "TimeLib.h"

static char SccsId[] = "$Id: VerifyFilename.c,v 1.3 2004/08/30 21:03:37 wib Exp $";

long VerifyFilename (char *filename, char *BaseFilename, char *FileKey,
                     int *year, int *month, int *day, char *SatId, 
                     long *VersionNumber, char *VersionChar, char *ext)
/*----------------------------------------------------------------------------->
/ purpose:  retrieve base filename from filename (which might include pathname)
/           and verify the the basefilename is a standard filename according
/           to
/                PRDID_YYYY-MM-DD_S_VS.ext
/ 
/ coded by: Gerhard L.H. Kruizinga                06/25/01
/ 
/ input:
/        filename      character string containing complete path + filename
/        BaseFilename  base filename retrieved from file name  
/        FileKey       PRDID for standand product filename
/        year          year from filename
/        month         month from filename
/        day           day from filename
/        SatId         satellite id from filename
/        VersionNumber Version number from filename is version is a numeral
/                      otherwise is set to -1
/        VersionChar   Character string of Version from file name
/        ext           filename extension (if any otherwise \0)
/
/        return  -1L   failure to read filename components
/                -2L   filename extension doesn't match "dat","asc","rpt","pass"
/                -3L   version number is not a numeral value
/                -4L   version number is outside the range of < 0 or >99
<-----------------------------------------------------------------------------*/
{   
  long           i,ext_check;
  long           counter,point_counter;

  GetBaseFilename (filename, BaseFilename);

  FileKey[0]     = '\0';
  *year          =   0 ;
  *month         =   0 ;
  *day           =   0 ;
  SatId[0]       = '\0';
  VersionChar[0] = '\0';
  ext[0]         = '\0';
  *VersionNumber  =  -1 ;

  if (sscanf(BaseFilename,"%5s_%d-%d-%d_%1s_%s",FileKey,year,month,day,SatId,
             VersionChar) != 6)
  {
    return -1L;
  }

  point_counter = -1;

  loop(counter,strlen(VersionChar))
  {
    if (strncmp(VersionChar+counter,".",1) == 0 && point_counter == -1) 
       point_counter = counter;
  }

  if (point_counter == -1) return -1L;

  strcpy(ext,VersionChar+point_counter+1);

  VersionChar[point_counter]='\0';
  
  /* check for standard extensions */

  ext_check = 0;
  if (strncmp(ext,"dat",3)  != 0) ext_check = 1;
  if (strncmp(ext,"asc",3)  != 0) ext_check = 2;
  if (strncmp(ext,"rpt",3)  != 0) ext_check = 3;
  if (strncmp(ext,"pass",4) != 0) ext_check = 4;

  if (ext_check == 0) return -2L;

  /* check if version is a numeral between 0 and 99 */

  if (sscanf(VersionChar,"%ld",VersionNumber) != 1) 
  {
    *VersionNumber  =  -1 ;
    return 0L;    
  }

  if (*VersionNumber < 0 || *VersionNumber > 99) return -4L; 

  return 0L;
}
