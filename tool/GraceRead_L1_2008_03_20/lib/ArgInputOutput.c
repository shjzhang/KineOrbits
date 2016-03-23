#include "GRACEiolib.h"
#include "GRACEio_utils.h"
#include <string.h>

static char SccsId[] = "$Id: ArgInputOutput.c,v 1.5 2004/08/30 21:03:33 wib Exp $";

/* ************************************************************************** */
int FindOpt (int argc, char *argv[], char *str)

{
  int        i, res;

  res = -1;
  for (i=1; i< argc; i++)
     if (strcmp(argv[i], str) == 0)
     res= i;

  return(res);
}

/* ************************************************************************** */
long GetLongArgv(int argc,char *argv[], char *str)

{

 int        idum;

 long       LongVal;

 idum = FindOpt(argc,argv, str);

 if (idum != -1 && idum <= argc-2)
   {
    sscanf(argv[idum + 1], "%ld", &LongVal);
    return LongVal;
   }
 else
   {
    fprintf(stderr,"\n %s must be specified ! \n\n",str);
    exit(1);
   }
}

void Get2LongArgv(int argc,char *argv[], char *str, long *quan1, long *quan2)

{

 int        idum;

 idum = FindOpt(argc,argv, str);

 if (idum != -1 && idum <= argc-3)
   {
    sscanf(argv[idum + 1], "%ld", quan1);
    sscanf(argv[idum + 2], "%ld", quan2);
    return ;
   }
 else
   {
    fprintf(stderr,"\n %s must be specified ! \n\n",str);
    exit(1);
   }
}
/* ************************************************************************** */
int GetIntArgv(int argc,char *argv[], char *str)

{

 int        idum;

 int        IntVal;

 idum = FindOpt(argc,argv, str);

 if (idum != -1 && idum <= argc-2)
   {
    sscanf(argv[idum + 1], "%ld", &IntVal);
    return IntVal;
   }
 else
   {
    fprintf(stderr,"\n %s must be specified ! \n\n",str);
    exit(1);
   }
}
/* ************************************************************************** */
char *GetStrArgv(int argc,char *argv[], char *str)

{

 int        idum;

 idum = FindOpt(argc,argv, str);

 if (idum != -1 && idum <= argc-2)
   {
    return argv[idum+1];
   }
 else
   {
    fprintf(stderr,"\n %s must be specified ! \n\n",str);
    exit(1);
   }
}
/* ************************************************************************** */
double GetDoubleArgv(int argc,char *argv[], char *str)

{

 int        idum;

 double     DoubleVal;

 idum = FindOpt(argc,argv, str);

 if (idum != -1 && idum <= argc-2)
   {
    sscanf(argv[idum + 1], "%lf", &DoubleVal);
    return DoubleVal;
   }
 else
   {
    fprintf(stderr,"\n %s must be specified ! \n\n",str);
    exit(1);
   }
}
long GetArgEnvGraceVersion(int argc, char *argv[])
/*----------------------------------------------------------------------------->
/ purpose: return version number based on argument list with key -version
/          or if arg list is not defined then extract version from enviroment
/          variable GRACE_PRODUCT_VERSION
/
/ coded by: Gerhard L.H. Kruizinga                           06/25/2001
/
/ return: version number 0<= <=99 or -1L on failure
/
/ note:   version number defaults to 0 if neither argv or GRACE_PRODUCT_VERSION
/         is defined
/
<-----------------------------------------------------------------------------*/
{
 long VersionNumber,arg_ndx;

 char VersionChar[1000];

 VersionNumber = 0;               

 if (getenv("GRACE_PRODUCT_VERSION"))
 {
   arg_ndx = FindOpt(argc,argv, "-version");
   if (arg_ndx != -1)
   {
     VersionNumber = GetLongArgv(argc,argv,"-version");
   }
   else
   {
     strcpy(VersionChar,getenv("GRACE_PRODUCT_VERSION"));
     if (sscanf(VersionChar,"%ld",&VersionNumber) != 1)
     {
       fprintf(stderr,
            "\n VersionNumber in enviroment variable GRACE_PRODUCT_VERSION = \"%s\"\n"
            ,VersionChar);
       fprintf(stderr," must be a numeral!! \n\n");
       return -1L;
     }   
     if (VersionNumber < 0 || VersionNumber > 99)
     {   
       fprintf(stderr,   
        "\n Version number in enviroment variable GRACE_PRODUCT_VERSION = %s\n",
               VersionChar);
       fprintf(stderr," needs to be defined within range >0 and <= 99 !!\n\n");
       return -1L;             
     }   
   }     
 }
 else    
 {                          
   arg_ndx = FindOpt(argc,argv, "-version");
   if (arg_ndx != -1) VersionNumber = GetLongArgv(argc,argv,"-version");
 }
 
 if (VersionNumber < 0 || VersionNumber > 99)
 {
   fprintf(stderr,"\n Specified Version number  = %ld ", VersionNumber); 
   fprintf(stderr," needs to be defined within range >0 and <= 99 !!\n\n");
   exit(1);
 }

 return VersionNumber;
}
