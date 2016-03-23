#include <stdio.h>
#include <string.h>
#define _mk_extern_
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEsyspath.h"

#define MAXREC  10
#define MAXCHAR 1000

#define loop3(A,B,C) for(A=B;A<=C;A++)

static char SccsId[] = "$Id: InitializeHeaders.c,v 1.8 2004/08/30 21:03:33 wib Exp $";

void    EncodeHeaderInfo(long,long,char *);
void    SetChar(char *, char *, long);

char *HeaderTypeChar[NFILETYPEMAX];
char *HeaderRecChar[NMAXHEADERREC];

void InitializeHeaders ()
/*----------------------------------------------------------------------------->
/ purpose: Initialize all header information for every filetype and store
/          information in Global Variables FileHeaderLabel and 
/          FileHeaderContents
/
/ coded by:  Gerhard L.H. Kruizinga                      08/15/00
/
<-----------------------------------------------------------------------------*/
{
  FILE *hdr;

  char line[MAXCHAR];
  char filename[MAXCHAR];
  char FileTypePointerName[HEADERMAXCHAR];
  char HeaderRecPointerName[HEADERMAXCHAR];


  long i,j,FileTypePointer,HeaderRecPointer;

  loop3(i,0,NFILETYPEMAX-1)
  {
    loop3(j,0,NMAXHEADERREC-1)
    {
     sprintf(&FileHeaderLabel[i][j][0],"NOT_DEFINED");
     sprintf(&FileHeaderContents[i][j][0],"NOT_DEFINED");
    }
  }

  sprintf(filename,"%s/HeaderText.txt",GRACESystemPath);

  hdr = fopen(filename,"rb"); 

  if (hdr == NULL)
  {
   fprintf(stderr,"\n Header text file %s cannot be opened !! \n\n",filename);
   exit(1);
  }

  while(fgets(line,MAXCHAR,hdr) != NULL)
  {
    if (strncmp(line,"ip",2) == 0)
    {
      sscanf(line,"%s",FileTypePointerName);
      FileTypePointer = GetFileType(FileTypePointerName);

      if (FileTypePointer < 0 || FileTypePointer > NFILETYPEMAX-1)
      {
        fprintf(stderr,"\n File Type Pointer %s is not defined in %s",
                           FileTypePointerName,GRACESystemPath);
        fprintf(stderr,"/GRACEiolib.h\n");
        fprintf(stderr," Or File type pointer (=%d) is out of range\n\n",FileTypePointer);
        exit(1);
      }

/*
      fprintf(stderr,"File type pointer (%20s) = %d\n",FileTypePointerName,
                      FileTypePointer);
*/
    }
    else
    {
      sscanf(line,"%s",HeaderRecPointerName);
      HeaderRecPointer = GetHeaderLabel(HeaderRecPointerName);

      if (HeaderRecPointer < 0 || HeaderRecPointer > NMAXHEADERREC-1)
      {
        fprintf(stderr,"\n Header Record Pointer %s is not defined in %s",
                           HeaderRecPointerName,GRACESystemPath);
        fprintf(stderr,"/GRACEiolib.h\n");
        fprintf(stderr," Or header pointer (=%d) is out of range\n\n",HeaderRecPointer);
        exit(1);
      }

/*
      fprintf(stderr,"  Header Rec pointer (%20s) = %d\n",HeaderRecPointerName,
                     HeaderRecPointer);
*/

      EncodeHeaderInfo(FileTypePointer,HeaderRecPointer,line);
    }
  }

/*
  loop3(i,0,NFILETYPEMAX-1)
  {
    loop3(j,0,NMAXHEADERREC-1)
      if (strcmp(&FileHeaderLabel[i][j][0],"NOT_DEFINED") != 0 &&
          strcmp(&FileHeaderContents[i][j][0],"NOT_DEFINED") != 0)
          fprintf(stderr,"%20s %20s\n",&FileHeaderLabel[i][j][0],
                                   &FileHeaderContents[i][j][0]);
  }
*/

  fclose(hdr);
}

void SetChar(char *Destination, char *Source, long Nchar)
/*----------------------------------------------------------------------------->
/ purpose: Copy Nchar characters from Source into Destination and remove new
/          line chars and \0 at the end of the copy
/
/ coded by:  Gerhard L.H. Kruizinga                      08/15/00
/
/ input:
/         Destination      character array where Nchar's from Source should be
                           copied to
/         Source           character array from which to copy Nchars
/         Nchar            Number of characters to copy from Source to Dest
<-----------------------------------------------------------------------------*/
{
  long i;

  char string[2];
  
  strcpy(string,"a");

  strncpy(Destination,Source,Nchar);

  if (Nchar > HEADERMAXCHAR-1) 
  {
    fprintf(stderr," Input string exceeds HEADERMAXCHAR = %d, Nchar = %d\n",
            HEADERMAXCHAR,Nchar);
    fprintf(stderr," Check input string: %s\n\n",Source);
    exit(1);
  }

  loop3(i,0,Nchar-1)
  {
   if (strncmp(&Destination[i],"\n",1) == 0) strncpy(&Destination[i]," ",1);
  }

/*
  strncpy(&Destination[Nchar],NULL,1);
*/
  Destination[Nchar]=string[1];
 
}
void EncodeHeaderInfo(long FileTypePointer, long HeaderRecPointer,char *line)
/*----------------------------------------------------------------------------->
/ purpose: Encode header label and contents information in Globar variables
/          FileHeaderLabel and FileHeaderContents
/
/ coded by:  Gerhard L.H. Kruizinga                      08/14/00
/
/ input:
/         FileTypePointer  Pointer for File Type                   
/         HeaderRecPointer Pointer to Header Record
/         line             input line for HeaderTxt.txt file to be encoded
/
<-----------------------------------------------------------------------------*/
{
  long i,LabelNdx,ContentsNdx,n,first,second;
  long nc;

  char string[HEADERMAXCHAR];

  i      =  0;
  first  =  0;
  second = -1;

  LabelNdx = -1;
  ContentsNdx = -1;

  nc = strlen(line);

  while (i < nc)
  {
    if (strncmp(&line[i],":",1) == 0 && second == 0) 
    {
      ContentsNdx = i;
      second      = 1;
    }
    if (strncmp(&line[i],":",1) == 0 && first  == 0) 
    {
      LabelNdx = i;
      first    = 1;
      second   = 0;
    }

    i++;
  }

  if (LabelNdx < 0 || ContentsNdx < 0)
  {
    fprintf(stderr,
         " The following line in HeaderTxt.txt cannot be parsed correctly!!\n\n");
    fprintf(stderr," %s\n",line);
    fprintf(stderr," Check Syntax for this line.\n\n");
    exit(1);
  }

  n = i;

  i=LabelNdx+1;
  while (strncmp(&line[i]," ",1) == 0)i++;
  SetChar(&FileHeaderLabel[FileTypePointer][HeaderRecPointer][0],line+i,
          ContentsNdx-i);

  i=ContentsNdx+1;
  while (strncmp(&line[i]," ",1) == 0)i++;
  SetChar(&FileHeaderContents[FileTypePointer][HeaderRecPointer][0],line+i,
          n-i);

  
}
