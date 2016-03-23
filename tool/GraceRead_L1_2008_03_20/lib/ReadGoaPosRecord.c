#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

/* $Id: ReadGoaPosRecord.c,v 1.3 2004/11/24 21:46:34 glk Exp $ */

#define MAXCHAR  1000
#define MAXWORD   100

long ReadGoaPosRecord(FILE* src, pos_goa_t *posgoa)
/*----------------------------------------------------------------------------->
/ purpose: read a pos_goa record form file src and return information in 
/          posgoa struct
/
/ coded by: G.L.H. Kruizinga        04/18/01
/
/ input:  *src      Pointer to fortran unformatted binary file
/ output: *posgoa   Pointer to goapos struct                               
/
/ Return 1L if end of file or error condition
/-----------------------------------------------------------------------------*/
{
  long i;
  int  Nwords;

  long int_seconds;

  char line[MAXCHAR];

  char *words[MAXWORD];

  if (fgets(line,MAXCHAR,src) == NULL) return 1L;

  posgoa->nfield   = 0;
  strcpy(posgoa->satname,"NONE");
  posgoa->frame    = 'X';
  posgoa->time     = 0.0;
  posgoa->xpos     = 0.0;
  posgoa->ypos     = 0.0;
  posgoa->zpos     = 0.0;
  posgoa->xvel     = 0.0;
  posgoa->yvel     = 0.0;
  posgoa->zvel     = 0.0;
  posgoa->xpos_sig = 1e30;
  posgoa->ypos_sig = 1e30;
  posgoa->zpos_sig = 1e30;
  posgoa->xvel_sig = 1e30;
  posgoa->yvel_sig = 1e30;
  posgoa->zvel_sig = 1e30;

  Nwords = get_words(line,words,MAXWORD);

  if (16 != Nwords)
  {
    fprintf(stderr,"\n Number of words in record differ from spec ");
    fprintf(stderr," Nwords, %d, Number of required elements = 16\n\n",Nwords);
    return 1L;
  }

  posgoa->nfield = Nwords;

  loop(i,Nwords)
  {
    switch (i)
    {
      case 0:
             sscanf(words[i],"%c",&posgoa->frame);
             break;
      case 1:
             sscanf(words[i],"%s",posgoa->satname);
             break;
      case 2:
             sscanf(words[i],"%ld",&int_seconds);
             break;  
      case 3:
             sscanf(words[i],"%lf",&posgoa->time);
             posgoa->time += (double) int_seconds;
             break;  
      case 4:
             sscanf(words[i],"%lf",&posgoa->xpos);
             break;
      case 5:
             sscanf(words[i],"%lf",&posgoa->ypos);
             break;
      case 6:
             sscanf(words[i],"%lf",&posgoa->zpos);
             break;
      case 7:
             sscanf(words[i],"%lf",&posgoa->xvel);
             break;          
      case 8:
             sscanf(words[i],"%lf",&posgoa->yvel);
             break;
      case 9:
             sscanf(words[i],"%lf",&posgoa->zvel);
             break;
      case 10:
             sscanf(words[i],"%lf",&posgoa->xpos_sig);
             break;
      case 11:
             sscanf(words[i],"%lf",&posgoa->ypos_sig);
             break;
      case 12:
             sscanf(words[i],"%lf",&posgoa->zpos_sig);
             break;
      case 13:
             sscanf(words[i],"%lf",&posgoa->xvel_sig);
             break;
      case 14:
             sscanf(words[i],"%lf",&posgoa->yvel_sig);
             break;
      case 15:     
             sscanf(words[i],"%lf",&posgoa->zvel_sig);
             break;
             
    } 
  }  

  return 0L;
}
int get_words(char *s, char **w, int n)
/*----------------------------------------------------------------------------->
/ purpose:  return all words of line separated by one or more white spaces
/
/ coded by: Monsieur Larry Romans              04/17/01
/
/ input: char *s              Pointer to string
/        int  n               maximum number of words allowed
/ output:char **w             Pointer to array of pointers
/       
/ return number of words in string *s
<-----------------------------------------------------------------------------*/
{   
  int nw, md;  
                   
  nw = md = 0; 
  while (*s) {
    if (isspace(*s)) md = 0;
    else {
      if (!md) {
        if (nw == n) return(nw);
        w[nw++] = s;
      }          
      md = 1;
    }            
    s++;
  }
  return(nw);  
} 

