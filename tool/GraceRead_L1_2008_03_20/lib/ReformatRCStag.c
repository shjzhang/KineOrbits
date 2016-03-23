#include <stdlib.h>
#include "GRACEiolib.h"
#include "GRACEfiletype.h"
#include "GRACEio_prototypes.h"
#include "GRACEgpslib.h"
#include "TimeLib.h"
#include <string.h>

#define MAXCHAR 1000
#define MAXWORD 100

static char SccsId[] = "$Id: ReformatRCStag.c,v 1.7 2004/09/14 20:02:33 glk Exp $";

boolean ReformatRCStag(char *SoftwareVersion,   /* RCS software version label */
                       char *NewLabel           /* Processing level (1A or 1B)*/
                      )
/*----------------------------------------------------------------------------->
/ purpose: Reformat RCS version label for Level1A and Level1B headers
/
/ coded by: Gerhard L.H. Kruizinga                           08/22/2004
/
/ input:  *SoftwareVersion    Pointer to input RCS version tag
/ output: *NewLabel           Pointer to output new label for L1A and L1B headers
/
/ return:      1       normal return
/              0       End Of File reached before header could be read
/ example: input  "$Id: ReformatRCStag.c,v 1.7 2004/09/14 20:02:33 glk Exp $"
/          output "@(#) atti_lsq_drv.c             1.52 08/17/04 17:45:31 glk"
/
/ note: "glk" will be added to output if there is enough room
<-----------------------------------------------------------------------------*/
{

  char     *words[MAXWORD],time_tag[MAXCHAR],format[MAXCHAR];
  char     input_string[MAXCHAR];

  long     Nwords,i,j;

  strcpy(input_string,SoftwareVersion);

  Nwords = get_words(input_string,words,MAXWORD);

  loop(i,Nwords)
  {
    loop(j,strlen(words[i]))
    if (words[i][j] == ' ' || words[i][j] == '\n') words[i][j] = '\0';
  }

  if (strncmp(words[0],"$Id:",4) != 0)
  {
    fprintf(stderr," Invalid RCS tag = %s\n Return tag as is\n");
    strcpy(NewLabel,SoftwareVersion);
  }

  /*>>>> remove ,v from source code filename <<<<*/

  loop(j,strlen(words[1]))
  if (words[1][j] == ',') words[1][j] = '\0';

  /*>>>> construct time tag <<<<*/

  loop(i,5) time_tag[i] = words[3][5+i];
  time_tag[5] = '/';
  loop(i,2) time_tag[6+i] = words[3][2+i];

  time_tag[8] = ' ';

  loop(i,8) time_tag[9+i] = words[4][i];

  time_tag[17] = '\0';

  if (strlen(words[1]) < 13)
    sprintf(format,"$Id: %%-%ds %%s %%s %%s                 ",13);
  else
    sprintf(format,"$Id: %%-%ds %%s %%s %%s                 ",strlen(words[1]));
  
  sprintf(NewLabel,format,words[1],words[2],time_tag,words[5]);

  NewLabel[45] = ' ';
  NewLabel[46] = '$';
  NewLabel[47] = '\0';

  return True;
}
