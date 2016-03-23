#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: WriteQmRecord.c,v 1.4 2004/08/30 21:03:32 wib Exp $";

boolean WriteQmRecord(FILE *dst, qm_head_t *qmhead,qm_obs_t *qmobs)
/*----------------------------------------------------------------------------->
/ purpose: Write a qm-record to a fortran unformatted qmfile 
/
/ coded by: G.L.H. Kruizinga         04/06/01
/
/ input:  *dst    Pointer to qmfile                 
/         *qmhead Pointer to qm header struct
/ output: *qmobs  Pointer to qm observation struct          
/
/ return:      True    normal return
/              False   End Of File reached
/-----------------------------------------------------------------------------*/
{
  long i,RecBytes;

/*----------------------------------------------------------------------------->
/ Test for EOF
/-----------------------------------------------------------------------------*/

  RecBytes = 40 + qmobs->nobs*8;

  WriteRecByte(dst,RecBytes);

  if (fwrite(&qmobs->RelTime,sizeof(double),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->RelTime\n");
    exit(1);
  }

  if (fwrite(&qmobs->RcvId,sizeof(long),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->RcvId\n");
    exit(1);
  }

  if (fwrite(&qmobs->TrnId,sizeof(long),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->TrnId\n");
    exit(1);
  }

  if (fwrite(&qmobs->dtyp,sizeof(long),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->dtyp\n");
    exit(1);
  }

  if (fwrite(&qmobs->mtyp,sizeof(long),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->mtyp\n");
    exit(1);
  }

  if (fwrite(&qmobs->qmbreak,sizeof(double),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->qmbreak\n");
    exit(1);
  }

  if (fwrite(&qmobs->sigma,sizeof(float),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->sigma\n");
    exit(1);
  }

  if (fwrite(&qmobs->nobs,sizeof(long),1,dst) !=1)
  {
    fprintf(stderr,"Error writing qmobs->nobs\n");
    exit(1);
  }

  if (qmobs->nobs > MAX_DATATYPE)
  {
    fprintf(stderr,
            "\n Too many observations in one record. Current max_obs = %d\n\n",
            MAX_DATATYPE);
    exit(1);
  }

  loop(i,qmobs->nobs)
  {
    if (fwrite(&qmobs->observations[i],sizeof(double),1,dst) != 1)
    {
      fprintf(stderr,"Error writing qmobs->observations; number = %d\n",i);
      exit(1);
    }
  }

  WriteRecByte(dst,RecBytes);

  return True;
}
