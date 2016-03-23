#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: ReadQmRecord.c,v 1.4 2004/08/30 21:03:32 wib Exp $";

boolean ReadQmRecord(FILE *src, qm_head_t *qmhead,qm_obs_t *qmobs)
/*----------------------------------------------------------------------------->
/ purpose: read a record from a fortran unformatted qmfile and store information
/          in struct qmobs
/
/ coded by: G.L.H. Kruizinga         04/06/01
/
/ input:  *src    Pointer to qmfile                 
/         *qmhead Pointer to qm header struct
/ output: *qmobs  Pointer to qm observation struct          
/
/ return:      True    normal return
/              False   End Of File reached
/-----------------------------------------------------------------------------*/
{
  long i,retrn,RecBytes;

  static char *ch_dtyp[] = 
                       {"L1","L2","LC","P1","P2","PC","CA","DP","R2","AZ","EL"};

  char ReceiverName[NAMELENGTH_STRING];
  char TransmitterName[NAMELENGTH_STRING];

/*----------------------------------------------------------------------------->
/ Test for EOF
/-----------------------------------------------------------------------------*/

  retrn = fread(&RecBytes,sizeof(long),1,src);

  if (feof(src) != 0) return False;

  if (fread(&qmobs->RelTime,sizeof(double),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->RelTime\n");
    exit(1);
  }

  if (fread(&qmobs->RcvId,sizeof(long),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->RcvId\n");
    exit(1);
  }

  if (fread(&qmobs->TrnId,sizeof(long),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->TrnId\n");
    exit(1);
  }

  if (fread(&qmobs->dtyp,sizeof(long),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->dtyp\n");
    exit(1);
  }

  if (fread(&qmobs->mtyp,sizeof(long),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->mtyp\n");
    exit(1);
  }

  if (qmobs->mtyp == 1)
  {
    strcpy(ReceiverName,qmhead->SatName[qmobs->RcvId-1]);
    strcpy(TransmitterName,qmhead->SatName[qmobs->TrnId-1]);
  }
  else if (qmobs->mtyp == 2)
  {
    strcpy(ReceiverName,qmhead->StaName[qmobs->RcvId-1]);
    strcpy(TransmitterName,qmhead->SatName[qmobs->TrnId-1]);
  }
  else if (qmobs->mtyp == 3)
  {
    strcpy(ReceiverName,qmhead->SatName[qmobs->RcvId-1]);
    strcpy(TransmitterName,qmhead->StaName[qmobs->TrnId-1]);
  }
  else if (qmobs->mtyp == 4)
  {
    strcpy(ReceiverName,qmhead->StaName[qmobs->RcvId-1]);
    strcpy(TransmitterName,qmhead->StaName[qmobs->TrnId-1]);
  }
  else
  {
    fprintf(stderr,"\n ERROR: unknown measurement type: %d\n\n",qmobs->mtyp);
    exit(1);
  }

  if (fread(&qmobs->qmbreak,sizeof(double),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->qmbreak\n");
    exit(1);
  }

  if (fread(&qmobs->sigma,sizeof(float),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->sigma\n");
    exit(1);
  }

  if (fread(&qmobs->nobs,sizeof(long),1,src) !=1)
  {
    fprintf(stderr,"Error reading qmobs->nobs\n");
    exit(1);
  }

  if (qmobs->nobs > MAX_DATATYPE)
  {
    fprintf(stderr,
            "\n Too many observations in one record. Current max_obs = %d\n\n",
            MAX_DATATYPE);
    exit(1);
  }

  if (fread(qmobs->observations,sizeof(double),qmobs->nobs,src) != qmobs->nobs)
  {
    fprintf(stderr,"Error reading qmobs->observations\n");
    exit(1);
  }

/*
  fprintf(stderr,"%d: %f %s %s %s %d %f %f %d",RecBytes,qmobs->RelTime,ReceiverName,
          TransmitterName,ch_dtyp[qmobs->dtyp-1],qmobs->mtyp,qmobs->qmbreak,
          qmobs->sigma,qmobs->nobs);

  loop (i,qmobs->nobs) fprintf(stderr," %.16g",qmobs->observations[i]);

  fprintf(stderr,"\n");
*/

  ReadRecByte(src,&RecBytes);

  return True;
}
