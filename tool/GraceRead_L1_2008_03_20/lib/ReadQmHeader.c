#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: ReadQmHeader.c,v 1.8 2004/08/30 21:03:32 wib Exp $";

boolean ReadQmHeader(FILE *src, qm_head_t *qmhead, long verbose_flag)
/*----------------------------------------------------------------------------->
/ purpose: read header information from fortran unformatted binary qmfile
/          and return information in struct qmhead
/
/ coded by: G.L.H. Kruizinga         04/06/01
/
/ input:  *src    Pointer to qmfile                 
/ output: *qmhead Pointer to qmheader struct          
/
/ return:      True    normal return
/              False   End Of File reached
/-----------------------------------------------------------------------------*/
{

  long i,j,RecBytes;

  ReadRecByte(src,&RecBytes);

  fread(&qmhead->Nstations,sizeof(long),1,src);
  fread(&qmhead->Nsatellites,sizeof(long),1,src);

  if (qmhead->Nstations > MAX_NSTATIONS || qmhead->Nstations < 0)
  {
    fprintf(stderr,"\n Number of stations = %d exceeds %d!!\n",qmhead->Nstations,
            MAX_NSTATIONS);
    exit(1);
  }
  if (qmhead->Nsatellites > MAX_NSATELLITES || qmhead->Nsatellites < 0)
  {
    fprintf(stderr,"\n Number of satellites = %d exceeds %d!!\n",
                   qmhead->Nsatellites,MAX_NSATELLITES);
    exit(1);
  }

  ReadRecByte(src,&RecBytes);
  ReadRecByte(src,&RecBytes);

  loop(i,qmhead->Nstations)
  {
    qmhead->StaName[i] =(char*)malloc(sizeof(char)*NAMELENGTH_STRING);
    loop(j,NAMELENGTH_STRING) qmhead->StaName[i][j] = '\0';
  }
  loop(i,qmhead->Nsatellites) 
  {
    qmhead->SatName[i] =(char*)malloc(sizeof(char)*NAMELENGTH_STRING);
    loop(j,NAMELENGTH_STRING) qmhead->SatName[i][j] = '\0';
  }

  loop(i,qmhead->Nstations)
  {
    fread(qmhead->StaName[i],sizeof(char),NAMELENGTH,src);
  }

  ReadRecByte(src,&RecBytes);
  ReadRecByte(src,&RecBytes);

  loop(i,qmhead->Nsatellites)
  {
    fread(qmhead->SatName[i],sizeof(char),NAMELENGTH,src);
  }

  ReadRecByte(src,&RecBytes);

  ReadRecByte(src,&RecBytes);
  fread(&qmhead->EpochTime,sizeof(double),1,src);
  ReadRecByte(src,&RecBytes);

  ReadRecByte(src,&RecBytes);
  fread(&qmhead->nmax_dtype,sizeof(long),1,src);
  ReadRecByte(src,&RecBytes);

  ReadRecByte(src,&RecBytes);
  fread(&qmhead->nmax_obs,sizeof(long),1,src);
  ReadRecByte(src,&RecBytes);

  ReadRecByte(src,&RecBytes);
  fread(&qmhead->n_meas,sizeof(long),1,src);
  ReadRecByte(src,&RecBytes);

  ReadRecByte(src,&RecBytes);
  loop(i,5) fread(&qmhead->sort_stat[i],sizeof(long),1,src);
  ReadRecByte(src,&RecBytes);

  if (verbose_flag == 1)
  {
    fprintf(stdout," Nstations,Nsatellites = %d %d\n",qmhead->Nstations,
                                                      qmhead->Nsatellites);
    loop(i,qmhead->Nstations)   fprintf(stdout," Station   %02d = %s\n",i+1,
                                               qmhead->StaName[i]);
    loop(i,qmhead->Nsatellites) fprintf(stdout," Satellite %02d = %s\n",i+1,
                                               qmhead->SatName[i]);
    fprintf(stdout," Epoch Time (sec 2000) = %f\n",qmhead->EpochTime);
    fprintf(stdout," nmax_dtype            = %d\n",qmhead->nmax_dtype);
    fprintf(stdout," nmax_obs              = %d\n",qmhead->nmax_obs);
    fprintf(stdout," n_meas                = %d\n",qmhead->n_meas);
    loop(i,5) fprintf(stdout,"sortstat[%d] = %d \n",i+1,qmhead->sort_stat[i]);
  }


}
