#include <stdio.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: WriteQmHeader.c,v 1.6 2004/08/30 21:03:32 wib Exp $";

boolean WriteQmHeader(FILE *dst, qm_head_t *qmhead)
/*----------------------------------------------------------------------------->
/ purpose: write qm header information to fortran unformatted binary qmfile
/
/ coded by: G.L.H. Kruizinga         04/07/01
/
/ input:  *dst    Pointer to qmfile                 
/ output: *qmhead Pointer to qmheader struct          
/
/ return:      True    normal return
/              False   End Of File reached
/-----------------------------------------------------------------------------*/
{

  long i,RecBytes;

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

  RecBytes = 8L;
  WriteRecByte(dst,RecBytes);

  fwrite(&qmhead->Nstations,sizeof(long),1,dst);
  fwrite(&qmhead->Nsatellites,sizeof(long),1,dst);

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

  WriteRecByte(dst,RecBytes);

  RecBytes = qmhead->Nstations*NAMELENGTH;
  WriteRecByte(dst,RecBytes);

  loop(i,qmhead->Nstations)
  {
    fwrite(qmhead->StaName[i],sizeof(char),NAMELENGTH,dst);
  }

  WriteRecByte(dst,RecBytes);
  RecBytes = qmhead->Nsatellites*NAMELENGTH;
  WriteRecByte(dst,RecBytes);

  loop(i,qmhead->Nsatellites)
  {
    fwrite(qmhead->SatName[i],sizeof(char),NAMELENGTH,dst);
  }

  WriteRecByte(dst,RecBytes);

  RecBytes = 8L;
  WriteRecByte(dst,RecBytes);
  fwrite(&qmhead->EpochTime,sizeof(double),1,dst);
  WriteRecByte(dst,RecBytes);

  RecBytes = 4L;
  WriteRecByte(dst,RecBytes);
  fwrite(&qmhead->nmax_dtype,sizeof(long),1,dst);
  WriteRecByte(dst,RecBytes);

  RecBytes = 4L;
  WriteRecByte(dst,RecBytes);
  fwrite(&qmhead->nmax_obs,sizeof(long),1,dst);
  WriteRecByte(dst,RecBytes);

  RecBytes = 4L;
  WriteRecByte(dst,RecBytes);
  fwrite(&qmhead->n_meas,sizeof(long),1,dst);
  WriteRecByte(dst,RecBytes);

  RecBytes = 20L;
  WriteRecByte(dst,RecBytes);
  loop(i,5) fwrite(&qmhead->sort_stat[i],sizeof(long),1,dst);
  WriteRecByte(dst,RecBytes);

  return True;
}
