#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <time.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

#define loop1(I,N) for(I=1;I<N;I++)

static char SccsId[] = "$Id: PurgeRnxBlock.c,v 1.4 2004/08/30 21:03:32 wib Exp $";

long PurgeRnxBlock(FILE *dst, Prn_t *RnxBlock,long Npoints, long do_snrs, long do_ca)
{   
 double       seconds;
 double       sec_frac;

 int          month,day,year,hour,min,sec;

 int          i, nprns, lli_phase,lli_other;

 Prn_t        *pP;

 nprns = 0;
 loop1(i, Npoints) 
 {
   if (RnxBlock[i].flg)
   {
     seccal(RnxBlock[i].ttag,&year,&month,&day,&hour,&min,&sec,&sec_frac);
     seconds = (double)sec + sec_frac;
     nprns++;
   }
 }
    
 fprintf(dst," %02d %02d %02d %02d %02d %010.7f  0 %2d",
         year%100, month, day,hour,min,seconds,nprns);
     
 loop1(i, Npoints) if (RnxBlock[i].flg) fprintf(dst," %02d", i);
 fprintf(dst,"\n");

 loop1(i, Npoints) 
 {
   pP = RnxBlock + i;
   if (pP->flg) 
   {
     lli_phase = (pP->flg_prev) ? 5 : 4;
     lli_other = 4; /* Lock of Loss Indicator is only set for phase! */
     fprintf(dst,"%14.3f%1d%1d%14.3f%1d%1d%14.3f%1d%1d%14.3f%1d%1d%14.3f%1d%1d\n",
       pP->fz[1], lli_phase, pP->ss[1],
       pP->fz[2], lli_phase, pP->ss[2],
       pP->tu[0], lli_other, pP->ss[0],
       pP->tu[1], lli_other, pP->ss[1],
       pP->tu[2], lli_other, pP->ss[2]);
     if (do_ca) {
       if (do_snrs) {
         fprintf(dst,"%14.3f%1d%1d%14.3f%1d%1d%14.3f%1d%1d%14.3f%1d%1d\n",
           (double)pP->fz[0], lli_phase, pP->ss[0],
           (double)pP->snr[0], lli_other, pP->ss[0],
           (double)pP->snr[1], lli_other, pP->ss[1],
           (double)pP->snr[2], lli_other, pP->ss[2]);
       }
       else {
         fprintf(dst,"%14.3f%1d%1d\n",
           (double)pP->fz[0], lli_phase, pP->ss[0]);
       }
     }
     else {
       if (do_snrs) {
         fprintf(dst,"%14.3f%1d%1d%14.3f%1d%1d\n",
           (double)pP->snr[1], lli_other, pP->ss[1],
           (double)pP->snr[2], lli_other, pP->ss[2]);
       }
     }
   }
   pP->flg = 0;
 }
 return(0);
}

