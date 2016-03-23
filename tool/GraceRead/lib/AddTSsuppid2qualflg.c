#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"

static char SccsId[] = "$Id: AddTSsuppid2qualflg.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

void AddTSsuppid2qualflg(signed char TSsuppId, unsigned char *qualflg)
/*----------------------------------------------------------------------------->
/ purpose:  copy Supplement Id TimeStamp packet bits into the qualflg
/
/ coded by: Gerhard L.H. Kruizinga                10/08/01
/
/ note: Timestamp supplementary bits will be inverted for qualflg. This is to
/       stay consitent with the qualflag philosophy that 0 indicates nominal
/       and 1 a suspicious condition 
/     
/       mapping    00 SCET time  -> 11
/                  01 GPS  time  -> 10
/                  10 SCET time + Pulse sync -> 01
/                  11 GPS  time + Pulse sync -> 00  (nominal condition)
<-----------------------------------------------------------------------------*/
{
  char           bits8[8];

  GetCharBits(TSsuppId,bits8);

  UnSetCharBit(qualflg, 0);
  UnSetCharBit(qualflg, 1);

  if (bits8[0] == 0) SetCharBit(qualflg, 0 );
  if (bits8[1] == 0) SetCharBit(qualflg, 1 );
}
