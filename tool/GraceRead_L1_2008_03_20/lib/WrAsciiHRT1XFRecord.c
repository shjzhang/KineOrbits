#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define Success 1 
#define Failure 0

static char SccsId[] = "$Id: WrAsciiHRT1XFRecord.c,v 1.2 2004/08/30 21:03:37 wib Exp $";

boolean WrAsciiHRT1XFRecord(FILE *dst, HRT1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of the HRT Level 1X data 
/          from file pointer dst 
/
/ coded by: Gerhard Kruizinga                       08/05/04
/
/ input:  *dst    Pointer to HRT1X Data Format File
/         *record Pointer to HRT1X Data struct (HRT1X_t)
<-----------------------------------------------------------------------------*/
{
 char bits8[8];
 
 long i;

 GetCharBits(record->qualflg,bits8);

         fprintf(dst,"%d %d %c %c %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g", 
         record->time_intg, record->time_frac,record->time_ref,record->GRACE_id,
         record->TEMP_MEP_neg_y, record->TEMP_MEP_pos_y, record->TEMP_MEPm,
         record->TEMP_ICU, record->TEMP_ICU_red, record->TEMP_ACC_neg_z,
         record->TEMP_ACC_pos_z, record->TEMP_CFRP_pos_x, record->TEMP_CFRP_pos_x_red,
         record->TEMP_CFRP_neg_x, record->TEMP_CFRP_neg_x_red, record->TEMP_CFRP_neg_y,
         record->TEMP_CFRP_neg_y_red, record->TEMP_ACCSen, record->TEMP_ICU_spec,
         record->TEMP_MWA_neg_y, record->TEMP_MWA_neg_yoff, record->TEMP_MWA_pos_y,
         record->TEMP_MWA_pos_yoff, record->TEMP_Horn_pos_x, record->TEMP_Horn_pos_x_red,
         record->TEMP_HornPl, record->TEMP_HornPl_red, record->TEMP_HMWA_neg_y,
         record->TEMP_HMWA_pos_y, record->TEMP_RFSamp, record->TEMP_USO_neg_y,
         record->TEMP_USO_neg_y_red, record->TEMP_USO_pos_y, record->TEMP_USO_pos_y_red);

 fprintf(dst,"  ");
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);
 fprintf(dst,"\n");
 
 return Success;
}
