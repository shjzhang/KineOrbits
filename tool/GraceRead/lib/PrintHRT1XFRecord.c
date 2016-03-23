#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintHRT1XFRecord.c,v 1.2 2004/08/30 21:03:34 wib Exp $";

void  PrintHRT1XFRecord(FILE *dst, HRT1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of  High Resolution Temperature
/          Data Format record to file printer dst
/
/ coded by: Gerhard L.H. Kruizinga           08/06/04
/
/ input:  *dst    Pointer to HRT1X Data Format File
/         *record Pointer to HRT1X Data struct (HRT1X_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/

  fprintf(dst," %-20s = %ld\n","record->time_intg",record->time_intg);
  fprintf(dst," %-20s = %ld\n","record->time_frac",record->time_frac);
  fprintf(dst," %-20s = %c\n","record->time_ref",record->time_ref);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_MEP_neg_y",record->TEMP_MEP_neg_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_MEP_pos_y",record->TEMP_MEP_pos_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_MEPm",record->TEMP_MEPm);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_ICU",record->TEMP_ICU);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_ICU_red",record->TEMP_ICU_red);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_ACC_neg_z",record->TEMP_ACC_neg_z);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_ACC_pos_z",record->TEMP_ACC_pos_z);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_CFRP_pos_x",record->TEMP_CFRP_pos_x);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_CFRP_pos_x_red",record->TEMP_CFRP_pos_x_red);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_CFRP_neg_x",record->TEMP_CFRP_neg_x);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_CFRP_neg_x_red",record->TEMP_CFRP_neg_x_red);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_CFRP_neg_y",record->TEMP_CFRP_neg_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_CFRP_neg_y_red",record->TEMP_CFRP_neg_y_red);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_ACCSen",record->TEMP_ACCSen);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_ICU_spec",record->TEMP_ICU_spec);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_MWA_neg_y",record->TEMP_MWA_neg_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_MWA_neg_yoff",record->TEMP_MWA_neg_yoff);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_MWA_pos_y",record->TEMP_MWA_pos_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_MWA_pos_yoff",record->TEMP_MWA_pos_yoff);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_Horn_pos_x",record->TEMP_Horn_pos_x);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_Horn_pos_x_red",record->TEMP_Horn_pos_x_red);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_HornPl",record->TEMP_HornPl);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_HornPl_red",record->TEMP_HornPl_red);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_HMWA_neg_y",record->TEMP_HMWA_neg_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_HMWA_pos_y",record->TEMP_HMWA_pos_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_RFSamp",record->TEMP_RFSamp);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_USO_neg_y",record->TEMP_USO_neg_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_USO_neg_y_red",record->TEMP_USO_neg_y_red);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_USO_pos_y",record->TEMP_USO_pos_y);
  fprintf(dst," %-20s = %.16g\n","record->TEMP_USO_pos_y_red",record->TEMP_USO_pos_y_red);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


}
