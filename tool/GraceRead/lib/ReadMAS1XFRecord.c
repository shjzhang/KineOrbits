#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 8

#define Success 1
#define Failure 0


static char SccsId[] = "$Id: ReadMAS1XFRecord.c,v 1.3 2004/08/30 21:03:36 wib Exp $";

boolean ReadMAS1XFRecord(FILE *src, MAS1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read SC Mass Data Format record from file pointer src
/
/ coded by: Gerhard L.H. Kruizinga             09/25/01
/
/
/ input:  *src    Pointer to MAS1X Data Format File
/ output: *record Pointer to MAS1X Data struct (MAS1X_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long time_intg;

  int    retrn,i;

  char   bits[NBITSMAX];

/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&time_intg,sizeof(record->time_intg),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("MAS1X: Error reading field 'time_intg' \n");
    return Failure;
  }
  record->time_intg = time_intg;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->time_frac,sizeof(record->time_frac),1,src) != 1)
  {
    printf("MAS1X: Error reading field 'time_frac' \n");
    return Failure;
  }

  if (fread_grace(&record->time_ref,sizeof(record->time_ref),1,src) != 1)
  {
    printf("MAS1X: Error reading field 'time_ref' \n");
    return Failure;
  }

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("MAS1X: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
     printf("MAS1X: Error reading field 'qualflg' \n");
     return Failure;
  }

  if (fread_grace(&record->prod_flag,sizeof(record->prod_flag),1,src) != 1)
  {
     printf("MAS1X: Error reading field 'prod_flag' \n");
     return Failure;
  }

/*----------------------------------------------------------------------------->
/ Decode Product flag
/-----------------------------------------------------------------------------*/
  
  GetCharBits(record->prod_flag,bits);
    
/*----------------------------------------------------------------------------->
/ reading all product specified by prod_flag from src
/-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
         case 0:
                if (fread_grace(&record->mass_thr,sizeof(record->mass_thr),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'mass_thr' \n");
                  return Failure;
                }
                break;
         case 1:

                if (fread_grace(&record->mass_thr_err,sizeof(record->mass_thr_err),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'mass_thr_err' \n");
                  return Failure;
                }
                break;
         case 2:
                if (fread_grace(&record->mass_tnk,sizeof(record->mass_tnk),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'mass_tnk' \n");
                  return Failure;
                }
                break;
         case 3:
                if (fread_grace(&record->mass_tnk_err,sizeof(record->mass_tnk_err),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'mass_tnk_err' \n");
                  return Failure;
                }
                break;
         case 4:
                if (fread_grace(&record->gas_mass_thr1,sizeof(record->gas_mass_thr1),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'gas_mass_thr1' \n");
                  return Failure;
                }
                break;
         case 5:
                if (fread_grace(&record->gas_mass_thr2,sizeof(record->gas_mass_thr2),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'gas_mass_thr2' \n");
                  return Failure;
                }
                break;
         case 6:
                if (fread_grace(&record->gas_mass_tnk1,sizeof(record->gas_mass_tnk1),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'gas_mass_tnk1' \n");
                  return Failure;
                }
                break;
         case 7:
                if (fread_grace(&record->gas_mass_tnk2,sizeof(record->gas_mass_tnk2),1,src) != 1)
                {
                  printf("MAS1X: Error reading field 'gas_mass_tnk2' \n");
                  return Failure;
                }
                break;
          default:
          fprintf(stderr,"Product Flag index %d in ReadMAS1X is invalid!!! \n\n",i);
          exit(0);
      }
    }
  }

  return True;

}
