#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 8

#define Success 1
#define Failure 0

static char SccsId[] = "$Id: WriteMAS1XFRecord.c,v 1.3 2004/08/30 21:03:38 wib Exp $";

boolean WriteMAS1XFRecord(FILE *dst, MAS1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write SC Mass Data Format record to file pointer dst
/
/ coded by: Gerhard L.H. Kruizinga           09/25/01
/
/
/ input:  *dst    Pointer to MAS1XF Data Format File
/         *record Pointer to MAS1XF Data struct (MAS1X_t)
<-----------------------------------------------------------------------------*/
{
  long i;

  char bits[NBITSMAX];
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->time_intg,sizeof(record->time_intg),1,dst) != 1)
  {
    printf("MAS1X: Error writing field 'time_intg' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_frac,sizeof(record->time_frac),1,dst) != 1)
  {
    printf("MAS1X: Error writing field 'time_frac' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_ref,sizeof(record->time_ref),1,dst) != 1)
  {
    printf("MAS1X: Error writing field 'time_ref' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("MAS1X: Error writing field 'GRACE_id' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("MAS1X: Error writing field 'qualflg' \n");
    return Failure;
  }

  if (fwrite_grace(&record->prod_flag,sizeof(record->prod_flag),1,dst) != 1)
  {
    printf("MAS1X: Error writing field 'prod_flag' \n");
    return Failure;
  }
/*----------------------------------------------------------------------------->
/ Decode Product flag
/-----------------------------------------------------------------------------*/
  
  GetCharBits(record->prod_flag,bits);
    
/*----------------------------------------------------------------------------->
/ writing all products specified by prod_flag from src
/-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
         case 0:
                if (fwrite_grace(&record->mass_thr,sizeof(record->mass_thr),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'mass_thr' \n");
                  return Failure;
                }
                break;
         case 1:

                if (fwrite_grace(&record->mass_thr_err,sizeof(record->mass_thr_err),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'mass_thr_err' \n");
                  return Failure;
                }
                break;
         case 2:
                if (fwrite_grace(&record->mass_tnk,sizeof(record->mass_tnk),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'mass_tnk' \n");
                  return Failure;
                }
                break;
         case 3:
                if (fwrite_grace(&record->mass_tnk_err,sizeof(record->mass_tnk_err),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'mass_tnk_err' \n");
                  return Failure;
                }
                break;
         case 4:
                if (fwrite_grace(&record->gas_mass_thr1,sizeof(record->gas_mass_thr1),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'gas_mass_thr1' \n");
                  return Failure;
                }
                break;
         case 5:
                if (fwrite_grace(&record->gas_mass_thr2,sizeof(record->gas_mass_thr2),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'gas_mass_thr2' \n");
                  return Failure;
                }
                break;
         case 6:
                if (fwrite_grace(&record->gas_mass_tnk1,sizeof(record->gas_mass_tnk1),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'gas_mass_tnk1' \n");
                  return Failure;
                }
                break;
         case 7:
                if (fwrite_grace(&record->gas_mass_tnk2,sizeof(record->gas_mass_tnk2),1,dst) != 1)
                {
                  printf("MAS1X: Error writing field 'gas_mass_tnk2' \n");
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
