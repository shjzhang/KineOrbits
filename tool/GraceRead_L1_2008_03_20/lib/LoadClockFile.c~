#include <stdio.h>
#include "GRACEiolib.h"

static char SccsId[] = "$Id: LoadClockFile.c,v 1.3 2004/08/30 21:03:33 wib Exp $";

boolean LoadClockFile(FILE *clk,double **xt, double **yt,long *Np)
/*----------------------------------------------------------------------------->
/ purpose:  Load Clock Table from CLK1B file
/       
/ coded by: Gerhard L.H. Kruizinga                04/30/01
/      
/ input:
/        clk    File pointer to clock file
/        xt     pointer to pointer x-coordinate
/        yt     pointer to pointer x-coordinate
/        Np     pointer to number of elments in xt and yt arrays
<-----------------------------------------------------------------------------*/
{       
  CLK1B_t  record;
        
  long ndump;
        
  FileHeader_t clk1b_header;
        
  ndump = 0;
        
  rewind (clk);
        
  if (ReadFileHeader(clk,&clk1b_header) == false)
  {     
    fprintf(stderr,"\n Problem 1 reading file header for file %s\n","CLK1B file");
    fprintf(stderr," See message above for problem\n\n");
    return false;
  }
        
  if (clk1b_header.filetype != GetFileType("ipCLK1BF"))
  {     
    fprintf(stderr,"\n Data file is not a CLK1A or CLK1B file. Current filetype = %d\n\n",
                   clk1b_header.filetype);
    return false;
  }

  while(ReadCLK1BFRecord(clk,&record) == true) ndump++;

  *xt = (double *) calloc( (size_t) ndump, sizeof(double) );
  *yt = (double *) calloc( (size_t) ndump, sizeof(double) );


  *Np = ndump;

  rewind(clk);

  if (ReadFileHeader(clk,&clk1b_header) == false)
  {  
    fprintf(stderr,"\n Problem 2 reading file header for file %s\n","CLK1B file");
    fprintf(stderr," See message above for problem\n\n");
    return false;
  }

  ndump = 0;
  while(ReadCLK1BFRecord(clk,&record) == true)
  {
    *(*xt+ndump) = (double) record.rcv_time;
    *(*yt+ndump) = (double) record.eps_time;
    ndump++;
  }

  fclose(clk);

  return true;
}

