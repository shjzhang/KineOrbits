#include<stdlib.h>
#include<stdio.h>
#include "iohb.h"

void read_HB1(char **filename, int* M, int* N, int* nonzeros, int* Nrhs,
char **mxtype, char **Rhstype, int* errflg)
{
   FILE *in_file; 
   char *Type;
   int Ptrcrd, Indcrd, Valcrd, Rhscrd;
   int Nrow, Ncol, Nnzero;
   char *mat_type;
/*   char Title[73], Key[9], Rhstype[4];*/
   char Title[73], Key[9];
   char Ptrfmt[17], Indfmt[17], Valfmt[21], Rhsfmt[21];

    mat_type = (char *) malloc(4);
    if ( mat_type == NULL ) IOHBTerminate("Insufficient memory for mat_typen");

   readHB_info(*filename, M, N, nonzeros, &Type, Nrhs);
   *mxtype = Type;
    if ( (in_file = fopen( *filename, "r")) == NULL ) {
       fprintf(stderr,"Error: Cannot open file: %s\n",*filename);
       *errflg = -1;
       return;
    }

   readHB_header(in_file, Title, Key, mat_type, &Nrow, &Ncol, &Nnzero, Nrhs,
                  Ptrfmt, Indfmt, Valfmt, Rhsfmt,
                  &Ptrcrd, &Indcrd, &Valcrd, &Rhscrd, *Rhstype); 
   fclose(in_file); 
  *(*Rhstype+3) = (char) NULL;

}

void read_HB2(char **filename, int* M, int* N, int* nonzeros, int *colptr, 
	int *rowind, double *val, int *colptr1, int *rowind1, double *val1)
{
   int i;
   readHB_newmat_double(*filename, M, N, nonzeros, &colptr, &rowind, &val);
   for(i = 0; i < *N+1; i++)
      colptr1[i] = colptr[i];
   for(i = 0; i < *nonzeros; i++){
      rowind1[i] = rowind[i];
      val1[i] = val[i];
	}
}

void read_HB3(char **filename, int *M, int *Nrhs, double *Rhs, char **rhsflag)
{
   int i;
   double *rhs;

   if (*Nrhs > 0){
      readHB_newaux_double(*filename, **rhsflag, &rhs);
   }
   for(i = 0; i < *Nrhs * *M; i++)
	Rhs[i] = rhs[i];
}

void write_HB1(char **filename, int *M, int *N, int *nonzeros, int *colptr,
int *rowind, double *val, int *Nrhs, double *rhs, double *guess, double *exact,
char **Title, char **Key, char **Type, char **Rhstype, char **Ptrfmt,
char **Indfmt, char **Valfmt, char **Rhsfmt)
{
   writeHB_mat_double(*filename, *M, *N, *nonzeros, colptr, rowind, val, *Nrhs,
rhs, guess, exact, *Title, *Key, *Type, *Ptrfmt, *Indfmt, *Valfmt, *Rhsfmt, *Rhstype);

}
