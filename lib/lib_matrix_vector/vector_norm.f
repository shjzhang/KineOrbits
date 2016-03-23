*
*  procedure vector_norm
*      
      subroutine vector_norm(A,  norm)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Routine to compute the norm of the vector
*
*  Input_Output Auguments
*  ======================
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  A(3)       REAL*8    I       real or complex matrix or vector(full or 
*                               sparse storage)
*  flag       CHARACTER I       flag can be 
*                               1, 2, inf, or fro, 'fro' means The
*                               Frobenius-norm of matrix.
*                               More details ref. Matlab or Scilab's
*                               help documents
*  norm       REAL*8    O       double precision number 
*
*  Usage
*  =====
*  if the flag equals           the norm returns
*  --------------------------------------------------------------------
*  1                            The 1-norm, or largest column sum of A, 
*                               max(sum(abs(A)).
*  2                            The largest singular value (same as
*                               norm(A)).
*  inf                          The infinity norm, or largest row sum
*                               of A, max(sum(abs(A')))
*  1                            The 1-norm, or largest column sum of A, 
*                               max(sum(abs(A)).
*
*  History
*  =======
*
*  Vesion 1.0
*    
*  Time         Author          Description
*  ----         ------          ----------------------------------------
*  07.06.01     S.J.Zhang       build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
*
*  Declaration_of_Local_Varialbes
*
      implicit none
c
c  Declaration_of_Input_Output_Varialbles
c
      double precision      A(3), norm      
c     character*3           flag
c
c  Compute norm
c
c     Norm for vector
c
c     if(flag.eq.'1  ')then .... to be extended
c     if(flag.eq.'inf')then .... to be extended
c     if(flag.eq.'fro')then .... to be extended
c   
c     if(flag.eq.'2  ')then
      norm = dsqrt(A(1)**2 + A(2)**2 + A(3)**2)
c     endif
c
      return
c
      end
