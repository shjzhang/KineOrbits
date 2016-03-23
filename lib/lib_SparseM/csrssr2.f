c
c   subroutine csrssr2
c
      subroutine csrssr2 (nrow,a,ja,ia,nzmax,ao,jao,iao,ierr)
c
c=======================================================================
c     ****f* lib_SparseM/csrssr2.f
c
c   FUNCTION   
c   
c     Compressed Sparse Row(csr) to Symmetric Sparse Row (ssr)
c     
c     This subroutine extracts the upper triangular part of a matrix.
c     It can used as a means for converting a symmetric matrix for 
c     which all the entries are stored in sparse format into one
c     in which only the upper part is stored. The routine is in place in 
c     that the output matrix ao, jao, iao can be overwritten on 
c     the  input matrix  a, ja, ia if desired. 
c
c   NOTES
c
c     This subroutine is different from csrssr, which store the lower
c     triangular part of a symmetric matrix.
c
c   INPUTS
c
c     nrow       = dimension of the matrix a.
c     a, ja, ia  = matrix stored in compressed row sparse format
c     nzmax      = length of arrays ao,  and jao. 
c
c   OUTPUT
c
c     ao,jao,iao = lower part of input matrix (a,ja,ia) stored in compressed sparse 
c                  row format format.
c     ierr       = integer error indicator. 
c                  ierr .eq. 0  means normal return
c                  ierr .eq. i  means that the code has stopped when processing
c                  row number i, because there is not enough space in ao, jao
c                  (according to the value of nzmax) 
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/14                   programmed
c
c     ***
c
C     $Id: csrssr2.f,v 1.0 2009/08/14 $
c=======================================================================
c
      implicit none
c
      integer   nrow,nzmax
      real*8    a(*)
      integer   ia(*), ja(*)
      real*8    ao(*)
      integer   iao(*), jao(*)
c
      integer   i, k, kold, ierr, ko
c
      ierr = 0
      ko = 0
c-----------------------------------------------------------------------
      do i=1, nrow
         kold = ko
         do k = ia(i), ia(i+1) -1
            if(ja(k)  .ge. i) then
               ko = ko+1
               if (ko .gt. nzmax) then
                  ierr = i
                  return
               endif
               ao(ko) = a(k)
               jao(ko) = ja(k)
            endif
         enddo
         iao(i) = kold+1
      enddo
c     redefine iao(n+1)
      iao(nrow+1) = ko+1
      return
c--------- end of csrssr2 ----------------------------------------------- 
      end

