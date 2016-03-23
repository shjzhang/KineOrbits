      subroutine csr (a,ra,ja,ia,m,n,nnz,eps)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c Convert the sparse matrix `a' into compressed sparse row format.
c INPUT:
c     a(m,n) -- an (m x n) sparse matrix
c     m -- number of rows in `a'
c     n -- number of columns in `a'
c     eps  -- the smallest postive floating-point number
c OUTPUT:
c     ra(nnz) -- values of `a' stored in CSR format
c     ja(nnz) -- column indices of the elements a(i,j) as stored in ra
c     ia(m+1) -- pointers to the beginning of each row in ra and ja
c        1         2         3         4         5         6         7
      double precision a(m,1),ra(nnz),eps
      integer ja(nnz),ia(m+1),m,n,nnz
      nnz = 0
      do 10 i = 1,m
         ia(i) = 1+nnz
         nz = 0
         do 20 j = 1,n
            if (dabs(a(i,j)) .ge. eps) then
               nnz = nnz+1
               ra(nnz) = a(i,j)
               ja(nnz) = j
            endif
   20    continue
   10 continue
      ia(m+1) = 1+nnz
      return
      end
c
      subroutine nzero (ra,ja,ia,nrow,ncol,nnz,nz,rao,jao,iao,colmn)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c Return the structure of the zero entries in ra,ja,ia, in 
c  compressed sparse row format via rao, jao, iao.
c INPUT:
c     ra, ja, ia -- csr format of the matrix A
c     nrow -- number of rows in `a'
c     ncol -- number of columns in `a'
c     nnz -- number of non-zero elements
c     nz -- number of zero elements
c OUTPUT:
c     rao, jao, iao -- strucvture of the zero entries in csr format
c WORK ARRAY:
c     colmn -- logical vector of length ncol
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
      double precision ra(nnz),rao(nz),one
      integer ja(nnz),ia(nrow+1),jao(nz),iao(nrow+1),
     &        nrow,ncol,nnz,nz,inz
      logical colmn(ncol)
      parameter (one = 1.d0)
      inz = 0
      iao(1) = 1
      do i = 1,nrow
         iao(i+1) = iao(i)
         do l = 1,ncol
            colmn(l) = .true.
         enddo
         do j = ia(i),ia(i+1)-1
            colmn(ja(j)) = .false.
         enddo
         do k = 1,ncol
            if(colmn(k)) then
               inz = inz + 1
               jao(inz) = k
               rao(inz) = one
               iao(i+1) = iao(i+1) + 1
            endif
         enddo
      enddo
      return
      end
