      subroutine bckslv(m,nsubmax,nsuper,nrhs,lindx,xlindx,nnzlmax,lnz,
     &                   xlnz,invp,perm,xsuper,newrhs,sol,b,timed)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c Sparse least squares solver via Ng-Peyton's sparse Cholesky 
c    factorization for sparse symmetric positive definite
c INPUT:
c     m -- the number of column in the design matrix X
c     nsubmax -- upper bound of the dimension of lindx
c     lindx -- an nsub-vector of interger which contains, in 
c           column major oder, the row subscripts of the nonzero
c           entries in L in a compressed storage format
c     xlindx -- an nsuper-vector of integer of pointers for lindx
c     nnzlmax -- the upper bound of the non-zero entries in
c                L stored in lnz, including the diagonal entries
c     lnz -- First contains the non-zero entries of d; later
c            contains the entries of the Cholesky factor
c     xlnz -- column pointer for L stored in lnz
c     invp -- an m-vector of integer of inverse permutation
c             vector
c     perm -- an m-vector of integer of permutation vector
c     xsuper -- array of length m+1 containing the supernode
c               partitioning
c     newrhs -- extra work vector for right-hand side and
c               solution
c     sol -- the least squares solution
c     b -- an m-vector, usualy the rhs of the equality constraint
c          X'a = (1-tau)X'e in the rq setting
c OUTPUT:
c     y -- an m-vector of least squares solution
c WORK ARRAYS:
c     b -- an m-vector, usually the rhs of the equality constraint
c          X'a = (1-tau)X'e in the rq setting
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
      integer nnzlmax,nsubmax,nsuper,nrhs,lindx(nsubmax),xlindx(m+1),
     &        invp(m),perm(m),xlnz(m+1),
     &        xsuper(m+1)
      double precision lnz(nnzlmax),b(m,nrhs),newrhs(m),sol(m,nrhs)
      double precision timed
c      real gtimer,timbegw,timendw
c
c      timbegw=gtimer()
c Call blkslv: Numerical solution
c
      do j = 1,nrhs
         do i = 1,m
            newrhs(i) = b(perm(i),j)
         enddo
         call blkslv(nsuper,xsuper,xlindx,lindx,xlnz,lnz,newrhs)
         do i = 1,m
            sol(i,j) = newrhs(invp(i))
         enddo
      enddo
  100 continue
c      timendw=gtimer()
c      timed = timendw - timbegw
      return
      end


c----------------------------------------------------------------------c
c                                                                      c
c  BEGIN BEN ADDED                                                     c
c                                                                      c
c  These fortran subroutines are blatant ripoffs of the one in the     c
c  file bckslv.f included in the SparseM package.                      c
c                                                                      c

      subroutine bckslb(m,nsubmax,nsuper,nrhs,lindx,xlindx,nnzlmax,lnz,
     &                  xlnz,invp,perm,xsuper,newrhs,sol,b,timed)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c Sparse back solver of triangular system via Ng-Peyton's sparse Cholesky 
c    factorization for sparse symmetric positive definite
c INPUT:
c     m -- the number of column in the design matrix X
c     nsubmax -- upper bound of the dimension of lindx
c     lindx -- an nsub-vector of interger which contains, in 
c           column major oder, the row subscripts of the nonzero
c           entries in L in a compressed storage format
c     xlindx -- an nsuper-vector of integer of pointers for lindx
c     nnzlmax -- the upper bound of the non-zero entries in
c                L stored in lnz, including the diagonal entries
c     lnz -- First contains the non-zero entries of d; later
c            contains the entries of the Cholesky factor
c     xlnz -- column pointer for L stored in lnz
c     invp -- an m-vector of integer of inverse permutation
c             vector
c     perm -- an m-vector of integer of permutation vector
c     xsuper -- array of length m+1 containing the supernode
c               partitioning
c     newrhs -- extra work vector for right-hand side and
c               solution
c     sol -- the least squares solution
c     b -- an m-vector, usualy the rhs of the equality constraint
c          X'a = (1-tau)X'e in the rq setting
c OUTPUT:
c     y -- an m-vector of least squares solution
c WORK ARRAYS:
c     b -- an m-vector, usually the rhs of the equality constraint
c          X'a = (1-tau)X'e in the rq setting
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
      integer nnzlmax,nsubmax,nsuper,nrhs,lindx(nsubmax),xlindx(m+1),
     &        invp(m),perm(m),xlnz(m+1),
     &        xsuper(m+1)
      double precision lnz(nnzlmax),b(m,nrhs),newrhs(m),sol(m,nrhs)
      double precision timed
c Call blkslb: Numerical solution
c

c      newrhs = b
c      call blkslb(nsuper,xsuper,xlindx,lindx,xlnz,lnz,newrhs)

      do j = 1,nrhs
         do i = 1,m
            newrhs(i) = b(perm(i),j)
         enddo
c         write(*,*) 'newrhs =', newrhs
         call blkslb(nsuper,xsuper,xlindx,lindx,xlnz,lnz,newrhs)
         do i = 1,m
            sol(i,j) = newrhs(invp(i))
         enddo
      enddo
  100 continue

      return
      end

c----------------------------------------------------------------------------

      subroutine bckslf(m,nsubmax,nsuper,nrhs,lindx,xlindx,nnzlmax,lnz,
     &                  xlnz,invp,perm,xsuper,newrhs,sol,b,timed)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c Sparse least squares solver via Ng-Peyton's sparse Cholesky 
c    factorization for sparse symmetric positive definite
c INPUT:
c     m -- the number of column in the design matrix X
c     nsubmax -- upper bound of the dimension of lindx
c     lindx -- an nsub-vector of interger which contains, in 
c           column major oder, the row subscripts of the nonzero
c           entries in L in a compressed storage format
c     xlindx -- an nsuper-vector of integer of pointers for lindx
c     nnzlmax -- the upper bound of the non-zero entries in
c                L stored in lnz, including the diagonal entries
c     lnz -- First contains the non-zero entries of d; later
c            contains the entries of the Cholesky factor
c     xlnz -- column pointer for L stored in lnz
c     invp -- an m-vector of integer of inverse permutation
c             vector
c     perm -- an m-vector of integer of permutation vector
c     xsuper -- array of length m+1 containing the supernode
c               partitioning
c     newrhs -- extra work vector for right-hand side and
c               solution
c     sol -- the least squares solution
c     b -- an m-vector, usualy the rhs of the equality constraint
c          X'a = (1-tau)X'e in the rq setting
c OUTPUT:
c     y -- an m-vector of least squares solution
c WORK ARRAYS:
c     b -- an m-vector, usually the rhs of the equality constraint
c          X'a = (1-tau)X'e in the rq setting
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
      integer nnzlmax,nsubmax,nsuper,nrhs,lindx(nsubmax),xlindx(m+1),
     &        invp(m),perm(m),xlnz(m+1),
     &        xsuper(m+1)
      double precision lnz(nnzlmax),b(m,nrhs),newrhs(m),sol(m,nrhs)
      double precision timed
c Call blkslf: Numerical solution
c
      do j = 1,nrhs
         do i = 1,m
            newrhs(i) = b(perm(i),j)
         enddo
c         write(*,*) 'newrhs =', newrhs
         call blkslf(nsuper,xsuper,xlindx,lindx,xlnz,lnz,newrhs)
         do i = 1,m
            sol(i,j) = newrhs(invp(i))
         enddo
      enddo
  100 continue

      return
      end

c                                                                      c
c  END BEN ADDED                                                       c
c----------------------------------------------------------------------c
