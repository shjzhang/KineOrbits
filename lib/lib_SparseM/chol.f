      subroutine chol(m,nnzdmax,d,jd,id,nnzdsm,dsub,jdsub,nsub,nsubmax,
     &                lindx,xlindx,nsuper,nnzlmax,lnz,xlnz,invp,perm,
     &                iwmax,iwork,colcnt,snode,xsuper,split,tmpmax,
     &                tmpvec,cachsz,level,ierr,timed)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c Sparse least squares solver via Ng-Peyton's sparse Cholesky 
c    factorization for sparse symmetric positive definite
c INPUT:
c     m -- the number of column in the design matrix X
c     nnzdmax -- upper bound of the non-zero elements in X'X
c     d -- an nnzdmax-vector of non-zero values of the transpose of
c          the design matrix multiplied by the design matrix stored in
c          csr format  (X'X)
c     jd -- an nnzdmax-vector of indices in d
c     id -- an (m+1)-vector of pointers to the begining of each
c           row in d and jd
c     dsub -- the values of d excluding the diagonal elements
c     jdsub -- the indices to dsub
c     nsubmax -- upper bound of the dimension of lindx
c     lindx -- an nsub-vector of integer which contains, in 
c           column major oder, the row subscripts of the nonzero
c           entries in L in a compressed storage format
c     xlindx -- an nsuper-vector of integer of pointers for lindx
c     nsuper -- the length of xlindx
c     nnzlmax -- the upper bound of the non-zero entries in
c                L stored in lnz, including the diagonal entries
c     lnz -- First contains the non-zero entries of d; later
c            contains the entries of the Cholesky factor
c     xlnz -- column pointer for L stored in lnz
c     invp -- an n-vector of integer of inverse permutation
c             vector
c     perm -- an n-vector of integer of permutation vector
c     colcnt -- array of length m, containing the number of
c               non-zeros in each column of the factor, including
c               the diagonal entries
c     snode -- array of length m for recording supernode
c              membership
c     xsuper -- array of length m+1 containing the supernode
c               partitioning
c     split -- an m-vector with splitting of supernodes so that
c              they fit into cache
c     tmpmax -- upper bound of the dimension of tmpvec
c     tmpvec -- a tmpmax-vector of temporary vector
c     cachsz -- size of the cache (in kilobytes) on the target
c               machine
c     level -- level of loop unrolling while performing numerical
c              factorization
c     ierr -- error flag
c       1 -- insufficient work space in call to extract
c       2 -- insufficient storage in iwork when calling ordmmd;
c       3 -- insufficient storage in iwork when calling sfinit;
c       4 -- nnzl > nnzlmax when calling sfinit
c       5 -- nsub > nsubmax when calling sfinit
c       6 -- insufficient work space in iwork when calling symfct
c       7 -- inconsistancy in input when calling symfct
c       8 -- tmpsiz > tmpmax when calling symfct; increase tmpmax
c       9 -- nonpositive diagonal encountered when calling
c            blkfct
c       10 -- insufficient work storage in tmpvec when calling
c            blkfct
c       11 -- insufficient work storage in iwork when calling
c            blkfct
c OUTPUT:
c     y -- an m-vector of least squares solution
c     nsub -- number of subscripts in lindx
c WORK ARRAYS:
c     iwmax -- upper bound of the general purpose integer
c              working storage iwork; set at 7*m+3
c     iwork -- an iwsiz-vector of integer as work space
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
      integer nsub,nsuper,nnzdmax,nnzdsm,nnzl,iwsiz,tmpsiz,iwmax,
     &        nnzlmax,nsubmax,cachsz,level,tmpmax,ierr,
     &        jdsub(nnzdsm),jd(nnzdmax),
     &        id(m+1),lindx(nsubmax),xlindx(m+1),
     &        invp(m),perm(m),xlnz(m+1),iwork(iwmax),
     &        colcnt(m),snode(m),xsuper(m+1),split(m)
      double precision dsub(nnzdsm),d(nnzdmax),
     &        lnz(nnzlmax),tmpvec(tmpmax)
      double precision timed
c      real gtimer,timbegw,timendw
      external smxpy1,smxpy2,smxpy4,smxpy8
      external mmpy1,mmpy2,mmpy4,mmpy8
c
c      timbegw=gtimer()
c
c Extract the non-diagonal structure  of d,jd,id
c   The diagonal entries are stored in dsub(1:m), the off-diagonal entries
c   are stored rowwise in dsub(m+2:nnzd+1), their column indices are stored 
c   in jsub(m+2:nnzd+1), the pointers to the nondiagonal entries in 
c   dsub(m+2:nnzd+1) and jsub(m+2:nnzd+1) are stored in jsub(1:m+1) 
c
      nsub = 0
      nnzd = id(m+1) - 1
      nnzdsub = nnzd - m
      call extract(d,jd,id,dsub,jdsub,m,nnzdmax,nnzdsm,ierr)
      if (ierr .eq. -1) then
         ierr = 1
         go to 100
      endif
c
c Save the X'X matrix structure from jdsub(m+2:nnzd+1),jdsub(1:m+1)
c   to lindx and xlindx because the matrix structure is destroyed by the 
c   minimum degree ordering routine
c
      do i = 1,m+1
         xlindx(i) = jdsub(i)
      enddo
      do i = 1,nnzdsub
         lindx(i) = jdsub(m+1+i)
      enddo
c
c Reorder the matrix using minimum degree ordering routine
c
      iwsiz = 4*m
      call ordmmd(m,xlindx,lindx,invp,perm,iwsiz,iwork,nsub,ierr)
      if (ierr .eq. -1) then
         ierr = 2
         go to 100
      endif
c
c Call sfinit: Symbolic factorization initialization
c   to compute supernode partition and storage requirements
c   for symbolic factorization. New ordering is a postordering 
c   of the nodal elimination tree
c
      iwsiz = 7 * m + 3
      call sfinit(m,nnzdsub,jdsub(1),jdsub(m+2),perm,
     &            invp,colcnt,nnzl,nsub,nsuper,snode,xsuper,iwsiz,
     &            iwork,ierr)
      if (ierr .eq. -1) then
         ierr = 3
         go to 100
      endif
      if (nnzl .gt. nnzlmax) then
         ierr = 4
         go to 100
      endif
      if (nsub .gt. nsubmax) then
         ierr = 5
         go to 100
      endif
c
c Call symfct: Perform supernodal symbolic factorization
c
      iwsiz = nsuper + 2 * m + 1
      call symfct(m,nnzdsub,jdsub(1),jdsub(m+2),perm,invp,
     &            colcnt,nsuper,xsuper,snode,nsub,xlindx,lindx,
     &            xlnz,iwsiz,iwork,ierr)
      if (ierr .eq. -1) then
         ierr = 6
         go to 100
      endif
      if (ierr .eq. -2) then
         ierr = 7
         go to 100
      endif
c
c Call inpnv: Input numerical values into data structures of L
c
      iwsiz = m
      call inpnv(m,id,jd,d,perm,invp,nsuper,xsuper,xlindx,lindx,
     &           xlnz,lnz,iwork)
c
c Call bfinit: Initialization for block factorization
c
      call bfinit(m,nsuper,xsuper,snode,xlindx,lindx,cachsz,tmpsiz,
     &            split)
      if (tmpsiz .gt. tmpmax) then
         ierr = 8
         go to 100
      endif
c
c Call blkfct: Numerical factorization
c
      iwsiz = 2 * m + 2 * nsuper
      if (level .eq. 1) then
         call blkfct(m,nsuper,xsuper,snode,split,xlindx,lindx,xlnz,
     &               lnz,iwsiz,iwork,tmpsiz,tmpvec,ierr,mmpy1,smxpy1)
      elseif (level .eq. 2) then
         call blkfct(m,nsuper,xsuper,snode,split,xlindx,lindx,xlnz,
     &               lnz,iwsiz,iwork,tmpsiz,tmpvec,ierr,mmpy2,smxpy2)
      elseif (level .eq. 4) then
         call blkfct(m,nsuper,xsuper,snode,split,xlindx,lindx,xlnz,
     &               lnz,iwsiz,iwork,tmpsiz,tmpvec,ierr,mmpy4,smxpy4)
      elseif (level .eq. 8) then
         call blkfct(m,nsuper,xsuper,snode,split,xlindx,lindx,xlnz,
     &               lnz,iwsiz,iwork,tmpsiz,tmpvec,ierr,mmpy8,smxpy8)
      endif
      if (ierr .eq. -1) then
         ierr = 9
         go to 100
      elseif (ierr .eq. -2) then
         ierr = 10
         go to 100
      elseif (ierr .eq. -3) then
         ierr = 11
         go to 100
      endif
  100 continue
c      timendw=gtimer()
c      timed = timendw - timbegw
      return
      end
