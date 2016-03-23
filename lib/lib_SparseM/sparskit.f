
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                     UNARY SUBROUTINES MODULE                         c
c----------------------------------------------------------------------c
c amask  : extracts     C = A mask M                                   c
c csort  : sorts the elements in increasing order of columns           c
c ivperm : permutes an integer vector (in-place)                       c
c dvperm : permutes a real vector (in-place)                           c
c getelm : returns a(i,j) for any (i,j) from a CSR-stored matrix.      c
c rperm  : permutes the rows of a matrix (B = P A)                     c
c filter1 : filters elements from a matrix according to their magnitudec
c amubdg : gets number of nonzeros in each row of A*B (as well as NNZ) c
c aplbdg : gets number of nonzeros in each row of A+B (as well as NNZ) c
c----------------------------------------------------------------------c
      subroutine amask (nrow,ncol,a,ja,ia,jmask,imask,
     *                  c,jc,ic,iw,nzmax,ierr)
c---------------------------------------------------------------------
      real*8 a(*),c(*) 
      integer ia(nrow+1),ja(*),jc(*),ic(nrow+1),jmask(*),imask(nrow+1) 
      logical iw(ncol)
c-----------------------------------------------------------------------
c This subroutine builds a sparse matrix from an input matrix by 
c extracting only elements in positions defined by the mask jmask, imask
c-----------------------------------------------------------------------
c On entry:
c---------
c nrow  = integer. row dimension of input matrix 
c ncol  = integer. Column dimension of input matrix.
c
c a,
c ja,
c ia    = matrix in Compressed Sparse Row format
c
c jmask,
c imask = matrix defining mask (pattern only) stored in compressed
c         sparse row format.
c
c nzmax = length of arrays c and jc. see ierr.
c 
c On return:
c-----------
c
c a, ja, ia and jmask, imask are unchanged.
c
c c
c jc, 
c ic    = the output matrix in Compressed Sparse Row format.
c 
c ierr  = integer. serving as error message.c
c         ierr = 0  means normal return
c         ierr .gt. 1 means that amask stopped when processing
c         row number ierr, because there was not enough space in
c         c, jc according to the value of nzmax.
c
c work arrays:
c------------- 
c iw    = logical work array of length ncol.
c
c note: 
c------ the  algorithm is in place: c, jc, ic can be the same as 
c a, ja, ia in which cas the code will overwrite the matrix c
c on a, ja, ia
c
c-----------------------------------------------------------------------
      ierr = 0
      len = 0
      do 1 j=1, ncol
         iw(j) = .false.
 1    continue
c     unpack the mask for row ii in iw
      do 100 ii=1, nrow
c     save pointer in order to be able to do things in place
         do 2 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .true.
 2       continue
c     add umasked elemnts of row ii
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         ic(ii) = len+1
         do 200 k=k1,k2 
            j = ja(k)
            if (iw(j)) then
               len = len+1
               if (len .gt. nzmax) then
                  ierr = ii
                  return
               endif
               jc(len) = j
               c(len) = a(k)
            endif
 200     continue         
c     
         do 3 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .false.
 3       continue
 100  continue    
      ic(nrow+1)=len+1
c
      return
c-----end-of-amask -----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csort (n,a,ja,ia,iwork,values) 
      logical values
      integer n, ja(*), ia(n+1), iwork(*) 
      real*8 a(*) 
c-----------------------------------------------------------------------
c This routine sorts the elements of  a matrix (stored in Compressed
c Sparse Row Format) in increasing order of their column indices within 
c each row. It uses a form of bucket sort with a cost of O(nnz) where
c nnz = number of nonzero elements. 
c requires an integer work array of length 2*nnz.  
c-----------------------------------------------------------------------
c on entry:
c--------- 
c n     = the row dimension of the matrix
c a     = the matrix A in compressed sparse row format.
c ja    = the array of column indices of the elements in array a.
c ia    = the array of pointers to the rows. 
c iwork = integer work array of length max ( n+1, 2*nnz ) 
c         where nnz = (ia(n+1)-ia(1))  ) .
c values= logical indicating whether or not the real values a(*) must 
c         also be permuted. if (.not. values) then the array a is not
c         touched by csort and can be a dummy array. 
c 
c on return:
c----------
c the matrix stored in the structure a, ja, ia is permuted in such a
c way that the column indices are in increasing order within each row.
c iwork(1:nnz) contains the permutation used  to rearrange the elements.
c----------------------------------------------------------------------- 
c Y. Saad - Feb. 1, 1991.
c-----------------------------------------------------------------------
c local variables
      integer i, k, j, ifirst, nnz, next  
c
c count the number of elements in each column
c
      do 1 i=1,n+1
         iwork(i) = 0
 1    continue
      do 3 i=1, n
         do 2 k=ia(i), ia(i+1)-1 
            j = ja(k)+1
            iwork(j) = iwork(j)+1
 2       continue 
 3    continue
c
c compute pointers from lengths. 
c
      iwork(1) = 1
      do 4 i=1,n
         iwork(i+1) = iwork(i) + iwork(i+1)
 4    continue
c 
c get the positions of the nonzero elements in order of columns.
c
      ifirst = ia(1) 
      nnz = ia(n+1)-ifirst
      do 5 i=1,n
         do 51 k=ia(i),ia(i+1)-1 
            j = ja(k) 
            next = iwork(j) 
            iwork(nnz+next) = k
            iwork(j) = next+1
 51      continue
 5    continue
c
c convert to coordinate format
c 
      do 6 i=1, n
         do 61 k=ia(i), ia(i+1)-1 
            iwork(k) = i
 61      continue
 6    continue
c
c loop to find permutation: for each element find the correct 
c position in (sorted) arrays a, ja. Record this in iwork. 
c 
      do 7 k=1, nnz
         ko = iwork(nnz+k) 
         irow = iwork(ko)
         next = ia(irow)
c
c the current element should go in next position in row. iwork
c records this position. 
c 
         iwork(ko) = next
         ia(irow)  = next+1
 7       continue
c
c perform an in-place permutation of the  arrays.
c 
         call ivperm (nnz, ja(ifirst), iwork) 
         if (values) call dvperm (nnz, a(ifirst), iwork) 
c
c reshift the pointers of the original matrix back.
c 
      do 8 i=n,1,-1
         ia(i+1) = ia(i)
 8    continue
      ia(1) = ifirst 
c
      return 
c---------------end-of-csort-------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine ivperm (n, ix, perm) 
      integer n, perm(n), ix(n)
c-----------------------------------------------------------------------
c this subroutine performs an in-place permutation of an integer vector 
c ix according to the permutation array perm(*), i.e., on return, 
c the vector x satisfies,
c
c         ix(perm(j)) :== ix(j), j=1,2,.., n
c
c-----------------------------------------------------------------------
c on entry:
c---------
c n     = length of vector x.
c perm  = integer array of length n containing the permutation  array.
c ix    = input vector
c
c on return:
c---------- 
c ix    = vector x permuted according (perm(*)) :=  ix(*)
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables
      integer tmp, tmp1
c
      init      = 1
      tmp       = ix(init)  
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
c     
c loop
c 
 6    k = k+1
c
c save the chased element --
c 
      tmp1    = ix(ii) 
      ix(ii)  = tmp
      next    = perm(ii) 
      if (next .lt. 0 ) goto 65
c     
c test for end 
c
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next 
c
c end loop 
c
      goto 6
c
c reinitilaize cycle --
c
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp = ix(init)
      ii  = perm(init)
      perm(init)=-perm(init)
      goto 6
c     
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue 
c     
      return
c-------------------end-of-ivperm--------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------  
      subroutine dvperm (n, x, perm)
      integer n, perm(n)
      real*8 x(n)
c-----------------------------------------------------------------------
c this subroutine performs an in-place permutation of a real vector x 
c according to the permutation array perm(*), i.e., on return, 
c the vector x satisfies,
c
c         x(perm(j)) :== x(j), j=1,2,.., n
c
c-----------------------------------------------------------------------
c on entry:
c---------
c n     = length of vector x.
c perm  = integer array of length n containing the permutation  array.
c x     = input vector
c
c on return:
c---------- 
c x     = vector x permuted according to x(perm(*)) :=  x(*)
c
c----------------------------------------------------------------------c
c           Y. Saad, Sep. 21 1989                                      c
c----------------------------------------------------------------------c
c local variables 
      real*8 tmp, tmp1
c
      init      = 1
      tmp       = x(init)   
      ii        = perm(init)
      perm(init)= -perm(init)
      k         = 0
c     
c loop
c 
 6    k = k+1
c
c save the chased element --
c 
      tmp1      = x(ii) 
      x(ii)     = tmp
      next      = perm(ii) 
      if (next .lt. 0 ) goto 65
c     
c test for end 
c
      if (k .gt. n) goto 101
      tmp       = tmp1
      perm(ii)  = - perm(ii)
      ii        = next 
c
c end loop 
c
      goto 6
c
c reinitilaize cycle --
c
 65   init      = init+1
      if (init .gt. n) goto 101
      if (perm(init) .lt. 0) goto 65
      tmp       = x(init)
      ii        = perm(init)
      perm(init)=-perm(init)
      goto 6
c     
 101  continue
      do 200 j=1, n
         perm(j) = -perm(j)
 200  continue 
c     
      return
c-------------------end-of-dvperm--------------------------------------- 
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      double precision function getelm (i,j,a,ja,ia,iadd,sorted) 
c-----------------------------------------------------------------------
c     purpose:
c     -------- 
c     this function returns the element a(i,j) of a matrix a, 
c     for any pair (i,j).  the matrix is assumed to be stored 
c     in compressed sparse row (csr) format. getelm performs a
c     binary search in the case where it is known that the elements 
c     are sorted so that the column indices are in increasing order. 
c     also returns (in iadd) the address of the element a(i,j) in 
c     arrays a and ja when the search is successsful (zero if not).
c----- 
c     first contributed by noel nachtigal (mit). 
c     recoded jan. 20, 1991, by y. saad [in particular
c     added handling of the non-sorted case + the iadd output] 
c-----------------------------------------------------------------------
c     parameters:
c     ----------- 
c on entry: 
c---------- 
c     i      = the row index of the element sought (input).
c     j      = the column index of the element sought (input).
c     a      = the matrix a in compressed sparse row format (input).
c     ja     = the array of column indices (input).
c     ia     = the array of pointers to the rows' data (input).
c     sorted = logical indicating whether the matrix is knonw to 
c              have its column indices sorted in increasing order 
c              (sorted=.true.) or not (sorted=.false.).
c              (input). 
c on return:
c----------- 
c     getelm = value of a(i,j). 
c     iadd   = address of element a(i,j) in arrays a, ja if found,
c              zero if not found. (output) 
c
c     note: the inputs i and j are not checked for validity. 
c-----------------------------------------------------------------------
c     noel m. nachtigal october 28, 1990 -- youcef saad jan 20, 1991.
c----------------------------------------------------------------------- 
      integer i, ia(*), iadd, j, ja(*)
      double precision a(*)
      logical sorted 
c
c     local variables.
c
      integer ibeg, iend, imid, k
c
c     initialization 
c
      iadd = 0 
      getelm = 0.0
      ibeg = ia(i)
      iend = ia(i+1)-1
c
c     case where matrix is not necessarily sorted
c     
      if (.not. sorted) then 
c
c scan the row - exit as soon as a(i,j) is found
c
         do 5  k=ibeg, iend
            if (ja(k) .eq.  j) then
               iadd = k 
               goto 20 
            endif
 5       continue
c     
c     end unsorted case. begin sorted case
c     
      else
c     
c     begin binary search.   compute the middle index.
c     
 10      imid = ( ibeg + iend ) / 2
c     
c     test if  found
c     
         if (ja(imid).eq.j) then
            iadd = imid 
            goto 20
         endif
         if (ibeg .ge. iend) goto 20
c     
c     else     update the interval bounds. 
c     
         if (ja(imid).gt.j) then
            iend = imid -1
         else 
            ibeg = imid +1
         endif
         goto 10  
c     
c     end both cases
c     
      endif
c     
 20   if (iadd .ne. 0) getelm = a(iadd) 
c
      return
c--------end-of-getelm--------------------------------------------------
c-----------------------------------------------------------------------
      end 
c-----------------------------------------------------------------------
      subroutine rperm (nrow,a,ja,ia,ao,jao,iao,perm,job)
      integer nrow,ja(*),ia(nrow+1),jao(*),iao(nrow+1),perm(nrow),job
      real*8 a(*),ao(*) 
c-----------------------------------------------------------------------
c this subroutine permutes the rows of a matrix in CSR format. 
c rperm  computes B = P A  where P is a permutation matrix.  
c the permutation P is defined through the array perm: for each j, 
c perm(j) represents the destination row number of row number j. 
c Youcef Saad -- recoded Jan 28, 1991.
c-----------------------------------------------------------------------
c on entry:
c----------
c n     = dimension of the matrix
c a, ja, ia = input matrix in csr format
c perm  = integer array of length nrow containing the permutation arrays
c         for the rows: perm(i) is the destination of row i in the
c         permuted matrix. 
c         ---> a(i,j) in the original matrix becomes a(perm(i),j) 
c         in the output  matrix.
c
c job   = integer indicating the work to be done:
c         job = 1   permute a, ja, ia into ao, jao, iao 
c                       (including the copying of real values ao and
c                       the array iao).
c         job .ne. 1 :  ignore real values.
c                     (in which case arrays a and ao are not needed nor
c                      used).
c
c------------
c on return: 
c------------ 
c ao, jao, iao = input matrix in a, ja, ia format
c note : 
c        if (job.ne.1)  then the arrays a and ao are not used.
c----------------------------------------------------------------------c
c           Y. Saad, May  2, 1990                                      c
c----------------------------------------------------------------------c
      logical values
      values = (job .eq. 1) 
c     
c     determine pointers for output matix. 
c     
      do 50 j=1,nrow
         i = perm(j)
         iao(i+1) = ia(j+1) - ia(j)
 50   continue
c
c get pointers from lengths
c
      iao(1) = 1
      do 51 j=1,nrow
         iao(j+1)=iao(j+1)+iao(j)
 51   continue
c
c copying 
c
      do 100 ii=1,nrow
c
c old row = ii  -- new row = iperm(ii) -- ko = new pointer
c        
         ko = iao(perm(ii)) 
         do 60 k=ia(ii), ia(ii+1)-1 
            jao(ko) = ja(k) 
            if (values) ao(ko) = a(k)
            ko = ko+1
 60      continue
 100  continue
c
      return
c---------end-of-rperm ------------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine filter1(n,rel,drptol,a,ja,ia,b,jb,ib,len,ierr)
      real*8 a(*),b(*),drptol
      integer ja(*),jb(*),ia(*),ib(*),n,len,ierr
c-----------------------------------------------------------------------
c  Modification of the original filter subroutine in Sparskit2 on 6/17/02
c  by Pin Ng
c     This module removes any elements whose value
c     is small from an input matrix A and puts the resulting
c     matrix in B.  
c-----------------------------------------------------------------------
c on entry:
c---------
c  n     = integer. row dimension of matrix
c rel    = integer that controls the drop criterion
c      1 = values greater than drptol are kept
c      2 = values greater than or equal to drptol are kept
c      3 = values equal to drptol are kept
c      4 = values not equal to drptol are kept
c drptol = real. drop tolerance used for dropping strategy.
c a 
c ja
c ia     = input matrix in compressed sparse format
c len    = integer. the amount of space available in arrays b and jb.
c
c on return:
c---------- 
c b 
c jb
c ib    = resulting matrix in compressed sparse format. 
c 
c ierr  = integer. containing error message.
c         ierr .eq. 0 indicates normal return
c         ierr .gt. 0 indicates that there is'nt enough
c         space is a and ja to store the resulting matrix.
c         ierr then contains the row number where filter stopped.
c note:
c------ This module is in place. (b,jb,ib can ne the same as 
c       a, ja, ia in which case the result will be overwritten).
c----------------------------------------------------------------------c
c           contributed by David Day,  Sep 19, 1989.                   c
c----------------------------------------------------------------------c
c local variables
      real*8 loctol
      integer index,row,k,k1,k2,rel 
c
      index = 1
      goto (100,200,300,400) rel
 100  do 10 row= 1,n
         k1 = ia(row)
         k2 = ia(row+1) - 1
         ib(row) = index
         loctol = drptol 
      do 11 k = k1,k2
         if( a(k) .gt. loctol)then 
               if (index .gt. len) then
               ierr = row 
               return
               endif
               b(index) =  a(k)
               jb(index) = ja(k)
               index = index + 1
            endif
 11   continue
 10   continue
      ib(n+1) = index
      return
 200     do 20 row= 1,n
         k1 = ia(row)
         k2 = ia(row+1) - 1
         ib(row) = index
         loctol = drptol 
      do 21 k = k1,k2
         if( a(k) .ge. loctol)then 
               if (index .gt. len) then
               ierr = row 
               return
               endif
               b(index) =  a(k)
               jb(index) = ja(k)
               index = index + 1
            endif
 21   continue
 20   continue
      ib(n+1) = index
      return
 300     do 30 row= 1,n
         k1 = ia(row)
         k2 = ia(row+1) - 1
         ib(row) = index
         loctol = drptol 
      do 31 k = k1,k2
         if( a(k) .eq. loctol)then 
               if (index .gt. len) then
               ierr = row 
               return
               endif
               b(index) =  a(k)
               jb(index) = ja(k)
               index = index + 1
            endif
 31   continue
 30   continue
      ib(n+1) = index
      return
 400     do 40 row= 1,n
         k1 = ia(row)
         k2 = ia(row+1) - 1
         ib(row) = index
         loctol = drptol 
      do 41 k = k1,k2
         if( a(k) .ne. loctol)then 
               if (index .gt. len) then
               ierr = row 
               return
               endif
               b(index) =  a(k)
               jb(index) = ja(k)
               index = index + 1
            endif
 41   continue
 40   continue
      ib(n+1) = index
      return
c--------------------end-of-filter -------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine amubdg (nrow,ncol,ncolb,ja,ia,jb,ib,ndegr,nnz,iw) 
      integer ja(*),jb(*),ia(nrow+1),ib(ncol+1),ndegr(nrow),iw(ncolb) 
c-----------------------------------------------------------------------
c gets the number of nonzero elements in each row of A*B and the total 
c number of nonzero elements in A*B. 
c-----------------------------------------------------------------------
c on entry:
c -------- 
c
c nrow  = integer.  row dimension of matrix A
c ncol  = integer.  column dimension of matrix A = row dimension of 
c                   matrix B.
c ncolb = integer. the colum dimension of the matrix B.
c
c ja, ia= row structure of input matrix A: ja = column indices of
c         the nonzero elements of A stored by rows.
c         ia = pointer to beginning of each row  in ja.
c 
c jb, ib= row structure of input matrix B: jb = column indices of
c         the nonzero elements of A stored by rows.
c         ib = pointer to beginning of each row  in jb.
c 
c on return:
c ---------
c ndegr = integer array of length nrow containing the degrees (i.e., 
c         the number of nonzeros in  each row of the matrix A * B 
c               
c nnz   = total number of nonzero elements found in A * B
c
c work arrays:
c-------------
c iw    = integer work array of length ncolb. 
c-----------------------------------------------------------------------
      do 1 k=1, ncolb 
         iw(k) = 0 
 1    continue
      
      do 2 k=1, nrow
         ndegr(k) = 0 
 2    continue
c     
c     method used: Transp(A) * A = sum [over i=1, nrow]  a(i)^T a(i)
c     where a(i) = i-th row of  A. We must be careful not to add  the
c     elements already accounted for.
c     
c     
      do 7 ii=1,nrow 
c     
c     for each row of A
c     
         ldg = 0 
c     
c    end-of-linked list
c     
         last = -1 
         do 6 j = ia(ii),ia(ii+1)-1 
c     
c     row number to be added:
c     
            jr = ja(j) 
            do 5 k=ib(jr),ib(jr+1)-1
               jc = jb(k) 
               if (iw(jc) .eq. 0) then 
c     
c     add one element to the linked list 
c     
                  ldg = ldg + 1
                  iw(jc) = last 
                  last = jc
               endif
 5          continue
 6       continue
         ndegr(ii) = ldg
c     
c     reset iw to zero
c     
         do 61 k=1,ldg 
            j = iw(last) 
            iw(last) = 0
            last = j
 61      continue
c-----------------------------------------------------------------------
 7    continue
c     
      nnz = 0
      do 8 ii=1, nrow 
         nnz = nnz+ndegr(ii) 
 8    continue
c     
      return
c---------------end-of-amubdg ------------------------------------------
c-----------------------------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine aplbdg (nrow,ncol,ja,ia,jb,ib,ndegr,nnz,iw) 
      integer ja(*),jb(*),ia(nrow+1),ib(nrow+1),iw(ncol),ndegr(nrow) 
c-----------------------------------------------------------------------
c gets the number of nonzero elements in each row of A+B and the total 
c number of nonzero elements in A+B. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c
c a,
c ja,
c ia    = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format.
c
c on return:
c----------
c ndegr = integer array of length nrow containing the degrees (i.e., 
c         the number of nonzeros in  each row of the matrix A + B.
c               
c nnz   = total number of nonzero elements found in A * B
c
c work arrays:
c------------
c iw    = integer work array of length equal to ncol. 
c
c-----------------------------------------------------------------------
      do 1 k=1, ncol 
         iw(k) = 0 
 1    continue
c
      do 2 k=1, nrow
         ndegr(k) = 0 
 2    continue
c
      do 7 ii=1,nrow 
         ldg = 0 
c     
c    end-of-linked list
c     
         last = -1 
c     
c     row of A
c     
         do 5 j = ia(ii),ia(ii+1)-1 
            jr = ja(j) 
c     
c     add element to the linked list 
c     
            ldg = ldg + 1
            iw(jr) = last 
            last = jr
 5       continue
c     
c     row of B
c     
         do 6 j=ib(ii),ib(ii+1)-1
            jc = jb(j)
            if (iw(jc) .eq. 0) then 
c     
c     add one element to the linked list 
c     
               ldg = ldg + 1
               iw(jc) = last 
               last = jc
            endif
 6       continue
c     done with row ii. 
         ndegr(ii) = ldg
c     
c     reset iw to zero
c     
         do 61 k=1,ldg 
            j = iw(last) 
            iw(last) = 0
            last = j
 61      continue
c-----------------------------------------------------------------------
 7    continue
c     
      nnz = 0
      do 8 ii=1, nrow 
         nnz = nnz+ndegr(ii) 
 8    continue
      return
c----------------end-of-aplbdg -----------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
c----------------------------------------------------------------------c
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c        BASIC LINEAR ALGEBRA FOR SPARSE MATRICES. BLASSM MODULE       c
c----------------------------------------------------------------------c
c amub   :   computes     C = A*B                                      c
c aplb   :   computes     C = A+B                                      c
c aplsb  :   computes     C = A + s B                                  c
c amudia :   Computes     C = A* Diag                                  c
c aemub  :   computes element-wise     C = A * B                       c
c aedib  :   computes element-wise     C = A / B                       c
c aeexpb  :   computes element-wise     C = A ^ B                       c
c----------------------------------------------------------------------c 
c Note: this module still incomplete.                                  c
c----------------------------------------------------------------------c
       subroutine amub (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *                  c,jc,ic,nzmax,iw,ierr) 
      real*8 a(*), b(*), c(*) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(*),ic(*),iw(ncol)
c-----------------------------------------------------------------------
c performs the matrix by matrix product C = A B 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A = row dimension of C
c ncol  = integer. The column dimension of B = column dimension of C
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format.
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format.
c           
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw    = integer work array of length equal to the number of
c         columns in B.
c Note: 
c-------
c   The row dimension of B is not needed. However there is no checking 
c   on the condition that ncol(A) = nrow(B). 
c
c----------------------------------------------------------------------- 
      real*8 scal 
      logical values
      values = (job .ne. 0) 
      len = 0
      ic(1) = 1 
      ierr = 0
c     initialize array iw.
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c
      do 500 ii=1, nrow 
c     row i 
         do 200 ka=ia(ii), ia(ii+1)-1 
            if (values) scal = a(ka)
               jj   = ja(ka)
               do 100 kb=ib(jj),ib(jj+1)-1
               jcol = jb(kb)
               jpos = iw(jcol)
               if (jpos .eq. 0) then
                  len = len+1
                  if (len .gt. nzmax) then
                     ierr = ii
                     return
                  endif
                  jc(len) = jcol
                  iw(jcol)= len
                  if (values) c(len)  = scal*b(kb)
               else
                  if (values) c(jpos) = c(jpos) + scal*b(kb)
               endif
 100           continue
 200     continue
         do 201 k=ic(ii), len
            iw(jc(k)) = 0
 201     continue
         ic(ii+1) = len+1
 500  continue
      return
c-------------end-of-amub-----------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aplb (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      real*8 a(*), b(*), c(*) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1),
     *     iw(ncol)
c-----------------------------------------------------------------------
c performs the matrix sum  C = A+B. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format.
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format.
c       
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw    = integer work array of length equal to the number of
c         columns in A.
c
c-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0) 
      ierr = 0
      len = 0
      ic(1) = 1 
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c     
      do 500 ii=1, nrow
c     row i 
         do 200 ka=ia(ii), ia(ii+1)-1 
            len = len+1
            jcol    = ja(ka)
            if (len .gt. nzmax) goto 999
            jc(len) = jcol 
            if (values) c(len)  = a(ka) 
            iw(jcol)= len
 200     continue
c     
         do 300 kb=ib(ii),ib(ii+1)-1
            jcol = jb(kb)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol
               if (values) c(len)  = b(kb)
               iw(jcol)= len
            else
               if (values) c(jpos) = c(jpos) + b(kb)
            endif
 300     continue
         do 301 k=ic(ii), len
            iw(jc(k)) = 0
 301     continue
         ic(ii+1) = len+1
 500  continue
      return
 999  ierr = ii
      return
c------------end of aplb ----------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aplsb (nrow,ncol,job,a,ja,ia,s,b,jb,ib,
     *     c,jc,ic,nzmax,iw,ierr)
      real*8 a(*), b(*), c(*), s 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1),
     *     iw(ncol)
c-----------------------------------------------------------------------
c performs the matrix sum  C = A+*B. 
c Modified from aplb by Pin Ng on 2/26/03
c There is no difference between the original aplsb and aplsb1 in SPARSKIT2
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format.
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format.
c       
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw    = integer work array of length equal to the number of
c         columns in A.
c
c-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0) 
      ierr = 0
      len = 0
      ic(1) = 1 
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c     
      do 500 ii=1, nrow
c     row i 
         do 200 ka=ia(ii), ia(ii+1)-1 
            len = len+1
            jcol    = ja(ka)
            if (len .gt. nzmax) goto 999
            jc(len) = jcol 
            if (values) c(len)  = a(ka) 
            iw(jcol)= len
 200     continue
c     
         do 300 kb=ib(ii),ib(ii+1)-1
            jcol = jb(kb)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol
               if (values) c(len)  = s*b(kb)
               iw(jcol)= len
            else
               if (values) c(jpos) = c(jpos) + s*b(kb)
            endif
 300     continue
         do 301 k=ic(ii), len
            iw(jc(k)) = 0
 301     continue
         ic(ii+1) = len+1
 500  continue
      return
 999  ierr = ii
      return
c------------end of aplsb ----------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aplsb1 (nrow,ncol,a,ja,ia,s,b,jb,ib,c,jc,ic,
     *     nzmax,ierr)
      real*8 a(*), b(*), c(*), s
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1)
c-----------------------------------------------------------------------
c performs the operation C = A+s B for matrices in sorted CSR format.
c the difference with aplsb is that the resulting matrix is such that
c the elements of each row are sorted with increasing column indices in
c each row, provided the original matrices are sorted in the same way. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c
c a,
c ja,
c ia    = Matrix A in compressed sparse row format with entries sorted
c
c s     = real. scalar factor for B.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format with entries sorted
c        ascendly in each row   
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format
c         with entries sorted ascendly in each row. 
c       
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c Notes: 
c-------
c     this will not work if any of the two input matrices is not sorted
c-----------------------------------------------------------------------
      ierr = 0
      kc = 1
      ic(1) = kc 
c
c     the following loop does a merge of two sparse rows + adds  them.
c 
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1 
 5       continue 
c
c     this is a while  -- do loop -- 
c 
         if (ka .le. kamax .or. kb .le. kbmax) then 
c     
            if (ka .le. kamax) then
               j1 = ja(ka)
            else
c     take j1 large enough  that always j2 .lt. j1
               j1 = ncol+1
            endif
            if (kb .le. kbmax) then 
               j2 = jb(kb)         
            else 
c     similarly take j2 large enough  that always j1 .lt. j2 
               j2 = ncol+1
            endif
c     
c     three cases
c     
            if (j1 .eq. j2) then 
               c(kc) = a(ka)+s*b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
               kc = kc+1
            else if (j1 .lt. j2) then
               jc(kc) = j1
               c(kc) = a(ka)
               ka = ka+1
               kc = kc+1
            else if (j1 .gt. j2) then
               jc(kc) = j2
               c(kc) = s*b(kb)
               kb = kb+1
               kc = kc+1
            endif
            if (kc .gt. nzmax) goto 999
            goto 5
c
c     end while loop
c
         endif
         ic(i+1) = kc
 6    continue
      return
 999  ierr = i 
      return
c------------end-of-aplsb --------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine amudia (nrow,job, a, ja, ia, diag, b, jb, ib)
      real*8 a(*), b(*), diag(nrow) 
      integer ja(*),jb(*), ia(nrow+1),ib(nrow+1) 
c-----------------------------------------------------------------------
c performs the matrix by matrix product B = A * Diag  (in place) 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A
c
c job   = integer. job indicator. Job=0 means get array b only
c         job = 1 means get b, and the integer arrays ib, jb.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c diag = diagonal matrix stored as a vector dig(1:n)
c
c on return:
c----------
c
c b, 
c jb, 
c ib    = resulting matrix B in compressed sparse row sparse format.
c       
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c 2)        algorithm in place (B can take the place of A).
c-----------------------------------------------------------------
      do 1 ii=1,nrow
c     
c     scale each element 
c     
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         do 2 k=k1, k2
            b(k) = a(k)*diag(ja(k)) 
 2       continue
 1    continue
c     
      if (job .eq. 0) return
c     
      do 3 ii=1, nrow+1
         ib(ii) = ia(ii)
 3    continue
      do 31 k=ia(1), ia(nrow+1) -1 
         jb(k) = ja(k)
 31   continue
      return
c-----------------------------------------------------------------------
c-----------end-of-amudiag----------------------------------------------
      end 
c-----------------------------------------------------------------------
      subroutine aemub (nrow,ncol,a,ja,ia,amask,jmask,imask,
     *                  c,jc,ic,iw,aw,nzmax,ierr)
c---------------------------------------------------------------------
      real*8 a(*),c(*),amask(*),aw(ncol)
      integer ia(nrow+1),ja(*),jc(*),ic(nrow+1),jmask(*),imask(nrow+1)
      logical iw(ncol)
c-----------------------------------------------------------------------
c Modified from amask by Pin T. Ng on 2/27/03 to perform 
c element-wise multiplication
c-----------------------------------------------------------------------
c On entry:
c---------
c nrow  = integer. row dimension of input matrix
c ncol  = integer. Column dimension of input matrix.
c
c a,
c ja,
c ia    = the A matrix in Compressed Sparse Row format
c
c amask,
c jmask,
c imask = matrix defining mask stored in compressed
c         sparse row format. (This is the B matrix)
c
c nzmax = length of arrays c and jc. see ierr.
c
c On return:
c-----------
c
c a, ja, ia and amask, jmask, imask are unchanged.
c
c c
c jc,
c ic    = the output matrix in Compressed Sparse Row format.
c
c ierr  = integer. serving as error message.c
c         ierr = 1  means normal return
c         ierr .gt. 1 means that amask stopped when processing
c         row number ierr, because there was not enough space in
c         c, jc according to the value of nzmax.
c
c work arrays:
c-------------
c iw    = logical work array of length ncol.
c aw    = real work array of length ncol.
c
c note:
c------ the  algorithm is in place: c, jc, ic can be the same as
c a, ja, ia in which cas the code will overwrite the matrix c
c on a, ja, ia
c
c-----------------------------------------------------------------------
      ierr = 0
      len = 0
      do 1 j=1, ncol
         iw(j) = .false.
         aw(j) = 0.0
 1    continue
c     unpack the mask for row ii in iw
      do 100 ii=1, nrow
c     save pointer and value in order to be able to do things in place
         do 2 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .true.
            aw(jmask(k)) = amask(k)
 2       continue
c     add umasked elemnts of row ii
         k1 = ia(ii)
         k2 = ia(ii+1)-1
         ic(ii) = len+1
         do 200 k=k1,k2
            j = ja(k)
            if (iw(j)) then
               len = len+1
               if (len .gt. nzmax) then
                  ierr = ii
                  return
               endif
               jc(len) = j
               c(len) = a(k)*aw(j)
            endif
 200     continue
c
         do 3 k=imask(ii), imask(ii+1)-1
            iw(jmask(k)) = .false.
            aw(jmask(k)) = 0.0
 3       continue
 100  continue
      ic(nrow+1)=len+1
c
      return
c-----end-of-aemub -----------------------------------------------------
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aemub1 (nrow,ncol,a,ja,ia,b,jb,ib,c,jc,ic,
     *     nzmax,ierr)
      real*8 a(*), b(*), c(*)
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1)
c-----------------------------------------------------------------------
c A modification of aplsb by Pin Ng on 6/12/02 to
c perform the element-wise operation C = A*B for matrices in 
c sorted CSR format.
c the difference with aplsb is that the resulting matrix is such that
c the elements of each row are sorted with increasing column indices in
c each row, provided the original matrices are sorted in the same way. 
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c
c a,
c ja,
c ia    = Matrix A in compressed sparse row format with entries sorted
c
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format with entries sorted
c        ascendly in each row   
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format
c         with entries sorted ascendly in each row. 
c       
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c Notes: 
c-------
c     this will not work if any of the two input matrices is not sorted
c-----------------------------------------------------------------------
      ierr = 0
      kc = 1
      ic(1) = kc 
c
c     the following loop does a merge of two sparse rows and 
c     multiplies  them.
c 
      do 6 i=1, nrow
         ka = ia(i)
         kb = ib(i)
         kamax = ia(i+1)-1
         kbmax = ib(i+1)-1 
 5       continue 
c
c     this is a while  -- do loop -- 
c 
         if (ka .le. kamax .or. kb .le. kbmax) then 
c     
            if (ka .le. kamax) then
               j1 = ja(ka)
            else
c     take j1 large enough  that always j2 .lt. j1
               j1 = ncol+1
            endif
            if (kb .le. kbmax) then 
               j2 = jb(kb)         
            else 
c     similarly take j2 large enough  that always j1 .lt. j2 
               j2 = ncol+1
            endif
c     
c     three cases
c     
            if (j1 .eq. j2) then 
               c(kc) = a(ka)*b(kb)
               jc(kc) = j1
               ka = ka+1
               kb = kb+1
               kc = kc+1
            else if (j1 .lt. j2) then
               ka = ka+1
            else if (j1 .gt. j2) then
               kb = kb+1
            endif
            if (kc .gt. nzmax) goto 999
            goto 5
c
c     end while loop
c
         endif
         ic(i+1) = kc
 6    continue
      return
 999  ierr = i 
      return
c------------end-of-aemub1 --------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aedib (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *     c,jc,ic,nzmax,iw,aw,ierr)
      real*8 a(*), b(*), c(*), aw(ncol) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1),
     *     iw(ncol)
c-----------------------------------------------------------------------
c performs the element-wise matrix division  C = A/B. 
c Modified from aplsb by Pin Ng on 2/27/03
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia    = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format.
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format.
c       
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw    = integer work array of length equal to the number of
c         columns in A.
c aw    = real work array of length equal to the number of
c         columns in A.
c
c-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0) 
      ierr = 0
      len = 0
      ic(1) = 1 
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c     
      do 500 ii=1, nrow
c     row i 
         do 200 ka=ia(ii), ia(ii+1)-1 
            len = len+1
            jcol    = ja(ka)
            if (len .gt. nzmax) goto 999
            jc(len) = jcol 
            if (values) c(len)  = a(ka)/0.0 
            iw(jcol)= len
            aw(jcol) = a(ka)
 200     continue
c     
         do 300 kb=ib(ii),ib(ii+1)-1
            jcol = jb(kb)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol
               if (values) c(len)  = 0.0
               iw(jcol)= len
            else
               if (values) c(jpos) = aw(jcol)/b(kb)
            endif
 300     continue
         do 301 k=ic(ii), len
            iw(jc(k)) = 0
 301     continue
         ic(ii+1) = len+1
 500  continue
      return
 999  ierr = ii
      return
c------------end of aedib ----------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine aeexpb (nrow,ncol,job,a,ja,ia,b,jb,ib,
     *     c,jc,ic,nzmax,iw,aw,ierr)
      real*8 a(*), b(*), c(*), aw(ncol) 
      integer ja(*),jb(*),jc(*),ia(nrow+1),ib(nrow+1),ic(nrow+1),
     *     iw(ncol)
c-----------------------------------------------------------------------
c performs the element-wise matrix division  C = A/B. 
c Modified from aplsb by Pin Ng on 2/27/03
c-----------------------------------------------------------------------
c on entry:
c ---------
c nrow  = integer. The row dimension of A and B
c ncol  = integer. The column dimension of A and B.
c job   = integer. Job indicator. When job = 0, only the structure
c                  (i.e. the arrays jc, ic) is computed and the
c                  real values are ignored.
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c b, 
c jb, 
c ib    =  Matrix B in compressed sparse row format.
c
c nzmax = integer. The  length of the arrays c and jc.
c         amub will stop if the result matrix C  has a number 
c         of elements that exceeds exceeds nzmax. See ierr.
c 
c on return:
c----------
c c, 
c jc, 
c ic    = resulting matrix C in compressed sparse row sparse format.
c       
c ierr  = integer. serving as error message. 
c         ierr = 0 means normal return,
c         ierr .gt. 0 means that amub stopped while computing the
c         i-th row  of C with i=ierr, because the number 
c         of elements in C exceeds nzmax.
c
c work arrays:
c------------
c iw    = integer work array of length equal to the number of
c         columns in A.
c aw    = real work array of length equal to the number of
c         columns in A.
c
c-----------------------------------------------------------------------
      logical values
      values = (job .ne. 0) 
      ierr = 0
      len = 0
      ic(1) = 1 
      do 1 j=1, ncol
         iw(j) = 0
 1    continue
c     
      do 500 ii=1, nrow
c     row i 
         do 200 ka=ia(ii), ia(ii+1)-1 
            len = len+1
            jcol    = ja(ka)
            if (len .gt. nzmax) goto 999
            jc(len) = jcol 
            if (values) c(len)  = 1.0
            iw(jcol)= len
            aw(jcol) = a(ka)
 200     continue
c     
         do 300 kb=ib(ii),ib(ii+1)-1
            jcol = jb(kb)
            jpos = iw(jcol)
            if (jpos .eq. 0) then
               len = len+1
               if (len .gt. nzmax) goto 999
               jc(len) = jcol
               if (values) c(len)  = 0.0**b(kb)
               iw(jcol)= len
            else
               if (values) c(jpos) = aw(jcol)**b(kb)
            endif
 300     continue
         do 301 k=ic(ii), len
            iw(jc(k)) = 0
 301     continue
         ic(ii+1) = len+1
 500  continue
      return
 999  ierr = ii
      return
c------------end of aeexpb ----------------------------------------------- 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c          BASIC MATRIX-VECTOR OPERATIONS - MATVEC MODULE              c
c         Matrix-vector Mulitiplications and Triang. Solves            c
c----------------------------------------------------------------------c
c contents: (as of Nov 18, 1991)                                       c
c----------                                                            c
c 1) Matrix-vector products:                                           c
c---------------------------                                           c
c amux  : A times a vector. Compressed Sparse Row (CSR) format.        c
c----------------------------------------------------------------------c
c 1)     M A T R I X    B Y    V E C T O R     P R O D U C T S         c
c----------------------------------------------------------------------c
      subroutine amux (n, x, y, a,ja,ia) 
      real*8  x(*), y(*), a(*) 
      integer n, ja(*), ia(*)
c-----------------------------------------------------------------------
c         A times a vector
c----------------------------------------------------------------------- 
c multiplies a matrix by a vector using the dot product form
c Matrix A is stored in compressed sparse row storage.
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c a, ja,
c    ia = input matrix in compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=Ax
c
c-----------------------------------------------------------------------
c local variables
c
      real*8 t
      integer i, k
c-----------------------------------------------------------------------
      do 100 i = 1,n
c
c     compute the inner product of row i with vector x
c 
         t = 0.0d0
         do 99 k=ia(i), ia(i+1)-1 
            t = t + a(k)*x(ja(k))
 99      continue
c
c     store result in y(i) 
c
         y(i) = t
 100  continue
c
      return
      end
c---------end-of-amux---------------------------------------------------
c                          S P A R S K I T                             c
c----------------------------------------------------------------------c
c                    FORMAT CONVERSION MODULE                          c
c----------------------------------------------------------------------c
c contents:                                                            c
c----------                                                            c
c csrdns  : converts a row-stored sparse matrix into the dense format. c
c dnscsr  : converts a dense matrix to a sparse storage format.        c
c coocsr  : converts coordinate to  to csr format                      c
c coicsr  : in-place conversion of coordinate to csr format            c
c csrcoo  : converts compressed sparse row to coordinate.              c
c csrcsc2 : rectangular version of csrcsc                              c
c csrmsr  : converts compressed sparse row format to modified sparse   c
c           row format                                                 c
c csrssr  : converts compressed sparse row to symmetric sparse row     c
c ssrcsr  : converts symmetric sparse row to compressed sparse row     c
c cscssc  : converts compressed sparse column to symmetric sparse      c
c           column                                                     c
c----------------------------------------------------------------------c
      subroutine csrdns(nrow,ncol,a,ja,ia,dns,ndns,ierr) 
      real*8 dns(ndns,*),a(*)
      integer ja(*),ia(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row    to    Dense 
c-----------------------------------------------------------------------
c
c converts a row-stored sparse matrix into a densely stored one
c
c On entry:
c---------- 
c
c nrow  = row-dimension of a
c ncol  = column dimension of a
c a, 
c ja, 
c ia    = input matrix in compressed sparse row format. 
c         (a=value array, ja=column array, ia=pointer array)
c dns   = array where to store dense matrix
c ndns  = first dimension of array dns 
c
c on return: 
c----------- 
c dns   = the sparse matrix a, ja, ia has been stored in dns(ndns,*)
c 
c ierr  = integer error indicator. 
c         ierr .eq. 0  means normal return
c         ierr .eq. i  means that the code has stopped when processing
c         row number i, because it found a column number .gt. ncol.
c 
c----------------------------------------------------------------------- 
      ierr = 0
      do 1 i=1, nrow
         do 2 j=1,ncol
            dns(i,j) = 0.0d0
 2       continue
 1    continue
c     
      do 4 i=1,nrow
         do 3 k=ia(i),ia(i+1)-1
            j = ja(k) 
            if (j .gt. ncol) then
               ierr = i
               return
            endif
            dns(i,j) = a(k)
 3       continue      
 4    continue
      return
c---- end of csrdns ----------------------------------------------------
c-----------------------------------------------------------------------
      end
      subroutine dnscsr(nrow,ncol,nzmax,dns,ndns,a,ja,ia,ierr)
      real*8 dns(ndns,*),a(*)
      integer ia(*),ja(*)
c-----------------------------------------------------------------------
c Dense     to    Compressed Row Sparse 
c----------------------------------------------------------------------- 
c
c converts a densely stored matrix into a row orientied
c compactly sparse matrix. ( reverse of csrdns )
c Note: this routine does not check whether an element 
c is small. It considers that a(i,j) is zero if it is exactly
c equal to zero: see test below.
c-----------------------------------------------------------------------
c on entry:
c---------
c
c nrow  = row-dimension of a
c ncol  = column dimension of a
c nzmax = maximum number of nonzero elements allowed. This
c         should be set to be the lengths of the arrays a and ja.
c dns   = input nrow x ncol (dense) matrix.
c ndns  = first dimension of dns. 
c
c on return:
c---------- 
c 
c a, ja, ia = value, column, pointer  arrays for output matrix 
c
c ierr  = integer error indicator: 
c         ierr .eq. 0 means normal retur
c         ierr .eq. i means that the the code stopped while
c         processing row number i, because there was no space left in
c         a, and ja (as defined by parameter nzmax).
c----------------------------------------------------------------------- 
      ierr = 0
      next = 1
      ia(1) = 1
      do 4 i=1,nrow
         do 3 j=1, ncol 
            if (dns(i,j) .eq. 0.0d0) goto 3
            if (next .gt. nzmax) then
               ierr = i
               return
            endif
            ja(next) = j
            a(next) = dns(i,j)
            next = next+1
 3       continue      
         ia(i+1) = next
 4    continue
      return
c---- end of dnscsr ---------------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine coocsr(nrow,nnz,a,ir,jc,ao,jao,iao)
c----------------------------------------------------------------------- 
      real*8 a(*),ao(*),x
      integer ir(*),jc(*),jao(*),iao(*)
c-----------------------------------------------------------------------
c  Coordinate     to   Compressed Sparse Row 
c----------------------------------------------------------------------- 
c converts a matrix that is stored in coordinate format
c  a, ir, jc into a row general sparse ao, jao, iao format.
c
c on entry:
c--------- 
c nrow  = dimension of the matrix 
c nnz   = number of nonzero elements in matrix
c a,
c ir, 
c jc    = matrix in coordinate format. a(k), ir(k), jc(k) store the nnz
c         nonzero elements of the matrix with a(k) = actual real value of
c         the elements, ir(k) = its row number and jc(k) = its column 
c         number. The order of the elements is arbitrary. 
c
c on return:
c----------- 
c ir      is destroyed
c
c ao, jao, iao = matrix in general sparse matrix format with ao 
c                continung the real values, jao containing the column indices, 
c                and iao being the pointer to the beginning of the row, 
c                in arrays ao, jao.
c
c Notes:
c------ This routine is NOT in place.  See coicsr
c
c------------------------------------------------------------------------
      do 1 k=1,nrow+1
         iao(k) = 0
 1    continue
c determine row-lengths.
      do 2 k=1, nnz
         iao(ir(k)) = iao(ir(k))+1
 2    continue
c starting position of each row..
      k = 1
      do 3 j=1,nrow+1
         k0 = iao(j)
         iao(j) = k
         k = k+k0
 3    continue
c go through the structure  once more. Fill in output matrix.
      do 4 k=1, nnz
         i = ir(k)
         j = jc(k)
         x = a(k)
         iad = iao(i)
         ao(iad) =  x
         jao(iad) = j
         iao(i) = iad+1
 4    continue
c shift back iao
      do 5 j=nrow,1,-1
         iao(j+1) = iao(j)
 5    continue
      iao(1) = 1
      return
c------------- end of coocsr ------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine coicsr (n,nnz,job,a,ja,ia,iwk)
      integer ia(nnz),ja(nnz),iwk(n+1) 
      real*8 a(*)
c------------------------------------------------------------------------
c IN-PLACE coo-csr conversion routine.
c------------------------------------------------------------------------
c this subroutine converts a matrix stored in coordinate format into 
c the csr format. The conversion is done in place in that the arrays 
c a,ja,ia of the result are overwritten onto the original arrays.
c------------------------------------------------------------------------
c on entry:
c--------- 
c n = integer. row dimension of A.
c nnz   = integer. number of nonzero elements in A.
c job   = integer. Job indicator. when job=1, the real values in a are
c         filled. Otherwise a is not touched and the structure of the
c         array only (i.e. ja, ia)  is obtained.
c a     = real array of size nnz (number of nonzero elements in A)
c         containing the nonzero elements 
c ja    = integer array of length nnz containing the column positions
c         of the corresponding elements in a.
c ia    = integer array of length nnz containing the row positions
c         of the corresponding elements in a.
c iwk   = integer work array of length n+1 
c on return:
c----------
c a
c ja 
c ia    = contains the compressed sparse row data structure for the 
c         resulting matrix.
c Note: 
c-------
c         the entries of the output matrix are not sorted (the column
c         indices in each are not in increasing order) use coocsr
c         if you want them sorted.
c----------------------------------------------------------------------c
c  Coded by Y. Saad, Sep. 26 1989                                      c
c----------------------------------------------------------------------c
      real*8 t,tnext
      logical values
c----------------------------------------------------------------------- 
      values = (job .eq. 1) 
c find pointer array for resulting matrix. 
      do 35 i=1,n+1
         iwk(i) = 0
 35   continue
      do 4 k=1,nnz
         i = ia(k)
         iwk(i+1) = iwk(i+1)+1
 4    continue 
c------------------------------------------------------------------------
      iwk(1) = 1 
      do 44 i=2,n
         iwk(i) = iwk(i-1) + iwk(i)
 44   continue 
c
c     loop for a cycle in chasing process. 
c
      init = 1
      k = 0
 5    if (values) t = a(init)
      i = ia(init)
      j = ja(init)
      ia(init) = -1
c------------------------------------------------------------------------
 6    k = k+1       
c     current row number is i.  determine  where to go. 
      ipos = iwk(i)
c     save the chased element. 
      if (values) tnext = a(ipos)
      inext = ia(ipos)
      jnext = ja(ipos)
c     then occupy its location.
      if (values) a(ipos)  = t
      ja(ipos) = j
c     update pointer information for next element to come in row i. 
      iwk(i) = ipos+1
c     determine  next element to be chased,
      if (ia(ipos) .lt. 0) goto 65
      t = tnext
      i = inext
      j = jnext 
      ia(ipos) = -1
      if (k .lt. nnz) goto 6
      goto 70
 65   init = init+1
      if (init .gt. nnz) goto 70
      if (ia(init) .lt. 0) goto 65
c     restart chasing --    
      goto 5
 70   do 80 i=1,n 
         ia(i+1) = iwk(i)
 80   continue
      ia(1) = 1
      return
c----------------- end of coicsr ----------------------------------------
c------------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
      subroutine csrcoo (nrow,job,nzmax,a,ja,ia,nnz,ao,ir,jc,ierr)
c-----------------------------------------------------------------------
      real*8 a(*),ao(*) 
      integer ir(*),jc(*),ja(*),ia(nrow+1) 
c----------------------------------------------------------------------- 
c  Compressed Sparse Row      to      Coordinate 
c----------------------------------------------------------------------- 
c converts a matrix that is stored in coordinate format
c  a, ir, jc into a row general sparse ao, jao, iao format.
c
c on entry: 
c---------
c nrow  = dimension of the matrix.
c job   = integer serving as a job indicator. 
c         if job = 1 fill in only the array ir, ignore jc, and ao.
c         if job = 2 fill in ir, and jc but not ao 
c         if job = 3 fill in everything.
c         The reason why these options are provided is that on return 
c         ao and jc are the same as a, ja. So when job = 3, a and ja are
c         simply copied into ao, jc.  When job=2, only jc and ir are
c         returned. With job=1 only the array ir is returned. Moreover,
c         the algorithm is in place:
c         call csrcoo (nrow,1,nzmax,a,ja,ia,nnz,a,ia,ja,ierr) 
c         will write the output matrix in coordinate format on a, ja,ia.
c
c a,
c ja,
c ia    = matrix in compressed sparse row format.
c nzmax = length of space available in ao, ir, jc.
c         the code will stop immediatly if the number of
c         nonzero elements found in input matrix exceeds nzmax.
c 
c on return:
c----------- 
c ao, ir, jc = matrix in coordinate format.
c
c nnz        = number of nonzero elements in matrix.
c ierr       = integer error indicator.
c         ierr .eq. 0 means normal retur
c         ierr .eq. 1 means that the the code stopped 
c         because there was no space in ao, ir, jc 
c         (according to the value of  nzmax).
c 
c NOTES: 1)This routine is PARTIALLY in place: csrcoo can be called with 
c         ao being the same array as as a, and jc the same array as ja. 
c         but ir CANNOT be the same as ia. 
c         2) note the order in the output arrays, 
c------------------------------------------------------------------------
      ierr = 0
      nnz = ia(nrow+1)-1
      if (nnz .gt. nzmax) then
         ierr = 1
         return
      endif
c------------------------------------------------------------------------
      goto (3,2,1) job
 1    do 10 k=1,nnz
         ao(k) = a(k)
 10   continue
 2    do 11 k=1,nnz
         jc(k) = ja(k)
 11   continue
c
c     copy backward to allow for in-place processing. 
c
 3    do 13 i=nrow,1,-1
         k1 = ia(i+1)-1
         k2 = ia(i)
         do 12 k=k1,k2,-1
            ir(k) = i
 12      continue
 13   continue
      return
c------------- end-of-csrcoo ------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
c-----------------------------------------------------------------------
      subroutine csrcsc2 (n,n2,job,ipos,a,ja,ia,ao,jao,iao)
      integer ia(n+1),iao(n2+1),ja(*),jao(*)
      real*8  a(*),ao(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to      Compressed Sparse Column
c
c (transposition operation)   Not in place. 
c----------------------------------------------------------------------- 
c Rectangular version.  n is number of rows of CSR matrix,
c                       n2 (input) is number of columns of CSC matrix.
c----------------------------------------------------------------------- 
c -- not in place --
c this subroutine transposes a matrix stored in a, ja, ia format.
c ---------------
c on entry:
c----------
c n     = number of rows of CSR matrix.
c n2    = number of columns of CSC matrix.
c job   = integer to indicate whether to fill the values (job.eq.1) of the
c         matrix ao or only the pattern., i.e.,ia, and ja (job .ne.1)
c
c ipos  = starting position in ao, jao of the transposed matrix.
c         the iao array takes this into account (thus iao(1) is set to ipos.)
c         Note: this may be useful if one needs to append the data structure
c         of the transpose to that of A. In this case use for example
c                call csrcsc2 (n,n,1,ia(n+1),a,ja,ia,a,ja,ia(n+2)) 
c         for any other normal usage, enter ipos=1.
c a     = real array of length nnz (nnz=number of nonzero elements in input 
c         matrix) containing the nonzero elements.
c ja    = integer array of length nnz containing the column positions
c         of the corresponding elements in a.
c ia    = integer of size n+1. ia(k) contains the position in a, ja of
c         the beginning of the k-th row.
c
c on return:
c ---------- 
c output arguments:
c ao    = real array of size nzz containing the "a" part of the transpose
c jao   = integer array of size nnz containing the column indices.
c iao   = integer array of size n+1 containing the "ia" index array of
c         the transpose. 
c
c----------------------------------------------------------------------- 
c----------------- compute lengths of rows of transp(A) ----------------
      do 1 i=1,n2+1
         iao(i) = 0
 1    continue
      do 3 i=1, n
         do 2 k=ia(i), ia(i+1)-1 
            j = ja(k)+1
            iao(j) = iao(j)+1
 2       continue 
 3    continue
c---------- compute pointers from lengths ------------------------------
      iao(1) = ipos 
      do 4 i=1,n2
         iao(i+1) = iao(i) + iao(i+1)
 4    continue
c--------------- now do the actual copying ----------------------------- 
      do 6 i=1,n
         do 62 k=ia(i),ia(i+1)-1 
            j = ja(k) 
            next = iao(j)
            if (job .eq. 1)  ao(next) = a(k)
            jao(next) = i
            iao(j) = next+1
 62      continue
 6    continue
c-------------------------- reshift iao and leave ---------------------- 
      do 7 i=n2,1,-1
         iao(i+1) = iao(i)
 7    continue
      iao(1) = ipos
c--------------- end of csrcsc2 ---------------------------------------- 
c-----------------------------------------------------------------------
      end
      subroutine csrmsr (n,a,ja,ia,ao,jao,wk,iwk,nnzao,ierr)
      real*8 a(*),ao(*),wk(n)
      integer ia(n+1),ja(*),jao(*),iwk(n+1),nnzao,ierr
c----------------------------------------------------------------------- 
c Compressed Sparse Row   to      Modified - Sparse Row 
c                                 Sparse row with separate main diagonal
c----------------------------------------------------------------------- 
c converts a general sparse matrix a, ja, ia into 
c a compressed matrix using a separated diagonal (referred to as
c the bell-labs format as it is used by bell labs semi conductor
c group. We refer to it here as the modified sparse row format.
c Note: this has been coded in such a way that one can overwrite
c the output matrix onto the input matrix if desired by a call of
c the form 
c
c     call csrmsr (n, a, ja, ia, a, ja, wk,iwk)
c
c In case ao, jao, are different from a, ja, then one can
c use ao, jao as the work arrays in the calling sequence:
c
c     call csrmsr (n, a, ja, ia, ao, jao, ao,jao)
c
c----------------------------------------------------------------------- 
c
c on entry :
c---------
c a, ja, ia = matrix in csr format. note that the 
c             algorithm is in place: ao, jao can be the same
c             as a, ja, in which case it will be overwritten on it
c             upon return.
c nnzao = the number of non-zero entries in ao, jao
c    
c on return :
c-----------
c
c ao, jao  = sparse matrix in modified sparse row storage format:
c          +  ao(1:n) contains the diagonal of the matrix. 
c          +  ao(n+2:nnz) contains the nondiagonal elements of the
c             matrix, stored rowwise.
c          +  jao(n+2:nnz) : their column indices
c          +  jao(1:n+1) contains the pointer array for the nondiagonal
c             elements in ao(n+2:nnz) and jao(n+2:nnz).
c             i.e., for i .le. n+1 jao(i) points to beginning of row i 
c             in arrays ao, jao.
c             here nnz = number of nonzero elements+1 
c ierr:
c    = -1 : length of ao, jao < itpr
c
c work arrays:
c------------
c wk    = real work array of length n
c iwk   = integer work array of length n+1
c
c notes: 
c------- 
c        Algorithm is in place.  i.e. both:
c
c          call csrmsr (n, a, ja, ia, ao, jao, ao,jao)
c          (in which  ao, jao, are different from a, ja)
c           and
c          call csrmsr (n, a, ja, ia, a, ja, wk,iwk) 
c          (in which  wk, jwk, are different from a, ja)
c        are OK.
c--------
c coded by Y. Saad Sep. 1989. Rechecked Feb 27, 1990.
c Modified by Pin Ng on June 11, 2002 to provide warning when 
c    iptr > nnzao+1
c-----------------------------------------------------------------------
      icount = 0
c
c store away diagonal elements and count nonzero diagonal elements.
c
      do 1 i=1,n
         wk(i) = 0.0d0
         iwk(i+1) = ia(i+1)-ia(i)
         do 2 k=ia(i),ia(i+1)-1
            if (ja(k) .eq. i) then
               wk(i) = a(k)
               icount = icount + 1 
               iwk(i+1) = iwk(i+1)-1
            endif
 2       continue
 1    continue
c     
c compute total length
c     
      iptr = n + ia(n+1) - icount
      if (iptr .gt. nnzao+1) then
         ierr = -1
         return
      endif
c     
c     copy backwards (to avoid collisions)
c     
      do 500 ii=n,1,-1
         do 100 k=ia(ii+1)-1,ia(ii),-1
            j = ja(k)
            if (j .ne. ii) then
               ao(iptr) = a(k)
               jao(iptr) = j 
               iptr = iptr-1
            endif
 100     continue
 500  continue
c
c compute pointer values and copy wk(*)
c
      jao(1) = n+2
      do 600 i=1,n
         ao(i) = wk(i) 
         jao(i+1) = jao(i)+iwk(i+1)
 600  continue
      return    
c------------ end of subroutine csrmsr ---------------------------------
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine csrssr (nrow,a,ja,ia,nzmax,ao,jao,iao,ierr)
      real*8 a(*), ao(*), t
      integer ia(*), ja(*), iao(*), jao(*)
c-----------------------------------------------------------------------
c Compressed Sparse Row     to     Symmetric Sparse Row
c----------------------------------------------------------------------- 
c this subroutine extracts the lower triangular part of a matrix.
c It can used as a means for converting a symmetric matrix for 
c which all the entries are stored in sparse format into one
c in which only the lower part is stored. The routine is in place in 
c that the output matrix ao, jao, iao can be overwritten on 
c the  input matrix  a, ja, ia if desired. Csrssr has been coded to
c put the diagonal elements of the matrix in the last position in
c each row (i.e. in position  ao(ia(i+1)-1   of ao and jao) 
c----------------------------------------------------------------------- 
c On entry
c-----------
c nrow  = dimension of the matrix a.
c a, ja, 
c    ia = matrix stored in compressed row sparse format
c
c nzmax = length of arrays ao,  and jao. 
c
c On return:
c----------- 
c ao, jao, 
c     iao = lower part of input matrix (a,ja,ia) stored in compressed sparse 
c          row format format.
c  
c ierr   = integer error indicator. 
c          ierr .eq. 0  means normal return
c          ierr .eq. i  means that the code has stopped when processing
c          row number i, because there is not enough space in ao, jao
c          (according to the value of nzmax) 
c
c----------------------------------------------------------------------- 
      ierr = 0
      ko = 0
c-----------------------------------------------------------------------
      do  7 i=1, nrow
         kold = ko
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .gt. i) goto 71
            ko = ko+1
            if (ko .gt. nzmax) then
               ierr = i
               return
            endif
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
         if (kdiag .eq. 0 .or. kdiag .eq. ko) goto 72
c     
c     exchange
c     
         t = ao(kdiag)
         ao(kdiag) = ao(ko)
         ao(ko) = t
c     
         k = jao(kdiag)
         jao(kdiag) = jao(ko)
         jao(ko) = k
 72      iao(i) = kold+1
 7    continue
c     redefine iao(n+1)
      iao(nrow+1) = ko+1
      return
c--------- end of csrssr ----------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine cscssc (ncol,a,ja,ia,nzmax,ao,jao,iao,ierr)
      real*8 a(*), ao(*)
      integer ia(*), ja(*), iao(*), jao(*)
c-----------------------------------------------------------------------
c Compressed Sparse Column   to   Symmetric Sparse Column
c----------------------------------------------------------------------- 
c
c Modified from csrssr by Pin Ng on 6/28/02
c
c this subroutine extracts the lower triangular part of a matrix.
c It can used as a means for converting a symmetric matrix for 
c which all the entries are stored in sparse format into one
c in which only the lower part is stored. The routine is in place in 
c that the output matrix ao, jao, iao can be overwritten on 
c the  input matrix  a, ja, ia if desired. Cscssc has been coded to
c put the diagonal elements of the matrix in the last position in
c each row (i.e. in position  ao(ia(i+1)-1   of ao and jao) 
c----------------------------------------------------------------------- 
c On entry
c-----------
c ncol  = dimension of the matrix a.
c a, ja, 
c    ia = matrix stored in compressed row sparse format
c
c nzmax = length of arrays ao,  and jao. 
c
c On return:
c----------- 
c ao, jao, 
c     iao = lower part of input matrix (a,ja,ia) stored in compressed sparse 
c          row format format.
c  
c ierr   = integer error indicator. 
c          ierr .eq. 0  means normal return
c          ierr .eq. i  means that the code has stopped when processing
c          row number i, because there is not enough space in ao, jao
c          (according to the value of nzmax) 
c
c----------------------------------------------------------------------- 
      ierr = 0
      ko = 0
c-----------------------------------------------------------------------
      do  7 i=1, ncol
         kold = ko
         kdiag = 0
         do 71 k = ia(i), ia(i+1) -1
            if (ja(k)  .lt. i) goto 71
            ko = ko+1
            if (ko .gt. nzmax) then
               ierr = i
               return
            endif
            ao(ko) = a(k)
            jao(ko) = ja(k)
            if (ja(k)  .eq. i) kdiag = ko
 71      continue
c         if (kdiag .eq. 0 .or. kdiag .eq. ko) goto 72
c     
c     exchange
c     
c         t = ao(kdiag)
c         ao(kdiag) = ao(ko)
c         ao(ko) = t
c     
c         k = jao(kdiag)
c         jao(kdiag) = jao(ko)
c         jao(ko) = k
 72      iao(i) = kold+1
 7    continue
c     redefine iao(n+1)
      iao(ncol+1) = ko+1
      return
c--------- end of cscssc ----------------------------------------------- 
c----------------------------------------------------------------------- 
      end
c----------------------------------------------------------------------- 
      subroutine ssrcsr(job, value2, nrow, a, ja, ia, nzmax,
     &                  ao, jao, iao, indu, iwk, ierr)
c     .. Scalar Arguments ..
      integer            ierr, job, nrow, nzmax, value2
c     ..
c     .. Array Arguments ..
      integer            ia(nrow+1), iao(nrow+1), indu(nrow),
     &                   iwk(nrow+1), ja(*), jao(nzmax)
      real*8             a(*), ao(nzmax)
c     ..
c-----------------------------------------------------------------------
c     Symmetric Sparse Row to Compressed Sparse Row format
c-----------------------------------------------------------------------
c     This subroutine converts a given matrix in SSR format to regular
c     CSR format by computing Ao = A + A' - diag(A), where A' is A
c     transpose.
c
c     Typically this routine is used to expand the SSR matrix of
c     Harwell Boeing matrices, or to obtain a symmetrized graph of
c     unsymmetric matrices.
c
c     This routine is inplace, i.e., (Ao,jao,iao) may be same as
c     (a,ja,ia).
c
c     It is possible to input an arbitrary CSR matrix to this routine,
c     since there is no syntactical difference between CSR and SSR
c     format. It also removes duplicate entries and perform a partial
c     ordering. The output matrix has an order of lower half, main
c     diagonal and upper half after the partial ordering.
c-----------------------------------------------------------------------
c on entry:
c---------
c
c job   = options
c         0 -- duplicate entries are not removed. If the input matrix is
c              SSR (not an arbitary CSR) matrix, no duplicate entry should
c              arise from this routine.
c         1 -- eliminate duplicate entries, zero entries.
c         2 -- eliminate duplicate entries and perform partial ordering.
c         3 -- eliminate duplicate entries, sort the entries in the
c              increasing order of clumn indices.
c              
c value2= will the values of A be copied?
c         0 -- only expand the graph (a, ao are not touched)
c         1 -- expand the matrix with the values.
c
c nrow  = column dimension of inout matrix
c a,
c ia,
c ja    = matrix in compressed sparse row format.
c
c nzmax = size of arrays ao and jao. SSRCSR will abort if the storage
c          provided in ao, jao is not sufficient to store A. See ierr.
c
c on return:
c----------
c ao, jao, iao
c       = output matrix in compressed sparse row format. The resulting
c         matrix is symmetric and is equal to A+A'-D. ao, jao, iao,
c         can be the same as a, ja, ia in the calling sequence.
c
c indu  = integer array of length nrow. INDU will contain pointers
c         to the beginning of upper traigular part if job > 1.
c         Otherwise it is also used as a work array (size nrow).
c
c iwk   = integer work space (size nrow+1).
c
c ierr  = integer. Serving as error message. If the length of the arrays
c         ao, jao exceeds nzmax, ierr returns the minimum value
c         needed for nzmax. otherwise ierr=0 (normal return).
c
c-----------------------------------------------------------------------
c     .. Local Scalars ..
      integer            i, ipos, j, k, kfirst, klast, ko, kosav, nnz
      real*8             tmp
c     ..
c     .. Executable Statements ..
      ierr = 0
      do 10 i = 1, nrow
         indu(i) = 0
         iwk(i) = 0
 10   continue
      iwk(nrow+1) = 0
c
c     .. compute number of elements in each row of (A'-D)
c     put result in iwk(i+1)  for row i.
c
      do 30 i = 1, nrow
         do 20 k = ia(i), ia(i+1) - 1
            j = ja(k)
            if (j.ne.i)
     &         iwk(j+1) = iwk(j+1) + 1
 20      continue
 30   continue
c
c     .. find addresses of first elements of ouput matrix. result in iwk
c
      iwk(1) = 1
      do 40 i = 1, nrow
         indu(i) = iwk(i) + ia(i+1) - ia(i)
         iwk(i+1) = iwk(i+1) + indu(i)
         indu(i) = indu(i) - 1
 40   continue
c.....Have we been given enough storage in ao, jao ?
      nnz = iwk(nrow+1) - 1
      if (nnz.gt.nzmax) then
         ierr = nnz
         return
      endif
c
c     .. copy the existing matrix (backwards).
c
      kosav = iwk(nrow+1)
      do 60 i = nrow, 1, -1
         klast = ia(i+1) - 1
         kfirst = ia(i)
         iao(i+1) = kosav
         kosav = iwk(i)
         ko = iwk(i) - kfirst
         iwk(i) = ko + klast + 1
         do 50 k = klast, kfirst, -1
            if (value2.ne.0)
     &         ao(k+ko) = a(k)
            jao(k+ko) = ja(k)
 50      continue
 60   continue
      iao(1) = 1
c
c     now copy (A'-D). Go through the structure of ao, jao, iao
c     that has already been copied. iwk(i) is the address
c     of the next free location in row i for ao, jao.
c
      do 80 i = 1, nrow
         do 70 k = iao(i), indu(i)
            j = jao(k)
            if (j.ne.i) then
               ipos = iwk(j)
               if (value2.ne.0)
     &            ao(ipos) = ao(k)
               jao(ipos) = i
               iwk(j) = ipos + 1
            endif
 70      continue
 80   continue
      if (job.le.0) return
c
c     .. eliminate duplicate entries --
c     array INDU is used as marker for existing indices, it is also the
c     location of the entry.
c     IWK is used to stored the old IAO array.
c     matrix is copied to squeeze out the space taken by the duplicated
c     entries.
c
      do 90 i = 1, nrow
         indu(i) = 0
         iwk(i) = iao(i)
 90   continue
      iwk(nrow+1) = iao(nrow+1)
      k = 1
      do 120 i = 1, nrow
         iao(i) = k
         ipos = iwk(i)
         klast = iwk(i+1)
 100     if (ipos.lt.klast) then
            j = jao(ipos)
            if (indu(j).eq.0) then
c     .. new entry ..
               if (value2.ne.0) then
                  if (ao(ipos) .ne. 0.0D0) then
                     indu(j) = k
                     jao(k) = jao(ipos)
                     ao(k) = ao(ipos)
                     k = k + 1
                  endif
               else
                  indu(j) = k
                  jao(k) = jao(ipos)
                  k = k + 1
               endif
            else if (value2.ne.0) then
c     .. duplicate entry ..
               ao(indu(j)) = ao(indu(j)) + ao(ipos)
            endif
            ipos = ipos + 1
            go to 100
         endif
c     .. remove marks before working on the next row ..
         do 110 ipos = iao(i), k - 1
            indu(jao(ipos)) = 0
 110     continue
 120  continue
      iao(nrow+1) = k
      if (job.le.1) return
c
c     .. partial ordering ..
c     split the matrix into strict upper/lower triangular
c     parts, INDU points to the the beginning of the strict upper part.
c
      do 140 i = 1, nrow
         klast = iao(i+1) - 1
         kfirst = iao(i)
 130     if (klast.gt.kfirst) then
            if (jao(klast).lt.i .and. jao(kfirst).ge.i) then
c     .. swap klast with kfirst ..
               j = jao(klast)
               jao(klast) = jao(kfirst)
               jao(kfirst) = j
               if (value2.ne.0) then
                  tmp = ao(klast)
                  ao(klast) = ao(kfirst)
                  ao(kfirst) = tmp
               endif
            endif
            if (jao(klast).ge.i)
     &         klast = klast - 1
            if (jao(kfirst).lt.i)
     &         kfirst = kfirst + 1
            go to 130
         endif
c
         if (jao(klast).lt.i) then
            indu(i) = klast + 1
         else
            indu(i) = klast
         endif
 140  continue
      if (job.le.2) return
c
c     .. order the entries according to column indices
c     bubble-sort is used
c
      do 190 i = 1, nrow
         do 160 ipos = iao(i), indu(i)-1
            do 150 j = indu(i)-1, ipos+1, -1
               k = j - 1
               if (jao(k).gt.jao(j)) then
                  ko = jao(k)
                  jao(k) = jao(j)
                  jao(j) = ko
                  if (value2.ne.0) then
                     tmp = ao(k)
                     ao(k) = ao(j)
                     ao(j) = tmp
                  endif
               endif
 150        continue
 160     continue
         do 180 ipos = indu(i), iao(i+1)-1
            do 170 j = iao(i+1)-1, ipos+1, -1
               k = j - 1
               if (jao(k).gt.jao(j)) then
                  ko = jao(k)
                  jao(k) = jao(j)
                  jao(j) = ko
                  if (value2.ne.0) then
                     tmp = ao(k)
                     ao(k) = ao(j)
                     ao(j) = tmp
                  endif
               endif
 170        continue
 180     continue
 190  continue
c
      return
c---- end of ssrcsr ----------------------------------------------------
      end
c----------------------------------------------------------------------- 
      subroutine atmux (n, x, y, a, ja, ia)
      real*8 x(*), y(*), a(*) 
      integer n, ia(*), ja(*)
c-----------------------------------------------------------------------
c         transp( A ) times a vector
c----------------------------------------------------------------------- 
c multiplies the transpose of a matrix by a vector when the original
c matrix is stored in compressed sparse row storage. Can also be
c viewed as the product of a matrix by a vector when the original
c matrix is stored in the compressed sparse column format.
c-----------------------------------------------------------------------
c
c on entry:
c----------
c n     = row dimension of A
c x     = real array of length equal to the column dimension of
c         the A matrix.
c a, ja,
c    ia = input matrix in compressed sparse row format.
c
c on return:
c-----------
c y     = real array of length n, containing the product y=transp(A)*x
c
c-----------------------------------------------------------------------
c     local variables 
c
      integer i, k 
c-----------------------------------------------------------------------
c
c     zero out output vector
c 
      do 1 i=1,n
         y(i) = 0.0
 1    continue
c
c loop over the rows
c
      do 100 i = 1,n
         do 99 k=ia(i), ia(i+1)-1 
            y(ja(k)) = y(ja(k)) + x(i)*a(k)
 99      continue
 100  continue
c
      return
c-------------end-of-atmux---------------------------------------------- 
c-----------------------------------------------------------------------
      end
