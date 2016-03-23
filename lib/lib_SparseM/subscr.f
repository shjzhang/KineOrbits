      subroutine subext(nsub,ir,jc,a,ja,ia,sorted,values,iadd)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
      integer i,nsub,ir(nsub),jc(nsub),ja(*),ia(*),iadd(nsub)
      double precision a(*),values(nsub),getelm
      logical sorted
      do i = 1,nsub
         values(i) = getelm(ir(i),jc(i),a,ja,ia,iadd(i),sorted)
      enddo
      return
      end

      subroutine subasg(nrow,ncol,nsub,nnza,nnzb,ir,jc,a,ja,ia,
     &           b,jb,ib,values,colmn,ierr)
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
c  Assign the new values into b,jb, ib
c  Input:
c     nrow = number of row
c     ncol = number of column
c     nsub = length of the new values to be assigned into b,jb,ib
c     nnza = number of nonzero elements in a,ja,ia
c     nnzb = number of nonzero elements in b,jb,ib
c     ir = row indices of the new values
c     jc = column indices of the new values
c     a, ja, ia = original matrix stored in csr format
c     values = vector of new values to be assigned into a, ja, ia
c  Output:
c     b, jb, ib = new matrix with the updated value with possible fill-in
c     Note: b, jb, ib are not sorted column wise
c     ierr = error flag
c        0 = normal return
c        1 = not enough space (nnzb) assigned to b,jb and ib
c  Work space:
c     colmn = logical values
c     
c        1         2         3         4         5         6         7
c23456789012345678901234567890123456789012345678901234567890123456789012
      integer nrow,ncol,nsub,nnza,nnzb,len,ierr
      integer ir(nsub),jc(nsub),ja(nnza),ia(nrow+1),jb(nnzb),ib(nrow+1)
      double precision a(nnza),b(nnzb),values(nsub)
      logical colmn(ncol)
      ierr = 0
      len = 0
      ib(1) = 1
      do i = 1,nrow
         ib(i+1) = ib(i) 
         do l = 1,ncol
            colmn(l) = .true.
         enddo
         do j = 1,nsub
            if (ir(j) .eq. i) then
               len = len + 1
               if (len .gt. nnzb) then
                  ierr = 1
                  return
               endif
               b(len) = values(j)
               jb(len) = jc(j)
               ib(i+1) = ib(i+1) + 1
               colmn(jc(j)) = .false.
            endif
         enddo
         do k = ia(i),ia(i+1)- 1
            if(colmn(ja(k))) then
               len = len + 1
               if (len .gt. nnzb) then
                  ierr = 1
                  return
               endif
               b(len) = a(k)
               jb(len) = ja(k)
               ib(i+1) = ib(i+1) + 1
            endif
         enddo
      enddo
      return
      end
