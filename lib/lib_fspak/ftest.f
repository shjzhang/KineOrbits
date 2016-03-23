c test program for FSPAK
c m. perez-enciso, thu 17 dec 1992-1994
C
      program testfspak
c--------------------------------------------------------------------
c this program utilize FSPAK for solving a linear system of eqns with
c the following symmetric positive definite coefficient matrix:
c
C       5.  0   0   3   0
c           3.  0   0   1.
c               4.  1.  1.
c         sym.      4.  0
c                       2.
c
c--------------------------------------------------------------------
      integer available
      parameter (max_n=100 , max_nze=1000 , available=2000
     +          ,max_node=10 )
      integer n, ia(max_n+1), ja(max_nze), flag, iout, ioor, needed
     +       ,is(available), fnode(max_node), bnode(max_node), fnseg
     +       ,bnseg, feqb
      real*8  a(max_nze), b(max_n), fx(max_node), one, zero
c     -----------------------------------------
c     initialize and input the matrix structure
c     -----------------------------------------
      data (ia(i),i=1,6)  /1,3,5,8,9,10/,
     +     (ja(i),i=1,9) /1,4,2,5,3,4,5,4,5/ ,
     +     (a(i),i=1,9)  /5.,3.,3,1.,4.,1.,1.,4.,2./
      one=1.
      zero=0.
      n=5
      iout=1
      ioor=2
      open(iout,file='fspak.out')
      open(ioor,file='fspak.ord')
c     -------------
c     find ordering
c     -------------
      call fspak (10
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c     ----------------------
c     symbolic factorization
c     ----------------------
      call fspak (20
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c     -----------------------
c     numerical factorization
c     -----------------------
      call fspak (40
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c     ----------------------------------------------------------------
c     Now three ways of obtaining the diagonal elements of the inverse
c
c     1 dense vector solving
c     ----------------------
      write(iout,'(a)') ' 1. DENSE VECTOR METHOD'
      write(iout,'(a)') '    COLUMN     DIAG VALUE'
      do i=1,n
         do j=1,n
            b(j)=zero
         enddo
         b(i)=one
c        -----
c        solve
c        -----
         call fspak (50
     +              ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
          write(iout,'(i10,d15.5)') i,b(i)
      enddo
c..   check whether Ax = b for last column of inverse
  
      do i=1,n
        WRITE(IOUT,*) B(I) 
        fx(i)=0
      enddo
      fx(n)=1
      call fspak (56
     +              ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      if(flag.ne.0) write(iout,*) 'Error in solution Ax <> b'
c     ------------------------
c     2. sparse vector solving
c     ------------------------
      write(iout,'(a)')
      write(iout,'(a)')
      write(iout,'(a)') ' 2. SPARSE VECTOR METHOD'
      write(iout,'(a)') '    COLUMN     DIAG VALUE'
      call fspak (51
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb)
      do i=1,n
         call fspak (52
     +              , n, ia, ja, a, b, flag, iout, ioor, available,
     +               needed, is,
     +               i, i, 1, 1, one, 1 ,irank)
         write(iout,'(i10,d15.5)') i,b(1)
      enddo
c     -------------------------
c     3. and takahashi's method
c     -------------------------
      write(iout,'(a)')
      write(iout,'(a)')
      write(iout,'(a)') ' 3. TAKAHASHI ET AL.s METHOD'
      write(iout,'(a)') '    COLUMN     DIAG VALUE'
c     ----------------------------------------
c     check before whether enough room in JA,A
c     ----------------------------------------
      if(max_nze .lt. 2*(ia(n+1)-1-n) ) then
         write(iout,*) 'Increase max_ja to ',2*(ia(n+1)-1-n)
         stop
      endif
      call fspak (60
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      do i=1,n
         do k=ia(i),ia(i+1)-1
            j=ja(k)
            if(i.eq.j) then
               write(iout,'(i10,d15.5)') i,a(k)
               goto 10
            endif
         enddo
10       continue
      enddo
c     --------------------------------------------------------------
c     ia,ja,a structures are destroyed, but the necessary structures
c     necessary to obtain the determinant remain
c     --------------------------------------------------------------
      call fspak (54
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      write(iout,'(a)')
      write(iout,'(a)')
      write(iout,'(a,d10.5)') 'Finally, the determinant is: ',b(1)
      call fspak (55
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      write(iout,'(a)')
      write(iout,'(a)')
      write(iout,'(a,d10.5)') 'Finally, log determinant is: ',b(1)
      write(iout,'(a)')
      write(iout,'(a)')
c     ------------------------------------------------------
     +' I forgot that I need element (2,4) of the inverse |'
c     ------------------------------------------------------
      call fspak (51
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      call fspak (52
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,2,4,1,1,one,0,irank)
      write(iout,'(d15.5)') b(1)
      write(iout,'(a)')
c     --------------------------------
     +' or, of course, element (4,2) '
c     --------------------------------
      call fspak (52
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,4,2,1,1,one,0,irank)
      write(iout,'(d15.5)') b(1)
c     ---------------------------------------------------
c     some useful statistics can be recovered at any time
c     ---------------------------------------------------
      call fspak (80
     +           ,n,ia,ja,a,b,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c
      stop
c
      end
