      program ftestinv
c Inversion test program for FSPAK
c Written by I. Misztal 11/22/94 based on Miguel Perez_Enciso FTEST.F
c tests inversion by solving, speed- and memory-optimized inversions.
c
c Format of data file:  "i   j   a_ij", upper-diagonalelements only, sorted
c           in ascending order
c
c Name of data file hard-coded in subroutine getfile below. When replacing this 
c file modify the number of equation max_n and number of nonzero elements max_nz
c----------------------------------------------------------------------------

      integer available,max_n,max_nze,irank,i,j
      parameter (max_n=600 , max_nze=5000 , available=25000)
      integer n, ia(max_n+1), ja(max_nze),ib(max_n+1), jb(max_nze),
     +        flag, iout, ioor, needed
     +       ,is(available), fnode(max_n), bnode(max_n), fnseg
     +       ,bnseg, feqb
      real*8  a(max_nze), b(max_nze), c(max_nze),d(max_nze), e(max_nze),
     +        fx(max_n), one, zero,
     +        x(3),y(max_n)
      real t,second

      print*,'  FSPAK inversion test by direct solve, options 60 and 61'
      print*,'    input file "egin"         I. Misztal (11/23/94)'
      print*
c     -----------------------------------------
c     read data file
c     -----------------------------------------
      call getfile(n,max_nze,max_n,'egin',ib,jb,b)
      call cpy(n,max_nze,max_n,ib,jb,b,ia,ja,a)
      one=1.
      zero=0.
      iout=1
      ioor=2
      open(iout,file='fspak.out')
      open(ioor,file='fspak.ord')
c     -------------
c     find ordering
c     -------------
      t=second()
      call fspak (10
     +           ,n,ia,ja,a,y,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      call checkf(10,flag,available,needed,t)
c     ----------------------
c     symbolic factorization
c     ----------------------
      call fspak (20
     +           ,n,ia,ja,a,y,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      call checkf(20,flag,available,needed,t)
c     -----------------------
c     numerical factorization
c     -----------------------
      call fspak (40
     +           ,n,ia,ja,a,y,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      call checkf(40,flag,available,needed,t)
c     ----------------------------------------------------------------
c     Now three ways of obtaining the diagonal elements of the inverse
c
c     - dense vector solving
c     - speed-optimized inversion
c     - memory-optimized inversion
c     ----------------------
      do i=1,n
         do j=1,n
            y(j)=zero
         enddo
         y(i)=one
c        -----
c        solve
c        -----
         call fspak (50
     +              ,n,ia,ja,a,y,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c        copy to c
         do j=ib(i),ib(i+1)-1
            c(j)=y(jb(j))
         enddo
      enddo
      if (i.eq.1) call checkf(50,flag,available,needed,t)

c..   check whether Ax = b for last column of inverse
  
      do i=1,n
        WRITE(IOUT,*) B(I) 
        fx(i)=0
      enddo
      fx(n)=1
      call fspak (56
     +              ,n,ia,ja,a,y,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      if(flag.ne.0) write(iout,*) 'Error in solution Ax <> b'

c     ----------------------------------------
c     check before whether enough room in JA,A
c     ----------------------------------------
      if(max_nze .lt. 2*(ia(n+1)-1-n) ) then
         write(iout,*) 'Increase max_ja to ',2*(ia(n+1)-1-n)
         stop
      endif
 
      call cpy(n,max_nze,max_n,ib,jb,b,ia,ja,a)
      t=second()
      call fspak (60
     +           ,n,ia,ja,a,y,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      call checkf(60,flag,available,needed,t)
c      call prss(n,max_nze,max_n,ia,ja,a)
      call rzero(n,y)
      do i=1,n
         do j=ia(i),ia(i+1)-1
            y(ja(j))=a(j)
         enddo
         do j=ib(i),ib(i+1)-1
            d(j)=y(jb(j))
         enddo
         do j=ia(i),ia(i+1)-1
            y(ja(j))=0
         enddo
      enddo

      call cpy(n,max_nze,max_n,ib,jb,b,ia,ja,a)
      call fspak (61
     +           ,n,ia,ja,a,y,flag,iout,ioor,available,needed,is
     +           ,fnode,bnode,fnseg,bnseg,fx,feqb,irank)
      call checkf(61,flag,available,needed,t)
      call rzero(n,y)
      do i=1,n
         do j=ia(i),ia(i+1)-1
            y(ja(j))=a(j)
         enddo
         do j=ib(i),ib(i+1)-1
            e(j)=y(jb(j))
         enddo
         do j=ia(i),ia(i+1)-1
            y(ja(j))=0
         enddo
      enddo
      print*,'differences over 1e-6:'
      x(1)=0
      x(2)=0
      x(3)=0
      do j=1,n
        do i=ib(j),ib(j+1)-1
         x(1)=x(1)+abs(c(i))
         x(2)=x(2)+abs(d(i)-c(i))
         x(3)=x(3)+abs(e(i)-c(i))
         if (abs(d(i)-c(i))+abs(e(i)-c(i)).gt.1d-4) then
           print '(2i6,2x,3g14.4)',j,jb(i),c(i),d(i),e(i)
         endif
        enddo
      enddo
      print*
      print*,'Avg difference between option 60 and solving:',x(2)/x(1)
      print*,'Avg difference between option 61 and solving:',x(3)/x(1)
      end

      subroutine getfile(n,m,p,name,ib,jb,b)
      character name*(*)
      integer n,p,m,ib(p),jb(m),i,j,k,oldi
      real*8 b(m),a

      open(1,file=name)
      n=0
      ib(1)=1
      k=0
      oldi=0
10    read(1,*,end=100)i,j,a
      if (oldi.gt.i) stop
      if (i.ge.p) then
         print*,'increase max_n'
         stop
      endif
      ib(i+1)=ib(i+1)+1
      k=k+1
      if (k.ge.m) then
         print*,'increase max_nze'
         stop
      endif
      jb(k)=j
      b(k)=a
      oldi=i
      n=i
      goto 10
100   if (n.eq.0) then
         print*,'0 elements'
         stop
      endif
      do i=2,n+1
         ib(i)=ib(i)+ib(i-1)
      enddo
      print '(''read'',i8,'' equations and'',i9,'' coefficients'')',i,k
      end
 
      subroutine cpy(n,m,p,ib,jb,b,ia,ja,a)
      integer n,p,m,ib(p),jb(m),ia(p),ja(m),i
      real*8 a(m),b(m)
      do i=1,n+1
         ia(i)=ib(i)
      enddo
      do i=1,ib(n+1)-1
         a(i)=b(i)
         ja(i)=jb(i)
      enddo
      end

      subroutine prss(n,m,p,ia,ja,a)
      integer n,m,p,ia(p),ja(m),i,j
      real*8 a(m)
      do i=1,n
         do j=ia(i),ia(i+1)-1
            print*,i,ja(j),a(j)
         enddo
      enddo
      end

      subroutine checkf(opt,flag,available,needed,t)
c checks flag and prints memory and time
      integer opt,flag,available,needed
      real t,second
c
      if (available.lt.needed) then
         print*,'option ',opt
         print*,'needed ',needed,' memory, available ',available
         stop
      endif
      if (flag.ne.0) then
         print*,'flag ',flag,' in option ', opt
         stop
      endif
      print 10,opt,100.0*needed/available, second()-t
10    format(' option',i3,' used ',f5.1,'% memory and',f8.2,' sec.')
      t=second()
      end
