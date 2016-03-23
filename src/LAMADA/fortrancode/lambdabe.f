      program LAMBDABE

c*rol example program for calling the LAMBDA routines
c*rol The routines are described in "The LAMBDA method for integer
c*rol ambiguity estimation: implementaion aspects", Delft Geodetic
c*rol Computing Centre, August 1996.
c*ver version 1.1, dd. 19-10-96 (19-12-95)

c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge 
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par cands           | 2-dimensional array to store the candidates 
c*par disall          | according squared norms \hat{a}-\check{a}
c*par Zt              inverse transpose of Z-matrix
c*par Q               variance-covariance matrix. This matrix is symmetric,
c*                    and only the lower triangular part is stored and
c*                    accessed. 
c*par a               the vector with real valued estimates \hat{a} (float
c*                    solution)
c*par truth           true integer values for the ambiguities
 
      implicit double precision (a-h, o-z)
      parameter (MaxCan=2, m=12)

      double precision 
     +  Q (m*m),  Zt(m*m), D (m), a (m),   
     +  disall (MaxCan), cands (m, MaxCan),
     +  v1 (m), v2 (m+1), v3 (m+1), v4 (m), v5 (m), v6 (m), v7 (m)  
      double precision
     +  L (m*m), Z (m*m)    ! <---- only needed for LAMBDA (old)
      integer 
     +  truth (m)
      integer
     +  num (m)             ! <---- only needed for LAMBDA (old)
      character
     +  rule*78

      log=1
      open (log, file='lambda.res', form='formatted', status='unknown')

      write (*,'(a)') ' 1. 3D-example of section 3.8 and 4.12'
      write (*,'(a)') ' 2. Example read from file: covar.dat'
      write (*,'(a)') ' Enter your choice : '
      read (*,*) iopt
      write (*,'(a)') ' 1. First computation of Z (old setup)'
      write (*,'(a)') ' 2. Direct computation of Z-transposed'
      write (*,'(a)') ' Enter your choice : '
      read (*,*) jopt
 
c*    fill the var-covar matrix columnwise, only lower triangular part
      if (iopt.eq.1) then
         n=3
         Q(1)=6.290
         Q(2)=5.978
         Q(3)=0.544
         Q(5)=6.292  
         Q(6)=2.340
         Q(9)=6.288

         a(1)=5.45
         a(2)=3.10
         a(3)=2.97
         truth(1)=5
         truth(2)=3
         truth(3)=4
      else
         open (2, file='covar.dat' , form='formatted', 
     +     status='unknown')
         read (2,'(a)') rule
         write (log,'(a)') rule
         read (2,'(a)') rule
         write (log,'(a)') rule
         read (2,*) n, (a(i),i=1,n)
         read (2,*) k, (Q(i),i=1,k)
         read (2,*) n, (truth(i),i=1,n)
      endif

      if (jopt.eq.1) then
         call LAMBDA (log, n, Q, a, cands, disall, L, D, Z, Zt, v1, v2, 
     +     v3, v4, v5, v6, v7, num)
      else if (jopt.eq.2) then
         call LAMBDA4 (log, n, Q, D, a, cands, disall, Zt, v1, v2, 
     +     v3, v4, v5, v6, v7)
      endif

      write (log,'(a,g14.4)') ' minimum squared norm  : ', disall(1)
      write (log,'(a,g14.4)') ' idem, second best     : ', disall(2)

      do i=1,n
         v1(i)=a(i)-truth(i)
      end do

      call INTOUT (log, 'truth', truth, 1, n)
      call DOUT (log, 'a_check-truth', v1, 1, n)

      end
