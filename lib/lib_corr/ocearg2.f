C*
      SUBROUTINE ocearg2(smjd, ANGLE)
CC
CC PURPOSE    :  COMPUTES THE ANGULAR ARGUMENT WHICH DEPENDS ON TIME FOR 54
CC               TIDAL ARGUMENT CALCULATIONS
CC
CC PARAMETERS :
CC         IN :  smjd :  MODIFIED JULIAN DAY                     R*8
CC         OUT:  ANGLE:  ANGULAR ARGUMENT IN RADIAN              R*8(54)
CC
CC AUTHOR     :  Koji MATSUMOTO @ National Atstonomical Observatory Japan
CC
CC VERSION    :  1.1
CC
CC CREATED    :  24-JAN-2002
CC
C*     
c*    Modfied by R. King (MIT) for explicit declarations 18-June 2002

      implicit none

      integer* 4 iarg, i,j

      real*8 smjd, angle, frac, rad, etmut, xarg, temp
     .     , tu, tu2, tu3, td, td2, fr, f1, f2, f3, f4, f5, f6 

      dimension iarg(7,54), xarg(7,54)
      dimension angle(54)   , fr(3,6)
c          
c     not used:
c      data pi      /3.14159265358979d0/
c      data deg     /5.729577951308232d+1/    
      data rad     /1.745329251994329d-2/

      data  etmut  / 58.0d0 /
c
c Major 8
c
      data ((iarg(i,j),i=1,7),j=1,8)
     +         /2, 0, 0, 0, 0, 0,  0, ! M2      1
     +          2, 2,-2, 0, 0, 0,  0, ! S2      2
     +          1, 1, 0, 0, 0, 0, 90, ! K1      3
     +          1,-1, 0, 0, 0, 0,-90, ! O1      4
     +          2,-1, 0, 1, 0, 0,  0, ! N2      5
     +          1, 1,-2, 0, 0, 0,-90, ! P1      6
     +          2, 2, 0, 0, 0, 0,  0, ! K2      7
     +          1,-2, 0, 1, 0, 0,-90/ ! Q1      8
c               m  s  h  p  n  ps
c
c Minor 8
c
      data ((iarg(i,j),i=1,7),j=9,16)
     +         /1, 0, 0, 1, 0, 0, 90, ! M1      9
     +          1, 2, 0,-1, 0, 0, 90, ! J1     10
     +          1, 3, 0, 0, 0, 0, 90, ! OO1    11
     +          2,-2, 0, 2, 0, 0,  0, ! 2N2    12
     +          2,-2, 2, 0, 0, 0,  0, ! Mu2    13
     +          2,-1, 2,-1, 0, 0,  0, ! Nu2    14
     +          2, 1, 0,-1, 0, 0,180, ! L2     15
     +          2, 2,-3, 0, 0, 1,  0/ ! T2     16
c               m  s  h  p  n  ps
c
c Diurnal Minor 17
      data ((iarg(i,j),i=1,7),j=17,33)
     +         /1,-3, 0, 2, 0, 0,-90, ! 2Q1     17
     +          1,-3, 2, 0, 0, 0,-90, ! Sigma1  18
     +          1,-2, 0, 1,-1, 0,-90, ! Q1'     19 
     +          1,-2, 2,-1, 0, 0,-90, ! Rho1    20
     +          1,-1, 0, 0,-1, 0,-90, ! O1'     21
     +          1,-1, 2, 0, 0, 0, 90, ! Tau1    22
     +          1, 0, 0, 1, 1, 0, 90, ! M1'     23
     +          1, 0, 2,-1, 0, 0, 90, ! Kai1    24
     +          1, 1,-3, 0, 0, 1,-90, ! Pi1     25
     +          1, 1,-2, 0,-1, 0, 90, ! P1'     26
     +          1, 1, 0, 0,-1, 0,-90, ! K1'     27
     +          1, 1, 0, 0, 1, 0, 90, ! K1'     28
     +          1, 1, 1, 0, 0,-1, 90, ! Psi1    29
     +          1, 1, 2, 0, 0, 0, 90, ! Phi1    30
     +          1, 2,-2, 1, 0, 0, 90, ! Theta1  31
     +          1, 2, 0,-1, 1, 0, 90, ! J1'     32
     +          1, 3, 0, 0, 1, 0, 90/ ! OO1'    33
c               m  s  h  p  n  ps
c
c Semi-diurnal minor 16
      data ((iarg(i,j),i=1,7),j=34,49)
     +         /2,-3, 2, 1, 0, 0,  0, ! Eps.2   34
     +          2,-2, 2, 0,-1, 0,180, ! Mu2'    35
     +          2,-1, 0, 1,-1, 0,180, ! N2'     36
     +          2,-1, 2,-1,-1, 0,180, ! Nu2'    37
     +          2, 0,-2, 2, 0, 0,180, ! Gamma2  38
     +          2, 0,-1, 0, 0, 1,180, ! Alpha2  39
     +          2, 0, 0, 0,-1, 0,180, ! M2'     40
     +          2, 0, 1, 0, 0,-1,  0, ! Beta2   41
     +          2, 0, 2, 0, 0, 0,  0, ! Delata2 42
     +          2, 1,-2, 1, 0, 0,180, ! Lambda2 43
     +          2, 2,-2, 0,-1, 0,  0, ! S2'     44
     +          2, 2,-1, 0, 0,-1,180, ! R2      45
     +          2, 2, 0, 0, 1, 0,  0, ! K2'     46
     +          2, 3,-2, 1, 0, 0,  0, ! Zeta2   47
     +          2, 3, 0,-1, 0, 0,  0, ! Eta2    48
     +          2, 3, 0,-1, 1, 0,  0/ ! Eta2'   49
c               m  s  h  p  n  ps
c
c Long period
c
      data ((iarg(i,j),i=1,7),j=50,54)
     +         /0, 3, 0,-1, 0, 0,  0, ! Mtm    50
     +          0, 2, 0, 0, 0, 0,  0, ! Mf     51
     +          0, 1, 0,-1, 0, 0,  0, ! Mm     52
     +          0, 0, 2, 0, 0, 0,  0, ! Ssa    53
     +          0, 0, 1, 0, 0,-1,  0/ ! Sa     54
c               m  s  h  p  n  ps
c
      data fr/280.4606184d0,  36000.7700536d0,  0.00038793d0,
     +        218.3166560d0, 481267.8813420d0, -0.00133000d0,
     +        280.4664490d0,  36000.7698220d0,  0.00030360d0,
     +         83.3532430d0,   4069.0137110d0, -0.01032400d0,
     +        234.9554440d0,   1934.1361850d0, -0.00207600d0,
     +        282.9373480d0,      1.7195330d0,  0.00045970d0/
c
c ----------------------------------------------------------------------
c
      tu  = (smjd - 51544.5d0) / 36525.d0
      tu2 = tu*tu
      tu3 = tu2*tu
      td  = tu + etmut/86400.d0/36525.d0
      td2 = td*td
      frac = smjd - dint(smjd)
c
      f2 = fr(1,2) + fr(2,2)*td + fr(3,2)*td2 
     +   + 0.0040d0*dcos((29.d0*133.d0*td)*rad)
c
      f1 = fr(1,1) + fr(2,1)*tu + fr(3,1)*tu2 
     +   - 0.0000000258d0*tu3   + 360.d0*frac - f2
c
      f3 = fr(1,3) + fr(2,3)*td + fr(3,3)*td2
     +   + 0.0018d0*dcos((159.d0+19.d0*td)*rad)
c
      f4 = fr(1,4) + fr(2,4)*td + fr(3,4)*td2
c
      f5 = fr(1,5) + fr(2,5)*td + fr(3,5)*td2
c
      f6 = fr(1,6) + fr(2,6)*td + fr(3,6)*td2
c
c
c
      do j = 1,54
         do i = 1,7
            xarg(i,j) = dfloat(iarg(i,j))
         enddo
      enddo
c
      do j = 1,54
c
         temp = f1*xarg(1,j) + f2*xarg(2,j) + f3*xarg(3,j)
     +        + f4*xarg(4,j) + f5*xarg(5,j) + f6*xarg(6,j)
     +        +    xarg(7,j)
         temp = mod(temp,360.d0)
         if (temp.lt.0.d0) temp = temp + 360.d0
c
         angle(j) = temp*rad ! in radian
c
      enddo
c
      RETURN
      END
