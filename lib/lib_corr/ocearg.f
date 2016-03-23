           SUBROUTINE ocearg (UT1, XJD, ANGLE)

C   1      ocearg
C
C   1.1    ocearg PROGRAM SPECIFICATIONS
C
C   1.1.1  THIS SUBROUTINE COMPUTES THE ANGULAR ARGUMENTS
C          FOR SCHWIDERSKI COMPUTATION OF 11 OCEAN TIDES.
C          IT WAS COPIED FROM /sr/kf/gen_util
c          IT IS CONSISTENT WITH IERS TECHNICAL NOTE 13
C          CAUTION CALCK has true Julian Day Numbers whereas GAMIT
C          has PEP type Julian Day Numbers
C
C          C A U T I O N
C          = = = = = = =
C
C          SCHWIDERSKI MODIFIES THE ANGULAR ARGUMENTS OF THE DIURNAL
C          TERMS BY +/- 90 DEGREES. THEREFORE HIS DIURNAL PHASES
C          CANNOT BE USED WITH THE STANDARD DOODSEN OR CARTWRIGHT
C          CONVENTIONS.
C
C
C   1.1.2  RESTRICTIONS  -  NONE
C
C   1.1.3  REFERENCES  -  MERIT STANDARS, APRIL 81, SECOND DRAFT,
C                         APPENDICES 7,11
c                         SUPER CEEDED BY TECHNICAL NOTE 13
C
C   1.2.   ocearg PROGRAM INTERFACE
C
C   1.2.1  CALLING SEQUENCE
C
C          INPUT VARIABLES  -
C
C          1.  UT1     -  THE UT1 TIME OF THE DAY. (SEC)
C
C          2.  XJD     -  THE JULIAN DATE AT ZERO HOURS UTC OF THE
C                         DATE IN QUESTION. (DAYS)
C
C          OUTPUT VARIABLES  -
C
C          1.  ANGLE(11) -THE ANGULAR ARGUMENTS FOR SCHWIDERSKI
C                         COMPUTATION OF THE OCEAN TIDES IN THE ORDER:
C                         M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM, SSA
C
C
C  1.2.3   PROGRAM SPECIFICATIONS
C
      REAL*8 ANGLE(11), SPEED(11), convd, ut1, xjd, pi
c      real*8 convds

      REAL*4 ANGFAC(4,11)
         
c    debug
c      integer*4 i,j
C
C  1.2.4   DATA BASE ACCESS  -  NONE
C
C  1.2.5   EXTERNAL INPUT/OUTPUT  -  POSSIBLE DEBUG
C
C
C  1.2.6   SUBROUTINE INTERFACE
C
C          CALLER SUBROUTINE  -  ??????
C
C          CALLED SUBROUTINE  -  DMOD
C
C  1.2.7   CONSTANTS USED  -
C
C          1.  CONVD
C
C          2.  PI
C
C          3.  ANGFAC(4,11) -  TABLE OF MULTIPLES OF ARGUMENTS.
C                              (UNITLESS)
C
C          4.  CENTJ        -  THE NUMBER OF JULIAN DAYS PER JULIAN
C                              CENTURY. (DAYS/CENT.) (CENTJ=36525.D0)
C
C          5.  SPEED(11)    -  COEFFICIENTS FOR THE COMPUTATION OF
C                              ARGUMENTS IN THE FOLLOWING ORDER OF TIDES
C                              M2, S2, N2, K2, K1, O1, P1, Q1, MF, MM,
C                              SSA. (RAD/SEC)
C
C          6.  XJD75        -  THE JULIAN DATE OF JAN 0.0 1975. (DAYS)

      real*8 centj, xjd75
      integer*4 k

C
C
      DATA ANGFAC / 2.D0, -2.D0, 0.D0, 0.D0,
     2              0.D0,  0.D0, 0.D0, 0.D0,
     3              2.D0, -3.D0, 1.D0, 0.D0,
     4              2.D0,  0.D0, 0.D0, 0.D0,
     5              1.D0,  0.D0, 0.D0, 0.25D0,
     6              1.D0, -2.D0, 0.D0,-0.25D0,
     7             -1.D0,  0.D0, 0.D0,-0.25D0,
     8              1.D0, -3.D0, 1.D0,-0.25D0,
     9              0.D0,  2.D0, 0.D0, 0.D0,
     A              0.D0,  1.D0,-1.D0, 0.D0,
     1              2.D0,  0.D0, 0.D0, 0.D0 /     

c** Q1 and Mf arguments fixed by R. King in response to corrected from Matsumoto of 
c   NAO Japan, 24 Jan 2002.  Old lines:
c      8              1.D0, -3.D0, 0.D0,-0.25D0,
c      9              0.D0,  2.D0, 1.D0, 0.D0,

      DATA CENTJ / 36525.D0 /
C
      DATA SPEED / 1.40519D-4, 1.45444D-4, 1.37880D-4,
     1             1.45842D-4, 0.72921D-4, 0.67598D-4,
     2             0.72523D-4, 0.64959D-4, 0.053234D-4,
     3             0.026392D-4, 0.003982D-4 /
C
      DATA XJD75 / 2442412.5D0 /
C
      DATA CONVD/0.017453292519943296D0/
c      DATA CONVDS/4.8481368110953599D-6/
C
C
C  1.2.8   PROGRAM VARIABLES
C  1.2.8   PROGRAM VARIABLES
C
C          1.  CAPT  -  THE NUMBER OF JULIAN CENTURIES BETWEEN JAN 0.5, 1900
C                       AND THE OBSERVATION
C
C          2.  FDAY  -  FRACTIONAL PART OF UNIVERSAL TIME (UT1) DAY IN
C                       SECONDS. (SEC)
C
C          3.  H0    -  MEAN LONGITUDE OF SUN AT BEGINNING OF DAY. (RAD)
C
C          4.  ICAPD -  JULIAN DAYS SINCE JANUARY 0.0 UT 1975. (DAYS)
C
C          5.  P0    -  MEAN LONGITUDE OF LUNAR PERIGEE AT BEGINNING
C                       OF DAY. (RAD)
C
C          6.  S0    -  MEAN LONGITUDE OF MOON AT BEGINNING OF DAY.
C                       (RAD)

      real*8 capt, fday, h0, icapd, p0, s0

C
C  1.2.9   PROGRAMMER  -  CLYDE GOAD
C          83:10:08 HAROLD M. SCHUH
C          89:10:08 Jim Ryan ANGFAC and SPEED moved to ema.
c          93:10:10  Peter Morgan changed for GAMIT use
C
C  1.3     ocearg PROGRAM STRUCTURE
C
C     Compute FDAY  -  fractional part of UT1 day in seconds.
      FDAY = UT1
      pi = convd*180.d0
c
C     Compute ICAPD  -  Days since JAN 0, 0.00 UT, 1975
C     and  CAPT   -  Julian centuries since JAN 0.5, 1900.
      ICAPD = XJD - XJD75
      CAPT = ( 27392.500528D0 + 1.000000035D0 * ICAPD ) / CENTJ
C
C
C     Compute mean longitude of sun at beginning of day.
      H0 = (279.69668D0 + (36000.768930485D0 + 3.03D-4 * CAPT) * CAPT)
     1     * CONVD
C
C     Compute mean longitude of moon at beginning of day.
      S0 = ((( 1.9D-6 * CAPT - 0.001133D0 ) * CAPT + 481267.88314137D0)
     1     * CAPT + 270.434358D0 ) * CONVD
C
C     Compute mean longitude of lunar perigee at beginning of day.
      P0 = ((( -1.2D-5 * CAPT - .010325D0 ) * CAPT + 4069.0340329577D0)
     1     * CAPT + 334.329653D0 ) * CONVD
C
C     Calculate the angular arguments.
C     Run a loop over the 11 main tides.
C
      DO K = 1,11
        ANGLE(K) = SPEED(K)*FDAY + ANGFAC(1,K)*H0 + ANGFAC(2,K)*S0
     1             + ANGFAC(3,K)*P0 + ANGFAC(4,K)*2.0d0*pi
C
        ANGLE(K) = MOD( ANGLE(K), 2.0d0*pi )
        IF( ANGLE(K) .LT. 0.D0 ) ANGLE(K) = ANGLE(K) + 2.0d0*pi
      Enddo
c      print *,'OCEANG xjd,ut1,capt ho so po ',xjd,ut1,capt,h0,s0,p0
c      print *,'OCEANG speed: ',speed
c      print *,'OCEANG angfac: '
c      do i=1,4
c       write(*,'(11f10.2)') (angfac(i,j),j=1,11)
c      enddo
c      print *,'OCEAN angle ',angle

C
C     Normal termination.
  800 RETURN
      END


