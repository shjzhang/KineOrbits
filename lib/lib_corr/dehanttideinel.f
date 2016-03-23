C****************************************************************************************
       SUBROUTINE TIDE(XSTA,iYR,iMONTH,iDAY,FHR,XSUN,XMON,DXTIDE)
C  
C PURPOSE    :  COMPUTATION OF TIDAL CORRECTIONS OF STATION DISPLACEMENTS  
C               CAUSED BY LUNAR AND SOLAR GRAVITATIONAL ATTRACTION  
C               (SEE IERS STANDARDS 1996)  
C         STEP 1 (HERE GENERAL DEGREE 2 AND 3 CORRECTIONS +  
C                CALL ST1IDIU + CALL ST1ISEM + CALL ST1L1) 
C         + STEP 2 (CALL STEP2DIU + CALL STEP2LON + CALL STEP2IDIU)  
C IT HAS BEEN DECIDED THAT THE STEP 3 NON-CORRECTION FOR PERMANENT TIDE 
C WOULD NOT BE APPLIED IN ORDER TO AVOID JUMP IN THE REFERENCE FRAME 
C (THIS STEP 3 MUST ADDED IN ORDER TO GET THE NON-TIDAL STATION POSITION  
C AND TO BE CONFORMED WITH THE IAG RESOLUTION.)  
C  
C    INPUT :  XSTA(I),I=1,2,3: GEOCENTRIC POSITION OF THE STATION (ITRF,
C                              CO-ROTATING FRAME) -- UNITS = METERS
C             XSUN(I),I=1,2,3: GEOC. POSITION OF THE SUN (ECEF FRAME) --
C                              UNITS = METERS
C             XMON(I),I=1,2,3: GEOC. POSITION OF THE MOON (ECEF FRAME) --
C                              UNITS = METERS
C             IYR : YEAR (UTC TIMESCALE)
C             IMONTH : MONTH (UTC TIMESCALE)
C             IDAY : DAY (UTC TIMESCALE)
C             FHR=hr+zmin/60.+sec/3600. : HR IN THE DAY 
C   OUTPUT :  DXTIDE(I),I=1,2,3: DISPLACEMENT VECTOR (GEOCENTRIC ITRF FRAME) --
C                                UNITS = METERS
C  
C SUBROUTINES CALLED  :  SPROD  
C                        ST1IDIU 
C                        ST1ISEM 
C                        ST1L1 
C                        STEP2DIU  
C                        STEP2LON 
C                        STEP2ILON  
C  
C AUTHOR IERS 1996 :  V. DEHANT, S. MATHEWS AND J. GIPSON
C    (TEST BETWEEN TWO SUBROUTINES) 
C AUTHOR IERS 2000 :  V. DEHANT AND S. MATHEWS
C    (TEST IN THE BERNESE PROGRAM BY C. BRUYNINX)
C  
C CREATED    :  96/03/23              LAST MODIFIED :  00/05/17 14:10  
C UPDATED    :2006/02/06  HEADER COMMENTS MODIFIED TO CLARIFY INPUT/OUTPUT
C                         UNITS AND SYSTEMS BY JIM RAY
C UPDATED    :2006/02/06  SUBROUTINE DUTC MODIFIED FOR LEAP SECOND ON
C                         2006.0 AND TO CORRECT do 5 i=1,87 from 84 to 87
C                         BY JIM RAY
C UPDATED    :2006/08/31  CORRECT DUTC FOR DATES AFTER 2007 (G. PETIT)
C UPDATED    :2007/06/20  MODIFIED SUBROUTINE DUTC TO CORRECT PAST MISTAKE (PROVIDED BY H. MANCHE) 
C                         CORRECT STEP2DIU LINE       DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)+
C                                            TO       DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)-
C UPDATED    :2007/10/23  Replace subroutines DUTC and FJLDY by SOFA routines iau_CAL2JD and iau_DAT.
C                         The source code of these routines is included in this file. 
C                         Correct time arguments of subroutine STEP2DIU: argument T is in TT, FHR is in UT.
C                         By H. Manche and G. Petit
C UPDATED    :2009/02/19  Update routine iau_DAT for 2009.0 leap second.
C UPDATED    :2009/04/09  FHR is now passed to iau_DAT as fraction of day, as expected (bug noted by T. Springer)
C  
C  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C  
      DOUBLE PRECISION XSTA(3),XSUN(3),XMON(3),DXTIDE(3),XCORSTA(3)
      DOUBLE PRECISION H20,L20,H3,L3,H2,L2
      DOUBLE PRECISION mass_ratio_sun,mass_ratio_moon
      integer statut
      DOUBLE PRECISION JJM0,JJM1
C  
C NOMINAL SECOND DEGREE AND THIRD DEGREE LOVE NUMBERS AND SHIDA NUMBERS  
C ---------------------------------------------------------------------  
      DATA H20/0.6078D0/,L20/0.0847D0/,H3/0.292D0/,L3/0.015D0/
C  
C SCALAR PRODUCT OF STATION VECTOR WITH SUN/MOON VECTOR  
C -----------------------------------------------------  
      CALL SPROD(XSTA,XSUN,SCS,RSTA,RSUN)  
      CALL SPROD(XSTA,XMON,SCM,RSTA,RMON)  
      SCSUN=SCS/RSTA/RSUN  
      SCMON=SCM/RSTA/RMON
C   
C COMPUTATION OF NEW H2 AND L2  
C ----------------------------  
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
      H2=H20-0.0006*(1.-3./2.*COSPHI**2)
      L2=L20+0.0002*(1.-3./2.*COSPHI**2)  
C
C P2-TERM  
C -------  
      P2SUN=3.*(H2/2.-L2)*SCSUN**2-H2/2.  
      P2MON=3.*(H2/2.-L2)*SCMON**2-H2/2.  
C  
C P3-TERM  
C -------  
      P3SUN=5./2.*(H3-3.*L3)*SCSUN**3+3./2.*(L3-H3)*SCSUN
      P3MON=5./2.*(H3-3.*L3)*SCMON**3+3./2.*(L3-H3)*SCMON

C  
C TERM IN DIRECTION OF SUN/MOON VECTOR  
C ------------------------------------  
      X2SUN=3.*L2*SCSUN  
      X2MON=3.*L2*SCMON  
      X3SUN=3.*L3/2.*(5.*SCSUN**2-1.)  
      X3MON=3.*L3/2.*(5.*SCMON**2-1.)
C  
C FACTORS FOR SUN/MOON  
C --------------------  
      MASS_RATIO_SUN=332945.943062d0
      MASS_RATIO_MOON=0.012300034d0
      RE =6378136.55d0
      FAC2SUN=MASS_RATIO_SUN*RE*(RE/RSUN)**3
      FAC2MON=MASS_RATIO_MOON*RE*(RE/RMON)**3
      FAC3SUN=FAC2SUN*(RE/RSUN)
      FAC3MON=FAC2MON*(RE/RMON)
C  
C TOTAL DISPLACEMENT  
C ------------------  
      DO 10 I=1,3  
      DXTIDE(I)=FAC2SUN*( X2SUN*XSUN(I)/RSUN + P2SUN*XSTA(I)/RSTA ) +
     1            FAC2MON*( X2MON*XMON(I)/RMON + P2MON*XSTA(I)/RSTA ) +  
     2            FAC3SUN*( X3SUN*XSUN(I)/RSUN + P3SUN*XSTA(I)/RSTA ) +   
     3            FAC3MON*( X3MON*XMON(I)/RMON + P3MON*XSTA(I)/RSTA )  
10    CONTINUE  
      call zero_vec8(xcorsta)
C  
C CORRECTIONS FOR THE OUT-OF-PHASE PART OF LOVE NUMBERS (PART H_2^(0)I  
C            AND L_2^(0)I )  
C FIRST, FOR THE DIURNAL BAND       

      CALL ST1IDIU(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
      DO 11 I=1,3
      DXTIDE(I)=DXTIDE(I)+XCORSTA(I)  
11    CONTINUE
C  
C SECOND, FOR THE SEMI-DIURNAL BAND       
C 
      CALL ST1ISEM(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
      DO 12 I=1,3
      DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
12    CONTINUE  
C  
C CORRECTIONS FOR THE LATITUDE DEPENDENCE OF LOVE NUMBERS (PART L^(1) )  
C   
      CALL ST1L1(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
      DO 13 I=1,3  
      DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
13    CONTINUE    
C  
C CONSIDER CORRECTIONS FOR STEP 2  
C  
C CORRECTIONS FOR THE DIURNAL BAND:  
C 
C  FIRST, WE NEED TO KNOW THE DATE CONVERTED IN JULIAN CENTURIES 
c        
c   1) CALL THE SUBROUTINE COMPUTING THE JULIAN DATE 
c 
      call iau_CAL2JD ( iyr, imonth, iday, JJM0, JJM1, statut )
c.....Modified 2009/04/09
      FHRD = FHR/24.d0
      T=((JJM0-2451545.0d0)+JJM1+FHRD)/36525.d0
c 
c   2) CALL THE SUBROUTINE COMPUTING THE CORRECTION OF UTC TIME  
c 
      call iau_DAT ( IYR,IMONTH,IDAY, FHRD, DTT, statut )
      DTT = DTT + 32.184d0
c     Conversion of T in TT time
      T=T+DTT/(3600.0d0*24.0d0*36525.0d0)
C  
C  SECOND, WE CAN CALL THE SUBROUTINE STEP2DIU, FOR THE DIURNAL BAND CORRECTIONS,
C   (in-phase and out-of-phase frequency dependence):
C  
      CALL STEP2DIU(XSTA,FHR,T,XCORSTA)  
      DO 14 I=1,3  
      DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
14    CONTINUE  
C  
C  CORRECTIONS FOR THE LONG-PERIOD BAND,
C   (in-phase and out-of-phase frequency dependence):  
C
      call STEP2LON(XSTA,FHR,T,XCORSTA)
      DO 15 I=1,3  
      DXTIDE(I)=DXTIDE(I)+XCORSTA(I)
15    CONTINUE  
C    
C CONSIDER CORRECTIONS FOR STEP 3  
C  
C UNCORRECT FOR THE PERMANENT TIDE  
C  
C      PI=3.141592654
C      SINPHI=XSTA(3)/RSTA  
C      COSPHI=dsqrt(XSTA(1)**2+XSTA(2)**2)/RSTA
C      COSLA=XSTA(1)/COSPHI/RSTA  
C      SINLA=XSTA(2)/COSPHI/RSTA  
C      DR=-DSQRT(5./4./PI)*H2*0.31460*(3./2.*SINPHI**2-0.5)
C      DN=-DSQRT(5./4./PI)*L2*0.31460*3.*COSPHI*SINPHI
C      DXTIDE(1)=DXTIDE(1)-DR*COSLA*COSPHI+DN*COSLA*SINPHI
C      DXTIDE(2)=DXTIDE(2)-DR*SINLA*COSPHI+DN*SINLA*SINPHI  
C      DXTIDE(3)=DXTIDE(3)-DR*SINPHI      -DN*COSPHI
C       
      RETURN  
      END  
C********************************************************************
C 
C 
      SUBROUTINE SPROD(X,Y,SCAL,R1,R2) 
C 
C  COMPUTATION OF THE SCALAR-PRODUCT OF TWO VECTORS AND THEIR NORMS 
C 
C  INPUT :  X(I),I=1,2,3: COMPONENTS OF VECTOR X 
C           Y(I),I=1,2,3: COMPONENTS OF VECTOR Y 
C  OUTPUT :  SCAL: SCALAR PRODUCT OF X AND Y 
C            R1,R2  : LENGTHS OF THE TWO VECTORS X AND Y 
C 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION X(3),Y(3)
      R1=DSQRT(X(1)**2+X(2)**2+X(3)**2) 
      R2=DSQRT(Y(1)**2+Y(2)**2+Y(3)**2) 
      SCAL=X(1)*Y(1)+X(2)*Y(2)+X(3)*Y(3) 
      RETURN 
      END 
C 
C-------------------------------------------------------------------------
C
      SUBROUTINE ST1L1(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
C  
C THIS SUBROUTINE GIVES THE CORRECTIONS INDUCED BY THE LATITUDE DEPENDENCE  
C GIVEN BY L^(1) IN MAHTEWS ET AL (1991)  
C  
C       INPUT : XSTA,XSUN,XMON,FAC3SUN,FAC3MON  
C      OUTPUT : XCORSTA  
C  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C compute the norm of a vector
      DOUBLE PRECISION norm8
      DIMENSION XSTA(3),XSUN(3),XMON(3),XCORSTA(3)  
      DOUBLE PRECISION L1,L1D,L1SD
      DATA L1D/0.0012d0/,L1SD/0.0024d0/
      RSTA=norm8(xsta)
      SINPHI=XSTA(3)/RSTA  
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA  
      SINLA=XSTA(2)/COSPHI/RSTA  
      COSLA=XSTA(1)/COSPHI/RSTA  
      RMON=norm8(XMON)
      rsun=norm8(xsun)
C
C FOR THE DIURNAL BAND  
C  
      L1=L1D  
      DNSUN=-L1*SINPHI**2*FAC2SUN*XSUN(3)*(XSUN(1)*COSLA+XSUN(2)*SINLA)
     &            /RSUN**2
      DNMON=-L1*SINPHI**2*FAC2MON*XMON(3)*(XMON(1)*COSLA+XMON(2)*SINLA)
     &            /RMON**2
      DESUN=L1*SINPHI*(COSPHI**2-SINPHI**2)*FAC2SUN*XSUN(3)*
     1 (XSUN(1)*SINLA-XSUN(2)*COSLA)/RSUN**2
      DEMON=L1*SINPHI*(COSPHI**2-SINPHI**2)*FAC2MON*XMON(3)*
     1 (XMON(1)*SINLA-XMON(2)*COSLA)/RMON**2
      DE=3.*(DESUN+DEMON)  
      DN=3.*(DNSUN+DNMON)  
      XCORSTA(1)=-DE*SINLA-DN*SINPHI*COSLA  
      XCORSTA(2)=DE*COSLA-DN*SINPHI*SINLA  
      XCORSTA(3)=DN*COSPHI  
C   
C FOR THE SEMI-DIURNAL BAND  
C  
      L1=L1SD  
      COSTWOLA=COSLA**2-SINLA**2  
      SINTWOLA=2.*COSLA*SINLA  
      DNSUN=-L1/2.*SINPHI*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*
     1 COSTWOLA+2.*XSUN(1)*XSUN(2)*SINTWOLA)/RSUN**2
      DNMON=-L1/2.*SINPHI*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*
     1 COSTWOLA+2.*XMON(1)*XMON(2)*SINTWOLA)/RMON**2
      DESUN=-L1/2.*SINPHI**2*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*
     1 SINTWOLA-2.*XSUN(1)*XSUN(2)*COSTWOLA)/RSUN**2
      DEMON=-L1/2.*SINPHI**2*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*
     1 SINTWOLA-2.*XMON(1)*XMON(2)*COSTWOLA)/RMON**2
      DE=3.*(DESUN+DEMON)  
      DN=3.*(DNSUN+DNMON)  
      XCORSTA(1)=XCORSTA(1)-DE*SINLA-DN*SINPHI*COSLA  
      XCORSTA(2)=XCORSTA(2)+DE*COSLA-DN*SINPHI*SINLA  
      XCORSTA(3)=XCORSTA(3)+DN*COSPHI  
      RETURN  
      END  

C*************************************************************************
C     Last change:  VD   17 May 00   1:20 pm
C  THESE ARE THE SUBROUTINES FOR THE STEP2 OF THE TIDAL CORRECTIONS. 
C  THEY ARE CALLED TO ACCOUNT FOR THE FREQUENCY DEPENDENCE  
C  OF THE LOVE NUMBERS. 
C 
      SUBROUTINE STEP2DIU(XSTA,FHR,T,XCORSTA)  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION XSTA(3),XCORSTA(3),DATDI(9,31)
      DOUBLE PRECISION deg2rad
      DATA deg2rad/0.0174532925d0/
      DATA ((DATDI(i,j),i=1,9),j=1,31)/  
     * -3., 0., 2., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,   
     * -3., 2., 0., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,   
     * -2., 0., 1.,-1., 0.,-0.02, 0.0 , 0.0 , 0.0,   
     * -2., 0., 1., 0., 0.,-0.08, 0.0 ,-0.01, 0.01,
     * -2., 2.,-1., 0., 0.,-0.02, 0.0 , 0.0 , 0.0,
     * -1., 0., 0.,-1., 0.,-0.10, 0.0 , 0.0 , 0.0,
     * -1., 0., 0., 0., 0.,-0.51, 0.0 ,-0.02, 0.03,
     * -1., 2., 0., 0., 0., 0.01, 0.0 , 0.0 , 0.0,
     *  0.,-2., 1., 0., 0., 0.01, 0.0 , 0.0 , 0.0,
     *  0., 0.,-1., 0., 0., 0.02, 0.0 , 0.0 , 0.0,
     *  0., 0., 1., 0., 0., 0.06, 0.0 , 0.0 , 0.0,
     *  0., 0., 1., 1., 0., 0.01, 0.0 , 0.0 , 0.0,
     *  0., 2.,-1., 0., 0., 0.01, 0.0 , 0.0 , 0.0,
     *  1.,-3., 0., 0., 1.,-0.06, 0.0 , 0.0 , 0.0,
     *  1.,-2., 0.,-1., 0., 0.01, 0.0 , 0.0 , 0.0,
     *  1.,-2., 0., 0., 0.,-1.23,-0.07, 0.06, 0.01,
     *  1.,-1., 0., 0.,-1., 0.02, 0.0 , 0.0 , 0.0,
     *  1.,-1., 0., 0., 1., 0.04, 0.0 , 0.0 , 0.0,
     *  1., 0., 0.,-1., 0.,-0.22, 0.01, 0.01, 0.0,
     *  1., 0., 0., 0., 0.,12.00,-0.80,-0.67,-0.03,
     *  1., 0., 0., 1., 0., 1.73,-0.12,-0.10, 0.0,
     *  1., 0., 0., 2., 0.,-0.04, 0.0 , 0.0 , 0.0, 
     *  1., 1., 0., 0.,-1.,-0.50,-0.01, 0.03, 0.0,
     *  1., 1., 0., 0., 1., 0.01, 0.0 , 0.0 , 0.0,
     *  0., 1., 0., 1.,-1.,-0.01, 0.0 , 0.0 , 0.0,
     *  1., 2.,-2., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,
     *  1., 2., 0., 0., 0.,-0.11, 0.01, 0.01, 0.0,
     *  2.,-2., 1., 0., 0.,-0.01, 0.0 , 0.0 , 0.0,
     *  2., 0.,-1., 0., 0.,-0.02, 0.0 , 0.0 , 0.0,
     *  3., 0., 0., 0., 0., 0.0 , 0.0 , 0.0 , 0.0,
     *  3., 0., 0., 1., 0., 0.0 , 0.0 , 0.0 , 0.0/
      S=218.31664563D0+481267.88194D0*T-0.0014663889D0*T**2 
     1 +0.00000185139D0*T**3 
      TAU=fhr*15.D0+280.4606184D0+36000.7700536D0*T+0.00038793D0*T**2 
     1 -0.0000000258D0*T**3-S 
      PR=1.396971278*T+0.000308889*T**2+0.000000021*T**3 
     1 +0.000000007*T**4 
      S=S+PR 
      H=280.46645D0+36000.7697489D0*T+0.00030322222D0*T**2 
     1 +0.000000020*T**3-0.00000000654*T**4  
      P=83.35324312D0+4069.01363525D0*T-0.01032172222D0*T**2
     1 -0.0000124991D0*T**3+0.00000005263D0*T**4  
      ZNS=234.95544499D0 +1934.13626197D0*T-0.00207561111D0*T**2
     1 -0.00000213944D0*T**3+0.00000001650D0*T**4  
      PS=282.93734098D0+1.71945766667D0*T+0.00045688889D0*T**2 
     1 -0.00000001778D0*T**3-0.00000000334D0*T**4
C Reduce angles to between 0 and 360.
      s=  dmod(s,360.d0)
      tau=dmod(tau,360.d0)
      h=  dmod(h,360.d0)
      p=  dmod(p,360.d0)
      zns=dmod(zns,360.d0)
      ps=dmod(ps,360.d0)
c      WRITE(2,'(6f10.3)') tau,s,h,p,zns,ps

      RSTA=DSQRT(XSTA(1)**2+XSTA(2)**2+XSTA(3)**2)  
      SINPHI=XSTA(3)/RSTA  
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA  

      COSLA=XSTA(1)/COSPHI/RSTA
      SINLA=XSTA(2)/COSPHI/RSTA
      ZLA = DATAN2(XSTA(2),XSTA(1))
      DO 99 I=1,3  
      XCORSTA(I)=0.
99    continue
      DO 98 J=1,31
      THETAF=(TAU+DATDI(1,J)*S+DATDI(2,J)*H+DATDI(3,J)*P+
     1 DATDI(4,J)*ZNS+DATDI(5,J)*PS)*deg2rad
      DR=DATDI(6,J)*2.*SINPHI*COSPHI*SIN(THETAF+ZLA)+
     1 DATDI(7,J)*2.*SINPHI*COSPHI*COS(THETAF+ZLA)
      DN=DATDI(8,J)*(COSPHI**2-SINPHI**2)*SIN(THETAF+ZLA)+
     1 DATDI(9,J)*(COSPHI**2-SINPHI**2)*COS(THETAF+ZLA)
c      DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)+
c     Modified 20 June 2007
      DE=DATDI(8,J)*SINPHI*COS(THETAF+ZLA)-
     1 DATDI(9,J)*SINPHI*SIN(THETAF+ZLA)
      XCORSTA(1)=XCORSTA(1)+DR*COSLA*COSPHI-DE*SINLA  
     1 -DN*SINPHI*COSLA  
      XCORSTA(2)=XCORSTA(2)+DR*SINLA*COSPHI+DE*COSLA  
     1 -DN*SINPHI*SINLA  
      XCORSTA(3)=XCORSTA(3)+DR*SINPHI+DN*COSPHI  
98    CONTINUE   

      DO 97 I=1,3
      XCORSTA(I)=XCORSTA(I)/1000.
97    CONTINUE  
      RETURN  
      END  
C  
C  *************************************************************  
C
      SUBROUTINE STEP2LON(XSTA,FHR,T,XCORSTA)  
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION deg2rad
      DOUBLE PRECISION XSTA(3),XCORSTA(3),DATDI(9,5)
      DATA deg2rad/0.0174532925d0/ 
      DATA ((DATDI(i,j),i=1,9),j=1,5)/  
     *   0, 0, 0, 1, 0,   0.47, 0.23, 0.16, 0.07,
     *   0, 2, 0, 0, 0,  -0.20,-0.12,-0.11,-0.05,
     *   1, 0,-1, 0, 0,  -0.11,-0.08,-0.09,-0.04,
     *   2, 0, 0, 0, 0,  -0.13,-0.11,-0.15,-0.07,
     *   2, 0, 0, 1, 0,  -0.05,-0.05,-0.06,-0.03/
C
      S=218.31664563D0+481267.88194D0*T-0.0014663889D0*T**2 
     1 +0.00000185139D0*T**3 
      PR=1.396971278*T+0.000308889*T**2+0.000000021*T**3 
     1 +0.000000007*T**4 
      S=S+PR 
      H=280.46645D0+36000.7697489D0*T+0.00030322222D0*T**2 
     1 +0.000000020*T**3-0.00000000654*T**4  
      P=83.35324312D0+4069.01363525D0*T-0.01032172222D0*T**2 
     1 -0.0000124991D0*T**3+0.00000005263D0*T**4  
      ZNS=234.95544499D0 +1934.13626197D0*T-0.00207561111D0*T**2
     1 -0.00000213944D0*T**3+0.00000001650D0*T**4  
      PS=282.93734098D0+1.71945766667D0*T+0.00045688889D0*T**2 
     1 -0.00000001778D0*T**3-0.00000000334D0*T**4  
      RSTA=DSQRT(XSTA(1)**2+XSTA(2)**2+XSTA(3)**2)  
      SINPHI=XSTA(3)/RSTA  
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA  
      COSLA=XSTA(1)/COSPHI/RSTA
      SINLA=XSTA(2)/COSPHI/RSTA
C reduce angles to between 0 and 360.
      s=  dmod(s,360.d0)
      tau=dmod(tau,360.d0)
      h=  dmod(h,360.d0)
      p=  dmod(p,360.d0)
      zns=dmod(zns,360.d0)
      ps=dmod(ps,360.d0)

      dr_tot=0.
      dn_tot=0.
      DO 99 I=1,3  
      XCORSTA(I)=0.
99    continue
      DO 98 J=1,5
      THETAF=(DATDI(1,J)*S+DATDI(2,J)*H+DATDI(3,J)*P+
     1 DATDI(4,J)*ZNS+DATDI(5,J)*PS)*DEG2RAD
      DR=DATDI(6,J)*(3.*SINPHI**2-1.)/2.*COS(THETAF)+
     1     DATDI(8,J)*(3.*SINPHI**2-1.)/2.*SIN(THETAF)
      DN=DATDI(7,J)*(COSPHI*SINPHI*2.)*COS(THETAF)+
     1     DATDI(9,J)*(COSPHI*SINPHI*2.)*SIN(THETAF)
      DE=0. 
      dr_tot=dr_tot+dr
      dn_tot=dn_tot+dn
      XCORSTA(1)=XCORSTA(1)+DR*COSLA*COSPHI-DE*SINLA  
     1 -DN*SINPHI*COSLA  
      XCORSTA(2)=XCORSTA(2)+DR*SINLA*COSPHI+DE*COSLA  
     1 -DN*SINPHI*SINLA  
      XCORSTA(3)=XCORSTA(3)+DR*SINPHI+DN*COSPHI  
98    CONTINUE   

      DO 97 I=1,3
      XCORSTA(I)=XCORSTA(I)/1000.
97    CONTINUE  
      RETURN  
      END  
C**************************************************************************************************
C-------------------------------------------------------------------------
C 
      SUBROUTINE ST1IDIU(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
C  
C THIS SUBROUTINE GIVES THE OUT-OF-PHASE CORRECTIONS INDUCED BY 
C MANTLE INELASTICITY IN THE DIURNAL BAND 
C  
C       INPUT : XSTA,XSUN,XMON,FAC2SUN,FAC2MON  
C      OUTPUT : XCORSTA  
C  

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION norm8
      DIMENSION XSTA(3),XSUN(3),XMON(3),XCORSTA(3)  
      DATA DHI/-0.0025/,DLI/-0.0007/  
      RSTA=NORM8(XSTA)
      SINPHI=XSTA(3)/RSTA  
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
      COS2PHI=COSPHI**2-SINPHI**2
      SINLA=XSTA(2)/COSPHI/RSTA  
      COSLA=XSTA(1)/COSPHI/RSTA  
      RMON=NORM8(XMON)
      RSUN=NORM8(XSUN)
      DRSUN=-3.*DHI*SINPHI*COSPHI*FAC2SUN*XSUN(3)*(XSUN(1)*
     1            SINLA-XSUN(2)*COSLA)/RSUN**2
      DRMON=-3.*DHI*SINPHI*COSPHI*FAC2MON*XMON(3)*(XMON(1)*
     1            SINLA-XMON(2)*COSLA)/RMON**2
      DNSUN=-3.*DLI*COS2PHI*FAC2SUN*XSUN(3)*(XSUN(1)*SINLA-
     1            XSUN(2)*COSLA)/RSUN**2
      DNMON=-3.*DLI*COS2PHI*FAC2MON*XMON(3)*(XMON(1)*SINLA-
     1            XMON(2)*COSLA)/RMON**2
      DESUN=-3.*DLI*SINPHI*FAC2SUN*XSUN(3)*
     1 (XSUN(1)*COSLA+XSUN(2)*SINLA)/RSUN**2
      DEMON=-3.*DLI*SINPHI*FAC2MON*XMON(3)*
     1 (XMON(1)*COSLA+XMON(2)*SINLA)/RMON**2
      DR=DRSUN+DRMON 
      DN=DNSUN+DNMON  
      DE=DESUN+DEMON 
      XCORSTA(1)=DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA  
      XCORSTA(2)=DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA  
      XCORSTA(3)=DR*SINPHI+DN*COSPHI  
      RETURN  
      END  
C
C-------------------------------------------------------------------------
      SUBROUTINE ST1ISEM(XSTA,XSUN,XMON,FAC2SUN,FAC2MON,XCORSTA)
C  
C THIS SUBROUTINE GIVES THE OUT-OF-PHASE CORRECTIONS INDUCED BY 
C MANTLE INELASTICITY IN THE DIURNAL BAND 
C  
C       INPUT : XSTA,XSUN,XMON,FAC2SUN,FAC2MON  
C      OUTPUT : XCORSTA  
C  

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION norm8
      DIMENSION XSTA(3),XSUN(3),XMON(3),XCORSTA(3)  
      DATA DHI/-0.0022/,DLI/-0.0007/  
      RSTA=NORM8(XSTA)
      SINPHI=XSTA(3)/RSTA  
      COSPHI=DSQRT(XSTA(1)**2+XSTA(2)**2)/RSTA
      SINLA=XSTA(2)/COSPHI/RSTA  
      COSLA=XSTA(1)/COSPHI/RSTA  
      COSTWOLA=COSLA**2-SINLA**2  
      SINTWOLA=2.*COSLA*SINLA  
      RMON=NORM8(XMON)
      RSUN=NORM8(XSUN)
      DRSUN=-3./4.*DHI*COSPHI**2*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*  
     1 SINTWOLA-2.*XSUN(1)*XSUN(2)*COSTWOLA)/RSUN**2
      DRMON=-3./4.*DHI*COSPHI**2*FAC2MON*((XMON(1)**2-XMON(2)**2)*  
     1 SINTWOLA-2.*XMON(1)*XMON(2)*COSTWOLA)/RMON**2
      DNSUN=3./2.*DLI*SINPHI*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*  
     1 SINTWOLA-2.*XSUN(1)*XSUN(2)*COSTWOLA)/RSUN**2
      DNMON=3./2.*DLI*SINPHI*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*  
     1 SINTWOLA-2.*XMON(1)*XMON(2)*COSTWOLA)/RMON**2
      DESUN=-3./2.*DLI*COSPHI*FAC2SUN*((XSUN(1)**2-XSUN(2)**2)*  
     1 COSTWOLA+2.*XSUN(1)*XSUN(2)*SINTWOLA)/RSUN**2
      DEMON=-3./2.*DLI*COSPHI*FAC2MON*((XMON(1)**2-XMON(2)**2)*  
     1 COSTWOLA+2.*XMON(1)*XMON(2)*SINTWOLA)/RMON**2
      DR=DRSUN+DRMON 
      DN=DNSUN+DNMON  
      DE=DESUN+DEMON 
      XCORSTA(1)=DR*COSLA*COSPHI-DE*SINLA-DN*SINPHI*COSLA  
      XCORSTA(2)=DR*SINLA*COSPHI+DE*COSLA-DN*SINPHI*SINLA  
      XCORSTA(3)=DR*SINPHI+DN*COSPHI  
      RETURN  
      END  
C
C*********************************************************************
      DOUBLE PRECISION function norm8(a)
      DOUBLE PRECISION a(3)
      norm8=dSQRT(a(1)*a(1)+a(2)*a(2)+a(3)*a(3))
      return
      end
C
C*********************************************************************
      subroutine zero_vec8(v)
      DOUBLE PRECISION v(3)
      v(1)=0.
      v(2)=0.
      v(3)=0.
      return
      end
C
      SUBROUTINE iau_DAT ( IY, IM, ID, FD, DELTAT, J )
*+
*  - - - - - - - -
*   i a u _ D A T
*  - - - - - - - -
*
*  For a given UTC date, calculate delta(AT) = TAI-UTC.
*
*     :------------------------------------------:
*     :                                          :
*     :                 IMPORTANT                :
*     :                                          :
*     :  A new version of this routine must be   :
*     :  produced whenever a new leap second is  :
*     :  announced.  There are five items to     :
*     :  change on each such occasion:           :
*     :                                          :
*     :  1) The parameter NDAT must be           :
*     :     increased by 1.                      :
*     :                                          :
*     :  2) A new line must be added to the set  :
*     :     of DATA statements that initialize   :
*     :     the arrays IDATE and DATS.           :
*     :                                          :
*     :  3) The parameter IYV must be set to     :
*     :     the current year.                    :
*     :                                          :
*     :  4) The "Latest leap second" comment     :
*     :     below must be set to the new leap    :
*     :     second date.                         :
*     :                                          :
*     :  5) The "This revision" comment, later,  :
*     :     must be set to the current date.     :
*     :                                          :
*     :  Change (3) must also be carried out     :
*     :  whenever the routine is re-issued,      :
*     :  even if no leap seconds have been       :
*     :  added.                                  :
*     :                                          :
*     :  Latest leap second:  2008 December 31   :
*     :                                          :
*     :__________________________________________:
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     IY       i     UTC:  year (Notes 1 and 2)
*     IM       i           month (Note 2)
*     ID       i           day (Notes 2 and 3)
*     FD       d           fraction of day (Note 4)
*
*  Returned:
*     DELTAT   d     TAI minus UTC, seconds
*     J        i     status (Note 5):
*                       1 = dubious year (Note 1)
*                       0 = OK
*                      -1 = bad year
*                      -2 = bad month
*                      -3 = bad day (Note 3)
*                      -4 = bad fraction (Note 4)
*
*  Notes:
*
*  1) UTC began at 1960 January 1.0 (JD 2436934.5) and it is improper
*     to call the routine with an earlier date.  If this is attempted,
*     zero is returned together with a warning status.
*
*     Because leap seconds cannot, in principle, be predicted in
*     advance, a reliable check for dates beyond the valid range is
*     impossible.  To guard against gross errors, a year five or more
*     after the release year of the present routine (see parameter IYV)
*     is considered dubious.  In this case a warning status is returned
*     but the result is computed in the normal way.
*
*     For both too-early and too-late years, the warning status is J=+1.
*     This is distinct from the error status J=-1, which signifies a
*     year so early that JD could not be computed.
*
*  2) If the specified date is for a day which ends with a leap second,
*     the UTC-TAI value returned is for the period leading up to the
*     leap second.  If the date is for a day which begins as a leap
*     second ends, the UTC-TAI returned is for the period following the
*     leap second.
*
*  3) The day number must be in the normal calendar range, for example
*     1 through 30 for April.  The "almanac" convention of allowing
*     such dates as January 0 and December 32 is not supported in this
*     routine, in order to avoid confusion near leap seconds.
*
*  4) The fraction of day is used only for dates before the introduction
*     of leap seconds, the first of which occurred at the end of 1971.
*     It is tested for validity (zero to less than 1 is the valid range)
*     even if not used;  if invalid, zero is used and status J=-4 is
*     returned.  For many applications, setting FD to zero is
*     acceptable;  the resulting error is always less than 3 ms (and
*     occurs only pre-1972).
*
*  5) The status value returned in the case where there are multiple
*     errors refers to the first error detected.  For example, if the
*     month and day are 13 and 32 respectively, J=-2 (bad month) will be
*     returned.
*
*  6) In cases where a valid result is not available, zero is returned.
*
*  References:
*
*  1) For dates from 1961 January 1 onwards, the expressions from the
*     file ftp://maia.usno.navy.mil/ser7/tai-utc.dat are used.
*
*  2) The 5ms timestep at 1961 January 1 is taken from 2.58.1 (p87) of
*     the 1992 Explanatory Supplement.
*
*  Called:
*     iau_CAL2JD   Gregorian calendar to Julian Day number
*
*  This revision:  2008 July 5
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION FD, DELTAT
      INTEGER J

*  Release year for this version of iau_DAT
      INTEGER IYV
      PARAMETER ( IYV = 2009 )

*  Number of Delta(AT) changes (increase by 1 for each new leap second)
      INTEGER NDAT
      PARAMETER ( NDAT = 39 )

*  Number of Delta(AT) expressions before leap seconds were introduced
      INTEGER NERA1
      PARAMETER ( NERA1 = 14 )

*  Dates (year, month) on which new Delta(AT) came into force
      INTEGER IDATE(2,NDAT)

*  New Delta(AT) which came into force on the given dates
      DOUBLE PRECISION DATS(NDAT)

*  Reference dates (MJD) and drift rates (s/day), pre leap seconds
      DOUBLE PRECISION DRIFT(2,NERA1)

*  Miscellaneous local variables
      LOGICAL MORE
      INTEGER JS, I, M, IS
      DOUBLE PRECISION DAT, DJM0, DJM

*  Dates and Delta(AT)s
      DATA (IDATE(I, 1),I=1,2),DATS(1)  / 1960,  1,  1.4178180D0 /
      DATA (IDATE(I, 2),I=1,2),DATS(2)  / 1961,  1,  1.4228180D0 /
      DATA (IDATE(I, 3),I=1,2),DATS(3)  / 1961,  8,  1.3728180D0 /
      DATA (IDATE(I, 4),I=1,2),DATS(4)  / 1962,  1,  1.8458580D0 /
      DATA (IDATE(I, 5),I=1,2),DATS(5)  / 1963, 11,  1.9458580D0 /
      DATA (IDATE(I, 6),I=1,2),DATS(6)  / 1964,  1,  3.2401300D0 /
      DATA (IDATE(I, 7),I=1,2),DATS(7)  / 1964,  4,  3.3401300D0 /
      DATA (IDATE(I, 8),I=1,2),DATS(8)  / 1964,  9,  3.4401300D0 /
      DATA (IDATE(I, 9),I=1,2),DATS(9)  / 1965,  1,  3.5401300D0 /
      DATA (IDATE(I,10),I=1,2),DATS(10) / 1965,  3,  3.6401300D0 /
      DATA (IDATE(I,11),I=1,2),DATS(11) / 1965,  7,  3.7401300D0 /
      DATA (IDATE(I,12),I=1,2),DATS(12) / 1965,  9,  3.8401300D0 /
      DATA (IDATE(I,13),I=1,2),DATS(13) / 1966,  1,  4.3131700D0 /
      DATA (IDATE(I,14),I=1,2),DATS(14) / 1968,  2,  4.2131700D0 /
      DATA (IDATE(I,15),I=1,2),DATS(15) / 1972,  1, 10D0 /
      DATA (IDATE(I,16),I=1,2),DATS(16) / 1972,  7, 11D0 /
      DATA (IDATE(I,17),I=1,2),DATS(17) / 1973,  1, 12D0 /
      DATA (IDATE(I,18),I=1,2),DATS(18) / 1974,  1, 13D0 /
      DATA (IDATE(I,19),I=1,2),DATS(19) / 1975,  1, 14D0 /
      DATA (IDATE(I,20),I=1,2),DATS(20) / 1976,  1, 15D0 /
      DATA (IDATE(I,21),I=1,2),DATS(21) / 1977,  1, 16D0 /
      DATA (IDATE(I,22),I=1,2),DATS(22) / 1978,  1, 17D0 /
      DATA (IDATE(I,23),I=1,2),DATS(23) / 1979,  1, 18D0 /
      DATA (IDATE(I,24),I=1,2),DATS(24) / 1980,  1, 19D0 /
      DATA (IDATE(I,25),I=1,2),DATS(25) / 1981,  7, 20D0 /
      DATA (IDATE(I,26),I=1,2),DATS(26) / 1982,  7, 21D0 /
      DATA (IDATE(I,27),I=1,2),DATS(27) / 1983,  7, 22D0 /
      DATA (IDATE(I,28),I=1,2),DATS(28) / 1985,  7, 23D0 /
      DATA (IDATE(I,29),I=1,2),DATS(29) / 1988,  1, 24D0 /
      DATA (IDATE(I,30),I=1,2),DATS(30) / 1990,  1, 25D0 /
      DATA (IDATE(I,31),I=1,2),DATS(31) / 1991,  1, 26D0 /
      DATA (IDATE(I,32),I=1,2),DATS(32) / 1992,  7, 27D0 /
      DATA (IDATE(I,33),I=1,2),DATS(33) / 1993,  7, 28D0 /
      DATA (IDATE(I,34),I=1,2),DATS(34) / 1994,  7, 29D0 /
      DATA (IDATE(I,35),I=1,2),DATS(35) / 1996,  1, 30D0 /
      DATA (IDATE(I,36),I=1,2),DATS(36) / 1997,  7, 31D0 /
      DATA (IDATE(I,37),I=1,2),DATS(37) / 1999,  1, 32D0 /
      DATA (IDATE(I,38),I=1,2),DATS(38) / 2006,  1, 33D0 /
      DATA (IDATE(I,39),I=1,2),DATS(39) / 2009,  1, 34D0 /

*  Reference dates and drift rates
      DATA (DRIFT(I, 1),I=1,2) / 37300D0, 0.001296D0 /
      DATA (DRIFT(I, 2),I=1,2) / 37300D0, 0.001296D0 /
      DATA (DRIFT(I, 3),I=1,2) / 37300D0, 0.001296D0 /
      DATA (DRIFT(I, 4),I=1,2) / 37665D0, 0.0011232D0 /
      DATA (DRIFT(I, 5),I=1,2) / 37665D0, 0.0011232D0 /
      DATA (DRIFT(I, 6),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I, 7),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I, 8),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I, 9),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,10),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,11),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,12),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,13),I=1,2) / 39126D0, 0.002592D0 /
      DATA (DRIFT(I,14),I=1,2) / 39126D0, 0.002592D0 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Initialize the result to zero and the status to OK.
      DAT = 0D0
      JS = 0

*  If invalid fraction of a day, set error status and give up.
      IF ( FD.LT.0D0 .OR. FD.GE.1D0 ) THEN
         JS = -4
         GO TO 9000
      END IF

*  Convert the date into an MJD.
      CALL iau_CAL2JD ( IY, IM, ID, DJM0, DJM, JS )

*  If invalid year, month, or day, give up.
      IF ( JS .LT. 0 ) GO TO 9000

*  If pre-UTC year, set warning status and give up.
      IF ( IY .LT. IDATE(1,1) ) THEN
         JS = 1
         GO TO 9000
      END IF

*  If suspiciously late year, set warning status but proceed.
      IF ( IY .GT. IYV+5 ) JS = 1

*  Combine year and month.
      M = 12*IY+IM

*  Prepare to search the tables.
      MORE = .TRUE.

*  Find the most recent table entry.
      DO 1 I=NDAT,1,-1
         IF ( MORE ) THEN
            IS = I
            MORE = M .LT. ( 12*IDATE(1,I) + IDATE(2,I) )
         END IF
 1    CONTINUE

*  Get the Delta(AT).
      DAT = DATS(IS)

*  If pre-1972, adjust for drift.
      IF ( IS .LE. NERA1 ) DAT = DAT +
     :                          ( DJM + FD - DRIFT(1,IS) ) * DRIFT(2,IS)

*  Return the Delta(AT) value and the status.
 9000 CONTINUE
      DELTAT = DAT
      J = JS

*  Finished.

*+-----------------------------------------------------------------------
*
*  Copyright (C) 2008
*  Standards Of Fundamental Astronomy Review Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Review Board ("the Board").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and 
*     restrictions listed below.
*
*  3. You (the user) may copy and adapt the SOFA software and its 
*     algorithms for your own purposes and you may copy and distribute
*     a resulting "derived work" to others on a world-wide, royalty-free 
*     basis, provided that the derived work complies with the following
*     requirements: 
*
*     a) Your work shall be marked or carry a statement that it (i) uses
*        routines and computations derived by you from software provided 
*        by SOFA under license to you; and (ii) does not contain
*        software provided by SOFA or software that has been distributed
*        by or endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon and/or differs from the
*        original SOFA software.
*
*     c) The name(s) of all routine(s) that you distribute shall differ
*        from the SOFA names, even when the SOFA content has not been
*        otherwise changed.
*
*     d) The routine-naming prefix "iau" shall not be used.
*
*     e) The origin of the SOFA components of your derived work must not
*        be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     f) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have granted 
*        a further right to modify the source code of your derived work.
*
*  4. In any published work or commercial products which includes
*     results achieved by using the SOFA software, you shall acknowledge
*     that the SOFA software was used in obtaining those results.
*
*  5. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  6. The SOFA software is provided "as is" and the Board makes no 
*     warranty as to its use or performance.   The Board does not and 
*     cannot warrant the performance or results which the user may obtain 
*     by using the SOFA software.  The Board makes no warranties, express 
*     or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  7. The provision of any version of the SOFA software under the terms 
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*     Internet email: sofa@rl.ac.uk
*     Postal address: IAU SOFA Center
*                     Rutherford Appleton Laboratory
*                     Chilton, Didcot, Oxon OX11 0QX
*                     United Kingdom
*
*-----------------------------------------------------------------------

      END
C
      SUBROUTINE iau_CAL2JD ( IY, IM, ID, DJM0, DJM, J )
*+
*  - - - - - - - - - - -
*   i a u _ C A L 2 J D
*  - - - - - - - - - - -
*
*  Gregorian Calendar to Julian Date.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     IY,IM,ID    i     year, month, day in Gregorian calendar (Note 1)
*
*  Returned:
*     DJM0        d     MJD zero-point: always 2400000.5
*     DJM         d     Modified Julian Date for 0 hrs
*     J           i     status:
*                           0 = OK
*                          -1 = bad year   (Note 3: JD not computed)
*                          -2 = bad month  (JD not computed)
*                          -3 = bad day    (JD computed)
*
*  Notes:
*
*  1) The algorithm used is valid from -4800 March 1, but this
*     implementation rejects dates before -4799 January 1.
*
*  2) The Julian Date is returned in two pieces, in the usual SOFA
*     manner, which is designed to preserve time resolution.  The
*     Julian Date is available as a single number by adding DJM0 and
*     DJM.
*
*  3) In early eras the conversion is from the "Proleptic Gregorian
*     Calendar";  no account is taken of the date(s) of adoption of
*     the Gregorian Calendar, nor is the AD/BC numbering convention
*     observed.
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P. Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 12.92 (p604).
*
*  This revision:  2001 September 16
*
*  Copyright (C) 2007 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION DJM0, DJM
      INTEGER J, MY, IYPMY

*  Earliest year allowed (4800BC)
      INTEGER IYMIN
      PARAMETER ( IYMIN = -4799 )

*  Month lengths in days
      INTEGER MTAB(12)
      DATA MTAB / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Preset status.
      J = 0

*  Validate year.
      IF ( IY.LT.IYMIN ) THEN
         J = -1
      ELSE

*     Validate month.
         IF ( IM.GE.1 .AND. IM.LE.12 ) THEN

*        Allow for leap year.
            IF ( MOD(IY,4) .EQ. 0 ) THEN
               MTAB(2) = 29
            ELSE
               MTAB(2) = 28
            END IF
            IF ( MOD(IY,100).EQ.0 .AND. MOD(IY,400).NE.0 ) MTAB(2) = 28

*        Validate day.
            IF ( ID.LT.1 .OR. ID.GT.MTAB(IM) ) J = -3

*        Result.
            MY = ( IM - 14 ) / 12
            IYPMY = IY + MY
            DJM0 = 2400000.5D0
            DJM = DBLE( ( 1461 * ( IYPMY + 4800 ) ) / 4
     :                + (  367 * ( IM-2 - 12*MY ) ) / 12
     :                - (    3 * ( ( IYPMY + 4900 ) / 100 ) ) / 4
     :                + ID - 2432076)

*        Bad month
         ELSE
            J = -2
         END IF
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2007
*  Standards Of Fundamental Astronomy Review Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Review Board ("the Board").
*
*  2. The Software is made available free of charge for use by:
*
*     a) private individuals for non-profit research; and
*
*     b) non-profit educational, academic and research institutions.
*
*  3. Commercial use of the Software is specifically excluded from the
*     terms and conditions of this license.  Commercial use of the
*     Software is subject to the prior written agreement of the Board on
*     terms to be agreed.
*
*  4. The provision of any version of the Software under the terms and
*     conditions specified herein does not imply that future versions
*     will also be made available under the same terms and conditions.
*
*  5. The user may modify the Software for his/her own purposes.  The
*     user may distribute the modified software provided that the Board
*     is informed and that a copy of the modified software is made
*     available to the Board on request.  All modifications made by the
*     user shall be clearly identified to show how the modified software
*     differs from the original Software, and the name(s) of the
*     affected routine(s) shall be changed.  The original SOFA Software
*     License text must be present.
*
*  6. In any published work produced by the user and which includes
*     results achieved by using the Software, the user shall acknowledge
*     that the Software was used in producing the information contained
*     in such publication.
*
*  7. The user may incorporate or embed the Software into other software
*     products which he/she may then give away free of charge but not
*     sell provided the user makes due acknowledgement of the use which
*     he/she has made of the Software in creating such software
*     products.  Any redistribution of the Software in this way shall be
*     made under the same terms and conditions under which the user
*     received it from the SOFA Center.
*
*  8. The user shall not cause the Software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  9. The Software is provided to the user "as is" and the Board makes
*     no warranty as to its use or performance.   The Board does not and
*     cannot warrant the performance or results which the user may
*     obtain by using the Software.  The Board makes no warranties,
*     express or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*     Internet email: sofa@rl.ac.uk
*     Postal address: IAU SOFA Center
*                     Rutherford Appleton Laboratory
*                     Chilton, Didcot, Oxon OX11 0QX
*                     United Kingdom
*
*
*-----------------------------------------------------------------------

      END

