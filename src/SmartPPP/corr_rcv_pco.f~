c
c subroutine corr_rcv_pco.f
c
      subroutine corr_rcv_pco(Time,xrcv)
c
c=======================================================================
c     ****f* SmartPPP/corr_rcv_pco.f
c
c   FUNCTION   
c   
c     compute the receiver related corrections
c
c   INPUTS
c
c     time           real*8        second past J2000.0 in GPS
c     xrcv           real*8        receiver parameter
c
c   OUTPUT
c
c     xrcv           real*8        receiver parameter
c
c   NOTES
c
c     1) only correct the L1 mean phase center offset!!!!
c     2) the difference of L1 and L2 PCO and the PCV are corrected into 
c        the observables
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/07/28                   programmed
c     2010/01/22                   modified for geodetic receiver
c
c     ***
c
C     $Id: corr_rcv_pco.f,v 1.0 2009/07/28 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
      include      '../../include/igs.h'
      include      '../../include/atx.h'
c
c     input/output variable
c
c     input
      real*8        Time
      real*8        xrcv(3)
c
c     local variables
c
      integer       i,j,k
      real*8        x,y,z
      real*8        SITE_XYZ(3)
      real*8        SITE_blh(3)
      real*8        rot_mat(3,3)
      real*8        B, L, H  ! unit[rads] 
      real*8        dis
c
      real*8        ANT_PCO_XYZ(3)
c
      real*8        Q0, Q1, Q2, Q3
      real*8        ECI2SAT (3,3)
      real*8        ECI2ECF (3,3)
      real*8        SAT2ECI (3,3)
      real*8        SAT2ECF (3,3)
c
      real*8        xQ(MAX_REC_ATT)
      real*8        yQ(MAX_REC_ATT)
c
c     common
c     ******
c
      integer       NREC_ATT
      real*8        ATT(MAX_REC_ATT, 6)
c
      common /att/  att,NREC_ATT
c
      real*8        ANT_PCO  (MAX_FREQ,3)
      real*8        dPCO     (3)
      real*8        xZEN     (MAX_REC_ZEN)
      real*8        yANT_PCV1(MAX_FREQ,MAX_REC_ZEN)
      real*8        yANT_PCV2(MAX_FREQ,MAX_REC_AZI,MAX_REC_ZEN)
      integer       NREC_ZEN
c
      common /atx/  ANT_PCO,dPCO,xZEN,yANT_PCV1,yANT_PCV2,NREC_ZEN
c
c     initial
c
      do i=1,3
      do k=1,3
         ECI2SAT (i,k) = 0.0d0
         ECI2ECF (i,k) = 0.0d0 
         SAT2ECI (i,k) = 0.0d0
         SAT2ECF (i,k) = 0.0d0
      enddo
      enddo
c
c     receiver's phase center offset and variations
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
         do i=1, NREC_ATT
            xQ(i) = ATT(i,1)
            yQ(i) = ATT(i,2)
         enddo
c
         call lagrange(xQ,yQ,NREC_ATT,Time,q0)
c
         do i=1, NREC_ATT
            xQ(i) = ATT(i,1)
            yQ(i) = ATT(i,3)
         enddo
c
         call lagrange(xQ,yQ,NREC_ATT,Time,q1)
c
         do i=1, NREC_ATT
            xQ(i) = ATT(i,1)
            yQ(i) = ATT(i,4)
         enddo
c
         call lagrange(xQ,yQ,NREC_ATT,Time,q2)
c
         do i=1, NREC_ATT
            xQ(i) = ATT(i,1)
            yQ(i) = ATT(i,5)
         enddo
c
         call lagrange(xQ,yQ,NREC_ATT,Time,q3)
c
         ECI2SAT(1,1) =   q0**2+q1**2-q2**2-q3**2
         ECI2SAT(1,2) =2*(q1*q2+q0*q3)
         ECI2SAT(1,3) =2*(q1*q3-q0*q2)
c
         ECI2SAT(2,1) =2*(q1*q2-q0*q3)
         ECI2SAT(2,2) =   q0**2-q1**2+q2**2-q3**2
         ECI2SAT(2,3) =2*(q2*q3+q0*q1)
c
         ECI2SAT(3,1) =2*(q1*q3+q0*q2)
         ECI2SAT(3,2) =2*(q2*q3-q0*q1)
         ECI2SAT(3,3) =   q0**2-q1**2-q2**2+q3**2
c
c        transform matrix from inertal to satellite fixed coordinate 
c        to        matrix form satellite fixed to inertal coordinate
c   
         call mtxtrs(ECI2SAT, SAT2ECI, 3, 3)
c
c        calculate the matrix from inertial to earth fixed coordinate
c
         call get_C2T_CIO(time, ECI2ECF)
c
c        calculate the matrix from satellite fixed to earth fixed coordinate
c
         call mtxmul(ECI2ECF, SAT2ECI, SAT2ECF, 3, 3, 3)
c
         do i=1,3
            ANT_PCO_XYZ(i) = 0.0d0
            do k=1,3
               ANT_PCO_XYZ(i)=ANT_PCO_XYZ(i)+SAT2ECF(i,k)*ANT_PCO(1,k)
            enddo
         enddo
c
         do i=1,3
            xrcv(i) = xrcv(i) + ANT_PCO_XYZ(i)
         enddo
c
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         SITE_XYZ(1) = xrcv(1)
         SITE_XYZ(2) = xrcv(2)
         SITE_XYZ(3) = xrcv(3)
c
         call xyz2geod(SITE_XYZ,SITE_BLH)
c
         B = SITE_BLH(1)
         L = SITE_BLH(2)
         H = SITE_BLH(3)
c
c        compose the transformation matrix from NEU2XYZ
c   
c        References: 
c        HUANG JINSONG, GPS Surveying and data processing [Chinese Edition]
c
c        compose X componet    
         rot_mat(1,1) = -dsin(B) * dcos(L)
         rot_mat(1,2) = -dsin(L)          
         rot_mat(1,3) =  dcos(B) * dcos(L) 
*
*        compose Y component
         rot_mat(2,1) = -dsin(B) * dsin(L) 
         rot_mat(2,2) =  dcos(L)
         rot_mat(2,3) =  dcos(B) * dsin(L) 
*
*        compose Z component
         rot_mat(3,1) =  dcos(B)
         rot_mat(3,2) =  0.d0
         rot_mat(3,3) =  dsin(B)
c
c        now transforming the antenna offset from NEU to the XYZ coordinate
c
c        OFFSET in X direction 
         ANT_PCO_XYZ(1) = rot_mat(1,1)*ANT_PCO(1,1) 
     &                  + rot_mat(1,2)*ANT_PCO(1,2)
     &                  + rot_mat(1,3)*ANT_PCO(1,3)
c
c        OFFSET in Y direction 
         ANT_PCO_XYZ(2) = rot_mat(2,1)*ANT_PCO(1,1) 
     &                  + rot_mat(2,2)*ANT_PCO(1,2)
     &                  + rot_mat(2,3)*ANT_PCO(1,3)
c
c        OFFSET in Z direction 
         ANT_PCO_XYZ(3) = rot_mat(3,1)*ANT_PCO(1,1) 
     &                  + rot_mat(3,2)*ANT_PCO(1,2)
     &                  + rot_mat(3,3)*ANT_PCO(1,3)
c
         write(*,*) 'ANT_PCO'
         write(*,*)  ANT_PCO(1,1),   ANT_PCO(1,2),   ANT_PCO(1,3)
         write(*,*)  ANT_PCO_XYZ(1), ANT_PCO_XYZ(2), ANT_PCO_XYZ(3)
c
c        ADD ANT_OFF into receiver's position
         xrcv(1) = xrcv(1) + ANT_PCO_XYZ(1)
         xrcv(2) = xrcv(2) + ANT_PCO_XYZ(2)
         xrcv(3) = xrcv(3) + ANT_PCO_XYZ(3)
c
      elseif(MARKER_TYPE.EQ.'NON_GEODETIC')then
c
         write(*,*) 'SmartPPP/corr_rcv_pco' 
         write(*,*) 'Please Make modification'
         stop
c
      endif    
c
      return
c
      end
