c
c   subroutine linear_combine
c
      subroutine linear_combine(nrec,L1,L2,LA,C1,P1,P2,
     +                          N2,Nw,L3,P3,L4,P4,Lw,Pw,dL1,MC1,MP1,MP2)
c=======================================================================
c     ****f* qualicontr/extrobs
c
c   FUNCTION   
c   
c     linear combinations 
c
c   INPUTS
c
c     nrec        (I)         record number
c     L1          (R)         phase observable 
c     L2          (R)         phase observable 
c     LA          (R)         phase observalbe on C/A channael
c     C1          (R)         code  observable
c     P1          (R)         code  observable 
c     P2          (R)         code  observable
c
c   OUTPUT
c
c     N2          (R)         N2    
c     Nw          (R)         MW    widelane        combination
c     L3          (R)         phase Ionoshpere-free combination
c     P3          (R)         code  Ionoshpere-free combination
c     L4          (R)         phase Geometry-free   combination
c     P4          (R)         code  Geometry-free   combination
c
c     dL1         (R)         dL1 = L1-LA
c     MC1         (R)         multiply effects on C1
c     MP1         (R)         multiply effects on P1
c     MP2         (R)         multiply effects on P2
c
c   COPYRIGHT
c
c     Copyright(c) 2006-      Shoujian Zhang,
c                             School of Geodesy and Geomatics,
c                             Wuhan University.
c     ***
c
C     $Id: extrobs.f,v 1.0 2009/07/11 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input/output variables
c
c     input
c
      integer       nrec
c
      real*8        L1 (MAX_OBS_REC)
      real*8        L2 (MAX_OBS_REC)
      real*8        LA (MAX_OBS_REC)
      real*8        C1 (MAX_OBS_REC)
      real*8        P1 (MAX_OBS_REC)
      real*8        P2 (MAX_OBS_REC)
c     output
      real*8        Nw (MAX_OBS_REC)
      real*8        N2 (MAX_OBS_REC)
      real*8        L3 (MAX_OBS_REC)
      real*8        P3 (MAX_OBS_REC)
      real*8        L4 (MAX_OBS_REC)
      real*8        P4 (MAX_OBS_REC)
      real*8        Lw (MAX_OBS_REC)
      real*8        Pw (MAX_OBS_REC)
c
      real*8        dL1(MAX_OBS_REC)
      real*8        MC1(MAX_OBS_REC)
      real*8        MP1(MAX_OBS_REC)
      real*8        MP2(MAX_OBS_REC)
c
c     local variables
c
      integer   i, j, k
      integer   irec
c
      real*8    tL1, tL2, tLA, tC1, tP1, tP2, tLw, tPw
      real*8    tNw, tL4, tP4, tL3, tP3
c
c     initialization
c
      do irec=1, MAX_OBS_REC
c
        Nw(irec)  = 0.0d0
        N2(irec)  = 0.0d0
        L3(irec)  = 0.0d0
        P3(irec)  = 0.0d0
        L4(irec)  = 0.0d0
        P4(irec)  = 0.0d0
        Lw(irec)  = 0.0d0
        Pw(irec)  = 0.0d0
c
        dL1(irec) = 0.0d0 
        MC1(irec) = 0.0d0
        MP1(irec) = 0.0d0
        MP2(irec) = 0.0d0
c
      enddo
c
c     do i=1,nrec
c        write(*,*) L1(irec),L2(irec),P1(irec),P2(irec)
c     enddo
c
c     form combinations
c
      do irec=1, nrec
c
         tL1   = L1(irec)*lam_L1_GPS
         tL2   = L2(irec)*lam_L2_GPS
         tLA   = LA(irec)*lam_L1_GPS
         tC1   = C1(irec)
         tP1   = P1(irec)
         tP2   = P2(irec)
c
         if(tP1.eq.0.0d0)then
c
         tP1   = tC1  
c
         endif
c
         Lw(irec) =(f1_GPS*tL1-f2_GPS*tL2)/(f1_GPS-f2_GPS)
         Pw(irec) =(f1_GPS*tP1+f2_GPS*tP2)/(f1_GPS+f2_GPS)
c
         tLw      = Lw(irec)
         tPw      = Pw(irec)
c
         Nw (irec)=(tLw-tPw)/(lam_Nw_GPS)
         L3 (irec)=(f1_GPS**2*tL1-f2_GPS**2*tL2)/(f1_GPS**2-f2_GPS**2)
         P3 (irec)=(f1_GPS**2*tP1-f2_GPS**2*tP2)/(f1_GPS**2-f2_GPS**2)
         L4 (irec)= tL1 - tL2
         P4 (irec)=-tP1 + tP2
c
         tNw      = Nw(irec)
         tL4      = L4(irec)
c
         N2 (irec)=(lam_L1_GPS*tNw - tL4)/(lam_L2_GPS - lam_L1_GPS)
c
         dL1(irec)= tL1 - tLA
c
         MC1(irec)= tC1 
     &            -(  f1_GPS**2 +  f2_GPS**2)/(f1_GPS**2-f2_GPS**2)*tL1
     &            +(             2*f2_GPS**2)/(f1_GPS**2-f2_GPS**2)*tL2
c
         MP1(irec)= tP1 
     &            -(  f1_GPS**2 +  f2_GPS**2)/(f1_GPS**2-f2_GPS**2)*tL1
     &            +(             2*f2_GPS**2)/(f1_GPS**2-f2_GPS**2)*tL2
c
         MP2(irec)= tP2
     &            -(2*f1_GPS**2             )/(f1_GPS**2-f2_GPS**2)*tL1
     &            +(  f1_GPS**2 +  f2_GPS**2)/(f1_GPS**2-f2_GPS**2)*tL2
c
      enddo
c
      return
c       
      end
