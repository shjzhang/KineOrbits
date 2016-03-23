*
*  subroutine xv_OMC
*      
      subroutine xv_OMC(time0,xrcv0,xtrs0,vtrs0,ctrs0,L30,
     &                  time1,xrcv1,xtrs1,vtrs1,ctrs1,L31,
     &                  xv,cv,iPRN,iobs,L)
c
c=======================================================================
c     ****f* SmartPPP/xv_OMC.f
c
c   FUNCTION   
c   
*     xv_OMC of the observation equation
c
c   INPUTS
c
c     xrcv        real*8           coordinates of receiver
c     xtrs        real*8           coordinates of GPS
c     vtrs        real*8           velocity of GPS 
c     ctrs        real*8           clock error of GPS 
c
c   OUTPUT
c
c     L           real*8           Observed Minus Computed value
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/03                   build
c
c     ***
c
C     $Id: xv_OMC.f.f,v 1.0 2009/07/28 $
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
      real*8        pi
      parameter (   pi = 3.1415926535897932D0 )
c
c     input/output
c
      real*8        time0
      real*8        xrcv0(3),xtrs0(3),vtrs0(3),ctrs0
      real*8        L30
      real*8        time1
      real*8        xrcv1(3),xtrs1(3),vtrs1(3),ctrs1
      real*8        L31
c
      real*8        xv(3),cv
      real*8        L(MAX_SAT_NUM,1)
c
      integer       iPRN
      integer       iobs
c
c     local
c
      real*8        dis0,dis1
      real*8        dis_observed0,dis_observed1
      real*8        dis_computed0,dis_computed1
      real*8        dis_dTs0, dis_dTs1
      real*8        dis_rel0, dis_rel1
c
      real*8        drou_dx1,drou_dy1,drou_dz1,drou_dt1
c
c     loop
c
      integer       i,j,k
c
c     common
c
      real*8        rcv_pco  (MAX_FREQ,3)
      real*8        dpco     (3)
      real*8        xZEN     (MAX_REC_ZEN)
      real*8        yant_pcv1(MAX_FREQ,MAX_REC_ZEN)
      real*8        yant_pcv2(MAX_FREQ,MAX_REC_AZI,MAX_REC_ZEN)
      integer       nrec_zen
c
      real*8        trs_pco  (MAX_PRN,3)
c*
      common /atx/  rcv_pco,dpco,xzen,yant_pcv1,yant_pcv2,nrec_zen,
     &              trs_pco
c
c     initial
c
      dis0          = 0.0d0
      dis_observed0 = 0.0d0 
      dis_computed0 = 0.0d0 
      dis_dTs0      = 0.0d0  
      dis_rel0      = 0.0d0 
c
      dis1          = 0.0d0
      dis_observed1 = 0.0d0 
      dis_computed1 = 0.0d0 
      dis_dTs1      = 0.0d0  
      dis_rel1      = 0.0d0 
c
c     distance from receiver to transmitter
c
      call distance(xrcv0,xtrs0,dis0)
c
      call distance(xrcv1,xtrs1,dis1)
c
      drou_dx1 = -(xtrs1(1)-xrcv1(1))/dis1
      drou_dy1 = -(xtrs1(2)-xrcv1(2))/dis1
      drou_dz1 = -(xtrs1(3)-xrcv1(3))/dis1
      drou_dt1 = 1
c
c     Observed Minus Computed
c     =======================
c
c     SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c        distance of GPS transimtter clock error
c
         dis_dTs0  = ctrs0*c_light
         dis_dTs1  = ctrs1*c_light
c
c        distance of relativity
c
         dis_rel0  = -2.0d0*( xtrs0(1)*vtrs0(1)+
     &                        xtrs0(2)*vtrs0(2)+
     &                        xtrs0(3)*vtrs0(3)  )/c_light
c
         dis_rel1  = -2.0d0*( xtrs1(1)*vtrs1(1)+
     &                        xtrs1(2)*vtrs1(2)+
     &                        xtrs1(3)*vtrs1(3)  )/c_light
c
c        observed         
c         
         dis_Observed0 = L30  
         dis_Observed1 = L31  
c
c        computed
c
         dis_Computed0 = dis0 
     &                 - dis_dTs0      ! GPS clock error
     &                 - dis_rel0      ! GPS relativity
c
         dis_Computed1 = dis1 
     &                 - dis_dTs1      ! GPS clock error
     &                 - dis_rel1      ! GPS relativity
     &                 + cv
c
c        observed minus computed
c        =======================
c
         L(iobs,1)     = ( dis_Observed1 - dis_Observed0 )- 
     &                   ( dis_Computed1 - dis_Computed0 )
c
c     GEODETIC
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         write(*,*) 'SmartPPP/xv_OMC'
         write(*,*) 'Please make modification for tropospheric deriv'
c
      else
c
         write(*,*) 'SmartPPP/xv_OMC'
         write(*,*) 'Please make modification for tropospheric deriv'
c
      endif
c
      return
c
      end
