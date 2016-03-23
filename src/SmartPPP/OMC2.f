*
*  subroutine OMC2
*      
      subroutine OMC2(time,xrcv,crcv,xtrs,vtrs,ctrs,zwd,elv,iPRN,iflag,
     &                irec,L3,P3,N3,L)
c
c=======================================================================
c     ****f* SmartPPP/OMC2.f
c
c   FUNCTION   
c   
*     OMC2 of the observation equation
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
C     $Id: OMC2.f.f,v 1.0 2009/07/28 $
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
      real*8        sec2mjd
c
c     input/output
c
      integer       irec
      integer       iPRN
      integer       iflag
c
      real*8        xrcv(3),crcv,xtrs(3),vtrs(3),ctrs
      real*8        zwd
      real*8        elv
      real*8        time
      real*8        L3, P3, N3
      real*8        L(MAX_SAT_NUM,1)
c
c     local
c
      real*8        dis
      real*8        dis_observed_P3
      real*8        dis_computed_P3
      real*8        dis_observed_L3
      real*8        dis_computed_L3
      real*8        dis_dTs
      real*8        dis_dPCO
      real*8        dis_PCV
      real*8        dis_rel
      real*8        dis_dry_trop
      real*8        dis_wet_trop
      real*8        dis_windup
c
      real*8        drou_dx,drou_dy,drou_dz
c
c     loop
c
      integer       i,j,k
      integer       iobs
c
      character*10  MF  ! Mapping Fucntion Type
      real*8        MJD ! Modified Julian Date
c
      real*8        zd  ! zenith distance in radian
      real*8        site_geod(3)
      real*8        slat, slon, shgt
      real*8        gmfh, gmfw
      real*8        zhd
c
      logical       debug
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
      dis             = 0.0d0
      dis_observed_L3 = 0.0d0 
      dis_computed_L3 = 0.0d0 
      dis_observed_P3 = 0.0d0 
      dis_computed_P3 = 0.0d0 
      dis_dTs         = 0.0d0  
      dis_dPCO        = 0.0d0 
      dis_PCV         = 0.0d0 
      dis_rel         = 0.0d0 
      dis_dry_trop    = 0.0d0
      dis_windup      = 0.0d0  
c
      debug           = .true.
      if(debug)then
         write(*,*) 'coeff2'
      endif
c
c     distance from receiver to transmitter
c
      call distance(xrcv,xtrs,dis)
c
c     partial derivative with respect to the receiver
c
      drou_dx    = -(xtrs(1)-xrcv(1))/dis
      drou_dy    = -(xtrs(2)-xrcv(2))/dis
      drou_dz    = -(xtrs(3)-xrcv(3))/dis
c
c     distance of GPS transimtter clock error
c
      dis_dTs  = ctrs*c_light
c
c     distance of relativity
c
      dis_rel  = -2.0d0*( xtrs(1)*vtrs(1)+
     &                    xtrs(2)*vtrs(2)+
     &                    xtrs(3)*vtrs(3)  )/c_light
c
c     distance of difference of L1 and L2 phase center
c     transform to f2**2*dpco/(f1**2-f2**2)
c
      dis_dPCO = (f2_GPS**2/(f1_GPS**2-f2_GPS**2))*
     &           (drou_dx*dpco(1) + drou_dy*dpco(2)+drou_dz*dpco(3))
c
c     distance of difference of phase variations:
c     notes: ignore azimuth-dependent variations
c
      call lagrange(xZEN,yant_pcv1,nrec_zen,elv,dis_PCV)   
c
c     distance of phase wind-up
c
c
      call compute_windup(time,iPRN,iflag,xrcv,xtrs,dis_windup)
c
      write(*,*) 'old windup', dis_windup
c
      call phase_windup(time,iPRN,iflag,xrcv,xtrs,dis_windup)
c
      write(*,*) 'new windup', dis_windup
c
c     distance of tropospheric hydrostatic delay
c
c>1   Mapping Function
c     default value
      MF = 'gmf'
c     Modified Julian Date
      MJD = sec2mjd(time)
c     zenith distance in radian
      zd = pi/2 - elv
c     geodetic coordinate
      call xyz2geod(xrcv,site_geod)
      slat = site_geod(1)
      slon = site_geod(2)
      shgt = site_geod(3)
c     MF
c>2   drou/dtrop
c     Global Mapping Function
      if(    trim(MF).EQ.'gmf')then
         call gmf(mjd,slat,slon,shgt,zd,gmfh,gmfw)
      elseif(trim(MF).EQ.'vmf1')then
         write(*,*) 'Not support the vmf1 now'
      endif
c
c>3   zenith path delay
      call trop_hydro_zpd(xrcv,zhd)
c>4
      dis_dry_trop = zhd*gmfh
      dis_wet_trop = zwd*gmfw
c
c     Observed Minus Computed
c     =======================
c
c     SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c        observed minus computed
c        ***********************
c****    P3
c        observed         
         dis_Observed_P3 = P3  
     &                   + dis_dPCO     ! difference of PCO
     &                   + dis_PCV      ! PCV
c        computed
         dis_Computed_P3 = dis 
     &                   + crcv
     &                   - dis_dTs      ! GPS clock error
     &                   - dis_rel      ! GPS relativity
c    
c****    L3    
         dis_Observed_L3 = L3  
     &                   + dis_dPCO     ! difference of PCO [m]
     &                   + dis_PCV      ! PCV  [m]
c        computed
         dis_Computed_L3 = dis 
     &                   + crcv
     &                   - dis_dTs      ! GPS clock error [m]
     &                   - dis_rel      ! GPS relativity [m]
c    &                   + dis_windup   ! phase windup [m]
     &                   + N3           ! ambiguity item [m]
c
c&&      Model1: traditional L3,P3
         if(    observ_model.EQ.1 )then
c
c++          Observed minus computed for P3
             iobs      =(irec-1)*2 + 1
             L(iobs,1) = dis_Observed_P3 - dis_Computed_P3
c
c++          Observed minus computed for L3
             iobs      =(irec-1)*2 + 2
             L(iobs,1) = dis_Observed_L3 - dis_Computed_L3
c
c**      Model2:receiver clock-decoupled L3
         elseif(observ_model.EQ.2 )then
c++          Observed minus computed for L3
             iobs      = irec
             L(iobs,1) = dis_Observed_L3 - dis_Computed_L3
         else
             write(*,*) 'SmartPPP/OMC2'
             write(*,*) 'Not support this observ_model'
             stop
         endif
c
c     GEODETIC
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
c****    P3
c        observed         
         dis_Observed_P3 = P3  
     &                   + dis_dPCO     ! difference of PCO
     &                   + dis_PCV      ! PCV
         if(debug)then
            write(*,*) 'observed'
            write(*,*) 'P3,dis_dPCO, dis_PCV, dis_windup'
            write(*,*)  P3,dis_dPCO, dis_PCV, dis_windup
         endif
c
c        computed
         dis_Computed_P3 = dis 
     &                   + crcv
     &                   - dis_dTs      ! GPS clock error
     &                   - dis_rel      ! GPS relativity
     &                   + dis_dry_trop ! dry tropopsheric delay
     &                   + dis_wet_trop
         if(debug)then
            write(*,*) 'computed'
            write(*,*) 'dis, crcv, dis_dts, dis_rel', 
     &                 'dis_dry_trop,dis_wet_trop'
            write(*,*)  dis, crcv, dis_dts, dis_rel, 
     &                  dis_dry_trop,dis_wet_trop 
         endif
c    
c****    L3    
         dis_Observed_L3 = L3  
     &                   + dis_dPCO     ! difference of PCO [m]
     &                   + dis_PCV      ! PCV  [m]
c    &                   + dis_windup   ! phase windup [m]
         if(debug)then
            write(*,*) 'observed'
            write(*,*) 'iPRN,L3,dis_dPCO',
     &                 'dis_PCV, dis_windup,dis_wet_trop'
            write(*,*)  iPRN,L3,dis_dPCO, 
     &                  dis_PCV, dis_windup,dis_wet_trop
         endif
c
c        computed
         dis_Computed_L3 = dis 
     &                   + crcv
     &                   - dis_dTs      ! GPS clock error [m]
     &                   - dis_rel      ! GPS relativity [m]
     &                   + dis_dry_trop ! dry tropopsheric delay [m]
     &                   + dis_wet_trop
     &                   + N3           ! ambiguity item [m]
c
         if(debug)then
            write(*,*) 'computed'
            write(*,*) 'Time,iPRN,dis,crcv,dis_dts,dis_rel',
     &                 'zhd,gmfh,zwd,gmfw,dis_dry_trop,dis_wet_trop,N3',
     &                 'xrcv,xsat'
            write(*,*)  Time,iPRN, dis, crcv, dis_dts, dis_rel,
     &                  zhd,gmfh,zwd,gmfw,dis_dry_trop, dis_wet_trop,N3,
     &                  xrcv(1),xrcv(2),xrcv(3),xtrs(1),xtrs(2),xtrs(3)
         endif
c
c&&      Model1: traditional L3,P3
         if(    observ_model.EQ.1 )then
c
c++          Observed minus computed for P3
             iobs      =(irec-1)*2 + 1
             L(iobs,1) = dis_Observed_P3 - dis_Computed_P3
             if(debug)then
                write(*,*) 'L'
                write(*,*)  L(iobs,1)
             endif
c
c++          Observed minus computed for L3
             iobs      =(irec-1)*2 + 2
             L(iobs,1) = dis_Observed_L3 - dis_Computed_L3
c
             if(debug)then
                write(*,*) 'L'
                write(*,*)  L(iobs,1)
             endif
c
c**      Model2:receiver clock-decoupled L3
         elseif(observ_model.EQ.2 )then
c
c++          Observed minus computed for L3
             iobs      = irec
             L(iobs,1) = dis_Observed_L3 - dis_Computed_L3
         else
             write(*,*) 'SmartPPP/OMC2'
             write(*,*) 'Not support this observ_model'
             stop
         endif
      else
         write(*,*) 'SmartPPP/OMC2'
         write(*,*) 'Please make modification for this type'
      endif
c
      return
c
      end
