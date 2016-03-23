c
c subroutine corr_rcv_pco.f
c
      subroutine phase_windup(time,iPRN,xrcv0,xtrs0,windup)
c
c=======================================================================
c     ****f* SmartPPP/corr_rcv_pco.f
c
c   FUNCTION   
c   
c     compute the transmitter / receiver antenna orientation dependent
c     phase corrections for Right Circularly Polarized electro-magnetic waves
c     Reference Wu J.T., et al., Manuscripta Geogetica (1993) 18, pp91-98...
c
c   INPUTS
c
c     time           real*8        second past J2000.0 in GPS
c     iPRN           integer       PRN number
c     xrcv0          real*8        receiver parameter
c     xtrs0          real*8        transmitter parameter
c
c   OUTPUT
c
c     windup         real*8        windup for iPRN
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/04                   programmed
c
c     ***
c
C     $Id: corr_rcv_pco.f.f,v 1.0 2009/08/04 $
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
      real*8        one
      parameter (   pi  = 3.1415926535897932D0 )
      parameter (   one = 1.D0 )
c
c     input/output variable
c
c     input
      real*8        Time
      integer       iPRN
      real*8        xrcv0(3)
      real*8        xtrs0(3)
c     outpu
      real*8        windup
      real*8        windup1,windup2
c
c     local 
c
      integer       i,j,k
      integer       n
c
      real*8        dphi,dphiv,dphi_prev(MAX_PRN)
      real*8        xrcv(3),xtrs(3)
c
      character*3   xyz, neu
      real*8        xrcv_loc_neu(3), yrcv_loc_neu(3)
      real*8        xrcv_loc_xyz(3), yrcv_loc_xyz(3)
      real*8        xtrs_loc_neu(3), ytrs_loc_neu(3)
      real*8        xtrs_loc_xyz(3), ytrs_loc_xyz(3)
      real*8        kvec0(3)
      real*8        kvec1(3)
      real*8        tmp1,tmp2(3),tmp3(3),tmp4(3),tmp5
      real*8        d_r(3),d_t(3)
      real*8        pos_neg,len_d_r,len_d_t
c
      real*8        f1, f2, lam1, lam2
c
      real*8        TIME_iPRN     (MAX_PRN)
      real*8        TIME_iPRN_PREV(MAX_PRN)
      logical       first
c
      save          dphi_prev, TIME_iPRN_PREV
c
      data          xrcv_loc_neu   / 1.0, 0.0, 0.0/
      data          yrcv_loc_neu   / 0.0,-1.0, 0.0/
      data          xtrs_loc_neu   / 1.0, 0.0, 0.0/
      data          ytrs_loc_neu   / 0.0,-1.0, 0.0/
c
      data          TIME_iPRN      /50*0.0d0/     
      data          TIME_iPRN_PREV /50*0.0d0/
      data          first /.true./
c
      save          xrcv_loc_xyz, yrcv_loc_xyz
      save          xtrs_loc_xyz, ytrs_loc_xyz
      save          xrcv
      save          first
c
c***  Convert receiver    antenna dipole unit vectors from local NEU coordinates
c     to geocentric earth fixed cartesian vectors.
c
      if(first)then
c
         xrcv(1) = xrcv0(1)
         xrcv(2) = xrcv0(2)
         xrcv(3) = xrcv0(3)
c
         call neu2xyz(     xrcv,xrcv_loc_neu,xrcv_loc_xyz)
         call neu2xyz(     xrcv,yrcv_loc_neu,yrcv_loc_xyz)
c
         first = .false.
c
      endif
c
c***  Convert transmitter antenna dipole unit vectors from local satellite body coordinates
c     to geocentric earth fixed cartesian vectors.
c
      xtrs(1) = xtrs0(1)
      xtrs(2) = xtrs0(2)
      xtrs(3) = xtrs0(3)
c
      call gps2xyz(time,xtrs,xtrs_loc_neu,xtrs_loc_xyz)
      call gps2xyz(time,xtrs,ytrs_loc_neu,ytrs_loc_xyz)
c
c***  Compute the Unit vectors pointing from transmitter to receiver...
c
      do j = 1,3
         kvec0(j) = xrcv(j) - xtrs(j)
      enddo
c
      call vector_unit (kvec0,kvec1)
c
c***  Compute effective dipoles for a transmitter and receiver, and
c     compute the phase correction between the 2 effective dipoles.
c
c---  Compute the receiver effective Dipole trem:
c     D_r = xhat_r - khat(khat . xhat_r) + khat x yhat_r
c     Where: xhat_r & yhat_r = the receiver antenna unit vectors in the directions
c                              of the two dipole elements.
c          : khat = the unit vector pointing from the transmitter to the receiver
c
      call vector_dot  (kvec1,xrcv_loc_xyz,tmp1)
c
      call mtxmul(kvec1,tmp1,tmp2,3,1,1)
c
      call vector_cross(kvec1,yrcv_loc_xyz,tmp3)

      do j=1,3
         d_r(j) = xrcv_loc_xyz(j) - tmp2(j) + tmp3(j)
      enddo
c
c
c---  Compute the transmitter effective Dipole trem:
c     D_t = xhat_t - khat(khat . xhat_t) - khat x yhat_t
c     Where: xhat_t & yhat_t = the transmitter antenna unit vectors in the directions
c                              of the two dipole elements.
c          : khat = the unit vector pointing from the transmitter to the receiver
c
      call vector_dot  (kvec1,xtrs_loc_xyz,tmp1)
c
      call mtxmul(kvec1,tmp1,tmp2,3,1,1)
c
      call vector_cross(kvec1,ytrs_loc_xyz,tmp3)

      do j=1,3
         d_t(j) = xtrs_loc_xyz(j) - tmp2(j) - tmp3(j)
      enddo
c
c     vector norm
c
      call vector_norm (d_r,len_d_r)
      call vector_norm (d_t,len_d_t)
c
c     normalises d_t and d_r
c
      do j=1,3
         d_r(j) = d_r(j)/len_d_r
         d_t(j) = d_t(j)/len_d_t
      enddo
c
c---  Compute the SIGN of the phase correction
c     SIGN = khat . (D_t x D_r)
c
      call vector_cross(d_t,d_r,tmp4)
      call vector_dot  (kvec1,tmp4,pos_neg)
c
c---  Compute the phase correction dphi
c     dphi = SIGN cos-1(D_t . D_r )
c
      call vector_dot  (d_t,d_r,tmp5)
c
      dphi = dsign(one,pos_neg) * dacos(tmp5)
c
c     write(*,*)  iPRN,time,n,dphi
c
c---  Compute the integer part of the phase correction
c     N = nint[ (DPHI_previous - dphi) / (2*pi) ]
c
      TIME_iPRN(iPRN) = time
c
      if(dabs(TIME_iPRN(iPRN)-TIME_iPRN_PREV(iPRN)).gt.600.0) then
         n = 0
         dphi_prev(iPRN) = 0 ! set the previous dphi to 0 for new arc
      else
         n = nint( ( ( dphi_prev(iPRN) - dphi)/(2*pi) ) )
      endif
c
c     Compute the total phase correction
c     DPHI  = 2 * N * pi + dphi
c
      dphi  = 2 * n * pi + dphi
c
c     variation of DPHI     !!! tested by S.J. Zhang
c
      if(dabs(TIME_iPRN(iPRN)-TIME_iPRN_PREV(iPRN)).gt.600.0) then
         dphiv = 0
      else
         dphiv = dphi - dphi_prev(iPRN)
      endif
c
c     Save dphi in dphi_previous for next computation
c
      dphi_prev(iPRN) = dphi
c
c     Save time in TIME_PREVious for next computation
c
      TIME_iPRN_PREV(iPRN) = time
c
c     Convert Phase correction (unitless) into L1 and L2 path delay
c     correction is required in distance.
c
      f1   = f1_GPS
      lam1 = lam_L1_GPS
      f2   = f2_GPS
      lam2 = lam_L2_GPS
c
c     write(*,*)  iPRN,time,n,dphiv
c     unit: [m]
      windup1 = dphiv/(2*pi*f1)*c_light
      windup1 = dphiv/(2*pi*f2)*c_light
c
      windup  =(f1**2*windup1-f2**2*windup2)/(f1**2-f2**2)
c
c     write(*,*)  iPRN,time,n,windup
c
c     THE END
c
      return
c
      end
