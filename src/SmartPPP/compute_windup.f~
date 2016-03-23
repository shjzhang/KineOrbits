c
c subroutine compute_windup.f
c
      subroutine compute_windup(time,iPRN,iflag,xrcv0,xsat0,windup)
c
c=======================================================================
c     ****f* SmartPPP/compute_windup.f
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
c     xsat0          real*8        transmitter parameter
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
C     $Id: compute_windup.f.f,v 1.0 2009/08/04 $
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
c     external function
      real*8        sec2jd
c
c     input/output variable
c
c     input
      real*8        Time
      integer       iPRN
      integer       iflag
      real*8        xrcv0(3)
      real*8        xsat0(3)
c     outpu
      real*8        windup
      real*8        windup1,windup2
c
c     local 
c
      integer       i,j,k
      integer       n
c
      real*8        xrcv(3),xsat(3)
c
      real*8        f1, f2, lam1, lam2
c
      real*8        TIME_iPRN     (MAX_PRN)
      real*8        TIME_iPRN_PREV(MAX_PRN)
c
c
      data          TIME_iPRN      /50*0.0d0/     
      data          TIME_iPRN_PREV /50*0.0d0/
c
      real*8        sun_state_icrs(6)
      real*8        sun_state_itrs(6)
      real*8        dvec_sat_earth(3)
      real*8        rvec_sat_earth(3)
      real*8        rvec_earth_sun(3)
      real*8        uvec_earth_sun(3)
      real*8        rk(3),rk1(3), rj(3), rj1(3),ri(3),ri1(3)
      real*8        rxPos(3),rrho(3), rrho1(3)
      real*8        zk
      real*8        dpp(3), gps_sun(3)
      real*8        xk,yk, alpha1, alpha2
      real*8        delta(3), delta1(3)
      character*20  block_type
      real*8        da1, da2
      real*8        windup_sat_pre(MAX_PRN), windup_rcv_pre(MAX_PRN)
      real*8        time_jd
c
      save          windup_sat_pre, windup_rcv_pre
      save          TIME_iPRN_PREV
c   
c     unit vector from satellite to earth mass center:rk   
      rvec_sat_earth(1) = -xsat0(1)
      rvec_sat_earth(2) = -xsat0(2)
      rvec_sat_earth(3) = -xsat0(3)
c
      call vector_unit(rvec_sat_earth,rk)
c
c     compute the sun's position
c
      time_jd = sec2jd(time)
c     sun's position in ICRF
      call planet_state(time_jd,11,3,sun_state_icrs)
      call icrs2itrs(time, sun_state_icrs,sun_state_itrs)
c
      rvec_earth_sun(1) = sun_state_itrs(1)*1000.0d0
      rvec_earth_sun(2) = sun_state_itrs(2)*1000.0d0
      rvec_earth_sun(3) = sun_state_itrs(3)*1000.0d0
c
c     unit vector from Earth center to Sun
      call vector_unit(rvec_earth_sun,ri)
c       
c     rj = rk x ri       
      call vector_cross(rk,ri,rj)
c       
c     rj = rk x ri       
      call vector_cross(rj,rk,ri)
c
c     convert ri, rj to unitary vectors
c     now ri, rj, rk form a base in the ECEF reference frame
      call vector_unit(ri,ri1)
      ri(1) = ri1(1)
      ri(2) = ri1(2)
      ri(3) = ri1(3)
      call vector_unit(rj,rj1)
      rj(1) = rj1(1)
      rj(2) = rj1(2)
      rj(3) = rj1(3)
c
c    *Get satellite rotation angle
c
c     Get vector from Earth mass center to receiver
      rxPos(1) = xrcv0(1)
      rxPos(2) = xrcv0(2)
      rxPos(3) = xrcv0(3)    
c     compute unitary vector from satellite to receiver
      rrho(1)  = rxPos(1) - xsat0(1)
      rrho(2)  = rxPos(2) - xsat0(2)
      rrho(3)  = rxPos(3) - xsat0(3)
c     unit vector
      call vector_unit(rrho,rrho1)
      rrho(1) = rrho1(1)
      rrho(2) = rrho1(2)
      rrho(3) = rrho1(3)
c     vector from Sv to Sun center of mass
      gps_sun(1) = sun_state_itrs(1) - xsat0(1)
      gps_sun(2) = sun_state_itrs(2) - xsat0(2)
      gps_sun(3) = sun_state_itrs(3) - xsat0(3)
c     redefine rk: unitry vector from SV to Earth mass center
      rk(1) = -xsat0(1)
      rk(2) = -xsat0(2)
      rk(3) = -xsat0(3)
      call vector_unit(rk,rk1)
      rk(1) = rk1(1)
      rk(2) = rk1(2)
      rk(3) = rk1(3)
c     redefine rj: rj = rk x gps_sun, then make it unitary
      call vector_cross(rk,gps_sun, rj)
      call vector_unit(rj, rj1)
      rj(1) = rj1(1)
      rj(2) = rj1(2)
      rj(3) = rj1(3)
c     redefine ri: ri = rj x rk, then make it unitary
      call vector_cross(rj,rk,ri)
      call vector_unit(ri,ri1)
      ri(1) = ri1(1)
      ri(2) = ri1(2)
      ri(3) = ri1(3)
c
c     Projection of 'rk' vector to line of sight vector(rrho)
c
      call vector_dot(rrho,rk,zk)
c
c     Get a vector without components on rk(i.e,belonging to ri, rj plane2)
      rk1(1) = zk*rk(1)
      rk1(2) = zk*rk(2)
      rk1(3) = zk*rk(3)
      call vector_minus(rrho,rk1,dpp)
c
c     Compute dpp components in ri, rj plane
      call vector_dot(dpp,ri,xk)
      call vector_dot(dpp,rj,yk)
c
c     Compute satellite rotation angle, in radians   
      alpha1 = datan2(yk,xk)
c
c     Get receiver rotation angle
c
c     Redefine rk: Unitary vector from Receiver to Earth mass center
      rk(1) = -rxPos(1)
      rk(2) = -rxPos(2)
      rk(3) = -rxPos(3)
      call vector_unit(rk,rk1)
      rk(1) = rk1(1)
      rk(2) = rk1(2)
      rk(3) = rk1(3)
c
c     Let's define a NORTH unitary vector in the Up, East, North
c    (UEN) topocentric reference frame
      delta(1) = 0.0
      delta(2) = 0.0
      delta(3) = 1.0
c
c     Rotate delta to XYZ reference frame
      call neu2xyz(xrcv0,delta,delta1)
      delta(1) = delta1(1)
      delta(2) = delta1(2)
      delta(3) = delta1(3)
c
c     Computation of reference trame unitary vectors for receiver
c     rj = rk x delta, and make it unitary
      call vector_cross(rk,delta,rj)
      call vector_unit(rj,rj1)
      rj(1) = rj1(1)
      rj(2) = rj1(2)
      rj(3) = rj1(3)
c
c     ri = rj x rk, and make it unitary
      call vector_cross(rj,rk,ri)
      call vector_unit(ri,ri1)
      ri(1) = ri1(1)
      ri(2) = ri1(2)
      ri(3) = ri1(3)
c
c     Projection of "rk" vector to line of sight vector (rrho)
      call vector_dot(rrho,rk,zk)
c
c     Get a vector without components on rk (i.e., belonging // to ri, rj plane)
      rk1(1) = zk*rk(1)
      rk1(2) = zk*rk(2)
      rk1(3) = zk*rk(3)
      call vector_minus(rrho,rk1,dpp)
c
c     compute dpp components in ri, rj plane
      call vector_dot(dpp,ri,xk)
      call vector_dot(dpp,rj,yk)
c
c     Compute receiver rotation angle, in radians
      alpha2 = atan2(yk,xk)
c
      call get_block(iPRN,block_type)     
c
      windup = 0.0d0
c
      if(    trim(block_type).eq.'BLOCK IIR-A'
     &   .or.trim(block_type).eq.'BLOCK IIR-B'
     &   .or.trim(block_type).eq.'BLOCK IIR_M')then
         windup = pi
      endif
c
      alpha1 = alpha1 + windup;   
c
      TIME_iPRN(iPRN) = time     
c
      if(iflag.eq.1) then
         windup_sat_pre(iPRN) = 0.0
         windup_rcv_pre(iPRN) = 0.0
      endif
c
      da1 = alpha1 - windup_sat_pre(iPRN)
      da2 = alpha2 - windup_rcv_pre(iPRN)
c
      write(*,*)'da1, da2'
      write(*,*) da1, da2
c
      write(*,*)'windup_sat_pre(iPRN), windup_rcv_pre(iPRN)'    
      write(*,*) windup_sat_pre(iPRN), windup_rcv_pre(iPRN)     
c
      windup_sat_pre(iPRN) = windup_sat_pre(iPRN) 
     &                     + atan2(sin(da1),cos(da1))
      windup_rcv_pre(iPRN) = windup_rcv_pre(iPRN) 
     &                     + atan2(sin(da2),cos(da2))
c
      write(*,*)'windup_sat_pre(iPRN), windup_rcv_pre(iPRN)'    
      write(*,*) windup_sat_pre(iPRN), windup_rcv_pre(iPRN)     
c
c     compute wind up effect in radians
      windup = windup_sat_pre(iPRN) - windup_rcv_pre(iPRN)
c
      write(*,*) 'windup in rad', windup
c
      windup1= windup/(2*pi)*lam_L1_GPS
      windup2= windup/(2*pi)*lam_L2_GPS
c
      write(*,*) windup1, windup2
c
      f1   = f1_GPS
      f2   = f2_GPS
c
      windup  =(f1**2*windup1-f2**2*windup2)/(f1**2-f2**2)
c
c     Save time in TIME_PREVious for next computation
c
      TIME_iPRN_PREV(iPRN) = time

c
c     THE END
c
      return
c
      end

