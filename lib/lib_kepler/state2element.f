*  
*  procedure state2element 
*  
*    
      subroutine state2element(y, kepler)
*
*********1*********2*********3*********4*********5*********6*********7**
*     
*  Purpose
*  =======
*  
*  Computes the osculating Keplerian elements from the satellite state
*  vector for elliptic orbits  
*  
*
*  Input/Output
*  ============
*
*  Name       Type   I/O    Description
*  ----       ----   ---    --------------------------------------------      
*  y          R      I      State vector (x,y,z,vx,vy,vz)
*  kepler     R      O      Keplerian elements (a,e,i,Omega,omega,M)
*                           with
*                           a      Semimajor axis
*                           e      Eccentricity
*                           i      Inclination [rad]                           
*                           Omega  Longitude of the ascending node [rad]
*                           omega  Argument of pericenter  [rad]
*                           M      Mean anomaly  [rad]
*  
*  Notes
*  =====  
*  
*  The state vector and GM must be given in consistent units,  
*  e.g. [m], [m/s] and [m^3/s^2]. The resulting unit of the semimajor
*  axis is implied by the unity of y, e.g. [m].
*  
*  The function cannot be used with state vectors describing a circular  
*  or non-inclined orbit.
*
*  History
*  =======
*
*  Time         Author      Description
*  ----         ------      --------------------------------------------
*  07/10/18     S.J.Zhang   program this subroutine
*  
*********1*********2*********3*********4*********5*********6*********7**
c
      implicit none
c
c     declaration of constants
c
c     pi
      real*8    pi
      parameter(pi      = 3.14159265358979324)
c     2*pi
      real*8    two_pi
      parameter(two_pi  = 6.28318530717958648)
c     GM
      real*8    GM_Earth    
      parameter(GM_Earth= 398600.4415D+9)
c
      real*8    Deg
      parameter(Deg     = 180.0d0/3.1415926535897932 )
c
c     declaration of input_output variables
c
      real*8    y(6), kepler(6)
c
c     declaration of local variables
c
      real*8    r(3), v(3), h(3)
      real*8    u, dis_r
      real*8    eCosE, eSinE, e2, E_anormly, nu
      real*8    a,e,i,omega_long,omega_argu,M
      real*8    h_Norm, vv_dot, rv_dot
c
      integer   k
c
c     initialization
c
      do k=1, 3
        r(k) = 0.0d0
        v(k) = 0.0d0
        h(k) = 0.0d0
      enddo
c
c     position vector from state y
c
      do k=1,3
        r(k) = y(k)
      enddo
c
c     velocity vector from state y
c
      do k=1,3
        v(k) = y(k+3)
      enddo
c
c     area velocity from position cross velcoity 
c
      call vector_cross(r, v, h)
      call vector_norm(h,  h_Norm)                    ! Norm of vector h
c
c     long.ascend.node    
c
      omega_long = atan2( h(1), -h(2) )
      omega_long = modulo(omega_long, two_pi)
c
c     inclination/Argument of latitude
c
      i = atan2( sqrt( h(1)*h(1)+h(2)*h(2) ), h(3) )    ! inclination
      u = atan2( r(3)*h_Norm, -r(1)*h(2)+r(2)*h(1) )
c
c     distance
c
      call vector_norm(r, dis_r)
c
c     vv dot/rv dot
c
      call vector_dot(v, v, vv_dot)
      call vector_dot(r, v, rv_dot)
c
c     axis
c
      write(*,*) dis_r, vv_dot, GM_Earth
c
      a = 1.0d0/(2.0d0/dis_r - vv_dot/GM_Earth )
c
c     e*cos(E)/e*sin(E)
      eCosE = 1.0d0- dis_r/a
      eSinE = rv_dot/sqrt(GM_Earth*a)
c
c     Eccentricity 
      e2    = eCosE*eCosE +eSinE*eSinE
      e     = sqrt(e2)
c
c     Eccentric anomaly 
      E_anormly = atan2(eSinE, eCosE)
c
c     Mean anomaly
      M  = Modulo(E_anormly-eSinE, two_pi)
c
c     True anomaly
      nu = atan2(sqrt(1.0d0-e2)*eSinE, eCosE-e2)
c
c     Arg. of perihelion
      omega_argu = Modulo(u-nu,two_pi)
c
c     kepler orbit elements
c
      kepler(1) = a
      kepler(2) = e
      kepler(3) = i
      kepler(4) = omega_long
      kepler(5) = omega_argu
      kepler(6) = M
c
      return
c
      end
