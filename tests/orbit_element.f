*  
*  procedure orbit_element 
*  
*    
      program orbit_element
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
*  07/10/18     S.J.Zhang   program this program
*  
*********1*********2*********3*********4*********5*********6*********7**
c
c     delaration of varaiables
c
      real*8    rad
      parameter(Rad       = 3.1415926535897932d0/ 180.0d0)
      real*8    Deg
      parameter(Deg       = 180.0d0/3.1415926535897932d0 )
c   
      integer   i
      real*8    y(6), kepler(6)
c
      real*8    M, e, ecc_anom
      real*8    rot_eci_rac(3,3)
c
c     initial
c
      do i=1, 6
        kepler(i) = 0.0d0
      enddo
c
      y(1) = +10000.000D3
      y(2) = +40000.000D3
      y(3) =  -5000.000D3
      y(4) =     -1.500D3
      y(5) =     +1.000D3
      y(6) =     -0.100D3
c
c     kepler elements
c
      call state2element(y, kepler)
c
c     output
c   
      write(*,'(A24, f20.8)') "Semi-major axis       a", kepler(1)/1000.0d0
      write(*,'(A24, f20.8)') "Eccentricity          e", kepler(2)
      write(*,'(A24, f20.8)') "Inclination           i", kepler(3)*Deg
      write(*,'(A24, f20.8)') "RA ascend.node    Omega", kepler(4)*Deg
      write(*,'(A24, f20.8)') "Arg.of perigee    omega", kepler(5)*Deg
      write(*,'(A24, f20.8)') "Mean anormaly         M", kepler(6)*Deg
c
c     kepler's equation
c   
      M = 4.0d0*rad
      e = 0.72d0
c
      call solve_kepler_equation(M, e, ecc_anom)
c
      write(*,*) ecc_anom
c
c     call  rotation_eci_rac(y, rot_eci_rac)
c
      end
