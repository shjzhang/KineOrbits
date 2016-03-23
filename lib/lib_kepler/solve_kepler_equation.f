*
*   procedure solve_kepler_equation
*
      subroutine solve_kepler_equation(M, e, ecc_anom)
*
**********1*********2*********3*********4*********5*********6*********7**
*     
*  Purpose
*  =======
*  
*  Computes the eccentric anomaly for elliptic orbits  
*
*  Input/Output
*  ============
*
*  Name       Type   I/O    Description
*  ----       ----   ---    --------------------------------------------      
*  M          R      I      Mean anomaly in [rad] 
*  e          R      I      Eccentricity of the orbit [0,1]
*  ecc_anom   R      O      Eccentric anomaly in [rad]
*  
*  
*  Notes
*  =====  
*  
*  Newton's iteration  
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
      implicit none
c
c     declaration of constants
c
      real*8    pi
      parameter(pi      = 3.14159265358979324)
      real*8    two_pi
      parameter(two_pi  = 6.28318530717958648)
c
c     delaration of varaiables
c
      double precision M, e, ecc_anom
c
c     local variables
c
      integer i, max_iter
c
      double precision eps
      double precision f
c
      i = 0
      max_iter = 15
      eps = 1.0d-15
c
      M = modulo(M, two_pi)
      if(e.lt.0.8)then
        ecc_anom = M
      else
        ecc_anom = pi
      endif
c
      f = 9999.0d0
c
      do while(abs(f).gt.eps)
        f        = ecc_anom - e*dsin(ecc_anom) - M
        ecc_anom = ecc_anom - f / ( 1.0d0 - e*dcos(ecc_anom) )
        i        = i + 1
        if(i.gt.max_iter)then
          write(*,*) "iteration problem occurs in solve kepler's
     +                equation!"
          write(*,*) 'iteration time little? or convergence problem?'
          stop
        endif
      enddo
c
      return
c
      end
