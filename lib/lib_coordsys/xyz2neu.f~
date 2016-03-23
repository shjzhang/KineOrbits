*
*  procedure xyz2neu
*   
      subroutine xyz2neu(site_xyz, targ_xyz, site_neu)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                               Purpose
*
*  Transformation geocentric xyz coordinate to local N/E/U coordinate
*
*
*                               Input_Output Auguments
*
*  Name         Type    I/O     Descriptin
*  ----         ----    ---     ---------------------------------------
*  site_xyz(3)  REAL*8  I       XYZ coordinates of the site (m)
*  targ_xyz(3)  REAL*8  I       XYZ coordinates of the target (m)
*  site_neu(3)  REAL*8  O       NEU coordinates of the target wrt local
*                               site(m)
*
*
*                               Restriction
*
*  User must supply the XYZ coordinates of the site and the target.  
*  If these are not available they can be computed from geod_pos using:
*
*           X = (N+h) cos(phi) cos(lambda)
*           Y = (N+h) cos(phi) sin(lambda)
*           Z = [(1-e**2)N+h]  sin(phi)
*
*  where  
*           e**2 = 2f-f**2; f is flattening
*           N**2 = a**/[1-(e*sin(phi))**2]
*           f    =(a-b)/a
*           e**2 =(a**2-b**2)/a**2
*           N is East-West Radius of curvature.
*           
*
*  History:
*
*  Vesion 1.0
*    
*  Time         Author          Description
*  ----         ------          ----------------------------------------
*  07.06.01     S.J.Zhang       build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
c
c     Declaration_of_Variables
c
      IMPLICIT NONE
c      
c     Declaration_of_Local_Varialbes
c
      REAL*8 site_xyz(3), targ_xyz(3), site_neu(3), geod_pos(3)
      REAL*8 vec_xyz(3)
      REAL*8 rot_mat(3,3)
      REAL*8 lon, lat
***
      INTEGER i, j
c
c     Roatation matrix from geocentric to topocentric coordinate
c
      call xyz2geod(site_xyz, geod_pos)
c
c     geod_pos(1):latitude      
c     geod_pos(2):longitude
c     geod_pos(3):geodetic height
c
      lat = geod_pos(1)
      lon = geod_pos(2)
c
c     calculate the rotation matrix
c   
c     The North component
      rot_mat(1,1) = -dsin(lat) * dcos(lon)
      rot_mat(1,2) = -dsin(lat) * dsin(lon)
      rot_mat(1,3) =  dcos(lat)
c     The East  component
      rot_mat(2,1) = -dsin(lon)
      rot_mat(2,2) =  dcos(lon)
      rot_mat(2,3) =  0.d0
c     THe UP    component
      rot_mat(3,1) =  dcos(lat) * dcos(lon)
      rot_mat(3,2) =  dcos(lat) * dsin(lon)
      rot_mat(3,3) =  dsin(lat)
c
c     Now do transfomation
c
      do i=1, 3
        vec_xyz(i) = targ_xyz(i) - site_xyz(i)
      enddo
***
      do i=1, 3
        site_neu(i) = 0.0d0
        do j=1, 3
          site_neu(i) = site_neu(i) + rot_mat(i,j)*vec_xyz(j)
        enddo
      enddo
c
      return
c
      end
