*
*  procedure xyz2geoc
*   
      subroutine xyz2geoc(site_pos, geoc_pos)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Routine to compute the geocentric latitudes, longitude and radius
*  from the cartesian XYZ coordinates of the site.  
*
*  Input_Output Auguments
*  ======================
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  site_pos(3)  REAL*8  I       XYZ coordinates of the site (m)
*  geoc_pos(3)  REAL*8  O       The site geodetic coordinate (m)
*
*  Algorithm
*  =========
*               X = R*cos(Lat)*cos(Lon)
*               Y = R*cos(Lat)*sin(Lon)
*               Z = R*sin(Lat)
*   
*
*  History:
*
*  Vesion 1.0
*    
*  Time         Author      Description
*  ----         ------      --------------------------------------------
*  07.06.01     S.J.Zhang   build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
*
*   Declaration_of_the_Evironment_Variables
*         
      IMPLICIT NONE
c
c   Declaration_of_the_Local_Variables
c
      REAL*8    pi
      parameter(pi=3.1415926535897932d0)
c
c   Declaration_of_the_Input_Output_Variables
c
      REAL*8    site_pos(3)
      REAL*8    geoc_pos(3)
c
c   Declaration_of_the_Local_Variables
c
      REAL*8    X,   Y,   Z
      REAL*8    lon, lat, rad
      REAL*8    r
c
c   Initialization
c
      X = site_pos(1)
      Y = site_pos(2)
      Z = site_pos(3)
c
c   Now do the transformation
c
      lon = datan2(Y, X)
      if(lon.lt.0.0d0)then
        lon = lon + 2*pi
      endif
c
      rad = dsqrt(X**2 + Y**2 + Z**2)
c
      lat = dasin(Z/RAD)
c
      geoc_pos(1) = lat
      geoc_pos(2) = lon
      geoc_pos(3) = rad
c
      return
c
      end
      
