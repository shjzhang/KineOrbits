*
*  procedure geoc2xyz
*   
      subroutine geoc2xyz(geoc_pos, site_pos)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Routine to compute the cartesian XYZ coordinates of the site
*  from the geocentric latitudes, longitude and radius.
*
*  Input_Output Auguments
*  ======================
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  geoc_pos(3)  REAL*8  I       The site geodetic coordinate (m)
*  site_pos(3)  REAL*8  O       XYZ coordinates of the site (m)
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
      REAL*8    geoc_pos(3), site_pos(3)
      REAL*8    lat, lon, rad
      REAL*8    X,   Y,   Z
c
c   Now do the transformation
c
      lat         = geoc_pos(1)
      lon         = geoc_pos(2)
      rad         = geoc_pos(3)
c
      X           = rad*dcos(lat)*dcos(lon)
      Y           = rad*dcos(lat)*dsin(lon)
      Z           = rad*dsin(lat)
c
      site_pos(1) = X
      site_pos(2) = Y
      site_pos(3) = Z
c
      return
c
      end
