*
*  procedure geod2xyz
*   
      subroutine geod2xyz(geod_pos, site_pos)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                               Purpose
*
*  Routine to convert the geodetic latitudes, longitude and elipsoidal 
*  height to the cartesian XYZ coordinates of the site.
*      
*  The algorithm used is adopted from:
*  Heiskanen W A and H. Moritz, Physical Geodesy, W.H.Freeman and company, 
*  San Francisco, 364, 1967. Chapter 5.3.
*
*                               Input_Output Auguments
*
*  Name         Type    I/O     Description                         unit   
*  ----         ----    ---     ----------------------------------------
*  gedo_pos(3)  REAL*8  I       Geodetic coordinates.  The order of the
*                               values is:
*                               (1) Geodetic latitudes              (rads)
*                               (2) Geodetic longitude              (rads)
*                               (3) ellipsoidal height              (m)
*  site_pos(3)  REAL*8  O       XYZ coordinates of the site         (m)
*
*
*                               Notes
*
*     X = (N+h) cos(phi) cos(lambda)
*     Y = (N+h) cos(phi) sin(lambda)
*     Z = [(1-e**2)N+h]  sin(phi)
*
*     where e**2 = 2f-f**2; f is flattening
*           N**2 = a**/[1-(e*sin(phi))**2]
*           N is East-West Radius of curvature.
*           f    =(a-b)/a
*           e**2 =(a**2-b**2)/a**2
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
*  Declaration_of_Local_Varialbes
*   
      IMPLICIT NONE 
*
*  Declaration_of_Input_Output_Arguments
*
      REAL*8    GEOD_POS(3), SITE_POS(3)
*
*  Declaration_of_Local_Varialbes
*     
*     Name            Description 
*     ------------------------------------------------------------------
*     rad_curve       radius of curvature at the site (N above)
*     eccsq           Eccentricity
*
      REAL*8    rad_curve, eccsq    
      REAL*8    lat, lon, height
c
c  Declaration_of_Constatns
c
      real*8    pi
      parameter(pi            = 3.141592653589793238D0 )
c      
c     WGS-84 parameters for the elliposoid (a and 1/f)
c
      real*8    earth_rad
      real*8    earth_flat
      parameter(earth_rad     = 6378137.D0             )
      parameter(earth_flat    = 1.d0/298.257223563D0   )
c
c  Latitue,longitude and height
c
      Lat    = geod_pos(1)
      Lon    = geod_pos(2)
      Height = geod_pos(3)
c
c  Calculate radius of curvature and ecdentricity
c
      eccsq     = 2.d0*earth_flat - earth_flat**2
      rad_curve = earth_rad/sqrt(1.d0 - eccsq*sin(lat)**2 )
c
c  Now do the transformation
c
      site_pos(1) = (              rad_curve + height)*cos(lat)*cos(lon)
      site_pos(2) = (              rad_curve + height)*cos(lat)*sin(lon)
      site_pos(3) = ((1.0d0-eccsq)*rad_curve + height)*sin(lat)
c
      return
c
      end 
