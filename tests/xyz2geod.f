*
*  procedure xyz2geod
*   
      subroutine xyz2geod(site_pos, geod_pos)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                               Purpose
*
*  Routine to compute the geodetic latitudes, longitude and elipsoidal 
*  height from the cartesian XYZ coordinates of the site.  
*  The algorithm used is adopted from:
*  Heiskanen W A and H. Moritz, Physical Geodesy, W.H.Freeman and company, 
*  San Francisco, 364, 1967. Chapter 5.3.
*
*  The routine also returns the tranformation between XYZ and
*  local NEU system.
*
*                               Input_Output Auguments
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  site_pos(3)  REAL*8  I       XYZ coordinates of the site (m)
*  gedo_pos(3)  REAL*8  O       The site geodetic coordinate (m)
*
*
*                               Restriction
*
*  User must supply the XYZ coordinates.  If these  are not available the 
*  can be computed from geod_pos using:
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
      
*  Geodetic datum               radius      flatten
*  -----------------------------------------------------------------
*  GRS80 (IUGG 1980)            6378137     298.257222
*  WGS84                        6378137     298.257223563     
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
*     Name            Description
*     ----            ----------------------------------------------------
*     dlatdN          Derivative of latitudes with respect to the
*                     (horizontal) North component of position
*                     This derivative should not be confused with
*                     dNdlat (same symbol N but different meanings)
*     dNdlat          Derivative of the curvature (rad_curve)
*                     with respect to latitudes.
*     dXdlat          Derivative of the X coordinate wrt latitudes
*     dYdlat          Derivative of the Y coordinate wrt latitudes
*     dZdlat          Derivative of the Z coordinate wrt latitudes
c
*     eccsq           eccentricity squared computed from flattening
*     equ_rad         radial distance of site from rotation axis.
*     geod_pos(3)     the geodetic coordinates.  The order of the
*                     values is:
*                       (1) - Geodetic latitudes (rads)
*                       (2) - Geodetic longitude (positive east) (rad)
*                       (3) - ellipsoidal height (m)
*     lat_i           approximate latitudes used in iteration
*     lat_p           approximate latitudes from previous iteration
*     long            longitude of the site.
*     h_i             approximate height used in iteration
*     h_p             approximate height from previous iteration
*     rad_lat         radius to be used in computing the geodetic
*                     latitudes
*     rad_curve       radius of curvature at the site (N above)
*     rot_mat(3,3)    Transformation matrix XYZ and NEU where
*                     N is -latitudes, E is longitude and U is
*                     along ellipsoidal height
*     site_pos(3)     XYZ coordinates of the site (m)
*
*     tolerance       toleranace for convergence on geodetic
*                     coordinates.
***
      real*8 dlatdN, dNdlat, dXdlat, dYdlat, dZdlat, eccsq, equ_rad,
     .       geod_pos(3), lat_i, lat_p, long, h_i, h_p, rad_lat,
     .       rad_curve, rot_mat(3,3), site_pos(3), tolerance,
     .       earth_rad, earth_flat, pi
***
      integer*4 len,rcpar
***
      character*80 prog_name
c
c  Declaration_of_Constatns
c
     
      parameter ( pi            = 3.141592653589793238D0 )
      parameter ( earth_rad     = 6378137.D0           )
      parameter ( earth_flat    = 1.d0/298.257223563D0 )
      
c
c     niter         Number of iterations.  EXITS if more than 50
c                   iterations are required.  (Removes problem if
c                   coordinates are NaN in IEEE floating point)
      integer*4 niter
c
c     converged     Indicate values have converged.
c
      logical converged
c
c  Initilization
c
      data    tolerance  / 0.0001d0 /
c
***** Start, get the latitudes and height by iteration.
c
      equ_rad = sqrt( site_pos(1)**2 + site_pos(2)**2 )
      eccsq   = 2.d0*earth_flat - earth_flat**2
c
***** Set previous iteration values
c
      lat_p = atan2( site_pos(3), equ_rad)
      h_p   = 0.d0
c
      converged = .false.
      niter = 0
c
      do while ( .not. converged )

c       Get radius of curvature using previous latitudes estimate
c       N         = a/sqrt(1-e**2*sin(phai)**2)
        rad_curve = earth_rad /sqrt(1.d0 - eccsq*sin(lat_p)**2 )
        rad_lat   = equ_rad*(1.d0 - eccsq*rad_curve/(rad_curve+h_p))
        lat_i     = atan2( site_pos(3), rad_lat)
*       Use cos lat formula
        if( abs(lat_i).lt. pi/4 ) then
          h_i   = equ_rad/cos(lat_i) - rad_curve
*       Use sin lat formula
        else
          h_i   = site_pos(3)/sin(lat_i) - (1.d0-eccsq)*rad_curve
        endif
c
****    Check for convergence
c
        if( abs(h_i-h_p)              .lt. tolerance .and.
     .      abs(lat_i-lat_p)*rad_curve.lt. tolerance       )then
            converged = .true.
        end if
c
c       Check for two many iterations
c
        niter = niter + 1
        if( niter.gt.50 ) then
          write(*,*) 'fatal error << xyz2geod.f'
          write(*,*) '    Convergence failure in computing geodetic
     &                    coordinates'
        end if
c
c       Save the latest values
c
        h_p   = h_i
        lat_p = lat_i
c
*****   iterating for latitudes and height
c
      end do
c
***** Save the final values
c
c     geod_pos(1):latitude
      geod_pos(1) = lat_i
c     gedo_pos(2):longitude
      long = atan2( site_pos(2),site_pos(1) )
*     add 2*pi
      if( long.lt.0 ) then
        geod_pos(2) = 2*pi + long
      else
        geod_pos(2) = long
      end if
c     geod_pos(3):geodetic height
      geod_pos(3) = h_i
c
***** Now do the transformation between XYZ and NEU
c      
c     Now do NORTH componet    
c
      rot_mat(1,1) = -dsin(lat_i) * dcos(long)
      rot_mat(1,2) = -dsin(lat_i) * dsin(long)
      rot_mat(1,3) =  dcos(lat_i)
*
*     Now do EAST component
*
      rot_mat(2,1) = -dsin(long)
      rot_mat(2,2) =  dcos(long)
      rot_mat(2,3) =  0.d0
*
*     Now do UP component
*
      rot_mat(3,1) = dcos(lat_i) * dcos(long)
      rot_mat(3,2) = dcos(lat_i) * dsin(long)
      rot_mat(3,3) = dsin(lat_i)
c
      return
c
      end

