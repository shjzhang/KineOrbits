
      subroutine XYZ_to_GEOD(rot_mat, site_pos, geod_pos )


*     Routine to compute the geodetic latitudes, longitude and
*     elipsoidal height from the cartesian XYZ coordinates of
*     the site.  The algorithm used is adopted from:
*     Heiskanen W A and H. Moritz, Physical Geodesy, W.H.Freeman
*     and company, San Francisco, 364, 1967. Chapter 5.3.
*
*     The routine also returns the tranformation between XYZ and
*     local NEU system.
*
*     RESTRICTION: User must supply the XYZ coordinates.  If these
*     are not available the can be computed from geod_pos using:
*
*     X = (N+h) cos(phi) cos(lambda)
*     Y = (N+h) cos(phi) sin(lambda)
*     Z = [(1-e**2)N+h]  sin(phi)
*
*     where  e**2 = 2f-f**2; f is flattening
*            N**2 = a**/[1-(e*sin(phi))**2]
*     N is East-West Radius of curvature.
*
*   dlatdN      - Derivative of latitudes with respect to the
*               - (horizontal) North component of position
*               - This derivative should not be confused with
*               - dNdlat (same symbol N but different meanings)
*   dNdlat      - the derivative of the curvature (rad_curve)
*               - with respect to latitudes.
*   dXdlat      - Derivative of the X coordinate wrt latitudes
*   dYdlat      - Derivative of the Y coordinate wrt latitudes
*   dZdlat      - Derivative of the Z coordinate wrt latitudes

*   eccsq       - eccentricity squared computed from flattening
*   equ_rad     - radial distance of site from rotation axis.
*   geod_pos(3) - the geodetic coordinates.  The order of the
*               - values is:
*               - (1) - Geodetic co-latitudes (rads)
*               - (2) - Geodetic longitude (positive east) (rad)
*               - (3) - ellipsoidal height (m)
*   lat_i       - approximate latitudes used in iteration
*   lat_p       - approximate latitudes from previous iteration
*   long        - longitude of the site.
*   h_i         - approximate height used in iteration
*   h_p         - approximate height from previous iteration
*   rad_lat     - radius to be used in computing the geodetic
*               - latitudes
*   rad_curve    - radius of curvature at the site (N above)
*   rot_mat(3,3)    - Transformation matrix XYZ and NEU where
*               - N is -colatitudes, E is longitude and U is
*               - along ellipsoidal height
*   site_pos(3) - XYZ coordinates of the site (m)

*   tolerance   - toleranace for convergence on geodetic
*               - coordinates.

      real*8 dlatdN, dNdlat, dXdlat, dYdlat, dZdlat, eccsq, equ_rad,
     .    geod_pos(3), lat_i, lat_p, long, h_i, h_p, rad_lat,
     .    rad_curve, rot_mat(3,3), site_pos(3), tolerance,
     .    earth_rad, earth_flat, pi

      integer*4 len,rcpar

      character*80 prog_name

C      The include file has been replaced with the following
C      parameters from /include/const_param.h
       parameter ( pi            = 3.1415926535897932D0 )
c*       parameter ( earth_flat    = 0.003352891869D0     )
c*       parameter ( earth_rad     = 6378145.D0           ) 
c* WGS-84 parameters for the elliposoid (a and 1/f)
      parameter ( earth_rad     = 6378137.D0           )
      parameter ( earth_flat    = 1.d0/298.257222101D0 )


*   niter       - Number of iterations.  EXITS if more than 50
*                 iterations are required.  (Removes problem if
*                 coordinates are NaN in IEEE floating point)

      integer*4 niter

*   converged   - Indicate values have converged.

      logical converged

      data
*                                     ! Converge to 0.1 mm
     .    tolerance  / 0.0001d0 /

***** Start, get the latitudes and height by iteration.

      equ_rad = sqrt( site_pos(1)**2 + site_pos(2)**2 )
      eccsq   = 2.d0*earth_flat - earth_flat**2

*                                            ! Set previous iteration values
      lat_p = atan2( site_pos(3), equ_rad)
      h_p   = 0.d0

      converged = .false.
      niter = 0

      do while ( .not. converged )

*         Get radius of curvature using previous latitudes estimate
          rad_curve = earth_rad /
     .               sqrt(1.d0 - eccsq*sin(lat_p)**2 )
          rad_lat  = equ_rad *
     .               ( 1.d0 - eccsq*rad_curve/(rad_curve+h_p) )

          lat_i = atan2( site_pos(3), rad_lat)

*                                          ! Use cos lat formula
          if( abs(lat_i).lt. pi/4 ) then
               h_i   = equ_rad/cos(lat_i) - rad_curve
*                                           ! Use sin lat formula
           else
               h_i   = site_pos(3)/sin(lat_i) - (1.d0-eccsq)*rad_curve
           end if


*         Check for convergence
          if( abs(h_i-h_p)              .lt. tolerance .and.
*                                                             ! Converged
     .        abs(lat_i-lat_p)*rad_curve.lt. tolerance ) then

              converged = .true.
          end if

*         Check for two many iterations
          niter = niter + 1
          if( niter.gt.50 ) then
c           get calling program name and m-file name for report_stat
            len = rcpar(0,prog_name)
            call report_stat('FATAL',prog_name,'lib/xyz_to_geod',' '
     .       ,'Convergence failure in computing geodetic coordinates',0)
          end if

*         Save the latest values
          h_p   = h_i
          lat_p = lat_i

*                     ! iterating for latitudes and height
      end do


***** Save the final values
      long = atan2( site_pos(2),site_pos(1) )

*                                     ! colatitudes
      geod_pos(1) = pi/2.d0 - lat_i
*                                     ! Add 2*pi
      if( long.lt.0 ) then
          geod_pos(2) = 2*pi + long
      else
          geod_pos(2) = long
      end if

      geod_pos(3) = h_i

***** Now do the transformation between XYZ and NEU

*     Compute derivative of the radius of curvature with to latitudes

      dNdlat = 2*earth_rad*eccsq *sin(lat_i)*cos(lat_i) /
     .         sqrt( (1.d0-eccsq* sin(lat_i)**2)**3 )

*     Now do NORTH component  (First do derivate wrt latitudes, then compute
*     the Northing derivative)

      dXdlat  = (dNdlat * cos(lat_i) -
     .                (rad_curve+h_i)* sin(lat_i) )*cos(long)
      dYdlat  = (dNdlat * cos(lat_i) -
     .                (rad_curve+h_i)* sin(lat_i) )*sin(long)
      dZdlat  = (1.d0-eccsq)*dNdlat         *sin(lat_i) +
     .             ((1.d0-eccsq)*rad_curve + h_i)*cos(lat_i)

      dlatdN  = sqrt(dXdlat**2 + dYdlat**2 + dZdlat**2 )

*     Now do rotation matrix

      rot_mat(1,1) = dXdlat / dlatdN
      rot_mat(1,2) = dYdlat / dlatdN
      rot_mat(1,3) = dZdlat / dlatdN


*     Now do EAST component
      rot_mat(2,1) = -sin(long)
      rot_mat(2,2) =  cos(long)
      rot_mat(2,3) =  0.d0

*     Now do UP component

      rot_mat(3,1) = cos(lat_i) * cos(long)
      rot_mat(3,2) = cos(lat_i) * sin(long)
      rot_mat(3,3) = sin(lat_i)

***** Thats all
      return
      end

