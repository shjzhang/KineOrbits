C$fculb
 
      real*8 FUNCTION  fculb(latitude,height_m,doy,elev_deg)
 
* new vbm 31 Mar 2000 compute global total FCUL mapping function
*       no dependence on meteorological data

* Ref. Mendes, V.B., G. Prates, E.C. Pavlis, D.E. Pavlis, 
*      and R.B. Langley (2002). "Improved mapping functions for
*      atmospheric refraction correction in SLR", Geophysical 
*      Res. Lett., 29(10), 1414, doi:10.1029/2001GL014394, 2002.
*
*     input:
*          latitude     - latitude (degrees)
*          height_m     - height (metres)
*          doy          - day of year
*          elev_deg     - elevation angle (degrees)
*          
*     local:
*          epsilon      - elevation angle (radians)
*          sine         - sin(elevation angle)
*          cosphi       - cos (latitude)
*          cosdoy       - cos (doy)
*
*     output:
*          fculb       - mapping function to scale total delay
*
*    These coefficients are based on a LS adjustment of 87766 (cleaned)
*    set of traces, based on Ciddor routines to compute refractivity,
*    according to IUGG recommendations (1999).
*    Questions and comments: vmendes@fc.ul.pt

      implicit none
      real*8 elev_deg
      real*8 epsilon
      real*8 pi, sine
      real*8 a1, a2, a3
      real*8 map_zen
      real*8 height_m, latitude
      real*8 cosphi , cosdoy, doy, doy_c
      real*8 a10, a11, a12, a13, a14
      real*8 a20, a21, a22, a23, a24
      real*8 a30, a31, a32, a33, a34
 
      PARAMETER (pi=3.141592654d0)

*     conversions
      epsilon = elev_deg * (pi/180)
      sine    = dsin(epsilon)
*     add 182.5 to account for southern hemisphere
      if (latitude.lt.0.0d0) then
        doy_c   = doy + 365.25/2.0
        else 
        doy_c   = doy
      end if
      cosdoy = dcos ((doy_c - 28.0) * 2.0 * pi / 365.25)
      cosphi = dcos (latitude*(pi/180.0))

*         coeficients for the model

      a10 =  0.116131D-02
      a11 = -0.9338D-5
      a12 = -0.5958D-8
      a13 = -0.24627D-07
      a14 =  0.12864D-03
*
      a20 =  0.298151D-02
      a21 = -0.569D-05
      a22 = -0.1655D-07
      a23 = -0.2725D-07
      a24 =  0.3020D-04
*
      a30 =  0.681839D-01
      a31 =  0.935D-04
      a32 = -0.2394D-06
      a33 =  0.304D-07
      a34 = -0.2308D-02

*     a, b, and c in Marini function
      a1 = a10 + a11*cosdoy + a12*latitude**2*cosdoy
     .   + a13 * height_m + a14*cosphi
      a2 = a20 + a21*cosdoy + a22*latitude**2*cosdoy
     .   + a23 * height_m + a24*cosphi
      a3 = a30 + a31*cosdoy + a32*latitude**2*cosdoy
     .   + a33 * height_m + a34*cosphi

*     numerator in continued fraction
      map_zen   = (1.0d0 + a1/(1.0d0 + a2/(1.0d0+a3)))
*
      fculb = map_zen/(sine+a1/(sine+a2/(sine+a3)))
 
      RETURN
      END
 