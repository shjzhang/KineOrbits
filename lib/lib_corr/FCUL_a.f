C$fcula
 
      real*8 FUNCTION  fcula (latitude,height_m,T_K,elev_deg)
 
* new vbm 31 Mar 2000 compute global total FCULa mapping function.
* function dependent on latitude, height, and surface temperature

* Ref. Mendes, V.B., G. Prates, E.C. Pavlis, D.E. Pavlis, 
*      and R.B. Langley (2002). "Improved mapping functions for
*      atmospheric refraction correction in SLR", Geophysical 
*      Res. Lett., 29(10), 1414, doi:10.1029/2001GL014394, 2002.
*
*     input:
*          latitude     - latitude (degrees)
*          height_m     - height (metres)
*          T_K          - temperature (kelvin)
*          elev_deg     - elevation angle (degrees)
*          
*     local:
*          epsilon      - elevation angle (radians)
*          T_C          - temperature (Celcius)
*          sine         - sin(elevation angle)
*          cosphi       - cos (latitude)
*
*     output:
*          fcula        - mapping function to scale total delay
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
      real*8 height_m, latitude, T_K, T_C
      real*8 cosphi
      real*8 a10, a11, a12, a13
      real*8 a20, a21, a22, a23
      real*8 a30, a31, a32, a33
 
      PARAMETER (pi=3.141592654d0)

*     conversions
      epsilon = elev_deg * (pi/180.0)
      sine    = dsin(epsilon)
      T_C     = T_K - 273.15d0
      cosphi  = dcos (latitude*(pi/180.0))

*         coeficients for the model

      a10 =  0.121008D-02
      a11 =  0.17295D-05
      a12 =  0.3191D-04
      a13 = -0.18478D-07
*
      a20 =  0.304965D-02
      a21 =  0.2346D-05
      a22 = -0.1035D-03
      a23 = -0.1856D-07

*
      a30 =  0.68777D-01
      a31 =  0.1972D-04
      a32 = -0.3458D-02
      a33 =  0.1060D-06

*     a, b, and c in Marini continued fraction (Eq. 5)
      a1 = a10+a11*T_C+a12*cosphi+a13*height_m 
      a2 = a20+a21*T_C+a22*cosphi+a23*height_m 
      a3 = a30+a31*T_C+a32*cosphi+a33*height_m 

*     numerator in continued fraction
      map_zen   = (1.0d0 + a1/(1.0d0 + a2/(1.0d0+a3)))
*
      fcula = map_zen/(sine+a1/(sine+a2/(sine+a3)))
 
      RETURN
      END
 