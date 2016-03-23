C$FCULZD_HPA
      subroutine fculzd_hPa(latitude, ellip_ht, pressure, wvp,
     .                  lambda_um, fcul_ztd, fcul_zhd, fcul_zwd)

C********1*********2*********3*********4*********5*********6*********7**
C FCULZD_HPA              06/29/04            0406.29   PGMR - V. MENDES
C
C FUNCTION:
C This routine calculates the Mendes-Pavlis zenith total delay,
C for optical wavelengths, valid for infrared to ultraviolet.
C
C I/O PARAMETERS:
C
C   NAME    I/O  A/S   DESCRIPTION OF PARAMETERS
C   ------  ---  ---   ------------------------------------------------
C  latitude  I          - latitude (degrees)
C  ellip_ht  I          - height (metres)
C  pressure  I          - surface pressure (hPa) (mbars)
C  wvp       I          - water vapor pressure (hPa) (mbars)
C  lambda_um I          - wavelength (micrometers)
C  fcul_ztd  O          - zenith total delay (m)
C  fcul_zhd  O          - zenith hydrostatic (dry) delay (m)
C  fcul_zwd  O          - zenith wet delay (m)
C
C COMMENTS:
C
C********1*********2*********3*********4*********5*********6*********7**

*     Creation: 29June2004, V.B. Mendes, for the RSG of the ILRS

*      This subroutine computes the zenith total delay, for optical 
*      wavelengths
*      For details see: Mendes, V. B. and E. C. Pavlis (2004),
*                       High-accuracy zenith delay prediction at
*                       optical wavelengths. Geophysical Research
*                       Letters, Vol. 31, doi:10.1029/2004GL020308.
*
*      Input parameters (note that pressures are given in hectoPascal units):
*          latitude  - geodetic latitude (degree)
*          ellip_ht  - height above ellipsoid (meter)
*          pressure  - surface pressure (hPa, i.e. mbars)
*          wvp       - water vapor pressure (hPa, i.e. mbars)
*          lambda_um - laser wavelength (micrometers)
*      Local parameters:
*          sigma     - wave number (1/lambda_um)
*          xc        - CO2 content, in ppm
*          c         - speed of light in vacuum (m/s)
*          pi        - pi (radians)
*      Output parameters:
*          fcul_ztd  - Zenith total delay (m)
*          fcul_zhd  - zenith hydrostatic delay (m)
*          fcul_zwd  - zenith non-hydrostatic delay (m)          
*
      implicit none
      real*8 c, pi, ellip_ht, latitude, f, pressure, wvp
      real*8 k0, k1, k2, k3
      real*8 xc, corr
      real*8 sigma, w0, w1, w2, w3, fh, fnh
      real*8 lambda_um
      real*8 fcul_zhd, fcul_zwd, fcul_ztd

      parameter (c = 2.99792458d8)  ! speed of light, m/s
      parameter (pi = 3.141592654) 
*
      xc = 375.0d0
*         constant values to be used in Equation (20)
*         k1 and k3 are k1* and k3* 
      k0 = 238.0185d0
      k1 = 19990.975d0   
      k2 = 57.362d0
      k3 = 579.55174d0

*         constant values to be used in Equation (32)
      w0 = 295.235d0
      w1 = 2.6422d0
      w2 = -0.032380d0
      w3 = 0.004028d0
*         
      sigma = 1/lambda_um   

*     correction factor - Equation (24)
      f = 1 - 0.00266*cos(2*pi/180*latitude) - 0.00028d-3*ellip_ht

*     correction for CO2 content
      corr = 1.0d0 + 0.534d-6*(xc-450)

*     dispersion equation for the hydrostatic component - Equation (20)
      fh = 0.01d0*corr*((k1*(k0+sigma**2))/((k0-sigma**2)**2) +
     .     k3*(k2+sigma**2)/((k2-sigma**2)**2))

*     computation of the hydrostatic component - Equation (26)
*     caution: pressure in hectoPascal units
      fcul_zhd = 2.416579d-3*fh*pressure/f

*     dispersion equation for the non-hydrostatic component - Equation (32)
      fnh = 0.003101d0*(w0+3.0*w1*sigma**2 +
     .      5.0*w2*sigma**4+7.0*w3*sigma**6)

*     computation of the non-hydrostatic component - Equation (38)
*     caution: pressure in hectoPascal units
      fcul_zwd = 1.d-4*(5.316d0*fnh-3.759*fh)*wvp/f

*      compute the zenith total delay
      fcul_ztd = fcul_zhd + fcul_zwd

      return
      end

