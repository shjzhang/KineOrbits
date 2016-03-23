      real*8 function saaszd(p,t,wetvar,h2otyp,phi,h)
c
c     Calculates zenith delay in meters.   

c     Ref: Saastamoinen, J. Atmospheric correction for the troposphere and stratosphere
c          in radio ranging of satellites, in "The Use of Artificial Satellites for
c          Geodesy. Mongr., Ser., Vol. 15, edit S. w. Henriksen et al., pp. 247-251, 
c          American Geophysical Union, Washington, D.C., 1972
c
c     INPUT:
c       P        Total pressure, mbars
c       T        Temperature, deg C
c       WETVAR   Water vapor variable, defined by H2OTYP
c       H2OTYP   Defines WETVAR.  H2OTYP = 'R' indicates that WETVAR is relativ
c                humidity (0-1).  H2OTYP = 'D' indicates that WETVAR is the dew
c                point temperature (deg C).
c       PHI      Geocentric latitude, radians
c       H        Elevation above the geoid, km
c
      real*8 p, t, wetvar, phi, h, e, ffun, wpress, tk
c
      character*1 h2otyp,UPPERC
c
c.... Calculate the partial pressure of water vapor, in mbars
      if (h2otyp .eq. UPPERC('R')) then
        e = wpress(wetvar,t)
      else
        e = wpress(1.0D0,wetvar)
      end if
c
c.... Temperature in Kelvin
      tk = t + 273.15
c
c.... Zenith delay, meters
      saaszd = 0.2277D-02 * (p + (.1255D+04 / tk + .5D-01) * e)
     .       / ffun(phi,h)
c
      end
