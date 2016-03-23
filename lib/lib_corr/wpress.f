      real*8 function wpress(rh,t)
c
c     Returns the partial pressure of water vapor, in mbar
c
c     INPUT:
c       RH     Relative humidity (0-1)
c       T      Temperature, deg C
c
      real*8 rh, t
c
      wpress = rh * 6.11D+00
     .            * 10.0D+00 ** (7.5D+00 * t / (t + 2.373D+02))
c
      end
