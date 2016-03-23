      real*8 function ffun(phi,h)
c
c     Computes the ellipsoidal/elevation-dependent variation of the
c     average acceleration of gravity from Saastamoinen model.
c
c     INPUT:
c       PHI    Geocentric latitude, radians
c       H      Elevation of site above geoid, m
c
      real*8 phi, h
c
c     convert meters to kilometers
      h = h / 1000.0d0
c
      ffun = 1.0D+00
     .     - 0.266D-02 * cos(2.0D0 * phi)
     .     - 0.28D-03 * h
c
c.... The following was used in the old ATMDEL
c      ffun = 1.0D+00
c     .     - 0.26D-02 * (1.0D+00 - 2.0D+00 * sin(phi) ** 2)
c     .                           - 0.31D-03 * h
c
      end
