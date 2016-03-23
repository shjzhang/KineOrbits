       program xyz_orbit
c-----------------------------------------------------------------------------
c the subroutine can be used for convertting the cartesian coordinates
c on inertial system (ECI, Earth-centered Inertial) together with Julian
c date of Siderial Time to coordinate in local orbital coordinates (LOC)
c-----------------------------------------------------------------------------
c   variables:
c       gm:     the gravitation constant
c       omega:  the rotation veloctiy of the Earth
c       ti:     the Julian date of Siderial Time
c       xyz:    the location vector on ECI
c       vxyz:   the velocity vector on ECI
c       ra,vincli,omi0,omie:  the coordiantes on LOC corresponding to
c               the radial radius, inclination, argument of ascending
c               node, longtitude of ascending node
c       arealv: the areal velocity vector 
c       uareal: the normalized areal velocity vector
c       asan  : the right ascension of the ascending node
c       arealn: the norm of the areal velocity vector
c       srectum:the semi-latus rectum
c       ve:     the norm of the velocity vector
c       saxis:  the semi-major axis
c       vmm:    the mean motion
c       ecce:   the eccentricity
c       anome:  the eccentric anomaly
c       anomlm: the mean anomaly
c       arguf:  the argument of latitude
c       anomlt: the true anomaly
c       argup:  the argument of perigee
c       t_sidal:the Siderial Time
c-----------------------------------------------------------------------------
c   written by guangbin zhu, December, 2009
c-----------------------------------------------------------------------------
      implicit none

        real*8    a1,a2,a3,b1,b2,b3
        real*8    gm, saxis, xyz, vxyz

        gm = 3.9860044150d14
         

        a1=10000.d3
        a2=40000.d3
        a3=-5000.d3
        b1=-1.5d3
        b2=1.d3
        b3=-0.1d3

        xyz = dsqrt(a1**2+a2**2+a3**2)
        vxyz = (b1**2+b2**2+b3**2)
        saxis   = 1.d0 / (2.d0/xyz-vxyz/gm)
        write(*,*) xyz, vxyz, gm
c
        write(*,"(f22.12)")saxis

        saxis   = dsqrt(a1**2+a2**2+a3**2)*gm/(2.d0*gm-
     $            dsqrt(a1**2+a2**2+a3**2)*(b1**2+b2**2+b3**2))
        write(*,"(f22.12)")saxis

      return
      end
