C
      SUBROUTINE PLANET_STATE(ET,NTARGET,NCENTER,RELATIVE_STATE)
C
      IMPLICIT NONE
c
      include '../include/IERS.h'
C
      real*8   ET,RELATIVE_STATE(6)
      INTEGER  NTARGET,NCENTER
c
      real*8   R,V
      integer  k
C

C     1 = MERCURY           8 = NEPTUNE
C     2 = VENUS             9 = PLUTO
C     3 = EARTH            10 = MOON
C     4 = MARS             11 = SUN
C     5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
C     6 = SATURN           13 = EARTH-MOON BARYCENTER
C     7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)
C                          15 = LIBRATIONS, IF ON EPH FILE
c
      ET = 2451604.5D0
      NTARGET = 11
      NCENTER = 3

c
c     IF ( NCENTER .NE. 3) THEN
c         WRITE(*,*)'WARNING : ARE YOU SURE NOT FOR EARTH REFERENCE ?'
c     END IF

      IF ( NCENTER .GT. 15) THEN
          STOP ' UNSUPPORTED EPH IN JPL EPH'
      END IF
c
      CALL PLEPH ( ET, NTARGET, NCENTER, RELATIVE_STATE)

c     [units: km]
      RELATIVE_STATE(1) = RELATIVE_STATE(1) * AU
      RELATIVE_STATE(2) = RELATIVE_STATE(2) * AU
      RELATIVE_STATE(3) = RELATIVE_STATE(3) * AU

c
c     [units: km/s]
      RELATIVE_STATE(4) = RELATIVE_STATE(4) *(AU/(24*3600.0d0))
      RELATIVE_STATE(5) = RELATIVE_STATE(5) *(AU/(24*3600.0d0))
      RELATIVE_STATE(6) = RELATIVE_STATE(6) *(AU/(24*3600.0d0))
C
      RETURN
C
      END
