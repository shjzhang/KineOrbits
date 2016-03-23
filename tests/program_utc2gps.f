        program program_utc2gps
c
        implicit none
c
        real*8  utc2gps
        real*8  jd2sec
c
        real*8  jd
        real*8  utc
        real*8  gps
c
 100    continue
c
        read(*,*,end=200) jd
c
        utc=jd2sec(jd)
c
        gps=utc2gps(utc)
c
        write(*,*) gps
c
        goto 100
c
 200    continue
c
        return
c
        end
