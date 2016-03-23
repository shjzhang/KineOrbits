        program test_xyz2geod
c
        real*8  sit_pos(3)
        real*8  sit_geod(3)
        real*8  pi
c
        integer i
c
        pi = 3.141592653589793
c
        sit_pos(1) = 4696990.0d0
        sit_pos(2) = 723994.0d0
        sit_pos(3) = 4239678.0d0
c
        call xyz2geod(sit_pos,sit_geod)
c
        write(*,*) sit_geod(1)*180.0d0/pi
        write(*,*) sit_geod(2)*180.0d0/pi
        write(*,*) sit_geod(3)
c
        end
