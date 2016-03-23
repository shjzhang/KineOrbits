      program state2element_test
c
      real*8 y(6)
      real*8 element(6)
      integer   i
c
      y(1) = 10000.0d3
      y(2) = 40000.0d3
      y(3) = -5000.0d3
c
      y(4) = -1.5d3
      y(5) = +1.0d3
      y(6) = -0.1d3
c
      call state2element(y,element)
c
      write(*,*) (element(i),i=1,6)
c
      end
