c
c
c
      program calc
c
      implicit none
c
      real*8    RMS(7,4)
c
      integer i, j
c
      RMS(1,1) = 3.848
      RMS(1,2) = 4.300
      RMS(1,3) = 3.366
      RMS(2,1) = 3.717
      RMS(2,2) = 3.761
      RMS(2,3) = 3.901
      RMS(3,1) = 4.764
      RMS(3,2) = 4.786
      RMS(3,3) = 2.584
      RMS(4,1) = 4.493
      RMS(4,2) = 5.494
      RMS(4,3) = 3.638
      RMS(5,1) = 5.211
      RMS(5,2) = 5.674
      RMS(5,3) = 4.484
      RMS(6,1) = 5.451
      RMS(6,2) = 4.565
      RMS(6,3) = 3.168
      RMS(7,1) = 4.939
      RMS(7,2) = 5.492
      RMS(7,3) = 3.915
c
      do i=1, 7
        RMS(i,4) = sqrt(RMS(i,1)**2+RMS(i,2)**2+RMS(i,3)**2)
      enddo
c
      do i=1, 7
      write(*,'(4f8.3)') (RMS(i,j),j=1,4)
      enddo
c
      end
