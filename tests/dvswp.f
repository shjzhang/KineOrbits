
CTITLE 'DVSWP'

      subroutine dvswp(v1, inc1, v2,inc2, iter)
c
c
*     Routine to swap elements in V1 and V2
*     THIS IS EXACTLY THE SAME ROUTINE AS DWSWP

c
*          inc1,inc2       - increment on V1, and v2
*   i               - loop counter
*   i1,i2           - position in V1 and v2
*   iter            - number of values to be searched
      integer*4 inc1,inc2, i, i1,i2, iter
*
c
C     real*8
c    .    v1(inc1,1),v2(inc2,1)     ! vector to be swapped
*   temp            - Copy for swapping
      real*8 v1(*), v2(*), temp
*
c
c
****  Loop swapping values
      i1 = 1
      i2 = 1
      do i = 1, iter
         temp = v1(i1)
         v1(i1) = v2(i2)
         v2(i2) = temp
         i1 = i1 + inc1
         i2 = i2 + inc2
c        temp = v1(1,i)
c        v1(1,i) = v2(1,i)
c        v2(1,i) = temp
      end do
c
***** Thats all
      return
      END

