

CTITLE 'dvdot'

      subroutine dvdot(r, v1, inc1, v2,inc2, iter)
c
c     Routine for a double precision dot product.  Result returned
c     in r

c
*   inc1,inc2       - increment on V1, and v2
*   i               - loop counter
*   i1,i2           - position in V1 and v2
*   iter            - number of values to be searched

      integer*4 inc1,inc2, i, i1,i2, iter
*
c
c   v1(inc1,1),v2(inc2,1)     ! vectors to be dotted
c   r   -  result of dot product
      real*8 v1(*), v2(*), r
*
c
c
****  Loop swapping values
      i1 = 1
      i2 = 1
      r = 0.d0
      do i = 1, iter
         r = r + v1(i1)*v2(i2)
         i1 = i1 + inc1
         i2 = i2 + inc2
      end do
c
***** Thats all
      return
      END

