      subroutine swap_bytes( size, var, num )

*     Routine to swap bytes in an array to allows conversion between
*     Linux and Sun/HP binaries.

* PASSED VARIABLES

* size -- Length of words in bytes (eg. integer*4, real*4 = 4;
*     real*8 = 8)
* num  -- Number of values in array.

      integer*4 size, num 

* var -- Variable to be byte swapped

      byte var(size,num)

* LOCAL VARIABLES

*  i, j -- Loop counters over bytes and array.
*  swap -- Array to save the swapped bytes, upto 16 bytes in a word
*          allowed.

      integer*4 i, j

      byte swap(16)


****  Loop over the array elements
      do i = 1, num

****     Loop over bytes 
         do j = 1, size
            swap(j) = var(j,i)
         end do

*        Now copy back the swapped version
         do j = 1, size
            var(j,i) = swap(size+1-j)
         end do
      end do

***** Thats all
      return 
      end





