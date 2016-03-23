*
*  procedure sort
*   
      subroutine sort(N,A)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  sort the input data in array A with selection sort method, and returned
*  in array B                    
*
*  Auguments
*  =========
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  A            I       I/O     input array
*  N            I       I       input array dimension
*
*
*  History
*  =======
*
*  Vesion 1.0
*    
*  Time         Author          Description
*  ----         ------          ----------------------------------------
*  06/09/16     S.J.Zhang       build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
*
*     Variables
*         
      implicit  none
c
c     Input/Output variables
c
      integer     N
      integer     A(N)
c
c     Local Variables
c
      integer     B(N)
c
      integer     i, j
      integer     min
      integer     tmp
c
c     Initialization
c
      do i=1, N
        B(i) = A(i)
      enddo
c
c     Sort with selection method
c
      do i=1, N
        min = B(i)
        do j=i+1, N
          if(min.gt.A(j))then
            tmp  = B(j)
            B(j) = B(i)
            B(i) = tmp
            min  = B(i)
          endif
        enddo
      enddo 
c
      do i=1, N
         A(i) = B(i)
      enddo
c 
      return
c
      end
