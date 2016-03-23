      subroutine polyfit(N, x, y, order, coeff)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*   
*  polynomial fit 
*
*  Input_Output Auguments
*  ======================
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  nprn         I       I       PRN number
*  nrec         I       I       Total record number for nprn
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
      implicit real*8(a-h, o-z)
c
c  Declaration_of_the_Input_Output_Variables
c
c     Input_Parameters
c
      integer   N, order
***
      real*8    x(*), y(*)
c
c     Ouput_Parameters
c
      real*8    coeff(*)
      real*8    sigma
c
c  Declaration_of_Local_Variables
c
      real*8    B(N, order+1)
      real*8    BT(order+1, N)
      real*8    Nbb(order+1, order+1)
      real*8    w(order+1)
      real*8    mean, std, xx(N)
c
c  centering and scaling transformation improves the numerical
c  properties of both the polynomial and the fitting algorithm.
c
      mean = 0.0
      do i=1, N
        mean = mean + x(i)
      enddo
c
      mean = mean/dfloat(N);
c
      std = 0.0
      do i=1, N
        std = std + (x(i)-mean)**2
      enddo
c
      std = dsqrt(std/dfloat(N-1))
c
      do i=1, N
        xx(i) = (x(i)-mean)/std 
      enddo
c
c  y=a+b*xx+c*xx^2
c
      do i=1, N
        do j=1, order+1
          if(j==1)then
             B(i,j) = 1
          else
             B(i,j) = xx(i)**(j-1)
          endif
        enddo
      enddo
c
c  transpose
c
      do i=1, N
        do j=1, order+1
          BT(j,i) = B(i,j)
        enddo
      enddo
c
c  Nbb, w
c
      do i=1, order+1
        do j=1, order+1
          Nbb(i,j) = 0
          w(i) = 0.0d0
        enddo
      enddo
c
c  design matrix calculation or normal eqution
c
c     y=Ac, B*P*B^T*c=B*P*y
c           Nbb = B*P*B^T
c           w   = B*P*y
c     here, P is set identical matrix
c
      do i=1, order+1
         do j=1,N
            w(i) = w(i) + BT(i,j)*y(j)
         enddo
      enddo
c
      if(N.gt.order)then
        do i=1, order+1
           do j=1, order+1
              do k=1, N
                Nbb(i,j) = Nbb(i,j) + BT(i,k)*B(k,j)
              enddo
           enddo
        enddo
      else
        write(*,*) "not enough obserbales!"
        write(*,*) "please modify order in predit.h!!"
        stop "abort"
      endif 
c
c call gaussj
c
      call gaussj(Nbb, order+1, w)
c
c coeff
c
      do i=1, order+1
         coeff(i) = 0.0
      enddo
c
      do i=1, order+1
        coeff(i) = w(i)
      enddo 
c
      return
c
      end
c
c  Procedure polyval
c
      subroutine polyval(N, x, order, coeff, py)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*   
*  polynomial fit 
*
*  Input_Output Auguments
*  ======================
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  nprn         I       I       PRN number
*  nrec         I       I       Total record number for nprn
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
      implicit real*8(a-h, o-z)
c
c  Declaration_of_the_Input_Output_Variables
c
c     Input_Parameters
c
      integer   N, order
***
      real*8    x(*), py(*)
      real*8    coeff(*)
c
c  Declaration_of_Local_Variables
c
      integer   i, j
      real*8    mean, std, xx(N)
c
c  centering and scaling transformation improves the numerical
c  properties of both the polynomial and the fitting algorithm.
c
      mean = 0.0
      do i=1, N
        mean = mean + x(i)
      enddo
c
      mean = mean/dfloat(N);
c
      std = 0.0
      do i=1, N
        std = std + (x(i)-mean)**2
      enddo
c
      std = dsqrt(std/dfloat(N-1))
c
      do i=1, N
        xx(i) = (x(i)-mean)/std 
      enddo
c
      do i=1, N
        py(i) = 0.0
        do j=1, order+1
          if(j==1)then
             py(i) = coeff(1)
          else
             py(i) = py(i) + xx(i)**(j-1)*coeff(j)
          endif
        enddo
      enddo
c
      return
c
      end

