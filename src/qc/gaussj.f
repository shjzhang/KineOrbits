*
*  Procedure gaussj
*
      subroutine gaussj(a, n, b)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  solve linear equation with GAUSS method                   
*
*  Input_Output_Auguments
*  ======================
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  a              R       O       angle
*  n              I       I       radian
*  b              R       O       solution
*
*  Notes
*  =====
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
c   
      implicit real*8(a-h, o-z)
c        
      integer   nmax
      parameter(nmax = 50)
***
      integer   n
***
      real*8    a(n,n), b(n)
c        
      integer i,icol,irow,j,k,l,ll,indxc(NMAX)
      integer indxr(NMAX),ipiv(NMAX)
      real*8  big,dum,pivinv
      do j=1,n
        ipiv(j)=0
      end do
      do i=1,n
        big=0.
        do j=1,n
          if(ipiv(j)/=1) then
            do k=1,n
              if (ipiv(k)==0) then
                if (abs(a(j,k))>=big) then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k)>1) then
                stop 'singular matrix in gaussj'
              endif
            end do
          endif
        end do
        ipiv(icol)=ipiv(icol)+1
        if (irow/=icol) then
          do l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
          end do
          dum=b(irow)
          b(irow)=b(icol)
          b(icol)=dum
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol)==0.) stop 'singular matrix in gaussj'
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do l=1,n
          a(icol,l)=a(icol,l)*pivinv
        end do
        b(icol)=b(icol)*pivinv
        do ll=1,n
          if(ll/=icol) then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
            end do
            b(ll)=b(ll)-b(icol)*dum
          endif
        end do
      end do
      do l=n,1,-1
        if(indxr(l)/=indxc(l)) then
          do k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
          end do
        endif
      end do
c
      return
c
      end

