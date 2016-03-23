      program kalman

      implicit double precision (a-h,o-z)
      double precision fi_x,fi_y,fi_z,fi_t
      parameter (nsat=32,nmax=nsat+4)
      dimension P((nmax**2+nmax)/2),AWA((nmax**2+nmax)/2),
     * xfi(nmax),Q(nmax),a(nmax),y(nmax),AWy(nmax),PIx(nmax),
     * x(nmax),iarc0(nmax)
      character*1 itype
      namelist /parameters/fi_x,fi_y,fi_z,fi_t,
     *  Pxx,Pyy,Pzz,Ptt,Qxx,Qyy,Qzz,Qtt

c =====================================================================
c    
c    cat file.dat --> |kalman| --->  XYZT: time dx dy dz dt 
c                        ^           BIAS: time b01 ... b32
c                        |                   
c           kalman.nml___|
c
c
c   -------------------------------------------------------------------
c
c   It applies to the linear system:  Y= A x, that is to say:
c
c  [  ]   [                                                     ] [dx ]
c  [  ]   [                                                     ] [dy ]
c  [  ]   [                                                     ] [dz ]
c  [  ] = [                                                     ] [dt ]
c  [yi]   [(x0-xs)/ro (y0-ys)/ro (z0-zs)/ro 1  0  ...  1  ...  0] [b1 ]
c  [  ]   [                                            ^        ] [...]
c  [  ]   [                                            |        ] [bk ]
c  [  ]   [                                            |        ] [...]
c  [  ]   [                                            |        ] [b32]
c                  coeff. for the "bk"             |
c                  if observation [yi] corresponds__|
c                  to the satellite with PRN=k
c
c
c
c    where:
c           [b1 ... bk ... b32] are the bias of the phase arcs
c                               for the satellites PRN01,...,PRN32  
c
c
c    being:
c                 [1/(sigma_y1)^2                       ]
c                 [     ...                             ]
c              W= [        1/(sigma_yi)^2               ]
c                 [                    ...              ]
c                 [                      1/(sigma_yn)^2 ]
c
c
c
c       [Pxx                    ]       [Qxx                    ] 
c       [   Pyy                 ]       [   Qyy                 ] 
c       [      Pzz              ]       [      Qzz              ] 
c   P0= [         Ptt           ]    Q= [         Qtt           ] 
c       [            Pb1        ]       [            Qb1        ]  
c       [               ...     ]       [               ...     ] 
c       [                  Pb32 ]       [                  Qb32 ]           
c
c
c
c
c       [fi_x                          ]    - If a cycle-slip is produced
c       [    fi_y                      ]      in the sat. PRN=k:
c       [        fi_z                  ]       fi_bk=0, Qbk= 9e16 m2
c   fi= [            fi_t              ]
c       [                fi_b1         ]    - If no cycle-slip is produced
c       [                     ...      ]       fi_bk=1, Qbk= 0
c       [                        fi_b32]   
c                                           * "iarc" allows us to identify
c                                              cycle-slips. 
c                                              If "iarc" changes=>cycle-slip
c
c
c
c    (See equations of Kalman filter in chapter 6)
c
c  ................................................................     
c   The values of [y(n),sigma_y,(x0-xs)/ro,(y0-ys)/ro,(z0-zs)/ro] 
c   are obtained from the file "file.dat":
c                                                                          
c
c   Example of file: file.dat
c  ---------------------------------------------------------------------
c  [itype time PRN y(n) sigma_y (x0-xs)/ro (y0-ys)/ro (z0-zs)/ro 1 iarc]
c                     (meters)  
c                    .....................
c    PC   900  03 5934.730 10.0 -0.557881  0.398805 -0.727820  1.0  1
c    LC   900  03 5935.241  0.1 -0.557881  0.398805 -0.727820  1.0  1    
c    PC   900  17 5939.028 10.0  0.058012  0.613973 -0.787191  1.0  2
c    LC   900  17 5938.107  0.1  0.058012  0.613973 -0.787191  1.0  2 
c    PC  1800  22 5933.606 10.0 -0.657670  0.369129 -0.656667  1.0  1
c    LC  1800  22 5932.513  0.1 -0.657670  0.369129 -0.656667  1.0  1     
c                    .....................
c
c  The values of Pxx, Pyy, Pzz, Ptt, Qxx, Qyy, Qzz, Qtt,
c  fi_x, fi_y, fi_z, fi_t, are established through the
c   namelist kalman.nml:
c                                                            
c  ... kalman.nml ...
c  $parameters
c    Pxx=1.d+8    
c    Pyy=1.d+8    (m2)
c    Pzz=1.d+8
c    Ptt=9.d+16
c    fi_x=1.d0     ctt ==> fi=1; Q=0
c    fi_y=1.d0     wn  ==> fi=0; Q=sigma**2  
c    fi_z=1.d0     rw  ==> fi=1; Q=sigma**2 * dt
c    fi_t=0.d0       
c    Qxx=0.d0
c    Qyy=0.d0     (m2)
c    Qzz=0.d0
c    Qtt=9.d+16  
c  $end   
c  .................  
c
c      @gAGE (Research group of Astronomy and GEomatics). UPC    
c ===================================================================


c   Initialization values..................     
          do i=1,nmax
           a(i)=0.d0
           x(i)=0.d0      
           y(i)=0.d0      
           AWy(i)=0.d0
           xfi(i)=0.d0
           Q(i)=0.d0 
           iarc0(i)=0
          enddo
          do i=1,(nmax**2+nmax)/2
           P(i)=0.d0
           AWA(i)=0.d0
          enddo                               
c   .......................................

        open (10,file="kalman.nml")          
        read (10,nml=parameters)    
        close(10)      

c   Kalman FILTER declaration matrix ...... 
c      State transition matrix.....
          xfi(1)=fi_x
          xfi(2)=fi_y        
          xfi(3)=fi_z
          xfi(4)=fi_t   
c      A priori covariance values (in meters)...
          P(1)=Pxx
          P(3)=Pyy
          P(6)=Pzz
          P(10)=Ptt      
c      Process noise matrix (in meters).......
          Q(1)=Qxx
          Q(2)=Qyy 
          Q(3)=Qzz 
          Q(4)=Qtt 
c      Arc bias:
          Pbias=9.d-16     
          Qbias=9.d+16 
c   ...........................................
        nvar=nmax
 
        do i=5,nvar
          P(i*(i+1)/2)=Pbias
          xfi(i)=1.d0
        enddo                              


c    BEGIN MAIN LOOP +++++++++++++++++++++++++++++++++++++++++++

c    :::Begin data loop ::::::::::::::::::::::::::::::::::::::::::
        nf=0
10      read (*,*,end=900) itype,tt,isat,yy,sigma_y,
     *                     (a(j),j=1,4),iarc    
        nf=nf+1   
        if (nf.eq.1) tt0=tt


c       Completing the Design Matrix ...............
        do i=5,nvar
          a(i)=0.d0
        enddo                       
        if (itype.eq."L") then        
          i=isat+4
          a(i)=1.d0         
          if (iarc.ne.iarc0(isat)) then
            xfi(i)=0.d0
            Q(i)=Qbias
          endif
          iarc0(isat)=iarc
c          print *, isat,Q(i),xfi(i),iarc,iarc0(isat)
        endif

        i=isat+4       
c       ............................................

c       ..........................................
        if (tt .gt. tt0) goto 200
25      continue     


c       -------------------------------------------
c       PREPARING  matrix and vector for ESTIMATION
c       Building the vector and matrix: ....... 
c             AWy:=A'(n)*W*Y(n)
c             AWA:=inv(P_(n))+A'(n)*W*A(n) 
c             (where W <--> 1/sigma_y**2)
c       .......................................

         do j=1,nvar
          AWy(j)=AWy(j)+a(j)*yy/sigma_y**2
          do i=1,j
            AWA(j*(j-1)/2+i)=AWA(j*(j-1)/2+i)+
     *      a(i)*a(j)/sigma_y**2
          enddo
         enddo
c       -------------------------------------------

        tt0=tt
        goto 10 

c   :::::::::::::::::::::::::::::::::::::::::::::End Data loop :::

200     continue       


c    ========================================================
c    BEGIN fordware propagation:

c       x:= x^_(n)=fi(n)*x^(n-1) ...........
         do i=1,nvar
          x(i)=x(i)*xfi(i)
         enddo
c       .................................
c
c       P:= P_(n)=fi(n)*P(n-1)*fi(n)'+Q(n) ................
         do i=1,nvar
          P(i*(i+1)/2)= P(i*(i+1)/2)*xfi(i)+Q(i)
         enddo
c       ............................................
c    END of fordware propagation.
c    ========================================================
c    ESTIMATION ==============================================               

c       P:=inv[P_(n)] ............
         call invsp(P,nvar,ier)
c       ..........................
c       PIx:=inv(P_(n))*x^_(n) ...........
         do i=1,nvar
          PIx(i)=0.d0
          do k=1,nvar
             if (k.lt.i) ik=k+i*(i-1)/2
             if (k.ge.i) ik=i+k*(k-1)/2
             PIx(i)=PIx(i)+P(ik)*x(k)
          enddo
         enddo
c      -------------------------------------------------------                          
c      P(n)=inv[inv(P_(n))+A'(n)*W(n)*A(n)]==>  P:=inv[P + AWA]
        do i=1,nvar*(nvar+1)/2
         P(i)=P(i)+AWA(i)
        enddo
        call invsp(P,nvar,ier0) 
c       ...............................................

c      x^(n)=P(n)*[inv(P_(n))*x^_(n)+A'(n)*W(n)*Y(n)] 
c      ==> x:=P(n)*[PIx + AWy]  

        do i=1,nvar
         y(i)=PIx(i)+AWy(i)
        enddo

        do i=1,nvar
          x(i)=0.d0
          do k=1,nvar
             if (k.lt.i) ik=k+i*(i-1)/2
             if (k.ge.i) ik=i+k*(k-1)/2
             x(i)=x(i)+P(ik)*y(k)
          enddo
        enddo
c       ..................................                        
c    End estimation 
c    ========================================================
c    ...PRINT THE KALMAN ESTIMATION...........................
        write(*,'(a4,1x,f8.2,4(1x,f10.5))') "XYZT",tt0,(x(i),i=1,4)
        write(*,'(a4,1x,f8.2,32(1x,f8.3))') "BIAS",tt0,(x(i),i=5,nvar)  
c    ........................................................


c       Reinitializing variables for the next iteration ...........
        tt0=tt
        do i=5,nvar
          xfi(i)=1.d0
          Q(i)=0.d0
        enddo     
        do i=1,nvar 
         AWy(i)=0.d0
        enddo
        do i=1,nvar*(nvar+1)/2
         AWA(i)=0.d0
        enddo                     
        goto 25
c       .......................................................
c  +++++++++++++++++++++++++++++++++++++++++++++++ END of Main LOOP

900     continue
        end

c -----------------------------------------------------------------

        subroutine invsp(A,n,ier)

c -----------------------------------------------------------
c       It calculates the INVERSE of a MATRIX nxn SIMETRIC
c       AND is POSITIVE DEFINED (the matrix must be vectorized
c       -as simetric- by columns) If the matrix is not positive
c       defined, the calculation  stops and gives an error
c       output: ier=1.
c       NOTE: the matrix A is replaced by its inverse.
c
c       NOTICE that
c        the inverse of a generic matrix can be calculated by:
c        inv(A)=inv(A'*A)*A' (where A'*A is sim. and pos. def.)
c
c      @gAGE (Research group of Astronomy and GEomatics). UPC
c ------------------------------------------------------------

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION a(*)

c       CHOLESKY decomposition  (A=T'*T)

        call chol(A,n,ier)
c       print *, "CHOLESKY", (a(i),i=1,20)

c       Inverse of the triangular matrix of Cholesky  {inv(T)}
        do 100 l=1,n
        i=n-l+1
        a(i+i*(i-1)/2)=1.d0/a(i+i*(i-1)/2)
        do 110 ll=i+1,n
        j=n-ll+i+1
        s=0.d0
        do 120 k=i+1,j
        s=s+a(i+k*(k-1)/2)*a(k+j*(j-1)/2)
120     continue
        a(i+j*(j-1)/2)=-s*a(i+i*(i-1)/2)
110     continue
100     continue


c       Inverse of the matrix A   {inv(A)=inv(T)*inv(T)'}
        do 200 i=1,n
        do 210 j=i,n
        s=0.d0
        do 220 k=j,n
        s=s+a(i+k*(k-1)/2)*a(j+k*(k-1)/2)
220     continue
        a(i+j*(j-1)/2)=s
210     continue
200     continue

        return
        end

c ------------------------------------------------------------

        subroutine chol(A,n,ier)

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION a(*)

c -----------------------------------------------------------
c       It calculates the CHOLESKY decomposition of a matrix
c       A nxn simetric and positively defined: (A=T'*T)
c       --In the output, A is replaced by T-- (the matrix must
c       be vectorized --as simetric-- by columns). If the
c       matrix is not positively defined, the calculation stops
c       and an error output is produced: ier=1 (or  ier=2 if
c       its determinat is null).
c
c
c      @gAGE (Research group of Astronomy and GEomatics). UPC
c ------------------------------------------------------------
        ier=0
        do 30 i=1,n
        do 20 j=i,n
        sum=a(i+j*(j-1)/2)
        do 10 k=i-1,1,-1
        sum=sum-a(k+i*(i-1)/2)*a(k+j*(j-1)/2)
10      continue
        if (i.eq.j) then
          if(sum.le.0.d0) then
                ier=1
                if (sum.eq.0) ier=2
                goto 100
          endif
          a(i+i*(i-1)/2)=dsqrt(sum)
          else
          a(i+j*(j-1)/2)=sum/a(i+i*(i-1)/2)
        endif
20      continue
30      continue

100     return
        end
c ------------------------------------------------------------


c       f77 -o kalman kalman.f
