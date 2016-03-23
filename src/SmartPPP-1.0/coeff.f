*
*  subroutine coeff
*      
      subroutine coeff(xrcv,xtrs,iobs,iepo,idx_iamb, 
     &                 NNZA,NRA,RA,CA,A)
c
c=======================================================================
c     ****f* SmartPPP/coeff.f
c
c   FUNCTION   
c   
*     coeff of the observation equation
c
c   INPUTS
c
c     xrcv        real*8           coordinates
c     xtrs        real*8           coordinates
c     iobs        integer          observable number
c     iepo        integer          epoch number
c     isat        integer          satellite number
c     iamb_isat   integer          ambiguity number for iobs
c
c   OUTPUT
c
c      A          real*8           coefficent with CSR format
c     CA          integer          column index of A, CSR format
c     RA          integer          row index for A and CA
c     NNZA        integer          Number of Non-Zero of A
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/03                   build
c
c     ***
c
C     $Id: coeff.f.f,v 1.0 2009/08/10 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
c     input/output
c
      real*8        xrcv(3)
      real*8        xtrs(3)
      real*8        A(MAX_NNZA)
c
      integer       iobs, iepo
      integer       idx_iamb
c
      integer       NRA, CA(MAX_NNZA), RA(MAX_NRA+1)
      integer       NNZA
c
c     local
c
      real*8        dis
      real*8        drou_dx,drou_dy,drou_dz,drou_dT,drou_dN,drou_dTrop
c
c     loop
c
      integer       i,j,k
c
c     distance from receiver to transmitter
c
      call distance(xrcv,xtrs,dis)
c
c     partial derivative with respect to the receiver
c
      drou_dx    = -(xtrs(1)-xrcv(1))/dis
      drou_dy    = -(xtrs(2)-xrcv(2))/dis
      drou_dz    = -(xtrs(3)-xrcv(3))/dis
c
c     partial derivative with respect to the receiver clock
c
      drou_dT    = 1
c
c     partial derivative with respect to the wet tropspheric
c
      drou_dTrop = 1
c
c     partial derivative with respect to ambiguity parameters
c
      drou_dN    = 1
c
c++   SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
c**      Model1: traditional L3,P3
         if(    observ_model.EQ.1)then
c
c           partial derivatives for P3
c
c           row no. of the coefficent matrix
            NRA      = (iobs-1)*2+1
            RA(NRA)  =  NNZA + 1
c
c&&         partial derivatives of (x,y,z) of the GPS receiver
c
            do i=1, 3
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) ' SmartPPP/coeff'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = -(xtrs(i)-xrcv(i))/dis
            CA(NNZA) =  (iepo-1)*4+i
c
            enddo
c
c&&         partial derivatives of clock parameters
c
c           increase the total nonzero elements of array a
            NNZA     = NNZA + 1
c
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) =  drou_dT
            CA(NNZA) = (iepo-1)*4+4
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RA(NRA+1)= NNZA+1
c
            if(NRA.GT.MAX_NRA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NRA in SmartPPP.h'
               stop
            endif
c
c&&         partial derivatives for L3
c
c                drou           drou
c           ------------- ... ----------
c           d(x,y,z,c*dt)     d(lamda*N)
c
c           row no. of the coefficent matrix
c
            NRA     = (iobs-1)*2 + 2 
            RA(NRA) =  NNZA + 1
c
c&&         partial derivatives of (x,y,z) of the GPS receiver
c
            do i=1, 3
c
c           increase the total nonzero elements of array a
            NNZA    =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = -(xtrs(i)-xrcv(i))/dis
            CA(NNZA) =  (iepo-1)*4+i
c
            enddo
c
c&&         partial derivatives of clock parameters
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) =  drou_dT
            CA(NNZA) = (iepo-1)*4+4
c
c&&         partial derivatives of ambiguity parameters
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = 1.0d0
            CA(NNZA) = idx_iamb
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RA(NRA+1)= NNZA+1
c
            if(NRA+1.GT.MAX_NRA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NRA in SmartPPP.h'
               stop
            endif
c
c**      Model2:receiver clock-decoupled L3
         elseif(observ_model.EQ.2)then
c
c&&         partial derivatives for L3
c
c                drou           drou
c           ------------- ... ----------
c           d(x,y,z,c*dt)     d(lamda*N)
c
c           row no. of the coefficent matrix
c
            NRA     =  iobs 
            RA(NRA) =  NNZA + 1
c
c&&         partial derivatives of (x,y,z) of the GPS receiver
c
            do i=1, 3
c
c           increase the total nonzero elements of array a
            NNZA    =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = -(xtrs(i)-xrcv(i))/dis
            CA(NNZA) =  (iepo-1)*4+i
c
            enddo
c
c&&         partial derivatives of clock parameters
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) =  drou_dT
            CA(NNZA) = (iepo-1)*4+4
c
c&&         partial derivatives of ambiguity parameters
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = 1.0d0
            CA(NNZA) = idx_iamb
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RA(NRA+1)= NNZA+1
c
            if(NRA+1.GT.MAX_NRA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NRA in SmartPPP.h'
               stop
            endif
c
c**      Model3:Model2 + state variation constraints
         elseif(observ_model.EQ.3)then
c
c&&         partial derivatives for L3
c
c                drou           drou
c           ------------- ... ----------
c           d(x,y,z,c*dt)     d(lamda*N)
c
c           row no. of the coefficent matrix
c
            NRA     =  iobs 
            RA(NRA) =  NNZA + 1
c
c&&         partial derivatives of (x,y,z) of the GPS receiver
c
            do i=1, 3
c
c           increase the total nonzero elements of array a
            NNZA    =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = -(xtrs(i)-xrcv(i))/dis
            CA(NNZA) =  (iepo-1)*4+i
c
            enddo
c
c&&         partial derivatives of clock parameters
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) =  drou_dT
            CA(NNZA) = (iepo-1)*4+4
c
c&&         partial derivatives of ambiguity parameters
c
c           increase the total nonzero elements of array a
            NNZA     =  NNZA + 1
            if(NNZA.GE.MAX_NNZA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NNZA in SmartPPP.h'
               stop
            endif
c
             A(NNZA) = 1.0d0
            CA(NNZA) = idx_iamb
c
c           calculate the 'n+1' virtual line's first position of nnz
c
            RA(NRA+1)= NNZA+1
c
            if(NRA+1.GT.MAX_NRA)then
               write(*,*) '<coeff> error'
               write(*,*) ' enlarge MAX_NRA in SmartPPP.h'
               stop
            endif
c
c**      Model4:Model2 + clock variation constraints
         elseif(observ_model.EQ.4)then
c
            write(*,*) 'SmartPPP/coeff'
            write(*,*) 'Please modify for this observation model'
            stop
c
         endif
c
c++   GEODETIC
c
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         write(*,*) 'SmartPPP/coeff'
         write(*,*) 'Please make modification for ground receiver'
         stop
c
      else
c
      endif
c
      return
c
      end
