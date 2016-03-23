c
c  subroutine residual.f
c
      subroutine residual(nsat,aPRN,L3,P3,flag,
     &                    time,q0,ndim,x0,A,P,L)
c
c=======================================================================
c     ****f* SmartPPP/residual.f
c
c   FUNCTION   
c   
c     compose observation equation for PPOS
c
c   INPUTS
c
c     nsat        integer          satellite number
c     time        real*8           time in second
c     x0          real*8           receiver parameter
c     L3          real*8           L3 combination
c     P3          real*8           L3 combination
c     aPRN        integer          PRN number
c
c   OUTPUT
c
c     A           real*8           coefficent
c     P           real*8           weight
c     L           real*8           OMC
c
c   REVISION
c
c     2009/7/28                    programmed
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: residual.f.f,v 1.0 2009/07/28 $
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
c     input/output variable
c
c     input
      integer       nsat
      real*8        time
      integer       aPRN(MAX_SAT_NUM)
      integer       Flag(MAX_SAT_NUM)
      integer       q0  (MAX_SAT_NUM)
      real*8        L3  (MAX_SAT_NUM)
      real*8        P3  (MAX_SAT_NUM)
c
c     output
      integer       ndim
      real*8        x0(6)
      real*8        x1(6)
      real*8        A(MAX_SAT_NUM,6)
      real*8        P(MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        L(MAX_SAT_NUM,1)
c
c     local         
c
      integer       iPRN, iflag
      real*8        tL3, tP3
      real*8        elv
      real*8        xrcv(3), crcv
c
      real*8        xtrs(3), vtrs(3), ctrs
      real*8        zwd
c
c     loop
      integer       i,j,k
      integer       iobs
c
      xrcv(1) = x0(1)
      xrcv(2) = x0(2)
      xrcv(3) = x0(3)
      crcv    = x0(4)
c
      do i=1,MAX_SAT_NUM
      do k=1,6
         A(i,k) = 0.0d0
      enddo
      enddo
c
      do i=1,MAX_SAT_NUM
      do k=1,MAX_SAT_NUM
         P(i,k) = 0.0d0
      enddo
      enddo
c
      do i=1,MAX_SAT_NUM
         L(i,1) = 0.0d0
      enddo
c
c**   receiver related corrections
c
      call corr_rcv(Time,xrcv)
c
      iobs = 0
c
      do i=1,nsat
c
         tL3   = L3(i)
         tP3   = P3(i)
         iPRN  = aPRN(i)
         iflag = flag(i)
         elv   = 90.0d0
         iobs  = iobs + 1
         
         write(*,*) 'resdidual', iPRN
c        
c**      GPS position and velocity, clock error           
c   
         call corr_trs(time,iPRN,xrcv,xtrs,vtrs,ctrs)           
c
c**      elvation
c
         call x_elv(xrcv,xtrs,elv)
c
c**      coefficent of the Observation Equation
c
         call coeff1(xrcv,xtrs,iobs,ndim,A,elv,time,iPRN)
c
c**      Observed Minus Computed
c
         call OMC1(xrcv,crcv,xtrs,vtrs,ctrs,zwd,elv,
     &             time,iPRN,iobs,tP3,L)
c
c**      weight matrix
c
         call weight1(iobs,elv,P)
c
      enddo
c
      return
c
      end
