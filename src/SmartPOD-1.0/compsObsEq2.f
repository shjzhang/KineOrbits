c
c  subroutine compsObsEq2.f
c
      subroutine compsObsEq2(nsat,aPRN,L3,P3,flag,time,nvar,x0,A,P,L)
c
c=======================================================================
c     ****f* SmartPPP/compsObsEq2.f
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
C     $Id: compsObsEq2.f.f,v 1.0 2009/07/28 $
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
      integer       nmax
      parameter    (nmax=MAX_SAT_NUM+5)
c
c     input/output variable
c
c     input
      integer       nsat
      real*8        time
      integer       aPRN(MAX_SAT_NUM)
      integer       Flag(MAX_SAT_NUM)
      real*8        L3  (MAX_SAT_NUM)
      real*8        P3  (MAX_SAT_NUM)
c
c     output
      integer       nvar
      real*8        x0(nmax)
      real*8        x1(nmax)
      real*8        A(MAX_SAT_NUM,nvar)
      real*8        P(MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        L(MAX_SAT_NUM,1)
c
c     local         
c
      integer       iPRN1, flag1
      real*8        time1, L31, P31
      real*8        elv
c
      real*8        xrcv(3), crcv
      real*8        xtrs(3), vtrs(3), ctrs
      real*8        N3
c
c     loop
      integer       i,j,k
      integer       irec
c
      xrcv(1) = x0(1)
      xrcv(2) = x0(2)
      xrcv(3) = x0(3)
      crcv    = x0(4)
c
      do i=1,MAX_SAT_NUM
      do k=1,nvar
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
      call corr_rcv(time,xrcv)
c
      irec = 0
c
      do i=1,nsat
c
c        Initial
c
         L31   = L3(i)
         P31   = P3(i)
c
         iPRN1 = aPRN(i)
         time1 = time
         flag1 = flag(i)
c
         irec  = irec + 1
c
         N3    = x0(4+iPRN1)
c        
c**      GPS position and velocity, clock error           
c   
         call corr_trs(time1,iPRN1,xrcv,xtrs,vtrs,ctrs)           
c
c**      elvation
c
         call x_elv(xrcv,xtrs,elv)
c
c**      coefficent of the Observation Equation
c
         call coeff2 (time1,xrcv,crcv,xtrs,vtrs,ctrs,elv,iPRN1,
     &                irec,A)
c
c**      Observed Minus Computed
c
         call OMC2   (time1,xrcv,crcv,xtrs,vtrs,ctrs,elv,iPRN1,
     &                irec,L31,P31,N3,L)
c
c**      weight matrix
c
         call weight2(irec,elv,P)
c
      enddo
c
      return
c
      end
