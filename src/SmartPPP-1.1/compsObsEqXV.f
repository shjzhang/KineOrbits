c
c  subroutine compsobseqxv.f
c
      subroutine compsobseqxv(nsat,aPRN,
     &                        time0,time1,cL30,cL31,cP30,cP31,
     &                        x0,ndim,xv0,q0,nobs,A,P,L)
c
c=======================================================================
c     ****f* SmartPPP/compsobseqxv.f
c
c   FUNCTION   
c   
c     compose observation equation for PPOS
c
c   INPUTS
c
c     nsat        integer          satellite number
c     Time        real*8           Time in second
c     xv0         real*8           receiver parameter
c     cL30        real*8           L3 combination of time0
c     cP30        real*8           P3 combination of time0
c     cL31        real*8           L3 combination of time1
c     cP31        real*8           P3 combination of time1
c     iPRN        integer          PRN number
c
c   OUTPUT
c
c     A           real*8           coefficent
c     P           real*8           weight
c     L           real*8           OMC
c
c   REVISION
c
c     2009/08/21                   programmed
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: compsobseqxv.f.f,v 1.0 2009/08/21 $
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
      integer       nmax
      parameter    (nmax=MAX_SAT_NUM+5)
c
c     input/output variable
c
c     input
      integer       nsat
      integer       aPRN(MAX_SAT_NUM)
      real*8        time0,time1
      real*8        cL30(MAX_SAT_NUM)
      real*8        cP30(MAX_SAT_NUM)
      real*8        cL31(MAX_SAT_NUM)
      real*8        cP31(MAX_SAT_NUM)
c
      integer       q0(MAX_SAT_NUM)
      integer       nobs
c
      real*8        x0(nmax)
c
c     output
      integer       ndim
      real*8        xv0(6)
      real*8        xv1(6)
      real*8        A(MAX_SAT_NUM,6)
      real*8        P(MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        L(MAX_SAT_NUM,1)
c
c     local         
c
      integer       iPRN 
      real*8        time, L30, L31, P30, P31
      real*8        elv, elv0, elv1
      real*8        xv(3), cv
c
      real*8        xrcv0(3), vrcv0(3), crcv0
      real*8        xrcv1(3), vrcv1(3), crcv1
      real*8        xtrs0(3), vtrs0(3), ctrs0
      real*8        xtrs1(3), vtrs1(3), ctrs1
c
      real*8        x,y,z
c
c     loop
      integer       i,j,k
      integer       iobs
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
      x = x0(1)
      y = x0(2)
      z = x0(3)
c
      xrcv0(1) = x
      xrcv0(2) = y
      xrcv0(3) = z
c
c     xrcv1 = xrcv0 + xv0
c
      xrcv1(1) = x + xv0(1)
      xrcv1(2) = y + xv0(2)
      xrcv1(3) = z + xv0(3)
      cv       =     xv0(4)
c
c     write(*,*) (xrcv0(k),k=1,3)
c     write(*,*) (xrcv1(k),k=1,3)
c
c**   receiver related corrections
c
c$    call corr_rcv(time0,xrcv0)
c$    call corr_rcv(time1,xrcv1)
c
      iobs = 0
c
      do i=1,nsat
c
         if(q0(i).EQ.0)then
c
         iobs = iobs + 1
c
         L30  = cL30(i)
         L31  = cL31(i)
c
c
c        write(*,*) L30,L31
c
         iPRN = aPRN(i)
c
c        write(*,*) iPRN
c           
c**      GPS position and velocity, clock error           
c
         call corr_trs(time0,iPRN,xrcv0,xtrs0,vtrs0,ctrs0)           
         call corr_trs(time1,iPRN,xrcv1,xtrs1,vtrs1,ctrs1)           
c
c        write(*,*) (xtrs0(k),k=1,3)
c        write(*,*) (xtrs1(k),k=1,3)
c
c**      elvation
c
         call x_elv(xrcv0,xtrs0,elv0)
         call x_elv(xrcv1,xtrs1,elv1)
c
c**      coefficent of the Observation Equation
c
c        xrcv0 is the reference point !!!!!
c
         call xv_coeff(xrcv1,xtrs1,iobs,ndim,A)
c
c**      Observed Minus Computed
c
         call xv_OMC  (time0,xrcv0,xtrs0,vtrs0,ctrs0,L30,
     &                 time1,xrcv1,xtrs1,vtrs1,ctrs1,L31,
     &                 xv,cv,iPRN,iobs,L)
c
         elv = (elv0+elv1)/2.0d0
c
c        write(*,*) 'elv',elv
c
c**      weight matrix
c
         call xv_weight(iobs,elv,P)
c
         endif
c
      enddo
c
      return
c
      end
