c
c  subroutine apriori_statvar
c
      subroutine apriori_statvar(
     &           ndim,x0,
     &           NNZA,NRA,NCA,RA,CA,A,NNZP,NRP,NCP,RP,CP,P,
     &           NNZL,NRL,NCL,RL,CL,L)
c
c=======================================================================
c     ****f* SmartPPP/apriori_statvar
c
c   FUNCTION   
c   
c     Add the a priori information of state variation
c     ***********************************************
c
c   INPUTS
c
c     ndim,x0,
c     NNZA,NRA,NCA,RA,CA,A,NNZP,NRP,NCP,RP,CP,P,
c     NNZL,NRL,NCL,RL,CL,L
c
c   OUTPUT
c
c     NNZA,NRA,NCA,RA,CA,A,NNZP,NRP,NCP,RP,CP,P,
c     NNZL,NRL,NCL,RL,CL,L
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: apriori_statvar.f,v 1.0 2009/08/24 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
c     input/output
c
      integer       ndim
      real*8        x0(MAX_PMS)
c
      integer       NRA,  NRP,  NRL
      integer       NCA,  NCP,  NCL
      integer       NNZA, NNZP, NNZL
c
      integer       CA(MAX_NNZA), RA(MAX_NRA+1)
      integer       CP(MAX_NNZP), RP(MAX_NRP+1)
      integer       CL(MAX_NNZL), RL(MAX_NRL+1)
c
      real*8        A (MAX_NNZA)
      real*8        P (MAX_NNZP)
      real*8        L (MAX_NNZL)
c
c     local
c
      integer       iepo1, iepo2
      integer       time1, time2
      integer       iobs
c
      real*8        svr0(3), svr1(3)     
c
c     loop
      integer       i, k 
c
c     common
c     ******
c
      integer       NSAT, NEPO
      integer       NAMB, NREC
      integer       iSAT_iPRN(MAX_PRN), iPRN_iSAT(MAX_PRN)
      integer       NAMB_iPRN(MAX_PRN), NAMB_iSAT(MAX_PRN)
      integer       NREC_iPRN(MAX_PRN), NREC_iSAT(MAX_PRN)
c
      character*3   cPRN_iSAT(MAX_PRN)
c
      real*8        TIME_SPAN(2)
      real*8        aEPO(MAX_EPO)
c
      common /obs/  NSAT,      NEPO,     
     &              NAMB,      NREC, 
     &              iPRN_iSAT, iSAT_iPRN,
     &              NREC_iPRN, NREC_iSAT,
     &              NAMB_iPRN, NAMB_iSAT, 
     &              cPRN_iSAT, TIME_SPAN,
     &              aEPO
c
      real*8        svr(MAX_EPO,6)
      integer       nrec_svr
c
      common /svr/  svr, nrec_svr
c
c     The a priori informatin of state variation
c     ******************************************
c
      do i=1,nrec_svr
c
         do k=1,NEPO
            if(svr(i,1).EQ.aEPO(k))then
               iepo1 = k
            endif
            if(svr(i,2).EQ.aEPO(k))then
               iepo2 = k
            endif
         enddo         
c
         time1 = svr(i,1)
         time2 = svr(i,2)
c
         if(    observ_model.EQ.3)then
            iobs  = 2*NREC + (i-1)*3
         elseif(observ_model.EQ.4)then
            iobs  = 1*NREC + NAMB + (i-1)*3
         endif
c
         svr0(1) = svr(i,3)
         svr0(2) = svr(i,4)
         svr0(3) = svr(i,5)
c
         svr1(1) = x0((iepo2-1)*4+1) - x0((iepo1-1)*4+1)
         svr1(2) = x0((iepo2-1)*4+2) - x0((iepo1-1)*4+2)
         svr1(3) = x0((iepo2-1)*4+3) - x0((iepo1-1)*4+3)
c
c        write(*,*) svr0(1) - svr1(1)
c        write(*,*) svr0(2) - svr1(2)
c        write(*,*) svr0(3) - svr1(3)
c
c++      coefficent of the apriori Equation
c
         call apriori_statvar_coeff( iobs,iepo1,iepo2,
     &                               NNZA,NRA,RA,CA,A)
c
c++      Observed Minus Computed
c
         call apriori_statvar_OMC(   iobs,svr0,svr1,
     &                               NNZL,NRL,RL,CL,L)
c
c++      weight matrix
c
         call apriori_statvar_weight(iobs,
     &                               NNZP,NRP,RP,CP,P)
c
      enddo
c
      return
c
      end
