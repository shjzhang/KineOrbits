c
c   subroutine PPPLSQ
c
      subroutine PPPLSQ(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ)
c
c=======================================================================
c     ****f* SmartPPP/PPPLSQ
c
c   FUNCTION   
c   
c     PPP(Precise Point Positioning) with Batch Least Square method.   
c
c   INPUTS
c
c     rnxvsn     real*8         rinex version
c     csat_sys   characeter     returned satellite systems in rinex 
c                               file
c     nobs_typ   integer        observation types number for existed 
c                               satellite systems.
c     cobs_typ   character      observation types for existed 
c                               satellite systems.
c
c   OUTPUT
c
c     NONE
c   
c   Reference
c
c     1. Kouba, J.       2001,  Precise Point Positioning Using IGS Orbit and
c                               Clock Products.
c     2. Montenbruck, O. 2003,  In flight performance analysis of the 
c                               CHAMP BlackJack GPS receiver. 
c     3. Montenbruck, O. 2003,  Satellite Orbits: Models, Methods And
c                               Applications.
c     4. Kroes,R.        2006,  Precise Relative Positioning of Formation
c                               Flying Spacecraft using GPS.
c
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: PPPLSQ.f,v 1.0 2009/07/27 $
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
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     local variable
c    
      real*8        pos(MAX_EPO,3)
      real*8        dtr(MAX_EPO)
      real*8        Elv(MAX_EPO,MAX_SAT_EPO)
      real*8        zpd(MAX_EPO)
c
      real*8        GDOP(MAX_EPO)
c
      integer       ndim
      integer       nobs
c
      real*8         x0(MAX_PMS)
      real*8        dx0(MAX_PMS)
      real*8        Qxx(MAX_PMS)
      real*8        sig
c
      real*8           X(MAX_PMS)
      real*8          dX(MAX_PMS)
      real*8        dX12(MAX_EPO,MAX_SAT_EPO)
c
      integer       iter
      logical       convgd
c
c     observation euqation A,P,L
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
      integer       i,j,k
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
c     A,P,L
c
      do i=1, MAX_NRA
         RA(i) = 0
      enddo
c
      do i=1, MAX_NRP
         RP(i) = 0
      enddo
c
      do i=1, MAX_NRL
         RL(i) = 0
      enddo
c
      do i=1, MAX_NNZA
         A(i) = 0.0d0
        CA(i) = 0
      enddo
c
      do i=1, MAX_NNZP
         P(i) = 0.0d0
        CP(i) = 0
      enddo
c
      do i=1, MAX_NNZL
         L(i) = 0.0d0
        CL(i) = 0
      enddo
c
c     A,P,L sparse matrix   
c   
      NRA  = 0
      NCA  = 0
      NNZA = 0
      NRP  = 0
      NCP  = 0
      NNZP = 0
      NRL  = 0
      NCL  = 0
      NNZL = 0
c
c     iteration
c     =========
c
c++   ONLY Initialize the epoch-size parameter i.e. position and clock
c
      convgd = .false.
c
      call x_dim(ndim,nobs)
c
      call x_ini(ndim,x0,dx0)
c
      write(*,*)             'number of parameters to be estimated:'
      write(*,'(  (x,I6))')   ndim
c
      iter = 1
c
      do while(iter.lt.max_iter)
c
         write(*,*)   'iter times:', iter
c
         call x_update(ndim,x0,dx0)
c
         call timestamp()
c
         write(*,*)   'step 1: composing observation equation ...'
c
         rewind(111)
c
         call compsObsEq(rnxvsn,
     &                   nsat_sys,csat_sys,
     &                   nobs_typ,cobs_typ,ndim,x0,
     &                   NRA,NCA,RA,CA,A,NRP,NCP,RP,CP,P,
     &                   NRL,NCL,RL,CL,L)
c
         call timestamp()
c
         write(*,*)   'step 2: solving   observation equation ...'
c
         call solveObsEq(NRA,NCA,RA,CA,A,
     &                   NRP,NCP,RP,CP,P,NRL,NCL,RL,CL,L,
     &                   nobs,ndim,dx0,sig,Qxx)
c
         call timestamp()
c
         call x_convgd(ndim,dx0,convgd)
c   
         if(convgd)then
c
            EXIT
c
         endif
c
         iter = iter + 1
c
      enddo
c
c     write information
c     *****************
c
      write(*,*) 'iteration converged'
c
      write(*,*) 
      write(*,*) 'writing results now, please wait'
      write(*,*) '********************************'
c
      rewind(111)
c
      call x_wrt(rnxvsn,
     &           nsat_sys,csat_sys,
     &           nobs_typ,cobs_typ,ndim,x0,sig,Qxx,
     &           NRA,NCA,RA,CA,A,NRP,NCP,RP,CP,P,
     &           NRL,NCL,RL,CL,L)
c
      return
c
      end
