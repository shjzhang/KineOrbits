c
c  subroutine solvObsEq
c
      subroutine solveObsEq(NRA,NCA,RA,CA,A,
     &                      NRP,NCP,RP,CP,P,NRL,NCL,RL,CL,L,
     &                      nobs,ndim,dx0,sig,Qxx)
c
c=======================================================================
c     ****f* SmartPPP/solvObsEq.f
c
c   FUNCTION   
c   
c     compose observation equation for PPOS
c
c   INPUTS
c
c     NRA,RA,CA,A                  coefficent matrix
c     NRP,RP,CP,P                  weight matrix
c     NRL,RL,CL,L                  OMC matrix
c     ndim                         parameter dimension
c
c   OUTPUT
c
c     dx0                          parameter increment
c
c   REVISION
c
c     2009/08/05                   programmed
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: solvObsEq.f,v 1.0 2009/08/10 $
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
      integer       NRA, NCA, NNZA  
      integer       NRP, NCP, NNZP
      integer       NRL, NCL, NNZL
c
      integer       CA(MAX_NNZA), RA(MAX_NRA+1)
      real*8         A(MAX_NNZA)
c
      integer       CP(MAX_NNZP), RP(MAX_NRP+1)
      real*8         P(MAX_NNZP)
c
      integer       CL(MAX_NNZL), RL(MAX_NRL+1)
      real*8         L(MAX_NNZL)
c
      integer       nobs, ndim
      real*8        dx0(MAX_PMS)
c
c     composing normal equation
c
      integer       NRN,  NCN,  RN  (ndim+1),CN  (MAX_NNZN)
      real*8        N(MAX_NNZN)                  
      integer       NRW,  NCW,  RW  (ndim+1),CW  (MAX_NNZW)
      real*8        W(MAX_NNZW)
c
      real*8        At( MAX_NNZA)
      integer       NRAt, NCAt, RAt (ndim+1),CAt (MAX_NNZA)
c
      real*8        AtP(MAX_NNZA)
      integer       NRAtP,NCAtP,RAtP(ndim+1),CAtP(MAX_NNZA)
c
      integer       iwa(NCA)
      integer       iwp(NCP)
      integer       iwl(NCL)
c
c     solving normal equation
c
      integer       NRM,  NCM,  RM  (ndim+1),CM  (MAX_NNZN)
      real*8        M(MAX_NNZN)                  
c
      real*8        y(ndim), Qxx(MAX_PMS), U(ndim)
c
c     fspak     
      integer       available, max_node
      parameter    (available=8000000,max_node=40000)
c
      integer       flag, iout, ioor, needed,
     &              is(available), fnode(max_node), bnode(max_node),
     &              fnseg, bnseg, feqb, irank
      real*8        fx(max_node), one, zero
c
c     sig
c
      real*8        VTPV
      real*8        sig
c
c     loop
c
      integer       i, j, k
      integer       ierr
c
c     composing Normal Equation
c     *************************
c
      NRAt  = NCA
      NCAt  = NRA
c
c     compute At
c
      call csrcsc2(NRA,NCA,1,1,A,CA,RA,At,CAt,RAt)
c
      NRAtP = NRAt
      NCAtP = NCAt
c
c     compute AtP=At*P
c
      call amub(NRAt,NCP,1,At,CAt,RAt,P,  CP,  RP,
     &                                AtP,CAtP,RAtP,MAX_NNZA,iwp,ierr)
c
      if(ierr.NE.0)then
         write(*,*) '<solvObsEq> error'
         write(*,*)   ierr
         write(*,*) ' sparse matrix multiply error: amub'
         stop
      endif
c
      NRN = NRAtP
      NCN = NCA
c
c     compute N=AtP*A 
c
      call amub(NRAtP,NCA,1,AtP,CAtP,RAtP,A,CA,RA,
     &                                    N,CN,RN,MAX_NNZN,iwa,ierr)
c
      if(ierr.NE.0)then
         write(*,*) '<solvObsEq> error'
         write(*,*)   ierr
         write(*,*) ' sparse matrix multiply error: amb, AtP*A'
         stop
      endif
c
c     compute W=AtP*L
c
      NRW = NRAtP
      NCW = NCL
c
      call amub(NRAtP,NCL,1,AtP,CAtP,RAtP,L,CL,RL,
     &                                    W,CW,RW,MAX_NNZW,iwl,ierr)
c
      if(ierr.NE.0)then
         write(*,*) '<solvObsEq> error'
         write(*,*)   ierr
         write(*,*) ' sparse matrix multiply error: amb, AtP*L'
         stop
      endif
c
c     ssr of N, stored in M; right hand side y = ATPL
c     ***********************************************
c
      NRM = NRN
      NCM = NCN
c
c     N --> M
c
      call csrssr2(NRN,N,CN,RN,MAX_NNZN,M,CM,RM,ierr)
c
      if(ierr.NE.0)then
         write(*,*) ' solvObsEq error'
         write(*,*)   ierr
         write(*,*) ' sparse matrix trans error: csrssr2:N->M'
         stop
      endif
c
c     W --> y
c
      do i=1, ndim
         y(i) = W(RW(i))
      enddo
c
c     solve the sparse linear equation y = M*x with fspak
c     ***************************************************
c
      one   = 1.0
      zero  = 0.0
      iout  = 1
      ioor  = 2
c
      open(iout,file='fspak.out')
      open(ioor,file='fspak.ord')
c     -------------
c     find ordering
c     -------------
      call fspak (10,
     &            NRM,RM,CM,M,y,flag,iout,ioor,available,needed,is,
     +            fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c
      if(flag.ne.0)then
         write(*,*) 'solvObsEq'
         write(*,*) 'fspak flag .ne. 0'
         stop
      endif
c     ----------------------
c     symbolic factorization
c     ----------------------
      call fspak (20,
     +            NRM,RM,CM,M,y,flag,iout,ioor,available,needed,is,
     +            fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c
      if(flag.ne.0)then
         write(*,*) 'solvObsEq'
         write(*,*) 'fspak flag .ne. 0'
         stop
      endif
c     -----------------------
c     numerical factorization
c     -----------------------
      call fspak (40,
     +            NRM,RM,CM,M,y,flag,iout,ioor,available,needed,is,
     +            fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c
      if(flag.ne.0)then
         write(*,*) 'solvObsEq'
         write(*,*) 'fspak flag .ne. 0'
         stop
      endif
c     ----------------------------------------------------------------
c     dense vector solving
c     ----------------------
      call fspak (50,
     +            NRM,RM,CM,M,y,flag,iout,ioor,available,needed,is,
     +            fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c
      if(flag.ne.0)then
         write(*,*) 'solvObsEq'
         write(*,*) 'fspak flag .ne. 0'
         stop
      endif
c
c     return parameter increment
c
      do i=1, ndim
         dx0(i) = y(i)
      enddo
c
c     sigma
c     *****
c   
      VTPV = 0.0d0
      do i=1, nobs
         VTPV = VTPV + L(RL(i))*P(RP(i))*L(RL(i))
      enddo
c
      sig = dsqrt(VTPV/(nobs-ndim))
c     
c     covariance 
c     **********
c
      do i=1, ndim
c..   initial
      do k=1, ndim
         U(k) = 0.0d0
      enddo
      U(i) = 1.0d0
c..   solve
      call fspak (50,
     &            NRM,RM,CM,M,U,flag,iout,ioor,available,needed,is,
     &            fnode,bnode,fnseg,bnseg,fx,feqb,irank)
c..   return
      Qxx(i) = U(i)
      enddo
c
      return
c
      end
